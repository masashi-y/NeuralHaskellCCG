
from __future__ import print_function, unicode_literals
import sys
import numpy as np
import json
import random
import chainer
import chainer.links as L
import chainer.functions as F
from chainer import cuda
from chainer import training, Variable
from chainer.training import extensions
from chainer.optimizer import WeightDecay, GradientClipping
from py.ccgbank import walk_autodir
from py.japanese_ccg import JaCCGReader
from collections import defaultdict, OrderedDict
from py.py_utils import read_pretrained_embeddings, read_model_defs
from py.tree import Leaf, Tree, get_leaves
from py.biaffine import Biaffine, Bilinear
from py.param import Param
from py.my_updater import MyUpdater, MyEvaluator

from py.lstm_tagger import UNK, OOR2, OOR3, OOR4, START, END, IGNORE, MISS
from py.lstm_tagger import log, get_suffix, get_prefix, normalize
from py.lstm_parser import TrainingDataCreator, FeatureExtractor, LSTMParserDataset
from py.lstm_parser import LSTMParser, LSTMParserTriTrainDataset

def scanl(f, base, l):
    res = [base]
    acc = base
    for x in l:
        acc = f(acc, x)
        res += [acc]
    return res


class FastBiaffineLSTMParser(chainer.Chain):
    ## chainer.links.Bilinear may have some problem with GPU
    ## and results in nan with batches with big size
    ## this implementation uses different implementation of bilinear
    ## and does not run into nan.
    def __init__(self, model_path, word_dim=None, afix_dim=None, nlayers=2,
            hidden_dim=128, dep_dim=100, dropout_ratio=0.5):
        self.model_path = model_path
        defs_file = model_path + "/tagger_defs.txt"
        if word_dim is None:
            self.train = False
            Param.load(self, defs_file)
            self.extractor = FeatureExtractor(model_path)
        else:
            # training
            self.train = True
            p = Param(self)
            p.dep_dim = dep_dim
            p.word_dim = word_dim
            p.afix_dim = afix_dim
            p.hidden_dim = hidden_dim
            p.nlayers = nlayers
            p.n_words = len(read_model_defs(model_path + "/words.txt"))
            p.n_suffixes = len(read_model_defs(model_path + "/suffixes.txt"))
            p.n_prefixes = len(read_model_defs(model_path + "/prefixes.txt"))
            p.targets = read_model_defs(model_path + "/target.txt")
            p.dump(defs_file)

        self.in_dim = self.word_dim + 8 * self.afix_dim
        self.dropout_ratio = dropout_ratio
        super(FastBiaffineLSTMParser, self).__init__(
                emb_word=L.EmbedID(self.n_words, self.word_dim, ignore_label=IGNORE),
                emb_suf=L.EmbedID(self.n_suffixes, self.afix_dim, ignore_label=IGNORE),
                emb_prf=L.EmbedID(self.n_prefixes, self.afix_dim, ignore_label=IGNORE),
                lstm_f=L.NStepLSTM(self.nlayers, self.in_dim, self.hidden_dim, 0.32),
                lstm_b=L.NStepLSTM(self.nlayers, self.in_dim, self.hidden_dim, 0.32),
                arc_dep=L.Linear(2 * self.hidden_dim, self.dep_dim),
                arc_head=L.Linear(2 * self.hidden_dim, self.dep_dim),
                rel_dep=L.Linear(2 * self.hidden_dim, self.dep_dim),
                rel_head=L.Linear(2 * self.hidden_dim, self.dep_dim),
                biaffine_arc=Biaffine(self.dep_dim),
                biaffine_tag=Bilinear(self.dep_dim, self.dep_dim, len(self.targets))
                )

    def load_pretrained_embeddings(self, path):
        self.emb_word.W.data = read_pretrained_embeddings(path)

    def __call__(self, xs):
        """
        xs [(w,s,p,y), ..., ]
        w: word, c: char, l: length, y: label
        """
        batchsize = len(xs)

        if len(xs[0]) == 5:
            ws, ss, ps, cat_ts, dep_ts = zip(*xs)
            xp = chainer.cuda.get_array_module(ws[0])
            weights = [xp.array(1, 'f') for _ in xs]
        else:
            ws, ss, ps, cat_ts, dep_ts, weights = zip(*xs)

        cat_ys, dep_ys = self.forward(ws, ss, ps, dep_ts if self.train else None)

        cat_loss = reduce(lambda x, y: x + y,
            [we * F.softmax_cross_entropy(y, t) \
                    for y, t, we  in zip(cat_ys, cat_ts, weights)])
        cat_acc = reduce(lambda x, y: x + y,
            [F.accuracy(y, t, ignore_label=IGNORE) \
                for y, t in zip(cat_ys, cat_ts)]) / batchsize

        dep_loss = reduce(lambda x, y: x + y,
            [we * F.softmax_cross_entropy(y, t) \
                    for y, t, we in zip(dep_ys, dep_ts, weights)])
        dep_acc = reduce(lambda x, y: x + y,
            [F.accuracy(y, t, ignore_label=IGNORE) \
                    for y, t in zip(dep_ys, dep_ts)]) / batchsize

        chainer.report({
            "tagging_loss": cat_loss,
            "tagging_accuracy": cat_acc,
            "parsing_loss": dep_loss,
            "parsing_accuracy": dep_acc
            }, self)
        return cat_loss + dep_loss

    def forward(self, ws, ss, ps, dep_ts=None):
        batchsize = len(ws)
        xp = chainer.cuda.get_array_module(ws[0])
        split = scanl(lambda x,y: x+y, 0, [w.shape[0] for w in ws])[1:-1]

        wss = self.emb_word(F.hstack(ws))
        sss = F.reshape(self.emb_suf(F.vstack(ss)), (-1, 4 * self.afix_dim))
        pss = F.reshape(self.emb_prf(F.vstack(ps)), (-1, 4 * self.afix_dim))
        ins = F.dropout(F.concat([wss, sss, pss]), self.dropout_ratio, train=self.train)

        xs_f = list(F.split_axis(ins, split, 0))
        xs_b = [x[::-1] for x in xs_f]
        cx_f, hx_f, cx_b, hx_b = self._init_state(xp, batchsize)
        _, _, hs_f = self.lstm_f(hx_f, cx_f, xs_f, train=self.train)
        _, _, hs_b = self.lstm_b(hx_b, cx_b, xs_b, train=self.train)
        hs_b = [x[::-1] for x in hs_b]
        # ys: [(sentence length, number of category)]
        hs = [F.concat([h_f, h_b]) for h_f, h_b in zip(hs_f, hs_b)]

        dep_ys = [self.biaffine_arc(
            F.elu(F.dropout(self.arc_dep(h), 0.32, train=self.train)),
            F.elu(F.dropout(self.arc_head(h), 0.32, train=self.train))) for h in hs]

        # if dep_ts is not None and random.random >= 0.5:
        if dep_ts is not None:
            heads = dep_ts
        else:
            heads = [F.argmax(y, axis=1) for y in dep_ys]

        heads = F.elu(F.dropout(
            self.rel_head(
                F.vstack([F.embed_id(t, h, ignore_label=IGNORE) \
                        for h, t in zip(hs, heads)])),
            0.32, train=self.train))

        childs = F.elu(F.dropout(self.rel_dep(F.vstack(hs)), 0.32, train=self.train))
        cat_ys = self.biaffine_tag(childs, heads)

        cat_ys = list(F.split_axis(cat_ys, split, 0))

        return cat_ys, dep_ys

    def predict_doc(self, doc, batchsize=16):
        """
        doc list of splitted sentences
        """
        res = []
        for i in xrange(0, len(doc), batchsize):
            res.extend([(i + j, 0, y)
                for j, y in enumerate(self.predict(doc[i:i + batchsize]))])
        return res

    def _init_state(self, xp, batchsize):
        res = [Variable(xp.zeros(( # forward cx, hx, backward cx, hx
                self.nlayers, batchsize, self.hidden_dim), 'f')) for _ in range(4)]
        return res

    @property
    def cats(self):
        return zip(*sorted(self.targets.items(), key=lambda x: x[1]))[0]


def converter(xs, device):
    if device is None:
        return xs
    elif device < 0:
        return map(lambda x: map(lambda m: cuda.to_cpu(m), x), xs)
    else:
        return map(lambda x: map(
            lambda m: cuda.to_gpu(m, device, cuda.Stream.null), x), xs)


def train(args):
    model = FastBiaffineLSTMParser(
            args.model, args.word_emb_size, args.afix_emb_size, args.nlayers,
            args.hidden_dim, args.dep_dim, args.dropout_ratio)
    with open(args.model + "/params", "w") as f: log(args, f)

    if args.initmodel:
        print('Load model from', args.initmodel)
        chainer.serializers.load_npz(args.initmodel, model)

    if args.pretrained:
        print('Load pretrained word embeddings from', args.pretrained)
        model.load_pretrained_embeddings(args.pretrained)

    if args.gpu >= 0:
        chainer.cuda.get_device(args.gpu).use()
        model.to_gpu()

    if args.tritrain is not None:
        train = LSTMParserTriTrainDataset(
                args.model, args.train, args.tritrain, args.tri_weight)
    else:
        train = LSTMParserDataset(args.model, args.train)

    train_iter = chainer.iterators.SerialIterator(train, args.batchsize)
    val = LSTMParserDataset(args.model, args.val)
    val_iter = chainer.iterators.SerialIterator(
            val, args.batchsize, repeat=False, shuffle=False)
    optimizer = chainer.optimizers.Adam(beta2=0.9)
    # optimizer = chainer.optimizers.MomentumSGD(momentum=0.7)
    optimizer.setup(model)
    optimizer.add_hook(WeightDecay(2e-6))
    optimizer.add_hook(GradientClipping(15.))
    updater = MyUpdater(train_iter, optimizer,
            device=args.gpu, converter=converter)
    trainer = training.Trainer(updater, (args.epoch, 'epoch'), args.model)

    val_interval = 1000, 'iteration'
    log_interval = 200, 'iteration'

    eval_model = model.copy()
    eval_model.train = False

    trainer.extend(extensions.ExponentialShift(
                    "eps", .75, init=2e-3, optimizer=optimizer), trigger=(2500, 'iteration'))
    trainer.extend(MyEvaluator(val_iter, eval_model,
                    converter, device=args.gpu), trigger=val_interval)
    trainer.extend(extensions.snapshot_object(
        model, 'model_iter_{.updater.iteration}'), trigger=val_interval)
    trainer.extend(extensions.LogReport(trigger=log_interval))
    trainer.extend(extensions.observe_lr(observation_key="eps"), trigger=log_interval)
    trainer.extend(extensions.PrintReport([
        'epoch', 'iteration',
        'main/tagging_accuracy', 'main/tagging_loss',
        'main/parsing_accuracy', 'main/parsing_loss',
        'validation/main/tagging_accuracy',
        'validation/main/parsing_accuracy',
        'eps'
    ]), trigger=log_interval)
    trainer.extend(extensions.ProgressBar(update_interval=10))

    trainer.run()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(
                "CCG parser's LSTM supertag tagger")
    subparsers = parser.add_subparsers()

    # Creating training data
    parser_c = subparsers.add_parser(
            "create", help="create tagger input data")
    parser_c.add_argument("path",
            help="path to ccgbank data file")
    parser_c.add_argument("out",
            help="output directory path")
    parser_c.add_argument("--cat-freq-cut",
            type=int, default=10,
            help="only allow categories which appear >= freq-cut")
    parser_c.add_argument("--word-freq-cut",
            type=int, default=5,
            help="only allow words which appear >= freq-cut")
    parser_c.add_argument("--afix-freq-cut",
            type=int, default=5,
            help="only allow afixes which appear >= freq-cut")
    parser_c.add_argument("--subset",
            choices=["train", "test", "dev", "all"],
            default="train")
    parser_c.add_argument("--mode",
            choices=["train", "test"],
            default="train")

    parser_c.set_defaults(func=
            (lambda args:
                TrainingDataCreator.create_traindata(args)
                    if args.mode == "train"
                else  TrainingDataCreator.create_testdata(args)))

            #TODO updater
    # Do training using training data created through `create`
    parser_t = subparsers.add_parser(
            "train", help="train supertagger model")
    parser_t.add_argument("model",
            help="path to model directory")
    parser_t.add_argument("--gpu", type=int, default=-1,
            help="path to model directory")
    parser_t.add_argument("train",
            help="training data file path")
    parser_t.add_argument("--tritrain",
            help="tri-training data file path")
    parser_t.add_argument("--tri-weight",
            type=float, default=0.4,
            help="multiply tri-training sample losses")
    parser_t.add_argument("val",
            help="validation data file path")
    parser_t.add_argument("--batchsize",
            type=int, default=16, help="batch size")
    parser_t.add_argument("--epoch",
            type=int, default=20, help="epoch")
    parser_t.add_argument("--word-emb-size",
            type=int, default=50,
            help="word embedding size")
    parser_t.add_argument("--afix-emb-size",
            type=int, default=32,
            help="character embedding size")
    parser_t.add_argument("--nlayers",
            type=int, default=1,
            help="number of layers for each LSTM")
    parser_t.add_argument("--hidden-dim",
            type=int, default=128,
            help="dimensionality of hidden layer")
    parser_t.add_argument("--dep-dim",
            type=int, default=100,
            help="dim")
    parser_t.add_argument("--dropout-ratio",
            type=float, default=0.5,
            help="dropout ratio")
    parser_t.add_argument("--initmodel",
            help="initialize model with `initmodel`")
    parser_t.add_argument("--pretrained",
            help="pretrained word embeddings")
    parser_t.set_defaults(func=train)

    args = parser.parse_args()

    args.func(args)
