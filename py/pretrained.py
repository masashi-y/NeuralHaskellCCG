
import os
import sys
import numpy as np
from py_utils import read_pretrained_embeddings, read_model_defs
from collections import OrderedDict

def augment_pretrained_with_random_initialization(args):
    words = OrderedDict()
    # words in pretrained word embedding
    for word in open(args.pretrained_vocab):
        words[word.strip()] = 1

    # words in specials e.g. PAD, START, END
    for word in args.specials:
        words[word] = 1

    # words found in training data
    for word, freq in read_model_defs(args.new_words).items():
        if freq >= args.freq_cut:
            words[word.encode("utf-8")] = freq

    new_pretrained_vocab = os.path.join(args.out, "new_words.txt")
    print >> sys.stderr, "writing to", new_pretrained_vocab
    with open(new_pretrained_vocab, "w") as f:
        for word, freq in words.items():
            f.write("{} {}\n".format(word, freq))

    embeddings = read_pretrained_embeddings(args.pretrained)
    assert embeddings.shape[0] <= len(words), "pretrained size: {}, read words: {}".format(embeddings.shape[0], len(words))
    new_embeddings = 0.02 * np.random.random_sample(
            (len(words), embeddings.shape[1])).astype('f') - 0.01
    for i in range(embeddings.shape[0]):
        new_embeddings[i] = embeddings[i]

    new_pretrained = os.path.join(args.out, "new_embeddings.txt")
    print >> sys.stderr, "writing to", new_pretrained
    np.savetxt(new_pretrained, new_embeddings)
    print >> sys.stderr, "vocabulary size", len(embeddings), "-->", len(new_embeddings)

def extract_subset_of_pretrained_embeddings(args):
    embeddings = read_pretrained_embeddings(args.pretrained)
    emb_words = [word.strip().decode("utf-8") for word in open(args.pretrained_vocab)]
    subset = read_model_defs(args.new_words).keys()

    new_pretrained = os.path.join(args.out, "extracted_embeddings.vector")
    new_vocab = os.path.join(args.out, "extracted_embeddings.words")
    print >> sys.stderr, "writing to", new_pretrained
    with open(new_vocab, "w") as v:
        with open(new_pretrained, "w") as f:
            for i, word in enumerate(emb_words):
                if word in subset:
                    f.write(" ".join([str(u) for u in embeddings[i]]) + "\n")
                    v.write(word.encode("utf-8") + "\n")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(
                "augment pretrained word embeddings with new entries.")

    subparsers = parser.add_subparsers()
    parser_c = subparsers.add_parser(
            "augment", help="augment")
    parser_c.add_argument("pretrained",
            help="path to pretrained word embedding file")
    parser_c.add_argument("pretrained_vocab",
            help="path to pretrained embedding vocabulary")
    parser_c.add_argument("new_words",
            help="path to file with new entries")
    parser_c.add_argument("--freq-cut", type=int, default=3,
            help="cut words in new-words with frequency less than this value")
    parser_c.add_argument("--specials", nargs="*", default=[],
            help="special tokens e.g. PAD, UNK")
    parser_c.add_argument("out",
            help="output directory")
    parser_c.set_defaults(func=augment_pretrained_with_random_initialization)

    parser_t = subparsers.add_parser(
            "extract", help="extract")
    parser_t.add_argument("pretrained",
            help="path to pretrained word embedding file")
    parser_t.add_argument("pretrained_vocab",
            help="path to pretrained embedding vocabulary")
    parser_t.add_argument("out",
            help="output directory")
    parser_t.add_argument("new_words",
            help="path to file with new entries")
    parser_t.set_defaults(func=extract_subset_of_pretrained_embeddings)

    args = parser.parse_args()
    args.func(args)
