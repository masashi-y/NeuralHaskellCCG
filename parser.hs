
import qualified Data.Map as M
import Data.Maybe
import Supertag
import CCG            hiding (parse)
import Data.List      (maximumBy)
import Control.Monad  (guard, forM_, foldM, when)
import GHC.Exts       ( sortWith )
import Data.Maybe
import System.FilePath
import System.Directory   (doesDirectoryExist)
import System.Environment (getArgs)
import Text.Printf
import qualified Data.Array.Unboxed as U

type HeadIndex = Int
data ParseTree = Leaf String
               | (Cat, Combinator, HeadIndex) :^ [ParseTree]
             deriving (Eq, Ord)

instance Show ParseTree where
    show (Leaf a)          = show a
    show ((c, _, _) :^ trees) = show c ++ "^" ++ show trees

terminals :: ParseTree -> [String]
terminals (Leaf s) = [s]
terminals (_ :^ children) = concat $ map terminals children

preterminals :: ParseTree -> [Cat]
preterminals (Leaf _) = []
preterminals ((cat, _, _) :^ [Leaf _]) = [cat]
preterminals (_ :^ children) = concat $ map preterminals children

-- ##########################################################################
-- ############################## Print Function ############################
-- ##########################################################################

showDerivation :: ParseTree -> IO ()
showDerivation tree = do
    putStrLn wordStr
    putStrLn catStr
    showDerivation' tree 0
    putStrLn ""
    where words = terminals tree
          cats = map show $ preterminals tree
          (catStr, wordStr) = foldl terminalString ("", "") $ zip cats words

          terminalString (catS, wordS) (cat, word) = (catS', wordS')
            where nextlen = 2 + max (length cat) (length word)
                  lcatlen = (nextlen - length cat) `div` 2
                  rcatlen = lcatlen + (nextlen - length cat) `mod` 2
                  lwordlen = (nextlen - length word) `div` 2
                  rwordlen = lwordlen + (nextlen - length word) `mod` 2
                  catS' = catS ++ (whiteSpace lcatlen) ++ cat ++ (whiteSpace rcatlen)
                  wordS' = wordS ++ (whiteSpace lwordlen) ++ word ++ (whiteSpace rwordlen)

          showDerivation' :: ParseTree -> Int -> IO Int
          showDerivation' ((cat, _, _) :^ [Leaf word]) lwidth = return $
               max lwidth (2 + lwidth + max (length $ show cat) (length word))
          showDerivation' ((cat, op, _) :^ children) lwidth = do
              rwidth <- foldM (\lw child -> do
                     rw <- showDerivation' child lw; return $ max lw rw) lwidth children
              let pad = (rwidth - lwidth - (length $ show cat)) `div` 2 + lwidth
              putStrLn $ whiteSpace lwidth ++ repeatChar (rwidth - lwidth) '-' ++ show op
              putStrLn $ whiteSpace pad ++ show cat
              return rwidth

          repeatChar n c = take n $ repeat c
          whiteSpace n   = repeatChar n ' '

-- ##########################################################################
-- ############################### CKY Parsing ##############################
-- ##########################################################################

type Cell   = M.Map Cat (ParseTree, Float)
type Vector = [(Int, Cell)]

(??) :: Ord s => M.Map s [a] -> s -> [a]
m ?? s = fromMaybe [] (M.lookup s m)

parse :: Int -> SeenRules -> UnaryRules -> Lexicon -> DependencyMatrix -> [(ParseTree, Float)]
parse nBest seenRules unaryRules input depMat
    | size == ncell = take nBest $ sortWith (negate . snd) $ filter (isPossibleRoot . fst) $ M.elems cell
    | otherwise     = []
      where (size, vectors) = foldl nextInputToken (0, []) input
            (ncell, cell) = last (last vectors)
            isPossibleRoot ((cat, _, _) :^ _) = cat `elem` [S "dcl", S "wq", S "q", S "qem", NP ""]

            nextInputToken :: (Int, [Vector]) -> (Int, String, [(Cat, Float)]) -> (Int, [Vector])
            nextInputToken (size, vectors) token = (size', vectors')
              where size'    = size + 1
                    vectors' = [(size', cell)] : updateVectors vectors [(size, cell)] size size'
                    cell     = terminalCell token

            updateVectors :: [Vector] -> Vector -> Int -> Int -> [Vector]
            updateVectors [] _ _ _ = []
            updateVectors (row:rows) col nrow ncol 
                | scProduct == M.empty =  row                    : updateVectors rows                  col  nrow' ncol
                | otherwise            = (row++[(ncol,scProduct)]) : updateVectors rows ((nrow',scProduct):col) nrow' ncol
                where scProduct = scalarProduct row col
                      nrow'   = nrow - 1

            scalarProduct :: Vector -> Vector -> Cell
            scalarProduct [] _ = M.empty
            scalarProduct _ [] = M.empty
            scalarProduct as@((i,a):as') bs@((j,b):bs')
                = case compare i j of
                    LT -> scalarProduct as' bs
                    GT -> scalarProduct as  bs'
                    EQ -> scalarProduct as' bs' `joinCell` binProduct a b

            joinCell :: Cell -> Cell -> Cell
            joinCell a b = M.unionsWith maxByScore [a, b]

            terminalCell :: (Int, String, [(Cat, Float)]) -> Cell
            terminalCell (index, term, supertags) = M.fromList $ do
                        (c, score) <- supertags
                        let preterminal = (c, Intro, index):^[Leaf term]
                        (c, (preterminal, score)):unaryTrees preterminal (score - 0.1)

            binProduct :: Cell -> Cell -> Cell
            binProduct acell bcell = M.unionsWith maxByScore $ do
                        (a, (atree, ascore)) <- M.toList acell
                        (b, (btree, bscore)) <- M.toList bcell
                        guard $ (a, b) `isSeen` seenRules
                        (op, c) <- applyRules a b
                        let (_, aOp, aIndex) :^ _ = atree
                            (_, bOp, bIndex) :^ _ = btree
                            parse = (c, op, aIndex):^[atree, btree]
                            score = ascore + bscore + (depMat U.! (bIndex, aIndex))
                        -- guard $ isNormalForm op aOp bOp a b
                        return $ M.fromList $ (c, (parse, score)):unaryTrees parse (score - 0.1)

            unaryTrees child@((cat, _, index):^_) score = [(c', (parse, score)) | c' <- (unaryRules ?? cat),
                                                        let parse = (c', Unary, index):^[child]]
            maxByScore a@(_, s1) b@(_, s2) = if s1 >= s2 then a else b


-- isNormalForm op leftOp rightOp
isNormalForm op leftOp rightOp leftCat rightCat
    | forwardComp leftOp && forward op       = False
    | backwardComp rightOp && backward op    = False
    | op == FwdApp && isTypeRaised leftCat   = False
    | op == BwdApp && isTypeRaised rightCat  = False
    | rightOp == CNJ && isTypeRaised leftCat = False
    | leftOp == FwdCmp && forwardComp op     = False
    | rightOp == BwdCmp && backwardComp op   = False
    | op == LRP && isTypeRaised rightCat     = False
    | op == RRP && isTypeRaised leftCat      = False
    | leftOp == CNJ                          = False
    | otherwise = True
    where forward      = (`elem` [FwdApp, FwdCmp, GenFwdCmp])
          backward     = (`elem` [BwdApp, BwdCmp, GenBwdCmp])
          forwardComp  = (`elem` [FwdCmp, GenFwdCmp])
          backwardComp = (`elem` [BwdCmp, GenBwdCmp])

showOneBestTags :: Lexicon -> IO ()
showOneBestTags lexicon = do
    forM_ lexicon $ \(_, word, supertags) -> do
        let cats = unwords $ map (\(a, b) -> printf "%s -> %4.3f" (show a) b) $ take 5 $ supertags
        putStrLn $ word ++ "\t-->\t" ++ cats

main :: IO ()
main = do
    (modelPath:_) <- getArgs
    isValidModelDir <- doesDirectoryExist modelPath
    when (not isValidModelDir) $
        fail $ "Model not found: " ++ modelPath
    let targetPath    = modelPath </> "target.txt"
        seenRulePath  = modelPath </> "seen_rules.txt"
        unaryRulePath = modelPath </> "unary_rules.txt"
    sents <- lines <$> getContents
    supertags <- readCatList targetPath
    seenRules <- readSeenRules seenRulePath
    unaryRules <- readUnaryRules unaryRulePath
    taggedSents <- assignCatsAndDeps modelPath 20 supertags sents
    let parse' = parse 1 seenRules unaryRules
        res = map (uncurry parse') taggedSents
    forM_ res $ \[(res, score)] -> do
        showDerivation res
        putStrLn $ "Probability: " ++ (show $ exp score)
