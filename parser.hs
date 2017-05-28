
import qualified Data.Map as M
import Data.Maybe
import Supertag
import CCG            hiding (parse)
import Data.List      (maximumBy)
import Control.Monad  (guard, forM_, foldM)
import GHC.Exts       ( sortWith )
import Data.Maybe
import System.FilePath
import System.Directory   (doesDirectoryExist)
import System.Environment (getArgs)
import Text.Printf

data ParseTree = Leaf String | (Cat, Combinator) :^ [ParseTree]
             deriving (Eq, Ord)

instance Show ParseTree where
    show (Leaf a)          = show a
    show ((c, _) :^ trees) = show c ++ "^" ++ show trees

terminals :: ParseTree -> [String]
terminals (Leaf s) = [s]
terminals (_ :^ children) = concat $ map terminals children

preterminals :: ParseTree -> [Cat]
preterminals (Leaf _) = []
preterminals ((cat, _) :^ [Leaf _]) = [cat]
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
          showDerivation' ((cat, _) :^ [Leaf word]) lwidth = return $
               max lwidth (2 + lwidth + max (length $ show cat) (length word))
          showDerivation' ((cat, op) :^ children) lwidth = do
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

parse :: Int -> SeenRules -> UnaryRules -> Lexicon -> [(ParseTree, Float)]
parse nBest seenRules unaryRules input
    | size == ncell = take nBest $ sortWith (negate . snd) $ filter (isPossibleRoot . fst) $ M.elems cell
    | otherwise     = []
      where (size, vectors) = foldl nextInputToken (0, []) input
            (ncell, cell) = last (last vectors)
            isPossibleRoot ((cat, _) :^ _) = cat `elem` [S "dcl", S "wq", S "q", S "qem", NP ""]

            nextInputToken :: (Int, [Vector]) -> (String, [(Cat, Float)]) -> (Int, [Vector])
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

            terminalCell :: (String, [(Cat, Float)]) -> Cell
            terminalCell (term, supertags) = M.fromList $ do
                        (c, score) <- supertags
                        let preterminal = (c, Intro):^[Leaf term]
                        case M.lookup c unaryRules of
                            Just c' -> let unaryTree = (c', Unary):^[preterminal]
                                           score' = score - 0.1
                                       in [(c, (preterminal, score)), (c', (unaryTree, score'))]
                            Nothing -> return $ (c, (preterminal, score))

            binProduct :: Cell -> Cell -> Cell
            binProduct acell bcell = M.unionsWith maxByScore $ do
                        (a, (atree, ascore)) <- M.toList acell
                        (b, (btree, bscore)) <- M.toList bcell
                        guard $ (a, b) `isSeen` seenRules
                        (op, c) <- applyRules a b
                        let (_, aOp) :^ _ = atree
                            (_, bOp) :^ _ = btree
                            res   = (c, op):^[atree, btree]
                            score = ascore + bscore
                        -- guard $ isNormalForm op aOp bOp a b
                        case M.lookup c unaryRules of
                            Just c' -> let res' = (c', Unary):^[res]
                                           score' = score - 0.1  -- unary penalty
                                in return $ M.fromList [(c, (res, score)), (c', (res', score'))]
                            Nothing -> return $ M.singleton c (res, score)

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
    forM_ lexicon $ \(word, supertags) -> do
        let cats = unwords $ map (\(a, b) -> printf "%s -> %4.3f" (show a) b) $ take 5 $ supertags
        putStrLn $ word ++ "\t-->\t" ++ cats

main :: IO ()
main = do
    (modelPath:_) <- getArgs
    isValidModelDir <- doesDirectoryExist modelPath
    if not isValidModelDir then
        error $ "Model not found: " ++ modelPath else return ()
    let targetPath    = modelPath </> "target.txt"
        seenRulePath  = modelPath </> "seen_rules.txt"
        unaryRulePath = modelPath </> "unary_rules.txt"
    input <- getLine
    supertags <- readCatList targetPath
    seenRules <- readSeenRules seenRulePath
    unaryRules <- readUnaryRules unaryRulePath
    probs <- assignSupertags modelPath 30 supertags input
    showOneBestTags probs
    let [(res, score)] = parse 1 seenRules unaryRules probs
    showDerivation $ res
    putStrLn $ "Probability: " ++ (show $ exp score)

