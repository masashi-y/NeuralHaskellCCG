
{-# LANGUAGE FlexibleInstances #-}

module CCG (
    Cat(..),
    Feat,
    Combinator(..),
    isTypeRaised,
    SeenRules,
    UnaryRules,
    isSeen,
    parse,
    readCatList,
    readSeenRules,
    readUnaryRules,
    applyRules
    ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe

type Feat = String

-- ##########################################################################
-- #################################### Cat #################################
-- ##########################################################################

data Cat =
 S Feat
  | N Feat
  | NP Feat
  | PP Feat
  | Semicolon
  | Colon
  | Comma
  | Period
  | Conj
  | LRB
  | RRB
  | Fwd Cat Cat
  | Bwd Cat Cat
  deriving Ord

instance Eq Cat where
  S f1 == S f2 = f1 == f2 || null f1 || null f2
  N f1 == N f2 = f1 == f2 || null f1 || null f2
  NP f1 == NP f2 = f1 == f2 || null f1 || null f2
  PP f1 == PP f2 = f1 == f2 || null f1 || null f2
  Semicolon == Semicolon = True
  Colon == Colon = True
  Comma == Comma = True
  Period == Period = True
  Conj == Conj = True
  LRB == LRB = True
  RRB == RRB = True
  Fwd res1 arg1 == Fwd res2 arg2 = res1 == res2 && arg1 == arg2
  Bwd res1 arg1 == Bwd res2 arg2 = res1 == res2 && arg1 == arg2
  _ == _ = False

removeFeat :: Cat -> Cat
removeFeat (S _) = S ""
removeFeat (N _) = N ""
removeFeat (NP _) = NP ""
removeFeat (PP _) = PP ""
removeFeat (Fwd c1 c2) = Fwd (removeFeat c1) (removeFeat c2)
removeFeat (Bwd c1 c2) = Bwd (removeFeat c1) (removeFeat c2)
removeFeat c = c

showFeat :: Feat -> String
showFeat feat = if null feat then "" else "[" ++ feat ++ "]"

withBracket (Fwd res arg) =
     "(" ++ withBracket res ++ "/" ++ withBracket arg ++ ")"
withBracket (Bwd res arg) =
     "(" ++ withBracket res ++ "\\" ++ withBracket arg ++ ")"
withBracket cat = show cat

instance Show Cat where
    show (S feat)  = "S" ++ showFeat feat
    show (N feat)  = "N" ++ showFeat feat
    show (NP feat) = "NP" ++ showFeat feat
    show (PP feat) = "PP" ++ showFeat feat
    show Semicolon = ";"
    show Colon     = ":"
    show Comma     = ","
    show Period    = "."
    show Conj      = "conj"
    show LRB       = "LRB"
    show RRB       = "RRB"
    show (Fwd res arg) = withBracket res ++ "/" ++ withBracket arg
    show (Bwd res arg) = withBracket res ++ "\\" ++ withBracket arg

-- ##########################################################################
-- ################################ CatParser ###############################
-- ##########################################################################

data CatParser a = CatParser (String -> [(a, String)])

instance Functor CatParser where
    fmap f (CatParser p) = CatParser $ \cs -> [(f a, cs') | (a, cs') <- p cs]

-- <*>: OK?
instance Applicative CatParser where
    pure a = CatParser $ \cs -> [(a, cs)]
    (CatParser f) <*> (CatParser a) = CatParser $
     \cs -> [(f' a', cs') | (f', _) <- f cs, (a', cs')<- a cs]

instance Monad CatParser where
    return a = CatParser $ \cs -> [(a, cs)]
    (CatParser p) >>= f = CatParser $
         \cs -> concat [b cs'| (a, cs') <- p cs, let (CatParser b) = f a]

instance MonadPlus CatParser where
    mzero = CatParser $ \cs -> []
    mplus (CatParser p) (CatParser q) = CatParser $ \cs -> (p cs) ++ (q cs)

instance Alternative CatParser where
    empty = mzero
    p <|> q = CatParser $ \cs -> let (CatParser r) = p `mplus` q in
                             case r cs of
                                [] -> []
                                (c:cs') -> [c]

item :: CatParser Char
item = CatParser $ \cs -> case cs of
        "" -> []
        c:cs' -> [(c, cs')]

satisfy :: (Char -> Bool) -> CatParser Char
satisfy cond = do
    c <- item
    if cond c then return c
         else mzero

char :: Char -> CatParser Char
char c = satisfy (c==)

string :: String -> CatParser String
string "" = return ""
string (c:cs) = do
    char c
    string cs
    return $ c:cs

satisfyString :: (Char -> Bool) -> CatParser String
satisfyString cond = satisfyString' cond ""
    where satisfyString' cond res = do
            c <- item
            if cond c then return res
                    else satisfyString' cond (res ++ c:[]) -- TODO

feat = feat' <|> return ""
    where feat' = do
            char '['
            res <- satisfyString (']'==)
            return res

atomic = do
        cat <- s <|> np <|> n <|> pp
        f <- feat
        return $ cat f
    where s  = string "S"  >> return S
          n  = string "N"  >> return N
          np = string "NP" >> return NP
          pp = string "PP" >> return PP

cat = functor
    <|> atomic
    <|> (char ';' >> return Semicolon)
    <|> (char ':' >> return Colon)
    <|> (char ',' >> return Comma)
    <|> (char '.' >> return Period)
    <|> (string "conj" >> return Conj)
    <|> (string "LRB"  >> return LRB)
    <|> (string "RRB"  >> return RRB)
    where functor = do
            res <- atomic <|> functor' <|> conj
            dir <- slash
            arg <- atomic <|> functor' <|> conj

            return $ dir res arg
          functor' = do
            char '('
            f <- functor
            char ')'
            return f
          slash = (char '/' >> return Fwd) <|> (char '\\' >> return Bwd)
          conj = (string "conj" >> return Conj)

isBracketed :: String -> Bool
isBracketed input = not $ snd $ foldl check (0, False) $ init input
    where check (depth, res) next = (depth', res')
            where res' = if res then True else depth' == 0
                  depth' = case next of
                      '(' -> depth + 1
                      ')' -> depth - 1
                      _   -> depth

dropBrackets input = if ("(" `isPrefixOf` input) &&
             (")" `isSuffixOf` input) && (isBracketed input) then tail $ init input else input

parse :: String -> Cat
parse input = case cat' $ dropBrackets input of
                [(c, rest@(_:_))] -> error $ "Failed to parse: " ++ input ++ " " ++ rest
                [(c, _)] -> c
                _ -> error $ "Failed to parse: " ++ input
    where CatParser cat' = cat

parseMaybe :: String -> Maybe Cat
parseMaybe input = case cat' $ dropBrackets input of
                [(c, _)] -> Just c
                _        -> Nothing
    where CatParser cat' = cat

isTypeRaised :: Cat -> Bool
isTypeRaised (Fwd (Bwd res arg) _) = res == arg
isTypeRaised (Bwd (Fwd res arg) _) = res == arg
isTypeRaised _ = False

isPunct :: Cat -> Bool
isPunct Comma     = True
isPunct Period    = True
isPunct Semicolon = True
isPunct Colon     = True
isPunct Conj      = True
isPunct _         = False

-- ##########################################################################
-- ########################### Combinatory Rules ############################
-- ##########################################################################

data Combinator =
    FwdApp
    | BwdApp
    | FwdCmp
    | BwdCmp
    | GenFwdCmp
    | GenBwdCmp
    | CNJ          -- Conjunction
    | LRP          -- LeftRemovePunctuation
    | RRP          -- RightRemovePunctuation
    | CommaVPtoADV -- CommaAndVerbPhraseToAdverb
    | Intro
    | Unary
    deriving (Eq, Ord)

instance Show Combinator where
    show FwdApp       = ">"
    show BwdApp       = "<"
    show FwdCmp       = ">B"
    show BwdCmp       = "<B"
    show GenFwdCmp    = ">B1"
    show GenBwdCmp    = "<B1"
    show CNJ          = "<P>" --"<Φ>"
    show LRP          = "<rp>"
    show RRP          = "<rp>"
    show CommaVPtoADV = "<*>"
    show Intro        = "<*>"
    show Unary        = "<u>"


apply :: Combinator -> Cat -> Cat -> [Cat]
apply FwdApp (Fwd res arg) x = [res | arg == x]
apply FwdApp _ _ = []

apply BwdApp x (Bwd res arg) = [res | arg == x]
apply BwdApp _ _ = []

apply FwdCmp (Fwd res arg) (Fwd res' arg') = [Fwd res arg' | arg == res']
apply FwdCmp _ _ = []

apply BwdCmp (Fwd res arg) (Bwd res' arg') = [Fwd res' arg | arg' == res]
apply BwdCmp _ _ = []

apply GenFwdCmp (Fwd res arg) (Fwd (Fwd res' arg') arg'') = [Fwd (Fwd res arg') arg'' | arg == res']
apply GenFwdCmp _ _ = []

apply GenBwdCmp (Bwd (Bwd res arg) arg') (Bwd res' arg'') = [Bwd (Bwd arg'' arg) arg' | arg'' == res]
apply GenBwdCmp _ _ = []

apply CNJ _ (Fwd (NP _) (NP _)) = []
apply CNJ _ (N _) = []
apply CNJ res arg = [Bwd arg arg |
    (isPunct res) && not (isPunct arg) && not (isTypeRaised arg)]

apply CommaVPtoADV Comma arg = [parse "(S\\NP)\\(S\\NP)" | verbPhrase arg ]
    where verbPhrase (Bwd (S f) (NP _)) = f == "ng" || f == "pss"
          verbPhrase _ = False
apply CommaVPtoADV _ _ = []

apply LRP res arg = [res | isPunct arg]
apply RRP res arg = [arg | isPunct res]

applyRules :: Cat -> Cat -> [(Combinator, Cat)]
applyRules a b = do
    op <- [FwdApp, BwdApp, FwdCmp, BwdCmp, GenFwdCmp,
                         GenBwdCmp, CNJ, LRP, RRP, CommaVPtoADV]
    res <- apply op a b
    return $ (op, res)


-- ##########################################################################
-- ############################## File Readers ##############################
-- ##########################################################################

type SeenRules = S.Set (Cat, Cat)
type UnaryRules = M.Map Cat Cat

isSeen :: (Cat, Cat) -> SeenRules -> Bool
isSeen (c1, c2) = S.member (removeFeat c1, removeFeat c2)

isComment :: String -> Bool
isComment ""      = True
isComment ('#':_) = True
isComment _       = False

readCatList :: String -> IO [Cat]
readCatList fileName = do
    items <- fmap lines $ readFile fileName
    return $ map (parse . head . words) items

readSeenRules :: String -> IO SeenRules
readSeenRules fileName = do
    items <- fmap lines $ readFile fileName
    let filtered = filter (not . isComment) items
        -- takes care of noisy seen_rules.txt
        parse' a b = case (parseMaybe a, parseMaybe b) of
            (Just a', Just b') -> Just (removeFeat a', removeFeat b')
            _                  -> Nothing
    return $ S.fromList $
        mapMaybe (\item -> let (a:b:_) = words item in parse' a b) filtered

readUnaryRules :: String -> IO UnaryRules
readUnaryRules fileName = do
    items <- fmap lines $ readFile fileName
    let filtered = filter (not . isComment) items
    return $ M.fromList $
        map (\item -> let (a:b:_) = words item in (parse a, parse b)) filtered

main = do
    let input = "NP/NP"
    print $ parse "S[dcl]"
    print $ parse "S"
    print $ parse "S[dcl]/S[dcl]"
    print $ parse "NP[nb]/N"
    print $ parse "(NP[nb]/N)"
    res <- readCatList "/Users/masashi-y/depccg/models/tri_headfirst/target.txt"
    return ()
