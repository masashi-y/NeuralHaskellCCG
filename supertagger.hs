
module Supertag (
    Lexicon,
    DependencyMatrix,
    assignSupertags,
    assignCatsAndDeps,
    ) where

import Control.Exception     ( bracket )
import Foreign.C.Types       ( CInt(..), CChar(..), CFloat(..) )
import Foreign.C.String      ( withCString, newCString, CString )
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array
import Foreign.Storable      ( Storable(..) )
import Foreign.Ptr           ( Ptr(..) )
import System.Posix.Env      ( putEnv )
import CCG
import Control.Monad         ( forM )
import GHC.Exts              ( sortWith )
import Data.Array.Unboxed    ( array, UArray )

foreign import ccall "tag_and_parse_doc" cTagAndParseDoc :: CString -> Ptr CString -> Ptr CInt -> CInt -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> IO ()
foreign import ccall "tag" cTag :: Ptr CChar -> Ptr CChar -> CInt -> Ptr CFloat -> IO ()
foreign import ccall "Py_Initialize" cPyInitialize :: IO ()
foreign import ccall "initchainer_tagger" cInitChainerTagger :: IO ()

type Lexicon = [(Int, String, [(Cat, Float)])]
type DependencyMatrix = UArray (Int, Int) Float

assignSupertags :: String -> Int -> [Cat] -> String -> IO Lexicon
assignSupertags modelPath beam superTags sent = do
    let tagSize = length superTags
        len = length $ words sent
    cPath <- newCString modelPath
    cSent <- newCString sent
    mat <- mallocArray (len * tagSize)
    putEnv "PYTHONPATH=.:$PYTHONPATH"  --TODO
    cPyInitialize
    cInitChainerTagger
    cTag cPath cSent (fromIntegral $ length sent) mat
    res <- forM (zip [0..] $ words sent) $ \(i, word) -> do
        let mat' = advancePtr mat (i * tagSize)
        probs <- peekArray tagSize mat'
        let probs' = map realToFrac probs
            bestTags = take beam $ sortWith (negate . snd) $ zip superTags probs'
        return (i+1, word, bestTags)
    return res


assignCatsAndDeps :: String -> Int -> [Cat] -> [String] -> IO [(Lexicon, DependencyMatrix)]
assignCatsAndDeps modelPath beam superTags sents = do
    let tagSize = length superTags
        nSents = length sents
        lengths = map (length . words) sents
    cPath <- newCString modelPath
    cSentsList <- sequence $ map newCString sents
    cSents <- mallocArray nSents
    pokeArray cSents cSentsList
    cTagsList <- sequence $ map (\len -> mallocArray (len * tagSize)) lengths
    cTags <- mallocArray nSents
    pokeArray cTags cTagsList
    cDepsList <- sequence $ map (\len -> mallocArray (len * (len + 1))) lengths
    cDeps <- mallocArray nSents
    pokeArray cDeps cDepsList
    cLengths <- mallocArray nSents
    pokeArray cLengths $ map (fromIntegral . length) sents
    putEnv "PYTHONPATH=.:$PYTHONPATH"  --TODO
    cPyInitialize
    cInitChainerTagger
    cTagAndParseDoc cPath cSents cLengths (fromIntegral nSents) cTags cDeps
    forM (zip3 sents cTagsList cDepsList) $ \(sent, tagMat, depMat) -> do
        let tokens = words sent
            sLen = length tokens
            constructArray = array ((1, 0), (sLen+1, sLen+1))
        res <- forM (zip [0..] tokens) $ \(i, word) -> do
            tagVec <- peekArray tagSize $ advancePtr tagMat (i * tagSize)
            let tagScores = take beam $ sortWith (negate . snd) $ zip superTags $ map realToFrac tagVec
            return (i+1, word, tagScores)
        depMat' <- fmap (constructArray . concat) $ forM [0..sLen] $ \i -> do
            depVec <- peekArray (sLen + 1) $ advancePtr depMat (i * (sLen + 1))
            return $ zip [(i+1, j) | j <- [0..]] $ map realToFrac depVec
        return (res, depMat')


main :: IO ()
main = do
    let targetPath = "/Users/masashi-y/depccg/models/tri_headfirst/target.txt"
        modelPath = "/Users/masashi-y/depccg/models/tri_headfirst"
    supertags <- readCatList targetPath
    probs <- assignCatsAndDeps modelPath 3 supertags ["this is a test .", "this is a second example ."]
    print probs



