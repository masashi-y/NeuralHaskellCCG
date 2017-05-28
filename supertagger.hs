
module Supertag (
    Lexicon,
    assignSupertags,
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

foreign import ccall "tag_and_parse_doc" cTagAndParseDoc :: CString -> Ptr CString -> Ptr CInt -> CInt -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> IO ()
foreign import ccall "tag" cTag :: Ptr CChar -> Ptr CChar -> CInt -> Ptr CFloat -> IO ()
foreign import ccall "Py_Initialize" cPyInitialize :: IO ()
foreign import ccall "initchainer_tagger" cInitChainerTagger :: IO ()

type Lexicon = [(String, [(Cat, Float)])]

assignSupertags :: String -> Int -> [Cat] -> String -> IO Lexicon
assignSupertags modelPath beam superTags sent = do
    let tagSize = 425  --TODO
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
        return (word, bestTags)
    return res


-- assignCatsAndDeps :: String -> Int -> [Cat] -> [String] -> IO Lexicon
assignCatsAndDeps modelPath beam superTags sents = do
    let tagSize = 425  --TODO
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
        let sLen = length tokens
        res <- forM (zip [0..] tokens) $ \(i, word) -> do
            let tagMat' = advancePtr tagMat (i * tagSize)
                depMat' = advancePtr depMat (i * (sLen + 1))
            tagVec <- peekArray tagSize tagMat'
            depVec <- peekArray (sLen + 1) depMat'
            let tagScores = take beam $ sortWith (negate . snd) $ zip superTags $ map realToFrac tagVec
                depScores = map realToFrac depVec
            return (i+1, word, tagScores, depScores)
        return res


main :: IO ()
main = do
    let targetPath = "/Users/masashi-y/depccg/models/tri_headfirst/target.txt"
        modelPath = "/Users/masashi-y/depccg/models/tri_headfirst"
    supertags <- readCatList targetPath
    probs <- assignCatsAndDeps modelPath 3 supertags ["this is a test .", "this is a second example ."]
    print probs



