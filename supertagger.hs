
module Supertag (
    Lexicon,
    assignSupertags
    ) where

import Control.Exception     ( bracket )
import Foreign.C.Types       ( CInt(..), CChar(..), CFloat(..) )
import Foreign.C.String      ( withCString, newCString )
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array
import Foreign.Storable      ( Storable(..) )
import Foreign.Ptr           ( Ptr(..) )
import System.Posix.Env      ( putEnv )
import CCG
import Control.Monad         ( forM )
import GHC.Exts              ( sortWith )

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


main :: IO ()
main = do
    let targetPath = "/Users/masashi-y/depccg/models/tri_headfirst/target.txt"
        modelPath = "/Users/masashi-y/depccg/models/tri_headfirst"
    supertags <- readCatList targetPath
    probs <- assignSupertags modelPath 3 supertags "this is a test ."
    print probs



