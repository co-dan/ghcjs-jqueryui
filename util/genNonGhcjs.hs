module Main where

import Data.List
import Control.Monad
import Control.Applicative
import System.IO

main = do
  ls <- lines <$> readFile "../JavaScript/JQuery/UI/Internal.hs"
  h <- openFile "../JavaScript/JQuery/UI/nonghcjs.txt" WriteMode
  forM_ ls $ \l -> do
    let sig = filter ("jq_" `isPrefixOf`) (tails l)
    case sig of
      []    -> return ()
      (x:_) -> do
        let fun = head (words x)
        hPutStrLn h (unwords . words $ x)
        hPutStrLn h (fun ++ " = error \"" ++ fun ++ ": only available in JavaScript\"")
  hClose h
