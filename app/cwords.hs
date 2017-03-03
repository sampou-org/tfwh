module Main where

import TFwH.CommonWords

main :: IO ()
main = cwords 30 "data/LoversLaboursLost.txt" "/tmp/CommonWordsOfLLL.txt"

cwords :: Int -> FilePath -> FilePath -> IO ()
cwords n infile outfile = do
  { text <- readFile infile
  ; writeFile outfile (commonWords n text)
  ; putStrLn "cwords done!"
  }


