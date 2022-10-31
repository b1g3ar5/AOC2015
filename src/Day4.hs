{-# LANGUAGE OverloadedStrings #-}

module Day4(
  day4
  ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Crypto.Hash -- .MD5 (hash)
import Data.ByteString.Base16 (encode)


day4 :: IO ()
day4 = do
  let txt = text
  putStrLn $ "Day4: Part1: " ++ show (trials "00000" txt)
  putStrLn $ "Day4: Part1: " ++ show (trials "000000" txt)
  return ()


trials :: ByteString -> ByteString -> Int
trials target str = go 0
  where
    go :: Int -> Int
    go 10000000 = -1
    go n = if B.take len hashed == target
            then
              n
            else
              go (n+1)
      where
        hashed :: ByteString        
        hashed = convertToBase Base16 (hashWith MD5 $ B.append str (C8.pack $ show n))
        len = B.length target


--  let bs = encodeUtf8 text
--  putStrLn $ "You entered: " ++ show bs
--  let digest :: Digest SHA256
      --digest = hash bs
  --putStrLn $ "SHA256 hash: " ++ show digest


text :: B.ByteString
text = "yzbqklnj"
