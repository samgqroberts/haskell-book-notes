module Vigenere where

import Data.Char

encode :: String -> String -> String
encode key text = go key text ""
  where go _ "" e = e
        go "" r e = go key r e
        go k (' ' : r) e = go k r (e ++ [' '])
        go (k' : k) (r' : r) t = let
          shift = ord k' - ord 'a'
          encodedChar = chr $ ord 'a' + mod (ord r' - ord 'a' + shift) 26
          in go k r (t ++ [encodedChar])