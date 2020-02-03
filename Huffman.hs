module Huffman where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           LeftistHeap (PQueue)
import qualified LeftistHeap as PQueue

import           HuffmanDef

decodehelper :: HuffmanTree -> [Bool] -> (Char,[Bool])
-- if current node is a leaf, return the char and remainder of the list
decodehelper leaf@(Leaf _ c) xs = (c,xs)
decodehelper branch@(Branch _ left right) xs@(x:xt)
    | x == True = decodehelper right xt
    | x == False = decodehelper left xt
 

decode :: HuffmanTree -> [Bool] -> [Char]
decode tree xs = go tree xs []
    where
        go tree xs@(x:xt) acc = let (c,remain) = decodehelper tree xs
                                in go tree remain (acc ++ [c])
        go _ [] acc = acc

huffmanTree :: [(Char, Int)] -> HuffmanTree
huffmanTree = error "TODO"

buildDict :: HuffmanTree -> Map Char [Bool]
buildDict = error "TODO"

encode :: HuffmanTree -> [Char] -> [Bool]
encode tree cs = concatMap (\c -> dict ! c) cs
  where
    dict = buildDict tree