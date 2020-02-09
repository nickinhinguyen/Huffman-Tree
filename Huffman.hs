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
huffmanTree xs = huffmanTreehelper (make xs PQueue.empty)
    where 
        make xs@((c,i) : xt) acc = let acc2 = PQueue.insert i (Leaf i c) acc
                                    in make xt acc2
        make [] acc = acc

-- incomplete since only consider the case item extract is a leaf not branch and not merging tree                     
huffmanTreehelper :: PQueue Int HuffmanTree -> HuffmanTree
huffmanTreehelper queue = case e2 of 
    Nothing -> min1
    _ -> huffmanTreehelper (PQueue.insert (getFreq newtree) newtree rqueue2)
    where 
        e2 = PQueue.extractMin rqueue1
        Just (rqueue1, min1) = PQueue.extractMin queue
        Just (rqueue2, min2) = e2
        newtree 
            |f1 < f2 = Branch sum min1 min2
            |otherwise = Branch sum min2 min1
            where 
                f1 = getFreq min1
                f2 = getFreq min2
                sum = f1 + f2
        
findChar :: HuffmanTree -> [Bool]-> Map Char [Bool] -> Map Char [Bool]
findChar (Leaf _ c) xs map = Map.insert (c,xs) map
findChar (Branch _ lt rt) xs map = findChar (lt xs:False map) ++ findChar (rt xs:True map)

    
    
buildDict :: HuffmanTree -> Map Char [Bool]
buildDict tree = findChar tree [] Map.empty
    

encode :: HuffmanTree -> [Char] -> [Bool]
encode tree cs = concatMap (\c -> dict ! c) cs
  where
    dict = buildDict tree
