module Rosalind.Dna
(
  Nucleotide(..)
, dnaFromString
, rnaFromString
, toString
, nrNucleotide
, dnaToRna
, dnaComplement
) where

import Data.List

data Nucleotide = A | C | G | T | U deriving (Show, Read, Eq, Ord, Bounded, Enum)

type Dna = [Nucleotide]
type Rna = [Nucleotide]

dnaFromString :: String -> Dna
dnaFromString [] = []
dnaFromString ('U':_) = error "U is not a DNA nucleotide"
dnaFromString (x:xs) = (read [x] :: Nucleotide):(dnaFromString xs)

rnaFromString :: String -> Rna
rnaFromString [] = []
rnaFromString ('T':_) = error "T is not a TNA nucleotide"
rnaFromString (x:xs) = (read [x] :: Nucleotide):(rnaFromString xs)

toString :: [Nucleotide] -> String
toString xs = foldr (\x acc -> (show x !! 0):acc) "" xs

nrNucleotide :: Nucleotide -> [Nucleotide] -> Int
nrNucleotide n d = length $ n `elemIndices` d

dnaToRna :: Dna -> Rna
dnaToRna xs = map toRna xs
    where toRna T = U
          toRna x = x 

dnaComplement :: Dna -> Dna
dnaComplement dna = map complement $ reverse dna
    where complement A = T
          complement T = A
          complement C = G
          complement G = C
