module Rosalind.Dna
(
  Nucleotide(..)
, Fasta(..)
, dnaFromString
, rnaFromString
, toString
, nrNucleotide
, nucContent
, dnaToRna
, dnaComplement
, readFastaLabel
, readFasta
, readFastaLines
) where

import Data.List

data Nucleotide = A | C | G | T | U deriving (Show, Read, Eq, Ord, Bounded, Enum)

type Dna = [Nucleotide]
type Rna = [Nucleotide]

data Fasta = Fasta { label :: String
                   , id :: String
                   , dna :: Dna
                   } deriving (Show)

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

nucContent :: [Nucleotide] -> Dna -> Double
nucContent nuc dna = nrNuc/lString
    where nrNuc = fromIntegral (foldl (\acc n -> nrNucleotide n dna + acc) 0 nuc)
          lString = fromIntegral (length dna)


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


readFastaLabel :: String -> (String, String)
readFastaLabel str = (drop 1 label, drop 1 id)
    where Just index = elemIndex '_' str
          (label, id) = splitAt index str

readFasta :: String -> Fasta
readFasta str = Fasta {label=fastaLab, Rosalind.Dna.id=fastaId, dna=d}
    where (labelStr:dnaStr) = lines str
          (fastaLab, fastaId) = readFastaLabel labelStr
          d = foldr (\x acc -> dnaFromString x ++ acc) [] dnaStr

-- better to use Data.List.Split but that train me
recSplit :: Char -> String -> [String]
recSplit sep str = subRecSplit $ elemIndex sep $ drop 1 str
    where subRecSplit (Just index) = let (preStr,postStr) = splitAt (index+1) str
                                     in preStr:(recSplit sep postStr)
          subRecSplit Nothing = [str]

readFastaLines :: String -> [Fasta]
readFastaLines str = map readFasta (recSplit '>' str)
