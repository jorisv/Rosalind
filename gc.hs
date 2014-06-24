import Data.Char
import Data.List
import Control.Monad
import qualified Rosalind.Dna as Dna

main = do
    contents <- getContents
    let fasta = Dna.readFastaLines contents
    let gcContent = map (*100) (map (\fa -> Dna.nucContent [Dna.C, Dna.G] (Dna.dna fa)) fasta)
    let fastaGc = zip (map (\x -> (Dna.label x) ++ "_" ++ (Dna.id x)) fasta) gcContent
    let (maxLabel, maxValue) = (maximumBy (\(_, gc1) (_, gc2) -> compare gc1 gc2) fastaGc)
    putStrLn maxLabel
    putStrLn (show maxValue)
