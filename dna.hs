import Data.Char
import Control.Monad
import qualified Rosalind.Dna as Dna

main = do
    contents <- getContents
    let dnaStr = filter (/='\n') contents
    let dna = Dna.dnaFromString dnaStr
    let nrNuc = map (flip Dna.nrNucleotide dna) [Dna.A, Dna.C, Dna.G, Dna.T]
    putStrLn $ unwords $ map show nrNuc
