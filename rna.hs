import Data.Char
import Control.Monad
import qualified Rosalind.Dna as Dna

main = do
    contents <- getContents
    let dnaStr = filter (/='\n') contents
    let dna = Dna.dnaFromString dnaStr
    let rna = Dna.dnaToRna dna
    putStrLn $ Dna.toString rna
