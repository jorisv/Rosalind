import Data.Char
import Control.Monad
import qualified Rosalind.Dna as Dna

main = do
    contents <- getContents
    let dnaStr = filter (/='\n') contents
    let dna = Dna.dnaFromString dnaStr
    let dnaCompl = Dna.dnaComplement dna
    putStrLn $ Dna.toString dnaCompl
