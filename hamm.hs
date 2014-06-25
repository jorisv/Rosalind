import Data.Char
import Control.Monad
import qualified Rosalind.Dna as Dna

main = do
    contents <- getContents
    let [dna1, dna2] = map Dna.dnaFromString $ lines contents
    let hammingDist = Dna.hammingDist dna1 dna2
    print hammingDist
