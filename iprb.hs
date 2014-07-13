import Data.Char
import Data.List
import Data.Monoid
import Data.Ratio
import Control.Monad
import Control.Monad.Writer
import Control.Applicative
import qualified Rosalind.Dna as Dna

data Allele = Dom | Rec deriving (Show, Enum, Bounded, Read, Ord, Eq)
type Pop = (Integer, Integer, Integer)
type ProbPop = (Pop, Rational, [[Allele]])


takeK :: ProbPop -> ProbPop
takeK ((k, m, n), prob, all) = ((k-1, m, n), (k % (k + m + n))*prob, [Dom,Dom]:all)

takeM :: ProbPop -> ProbPop
takeM ((k, m, n), prob, all) = ((k, m-1, n), (m % (k + m + n))*prob, [Dom,Rec]:all)

takeN :: ProbPop -> ProbPop
takeN ((k, m, n), prob, all) = ((k, m, n-1), (n % (k + m + n))*prob, [Rec,Rec]:all)

validProbPop :: ProbPop -> Bool
validProbPop ((k, m, n), _, _) = k >= 0 && m >= 0 && n >= 0

-- Compute a new population by taking a K, M and N
popup :: ProbPop -> [ProbPop]
popup probPop = let newPop = (takeK probPop) : (takeM probPop) : (takeN probPop) : []
                in filter validProbPop newPop

-- From a ProbPop compute all possible son's and their probability
combi :: ProbPop -> [(Rational, (Allele, Allele))]
combi (pop, prob, [[a11,a12],[a21,a22]]) = [(prob*(1%4), (a11,a21)), (prob*(1%4), (a11, a22)),
                                            (prob*(1%4), (a12,a21)), (prob*(1%4), (a12, a22))]


main = do
    contents <- getContents
    let [k,m,n] = (map read $ words contents) :: [Integer]
    let pop = (k, m, n)
    let pop1 = popup (pop, 1%1, [])
    let pop2 = concat $ map popup pop1
    let sons = concat $ map combi pop2
    let res = foldl (\acc (prob, (a1, a2)) -> acc + if (a1 == Dom || a2 == Dom) then prob else 0) 0 sons
    print $ fromRational res
