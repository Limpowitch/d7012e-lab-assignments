module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program s) = concatMap Statement.toString s

exec :: T -> [Integer] -> [Integer]
exec (Program s) input = Statement.exec s Dictionary.empty input
