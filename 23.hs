{-# LANGUAGE BangPatterns #-}
import Text.Read (readMaybe)
import Data.Char (ord)
import Data.Sequence (Seq, fromList, index)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V (replicate)

type Registers = Vector Int
type Instruction = State -> State
data Value = Register Char | Number Int
data Operation = Set | Sub | Mul deriving Eq
data Function = Function {
    operation :: Operation,
    function  :: Int -> Int -> Int
}
data State = State {
    registers :: Registers,
    position  :: Int,
    countMul  :: Int
}

getIndex :: Value -> Int
getIndex (Register c) = ord c - ord 'a'

getValue :: Registers -> Value -> Int
getValue r v = case v of
    Number i -> i
    c -> r ! (getIndex c)

app :: Function -> Value -> Value -> Instruction
app (Function op fn) x y (State reg pos cnt) =
    let i = getIndex x
    in  State {
        registers = reg // [(i, reg ! i `fn` getValue reg y)],
        position  = pos + 1,
        countMul  = cnt + fromEnum (op == Mul)
    }

jnz :: Value -> Value -> Instruction
jnz x y state@(State reg pos _) = 
    state {
        position = pos + if getValue reg x /= 0 then getValue reg y else 1
    }

parseLine :: String -> Instruction
parseLine str =
    let op : vs = words str
    in  case op of
        "set" -> app (Function Set (flip const)) (parseValue $ head vs) (parseValue $ last vs)
        "sub" -> app (Function Sub (-))          (parseValue $ head vs) (parseValue $ last vs)
        "mul" -> app (Function Mul (*))          (parseValue $ head vs) (parseValue $ last vs)
        "jnz" -> jnz                             (parseValue $ head vs) (parseValue $ last vs)
    where parseValue s = case readMaybe s of
            Just i  -> Number i
            Nothing -> Register $ head s

runInstructions :: Seq Instruction -> State -> Int
runInstructions instructions state@(State _ pos cnt) =
    if pos >= length instructions then cnt else
    let !nextState = instructions `index` pos $ state in runInstructions instructions nextState

count :: Int
count =
    foldr (\b h -> h + (fromEnum . any id . map ((== 0) . (b `mod`)) $ [2..squareroot b])) 0 [108100, 108117..125083]
    where squareroot = floor . sqrt . fromIntegral

main :: IO ()
main = do
    instructions <- fromList . map parseLine . lines <$> readFile "23.txt"
    print $ runInstructions instructions (State (V.replicate 8 0) 0 0)
    print $ count