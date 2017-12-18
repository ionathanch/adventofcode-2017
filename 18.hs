import Data.Sequence (Seq, fromList, index)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V (replicate)

type Registers = Vector Int
type State = (Registers, Int, Int, Int)
type Instruction = State -> State
data Value = Register Char | Number Int

getIndex :: Value -> Int
getIndex (Register c) = case c of
    'a' -> 0
    'b' -> 1
    'f' -> 2
    'i' -> 3
    'p' -> 4

getValue :: Value -> Registers -> Int
getValue value registers = case value of
    Number i -> i
    c -> registers ! (getIndex c)

parseValue :: String -> Value
parseValue str =
    Register $ head str -- TODO: implement correctly!

son :: Value -> State -> State
son freq (reg, pos, _, rec) = 
    (reg, pos + 1, getValue freq reg, rec)

rcv :: Value -> State -> State
rcv v (reg, pos, freq, rec) =
    (reg, pos + 1, freq, if getValue v reg == 0 then rec else freq)

app :: (Int -> Int -> Int) -> Value -> Value -> State -> State
app f i v (reg, pos, freq, rec) = 
    let index = getIndex i
        value = getValue v reg
    in (reg // [(index, reg ! index `f` value)], pos + 1, freq, rec)

jgz :: Value -> Value -> State -> State
jgz condition offset (reg, pos, freq, rec) =
    (reg, pos + if getValue condition reg > 0 then getValue offset reg else 1, freq, rec)

parseLine :: String -> State -> State
parseLine str =
    let op : vs = words str
    in  case op of
        "snd" -> son $ parseValue $ head vs
        "set" -> app const (parseValue $ head vs) (parseValue $ last vs)
        "add" -> app (+)   (parseValue $ head vs) (parseValue $ last vs)
        "mul" -> app (*)   (parseValue $ head vs) (parseValue $ last vs)
        "mod" -> app mod   (parseValue $ head vs) (parseValue $ last vs)
        "rcv" -> rcv $ parseValue $ head vs
        "jgz" -> jgz (parseValue $ head vs) (parseValue $ last vs)

-- precondition: pos < length instructions
executeNextInstruction :: Seq Instruction -> State -> State
executeNextInstruction instructions (reg, pos, freq, rec) =
    instructions `index` pos $ (reg, pos, freq, rec)

recover :: Seq Instruction -> State -> Int
recover instructions (reg, pos, freq, rec) =
    if rec /= 0 then rec else
    recover instructions $ executeNextInstruction instructions (reg, pos, freq, rec)

main :: IO ()
main = do
    instructions <- fmap (fromList . map parseLine . lines) $ readFile "18.hs"
    let initialState = (V.replicate 5 0, 0, 0, 0)
    print $ recover instructions initialState