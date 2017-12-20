import Text.Read (readMaybe)
import Data.Sequence (Seq, fromList, empty, index, deleteAt, (|>))
import qualified Data.Sequence as S (null)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V (replicate)

-- DEFINITIONS

type Registers = Vector Int
type Instruction = ProgramId -> State -> State
data Value = Register Char | Number Int
data ProgramId = Zero | One
data Program = Program {
    registers :: Registers,
    position  :: Int,
    queue     :: Seq Int
}
data State = State {
    zero  :: Program,
    one   :: Program,
    stop  :: (Bool, Bool),
    count :: Int
}

-- HELPERS
setStop :: ProgramId -> Bool -> State -> State
setStop Zero b state = state { stop = (b, snd $ stop state) }
setStop One b state = swap . setStop Zero b . swap $ state

getProgram :: ProgramId -> State -> Program
getProgram Zero state = zero state
getProgram One  state = one  state

swap :: State -> State
swap (State p0 p1 (s0, s1) c) = State p1 p0 (s1, s0) c

getIndex :: Value -> Int
getIndex (Register c) = case c of
    'a' -> 0
    'b' -> 1
    'f' -> 2
    'i' -> 3
    'p' -> 4

getValue :: Value -> Registers -> Int
getValue v r = case v of
    Number i -> i
    c -> r ! (getIndex c)

-- OPERATIONS

sen :: Value -> Instruction
sen v Zero state@(State p0 p1 s c) =
    state { 
        zero = p0 { position = position p0 + 1 }, 
        one  = p1 { queue = queue p1 |> getValue v (registers p0) }, 
        stop = (fst s, False)
    }
sen v One state = swap . sen v Zero . swap $ state { count = count state + 1 }

rcv :: Value -> Instruction
rcv i Zero state@(State p0 p1 s c) =
    if S.null $ queue p0 then state { stop = (True, snd s) } else
    let (que, val) = pop $ queue p0
    in  state { zero = p0 {
            registers = registers p0 // [(getIndex i, val)],
            position  = position  p0 + 1,
            queue     = que
        }}
    where pop q = (deleteAt 0 q, q `index` 0)
rcv i One state = swap . rcv i Zero . swap $ state

app :: (Int -> Int -> Int) -> Value -> Value -> Instruction
app f i v Zero state@(State p0 p1 s c) = 
    let reg = registers p0
        ind = getIndex i
        val = getValue v reg
    in  state { zero = p0 {
            registers = reg // [(ind, reg ! ind `f` val)],
            position  = position p0 + 1
        }}
app f i v One state = swap . app f i v Zero . swap $ state

jgz :: Value -> Value -> Instruction
jgz condition offset Zero state@(State p0 p1 s c) =
    let reg = registers p0
    in  state { zero = p0 {
            position = position p0 + if getValue condition reg > 0 then getValue offset reg else 1
        }}
jgz condition offset One state = swap . jgz condition offset Zero . swap $ state

-- PARSE

parseLine :: String -> Instruction
parseLine str =
    let op : vs = words str
    in  case op of
        "snd" -> sen  $            parseValue $ head vs
        "set" -> app (flip const) (parseValue $ head vs) (parseValue $ last vs)
        "add" -> app (+)          (parseValue $ head vs) (parseValue $ last vs)
        "mul" -> app (*)          (parseValue $ head vs) (parseValue $ last vs)
        "mod" -> app mod          (parseValue $ head vs) (parseValue $ last vs)
        "rcv" -> rcv  $            parseValue $ head vs
        "jgz" -> jgz              (parseValue $ head vs) (parseValue $ last vs)
    where parseValue s = case readMaybe s of
            Just i  -> Number i
            Nothing -> Register $ head s

-- SOLVE

executeNextInstruction :: Seq Instruction -> ProgramId -> State -> State
executeNextInstruction instructions pid state =
    let pos = position . getProgram pid $ state
    in  if pos >= length instructions then setStop pid True state 
        else (instructions `index` pos) pid state

getCount :: Seq Instruction -> State -> Int
getCount _ (State _ _ (True, True) c) = c
getCount instructions state =
    if not . fst . stop $ state 
    then getCount instructions $ executeNextInstruction instructions Zero state
    else getCount instructions $ executeNextInstruction instructions One  state

main :: IO ()
main = do
    instructions <- fromList . map parseLine . lines <$> readFile "18.txt"
    let initialState = (State (Program (V.replicate 5 0) 0 empty) (Program (V.replicate 5 0 // [(4, 1)]) 0 empty) (False, False) 0)
    print $ getCount instructions initialState