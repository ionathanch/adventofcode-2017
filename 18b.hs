import Text.Read (readMaybe)
import Data.Sequence (Seq, fromList, empty, index, deleteAt, (|>))
import qualified Data.Sequence as S (null)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V (replicate)

-- DEFINITIONS

type Registers = Vector Int
type Program = (Registers, Int, Seq Int)
type Instruction = ProgramId -> State -> State
data State = State Program Program (Bool, Bool) Int
data Value = Register Char | Number Int
data ProgramId = Zero | One

-- HELPERS

getPos  :: ProgramId -> State -> Int
getPos Zero (State (_, pos, _) _ _ _) = pos
getPos One  (State _ (_, pos, _) _ _) = pos

getStop :: ProgramId -> State -> Bool
getStop Zero (State _ _ (stop, _) _) = stop
getStop One  (State _ _ (_, stop) _) = stop

setStop :: ProgramId -> Bool -> State -> State
setStop Zero b (State p0 p1 (_, stop1) count) = State p0 p1 (b, stop1) count
setStop One  b (State p0 p1 (stop0, _) count) = State p0 p1 (stop0, b) count

pop :: Seq Int -> (Seq Int, Int)
pop queue = (deleteAt 0 queue, queue `index` 0)

swap :: State -> State
swap (State p0 p1 (stop0, stop1) count) = State p1 p0 (stop1, stop0) count

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

-- OPERATIONS

sen :: Value -> ProgramId -> State -> State
sen v Zero (State (reg0, pos0, que0) (reg1, pos1, que1) stop count) =
    setStop One  False $ State (reg0, pos0 + 1, que0) (reg1, pos1, que1 |> getValue v reg0) stop count
sen v One  (State (reg0, pos0, que0) (reg1, pos1, que1) stop count) =
    setStop Zero False $ State (reg0, pos0, que0 |> getValue v reg1) (reg1, pos1 + 1, que1) stop (count + 1)

rcv :: Value -> ProgramId -> State -> State
rcv i Zero (State (reg0, pos0, que0) p1 stop count) =
    if S.null que0 then setStop Zero True $ State (reg0, pos0, que0) p1 stop count else
    let ind = getIndex i
        (que, val) = pop que0
    in  State (reg0 // [(ind, val)], pos0 + 1, que) p1 stop count
rcv i One state = swap . rcv i Zero . swap $ state

app :: (Int -> Int -> Int) -> Value -> Value -> ProgramId -> State -> State
app f i v Zero (State (reg0, pos0, que0) p1 stop count) = 
    let ind = getIndex i
        val = getValue v reg0
    in  State (reg0 // [(ind, reg0 ! ind `f` val)], pos0 + 1, que0) p1 stop count
app f i v One state = swap . app f i v Zero . swap $ state

jgz :: Value -> Value -> ProgramId -> State -> State
jgz condition offset Zero (State (reg0, pos0, que0) p1 stop count) =
    State (reg0, pos0 + if getValue condition reg0 > 0 then getValue offset reg0 else 1, que0) p1 stop count
jgz condition offset One state = swap . jgz condition offset Zero . swap $ state

-- PARSE

parseLine :: String -> Instruction
parseLine str =
    let op : vs = words str
    in  case op of
        "snd" -> sen $ parseValue $ head vs
        "set" -> app (flip const) (parseValue $ head vs) (parseValue $ last vs)
        "add" -> app (+)          (parseValue $ head vs) (parseValue $ last vs)
        "mul" -> app (*)          (parseValue $ head vs) (parseValue $ last vs)
        "mod" -> app mod          (parseValue $ head vs) (parseValue $ last vs)
        "rcv" -> rcv $             parseValue $ head vs
        "jgz" -> jgz              (parseValue $ head vs) (parseValue $ last vs)
    where parseValue s = case readMaybe s of
            Just i  -> Number i
            Nothing -> Register $ head s

-- SOLVE

executeNextInstruction :: Seq Instruction -> ProgramId -> State -> State
executeNextInstruction instructions pid state =
    let pos = getPos pid state
    in  if pos >= length instructions then setStop pid True state 
        else (instructions `index` pos) pid state

getCount :: Seq Instruction -> State -> Int
getCount _ (State _ _ (True, True) count) = count
getCount instructions state =
    if not $ getStop Zero state 
    then getCount instructions $ executeNextInstruction instructions Zero state
    else getCount instructions $ executeNextInstruction instructions One  state

main :: IO ()
main = do
    instructions <- fmap (fromList . map parseLine . lines) $ readFile "18.txt"
    let initialState = (State (V.replicate 5 0, 0, empty) (V.replicate 5 0 // [(4, 1)], 0, empty) (False, False) 0) :: State
    print $ getCount instructions initialState