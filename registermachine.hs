module Models where
import Data.Maybe

type Program = [Instruction]
data Instruction = Instr Label Body
type Label = Int
data Body = Halt | Add Reg Label | Sub Reg Label Label
type Reg = Int

instance Show Instruction where
  show (Instr l b)
    = "L" ++ show l ++ " : " ++ show b

instance Show Body where
  show Halt = "HALT"
  show (Add r l)
    = "R" ++ show r ++ "+ -> " ++ "L" ++ show l 
  show (Sub r l1 l2)
    = "R" ++ show r ++ "- -> " ++ "L" ++ show l1
                       ++ ", " ++ "L" ++ show l2

addLine [] = []
addLine (x:xs) = x : '\n' : (addLine xs)
display prog = mapM_ putStrLn (map show prog)

-- Coding

codePair x y = 2^x * (2*y + 1)
codePairNat x y = (codePair x y) - 1

codeList x = foldr codePair 0 x

codeBody Halt
  = 0
codeBody (Add r l)
  = codePair (2*r) l
codeBody (Sub r l1 l2)
  = codePair (2*r + 1) (codePairNat l1 l2)

codeProgram = codeList . (map (codeBody.getBody))
getBody (Instr l b) = b

testProg = map (\(l, b) -> Instr l b) testProg'
testProg' = [(0, Sub 0 0 2), (1, Halt)]

-- Decoding

greatestExp n = greatestExp' n 0
  where
    greatestExp' n acc
      | even n    = greatestExp' (quot n 2) (acc+1)
      | otherwise = acc

decodePair n = (x, y)
 where
   x = greatestExp n
   y = quot ((quot n (2^x)) - 1)  2

decodePairNat n = decodePair (n+1)

decodeList 0 = []
decodeList n = x : (decodeList y)
  where
    (x, y) = decodePair n

decodeBody 0 = Halt
decodeBody n
  | even x    = Add (quot x 2) y
  | otherwise = Sub (quot (x-1) 2) l1 l2 
  where
    (x, y)   = decodePair n
    (l1, l2) = decodePairNat y

decodeProgram n
  = zipWith addLabel [0..] (map decodeBody (decodeList n))
addLabel l b = Instr l b

compute prog rs = compute' (head prog) rs
  where
    compute' (Instr _ Halt) rs = rs
    compute' (Instr _ (Sub r l1 l2)) rs
      | lookUp r rs > 0 = compute' (getInstr l1 prog) newrs
      | otherwise    = compute' (getInstr l2 prog) rs
        where newrs = insert (r, (lookUp r rs)-1) rs
    compute' (Instr _ (Add r l)) rs
      = compute' (getInstr l prog) newrs 
        where newrs = insert (r, (lookUp r rs)+1) rs 

getInstr l prog
  = head (filter (isLabel l) prog)
isLabel l (Instr l' b) = l == l'

p = [Instr 0 (Sub 1 2 1), Instr 1 Halt, Instr 2 (Sub 1 3 4), Instr 3 (Sub 1 5 4), Instr 4 Halt, Instr 5 (Add 0 0)] 

rs :: [(Reg, Integer)]
rs = [(0, 0), (1, 7)]

pval = (2 ^ 46) * 20483

prog2n n = nsteps n
  where
    nsteps n = take n (map step [0..])
    step   n = Add 1 (n+1) 

prog2nFull n = mapLabels fullProg
  where
    fullProg = prog2n n ++ [Sub 1 (n+1) (n+3),
                            Add 0 (n+2),
                            Add 0 n, Halt]

mapLabels xs = zipWith addLabel [0..] xs

steps k
  | k < 6     = k
  | even k    = (steps (quot k 2)) + 3
  | otherwise = (steps (quot k 2)) + 4

lookUp key = fromJust . (lookup key)
insert (k, v) ((k', v'):xs)
 | k == k'   = (k, v):xs
 | otherwise = (k', v') : (insert (k, v) xs)
