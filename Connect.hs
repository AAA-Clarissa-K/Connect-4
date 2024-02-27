-- CPSC 312 - 2024 - Project - Connect
module Connect where

import System.IO
import System.Random
import Data.List
import System.IO.Unsafe (unsafePerformIO)

data State = State InternalState [Action]  -- internal_state available_actions
        deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with next state
        deriving (Eq, Show)

type Game = Action -> State -> Token -> Result

type Player = Difficulty -> State -> Action

type Difficulty = Int                   -- computer difficulty, where 0==Easy, 1==Medium, 2==Hard

data Action = Action Int                -- player move is an Int for column chosen
        deriving (Ord,Eq)

type InternalState = [[Token]]          -- columns [1,2,3,4,5,6,7], max 6 rows

data Token = TokenPlayer Int            -- where 0=Human, 1=Computer
            | TokenEmpty Char
        deriving (Ord, Eq)

instance Show Token where    -- overwrite show for each type, otherwise it prints like 'TokenChar ' or 'TokenInt 1', now this just prints 1 and 
    show (TokenPlayer n) = show n
    show (TokenEmpty c) = [c]

------ Connect 4 Game -------
-- default 7x6 board size
boardSizeX = 7
boardSizeY = 6

connect :: Game
-- evaluates if winning, tie, or continuing
-- precon: move is legal (will not go off or overflow board)
connect move (State internal_state available) turn =
    let column_index = (toInt move - 1)
        column = internal_state !! column_index
        index_empty = findEmptyIndex column
        new_column = replaceAt column index_empty turn
        updated_state = replaceAt internal_state column_index new_column
        avail = checkAvailable updated_state
    in if win column_index turn updated_state
        then EndOfGame 1 connect_start     -- agent wins
    else if avail == []
        then EndOfGame 0 connect_start     -- no more moves, tie
    else ContinueGame (State updated_state avail)

toInt (Action i) = i -- converts action to int

-- finds index of first TokenEmpty in a column
-- precon: there is an TokenEmpty in the column
findEmptyIndex :: [Token] -> Int
findEmptyIndex (h:t) = accIndex (h:t) 0
    where
        accIndex (h:t) n
            | h==te = n
            | otherwise = accIndex t n+1

-- replaces element of list at specified index
-- precon: 0 <= index < 6, and is where the first TokenEmpty is
replaceAt :: [a] -> Int -> a -> [a]
replaceAt [] _ _ = []
replaceAt (h:t) n newVal
    | n==0 = newVal:t
    | otherwise = h : replaceAt t (n-1) newVal

-- check if there are at least 4 in a row of token in list
checkLineWin :: Token -> [Token] -> Bool -- input token turn + line to check (line is a list that represents row/column/diagonal)
checkLineWin token line =
    any (\group -> length group >= 4 && head group == token) (group line) -- group consecutive tokens, if there are at least 4 in a row + same as token turn then win

-- evaluate if state is a winning state
win :: Int -> Token -> [[Token]] -> Bool -- input column + token
win column_index playerToken board = 
    let columnWin = checkLineWin playerToken (board!!column_index)      -- check if at least 4 in a row of playerToken in last played column
        row_index = (length (filter (/= (TokenEmpty '_')) (board!!column_index)) - 1) -- find row index by finding row last played token fell into
        rowWin = checkLineWin playerToken ((transpose board)!!row_index) -- check if at least 4 in a row of playerToken in last played row
        diagLWin = checkLineWin playerToken ((leftToRightDiagonals board)!!(column_index + row_index))
        diagRWin = checkLineWin playerToken ((rightToLeftDiagonals board)!!(boardSizeY - 1 - row_index + column_index))
    in or[columnWin, rowWin, diagLWin, diagRWin]

-- Function to get all the diagonals of the board in this direction \\\\\\
-- basically it takes the first row, then shifts the second row one space over and concats to list, then shifts the third row two spaces over and concats to list

-- ex. if starting board was this
-- [_, _, _, _, 1]
-- [_, _, _, _, 1]
-- [1, _, _, _, 1]     
-- [1, _, _, _, 1]  
-- [0, _, _, 1, 0]
-- [0, 1, 0, 1, 0] 

-- function does this and concats new columns

-- [] [] [] [] [] [_, _, _, _, 1]
-- [] [] [] [] [_, _, _, _, 1]
-- [] [] [] [1, _, _, _, 1]     
-- [] [] [1, _, _, _, 1]  
-- [] [0, _, _, 1, 0]
-- [0, 1, 0, 1, 0] 

-- results in [[0],[1,0],[0,_,1],[1,_,_,1],[0,1,_,_,_],[0,_,_,_,_],[1,_,_,_],[1,_,_],[1,_],[1]]
-- The index of the list of diagonals a token lives in is equal to its x + y coordinates, ex. Token 1 at coordinate (3,1) can be found in diagonal!!(3+1) -> [0,1,_,_,_]
leftToRightDiagonals :: [[Token]] -> [[Token]]
leftToRightDiagonals [] = []
leftToRightDiagonals ([]:t) = t
leftToRightDiagonals board = zipWith (++) (map (\(h:_) -> [h]) board ++ repeat []) ([]:(leftToRightDiagonals (map tail board))) 

-- Function to get all the diagonals of the board in this direction ///////
-- Flips the columns upside down and then does leftToRightDiagonals
-- results in [[_],[_,_],[_,_,1],[_,_,_,1],[1,_,_,_,0],[1,_,_,_,0],[1,_,_,1],[1,1,0],[0,1],[0]]
-- The index of the list of diagonals a token lives in is equal to its max y index - y + x coordinates, 
-- ex. This example board above is 5x6, meaning its max y index is 6 - 1 or 5 (since 0-based index so it goes from 0-5)
-- Then token 1 at coordinate (3,1) can be found in diagonal!!(5-1+3) -> [1,1,0]
rightToLeftDiagonals :: [[Token]] -> [[Token]]
rightToLeftDiagonals board = leftToRightDiagonals (map reverse board)

checkAvailable :: InternalState -> [Action]
checkAvailable internal_state =
    [(Action index) | (index,column) <- zip [1..] internal_state, length (filter (/= (TokenEmpty '_')) column) < 6]

empty_internal = replicate 7 (replicate 6 te)
connect_start = State empty_internal [Action n | n <- [1..7]]

-- show and read actions just as (Int,Int)
instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

------ The (Default) Computer Player -------
simple_player :: Player
simple_player _ (State _ avail) =
    let
        index = unsafePerformIO $ random_int 0 ((length avail)-1)
    in select_move avail index

random_int :: Int -> Int -> IO Int
random_int lower upper = getStdRandom (uniformR (lower,upper))

select_move :: [Action] -> Int -> Action
-- used for random move selection - assumes int was generated randomly
select_move avail index = avail !! index

-- Test cases --
a i = Action i
as lst = [Action i | i <- lst]
tp i = TokenPlayer i
te = TokenEmpty '_'
tce = [te,te,te,te,te,te]
tc0 = [tp 0,tp 1,te,te,te,te]
tc1 = [tp 1,tp 0,te,te,te,te]
tcf = [tp 1,tp 0,tp 1,tp 0,tp 1,tp 0]
{-
-- first move made
connect (a 7) connect_start (tp 1) gives:
-- ContinueGame (State [tce,tce,tce,tce,tce,tce,[1,te,te,te,te,te]] [1,2,3,4,5,6,7])

-- continue with board partially filled
connect (a 7) (State [tc0,tc1,tc0,tc1,tc0,tc1,[tp 0,tp 1,te,te,te,te]] (as [1,2,3,4,5,6,7])) (tp 0) 
-- ContinueGame (State [tc0,tc1,tc0,tc1,tc0,tc1,[0,1,0,_,_,_]] [1,2,3,4,5,6,7])

-- some board full (some col unavail), and making move fills a col
connect (a 7) (State [tc0,tc1,tc0,tcf,tc0,tcf,[tp 0,tp 1,tp 1,tp 0,tp 1,te]] (as [1,2,3,5,7])) (tp 0) 
-- ContinueGame (State [[0,1,_,_,_,_],[1,0,_,_,_,_],[0,1,_,_,_,_],[1,0,1,0,1,0],[0,1,_,_,_,_],[1,0,1,0,1,0],[0,1,1,0,1,0]] [1,2,3,5])

-- tie case
connect (a 7) (State [tcf,tcf,tcf,tcf,tcf,tcf,[tp 0,tp 1,tp 0,tp 1,tp 0,te]] (as [7])) (tp 0)
-- EndOfGame 0.0 (State [[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_]] [1,2,3,4,5,6,7])
-}

{-
findEmptyIndex [te,te,te,te,te,te] should give 0
findEmptyIndex tc1 should give 2
-}

{-
-- testing replacing element in column:
replaceAt tc1 2 (tp 1) should give [1,0,1,_,_,_]

-- testing replacing column in internal_state:
replaceAt empty_state 3 tc1 should give [[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[1,0,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_],[_,_,_,_,_,_]]
replaceAt [tc1,tc0,tcf,tcf,tcf,tcf,tcf] 2 tc1 should give [tc1,tc0,tc1,tcf,tcf,tcf,tcf]
-}

{-
checkAvailable empty_internal should give [1,2,3,4,5,6,7]
checkAvailable [tc0,tc1,tc0,tc1,tc0,tc1,tc0] should give [1,2,3,4,5,6,7]
checkAvailable [tc1,tc0,tcf,tcf,tc0,tc1,tcf] should give [1,2,5,6]
checkAvailable [tcf,tcf,tcf,tcf,tcf,tcf,tcf] should give []
-}

{-
-- column win, i.e
[_, _, _, _, 1]
[_, _, _, _, 1]
[1, _, _, _, 1]
[1, _, _, _, 1] 
[0, _, _, 1, 0]
[0, 1, 0, 1, 0]

[[tp 0,tp 0,tp 1,tp 1,te,te],
[tp 1,te,te,te,te,te],
[tp 0,te,te,te,te,te], 
[tp 1,tp 1,te,te,te,te], 
[tp 0,tp 0,tp 1,tp 1,tp 1,tp 1]]

testing above board, assuming last move was from player1 in fifth column
win (5-1) (tp 1) [[tp 0,tp 0,tp 1,tp 1,te,te],[tp 1,te,te,te,te,te],[tp 0,te,te,te,te,te],[tp 1,tp 1,te,te,te,te],[tp 0,tp 0,tp 1,tp 1,tp 1,tp 1]]

-- row win, i.e
[_, _, _, _, _]
[_, _, _, _, _]
[1, _, _, _, _]
[1, _, _, _, _] 
[0, 0, 0, 0, 1]
[0, 1, 0, 1, 0]

[[tp 0,tp 0,tp 1,tp 1,te,te],
[tp 1,tp 0,te,te,te,te],
[tp 0,tp 0,te,te,te,te], 
[tp 1,tp 0,te,te,te,te], 
[tp 0,tp 1,te,te,te,te]]

testing above board assuming last move was from player0 in 4th column
win (4-1) (tp 0) [[tp 0,tp 0,tp 1,tp 1,te,te],[tp 1,tp 0,te,te,te,te],[tp 0,tp 0,te,te,te,te],[tp 1,tp 0,te,te,te,te],[tp 0,tp 1,te,te,te,te]]

-- diagonal win 1, i.e
make sure test cases for diagonal are 6 tokens tall, x*6 board size
[_, _, _, _, _]
[_, 0, _, _, _]
[0, 1, 0, _, _]
[1, 1, 1, 0, _] 
[0, 0, 1, 0, 0]
[0, 1, 0, 1, 0]

[[tp 0,tp 0,tp 1,tp 0,te,te],
[tp 1,tp 0,tp 1,tp 1,tp 0,te],
[tp 0,tp 1,tp 1,tp 0,te,te], 
[tp 1,tp 0,tp 0,te,te,te], 
[tp 0,tp 0,te,te,te,te]]

testing above board assuming last move was from player0 in 2nd column
win (2-1) (tp 0) [[tp 0,tp 0,tp 1,tp 0,te,te],[tp 1,tp 0,tp 1,tp 1,tp 0,te],[tp 0,tp 1,tp 1,tp 0,te,te],[tp 1,tp 0,tp 0,te,te,te],[tp 0,tp 0,te,te,te,te]]

-- diagonal win 2, i.e
[_, _, _, _, _]
[_, _, _, _, _]
[0, _, _, 1, _]
[1, 1, 1, 0, _] 
[0, 1, 1, 0, 0]
[1, 1, 0, 1, 0]

[[tp 1,tp 0,tp 1,tp 0,te,te],
[tp 1,tp 1,tp 1,te,te,te],
[tp 0,tp 1,tp 1,te,te,te], 
[tp 1,tp 0,tp 0,tp 1,te,te], 
[tp 0,tp 0,te,te,te,te]]

testing above board assuming last move was from player1 in 3rd column
win (3-1) (tp 1) [[tp 1,tp 0,tp 1,tp 0,te,te],[tp 1,tp 1,tp 1,te,te,te],[tp 0,tp 1,tp 1,te,te,te],[tp 1,tp 0,tp 0,tp 1,te,te],[tp 0,tp 0,te,te,te,te]]

False case(s):
win (5-1) (tp 1) [[tp 0,tp 0,tp 1,tp 1,te,te],[tp 1,te,te,te,te,te],[tp 0,te,te,te,te,te],[tp 1,tp 1,te,te,te,te],[tp 0,tp 0,tp 1,tp 1,tp 1,te]]
win (4-1) (tp 0) [[tp 0,tp 1,tp 1,tp 1,te,te],[tp 1,tp 0,te,te,te,te],[tp 0,tp 0,te,te,te,te],[tp 1,tp 0,te,te,te,te],[tp 0,tp 1,te,te,te,te]]
win (3-1) (tp 1) [[tp 0,tp 0,tp 1,tp 0,te,te],[tp 1,tp 1,tp 1,te,te,te],[tp 0,tp 1,tp 1,te,te,te],[tp 1,tp 0,tp 0,tp 1,te,te],[tp 0,tp 0,te,te,te,te]]
win (2-1) (tp 0) [[tp 0,tp 0,tp 1,tp 0,te,te],[tp 1,tp 0,tp 1,tp 1,tp 1,te],[tp 0,tp 1,tp 1,tp 0,te,te],[tp 1,tp 0,tp 0,te,te,te],[tp 0,tp 0,te,te,te,te]]
-}
