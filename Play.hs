-- CPSC 312 - 2024 - Project - 
module Play where

import Connect
import Minimax

import System.IO
import Data.List
import Text.Read

type TournammentState = (Int,Int,Int)   -- wins, losses, ties

start = play connect connect_start simple_player (0,0,0)

play :: Game -> State -> Player -> TournammentState -> IO TournammentState
play game start_state opponent ts =
   let (wins,losses,ties) = ts in
   do
      putStrLn "Do you want to play Connect 4?"
      ans <- getLine
      if (ans `elem` ["y","yes","ye","oui"])
         then do
            cpu_difficulty <- select_cpu_difficulty
            if cpu_difficulty==0
               then select_first_player game start_state opponent cpu_difficulty ts
            else do
               select_first_player game start_state (mm_player connect) cpu_difficulty ts
      else
         do
         putStrLn ("Final results: " ++ show wins ++ " wins " ++ show losses ++ " losses " ++ show ties ++ " ties")
         return ts

select_cpu_difficulty :: IO Difficulty
select_cpu_difficulty =
   do
      putStrLn "What computer difficulty is your opponent?"
      putStrLn "0 = Easy, 1 = Medium, 2 = Hard"
      ans <- getLine
      if ('0' `elem` ans)
         then
            do
            putStrLn "You chose the EASY computer mode"
            return 0
      else if ('1' `elem` ans)
         then
            do
            putStrLn "You chose the MEDIUM computer mode"
            return 1
      else if ('2' `elem` ans)
         then
            do
            putStrLn "You chose the HARD computer mode"
            return 2
      else
         select_cpu_difficulty

select_first_player :: Game -> State -> Player -> Difficulty -> TournammentState -> IO TournammentState
select_first_player game start_state opponent difficulty ts = 
   do
      putStrLn "\nWho goes first? 0 = Player (You), 1 = Computer"
      ans <- getLine
      if ('0' `elem` ans)
         then
            person_play game (ContinueGame start_state) opponent difficulty ts
      else if ('1' `elem` ans)
         then
            computer_play game (ContinueGame start_state) opponent difficulty ts
      else
         do
         putStrLn "Please select the first player with 0 or 1"
         select_first_player game start_state opponent difficulty ts

person_play :: Game -> Result -> Player -> Difficulty -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play
person_play game (ContinueGame state) opponent difficulty ts =
   do
      let State internal_state avail = state
      putStrLn "Here is the board now:"
      putStrLn "(Player is 0 | Computer is 1)\n"
      mapM_ print(reverse(transpose internal_state))
      putStrLn "---------------\n[1,2,3,4,5,6,7]"
      putStrLn "\nWhere do you want to place your piece?"
      putStrLn ("Choose one of "++show avail++", or input q to quit")
      line <- getLine
      if line == "q"
         then
            return ts
      else case (readMaybe line :: Maybe Action) of
         Nothing ->
            person_play game (ContinueGame state) opponent difficulty ts
         Just action ->
            if (action `elem` avail)
               then
                  computer_play game (game action state (TokenPlayer 0)) opponent difficulty ts
               else
                  do
                  putStrLn ("Illegal move: "++ show action)
                  person_play game (ContinueGame state) opponent difficulty ts

person_play game (EndOfGame val start_state) opponent difficulty ts =
  do
    newts <- update_tournament_state (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

computer_play :: Game -> Result -> Player -> Difficulty -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent ts
-- person has played, the computer must now play
computer_play game (EndOfGame val start_state) opponent difficulty ts =
   do
      newts <- update_tournament_state val ts
      play game start_state opponent newts

computer_play game (ContinueGame state) opponent difficulty ts =
      let 
          opponent_move = opponent difficulty state
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game opponent_move state (TokenPlayer 1)) opponent difficulty ts

update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)