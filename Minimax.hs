-- CPSC 312 - 2024 - Project - Minimax
module Minimax where

import Connect
import TreeDict
import System.IO
import System.Random
import System.IO.Unsafe (unsafePerformIO)

type Mem = Dict State (Action, Double)

----   Determining the best move  ---
minimax:: Game -> State -> Mem -> ((Action, Double), Mem)
-- precondition: there are available moves
minimax game st mem =
   case getval st mem of
      Just act_val  -> (act_val,mem)
      Nothing ->
        let (act_val,mem1) =
              argmax_mem (valueact game st) avail mem
        in (act_val, (insertval st act_val mem1))
    where State _ avail = st

-- the value of doing action act in state st for game
valueact:: Game -> State -> Action -> Mem -> (Double,Mem)
valueact game st act mem = value game (game act st (TokenPlayer 0)) act mem

-- value for current player after result
value:: Game -> Result -> Action -> Mem -> (Double,Mem)
value _ (EndOfGame val _) _ mem = (val,mem)
value game (ContinueGame st) act mem =
   let ((_,val), mem2) = minimax game st mem
      in  (-val,mem2)

mm_player:: Game -> Player
mm_player game difficulty state =
   let 
      prob = unsafePerformIO $ random_int 1 100
   in if (difficulty==2 || prob <= 60) -- if HARD difficulty, default to minimax, if MEDIUM, only do this 60% of the time
      then fst (fst ( minimax game state emptyDict))
   else let index = unsafePerformIO $ random_int 0 ((length avail)-1)
      in select_move avail index
      where State _ avail = state

-- Precondition: lst is not empty
--  each act has maximal value for f
--  updates the memory to mem1. Each call to f updates memory
argmax_mem :: (Action -> Mem -> (Double,Mem)) -> [Action] -> Mem -> ((Action,Double),Mem)
argmax_mem f [act] mem = ((act,val),mem1)
     where (val,mem1) = f act mem
argmax_mem f (h:t) mem
   | fh > ft = ((h,fh),mem2)
   | fh < ft = ((bt,ft),mem2)
   | otherwise = if randomBool then ((h,fh),mem2) else ((bt,ft),mem2)   -- 50/50 chance of picking equal options since
                                                                        -- it would otherwise default to choosing largest column option
   where
      ((bt,ft),mem1) = argmax_mem f t mem
      (fh,mem2) = f h mem1
      randomBool = (unsafePerformIO $ random_int 1 100) > 50
