module Levenshtein where

import ENFA
import Data.Map
import Data.Set
import Data.List

-- *Levenshtein function takes the first element of v list, and creates a list
-- with the values of truth for each word in v. Its parameters are a word w, an
-- int n and a list of strings [v]
levenshtein :: String -> Int -> [String] -> [Bool]
levenshtein w n (x:xs) = [if ((length x )<= (length w +n) )
                          then (oneWord w n x) else (False)]
                          ++ levenshtein w n xs
levenshtein w n []  = []

-- "oneWord" determines if the word v is in an accepting state
-- at the ENFA built with w and n
oneWord :: (Num t, Ord a, Ord t) => [a] -> t -> [a] -> Bool
oneWord w n v
  |Data.Set.null (Data.Set.intersection (accepting(buildEnfa w n))
   (evaluateEnfa v (buildEnfa w n)))                                  = False
  |otherwise                                                          = True

-- "isFinalState" determines if the current state (i,j) is located at the right 
-- upper corner of the Levenshtein automaton constructed with the word w and the 
-- maximun distance n, based on the tuple contained in each state.
isFinalState :: (Eq a1, Foldable t) => (Int, a1) -> t a -> a1 -> Bool
isFinalState (i,j) w n
  |i== (length w) && j==n     =True
  |otherwise                  =False

-- "isUpperState" determines if the current state is located on the highest 
-- row of the Levenshtein automaton with reference w and degree n,  except for 
-- the state at the upper right corner
isUpperState :: (Foldable t, Eq a1) => (Int, a1) -> t a -> a1 -> Bool
isUpperState (i,j) w n
  |j==n && i/= (length w)     =True
  |otherwise                  =False
  
-- "isRightState" determines if the current state is located at the last 
--column the Levenshtein automaton with word w and degree n, except for the 
-- state at the upper right corner
isRightState :: (Eq a1, Foldable t) => (Int, a1) -> t a -> a1 -> Bool
isRightState (i,j) w n
  |i== (length w) && j /=n    =True
  |otherwise                  =False

-- "buildEnfa" constructs the Levenshtein automaton, "floor" by "floor"
buildEnfa :: (Ord a1, Ord a, Num a) => [a1] -> a -> ENFA (Int, a) a1
buildEnfa w n = buildState (0,0) w w n (initENFA (0,0))

-- "buildState" constructs the states for the Levenshtein automaton with its 
-- respective transitions, defining if a transition is allowed or not and 
-- creating the acceptance states set
buildState :: (Num a1, Ord a1, Ord a) => (Int, a1) -> [a] -> [a] -> a1 
              -> ENFA (Int, a1) a -> ENFA (Int, a1) a
buildState (i,j) [] w n e
  |isFinalState (i,j) w n  = accept (i,j) $ e
  |isRightState (i,j) w n  =trans ((i,j),Any,(i,j+1)).accept (i,j)
                            $ (buildState (0,j+1) w w n e)
buildState (i,j) (x:xs) w n e
  |isUpperState (i,j) w n  =trans ((i,j),Symbol x, (i+1,j))
                            (buildState (i+1,j) xs w n e)
  |otherwise               =trans ((i,j), Symbol x,(i+1,j)) .
                            trans ((i,j), Any, (i,j+1)) .
                            trans ((i,j) , Eps ,(i+1,j+1)) .
                            trans ((i,j), Any, (i+1,j+1))
                            $ buildState (i+1,j) xs w n e

-- jump determines if it's possible to make a transition from one state to other
jump :: (Ord a, Ord t, Ord t1) =>(t1, t) -> Input a -> ENFA (t1, t) a -> Bool
jump (i,j) a e
  |Data.Set.member (i,j) (accepting e)   =False
  |Data.Map.member a (delta e ! (i,j))   =True
  |otherwise                             =False

-- "evaluateEnfa" evaluates the word v at the ENFA e
evaluateEnfa :: (Num t, Num t1, Ord a, Ord t, Ord t1) =>
                [a] -> ENFA (t1, t) a -> Set (t1, t)
evaluateEnfa w e = evaluate w e (0,0)

-- "evaluate" executes the transitions, identifying its kind according to the 
-- input received by the state
evaluate ::(Ord t1, Ord t, Ord a)=>[a]->ENFA (t1, t) a-> (t1, t) -> Set (t1, t)
evaluate [] e (i,j)
  |jump (i,j) Eps e        =evaluate [] e (Data.Set.elemAt 0
                            ((delta e)!(i,j)!Eps))
  |otherwise               =Data.Set.singleton (i,j)
evaluate (x:xs) e (i,j)
  |jump (i,j) (Symbol x) e = evaluate xs e (Data.Set.elemAt 0 ((delta e) !(i,j)
                             ! (Symbol x )))
  |jump (i,j) Any e        = Data.Set.union (evaluate  xs e (Data.Set.elemAt 0
                            ((delta e )! (i,j) ! Any)))
                            (evaluate xs e
                            (Data.Set.elemAt
                            (if (Data.Map.size ((delta e) ! (i,j)) > 1)
                            then 1 else 0) ((delta e) ! (i,j) ! Any)))
  |otherwise               = Data.Set.singleton (i,j)
