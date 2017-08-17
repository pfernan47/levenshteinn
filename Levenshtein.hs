module Levenshtein where

import ENFA
import Data.Map
import Data.Set
import Data.List

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

isFinalState (i,j) w n
  |i== (length w) && j==n     =True
  |otherwise                  =False

isUpperState (i,j) w n
  |j==n && i/= (length w)     =True
  |otherwise                  =False

isRightState (i,j) w n
  |i== (length w) && j /=n    =True
  |otherwise                  =False

buildEnfa w n = buildState (0,0) w w n (initENFA (0,0))
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

-- evaluateEnfa evaluates the word v at the ENFA e
evaluateEnfa :: (Num t, Num t1, Ord a, Ord t, Ord t1) =>
                [a] -> ENFA (t1, t) a -> Set (t1, t)
evaluateEnfa w e = evaluate w e (0,0)


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
