# Levenshtein.hs

* Paola Fernández López - pfernan4(at)eafit(dot)edu(dot)co
* Juliana Vallejo Díez - jvalle22(at)eafit(dot)edu(dot)co

## Function definition:

Levenshtein function receives an string w, an int n and a list of strings v, and
returns a boolean list where each element corresponds to the value of truth 
gotten from evaluate if w is at a distance n separated from each word in v, 
respectively. This evaluation is done using a Levenshtein automaton, developed
by Klaus U. Schulz and Stoyan Mihov (2002). 
This program creates the Levenshtein automaton considering its states like 
floors, and its connections will depend of its transition kind (epsilon, any, 
symbol), then, the final purpose is to determinate if after go over an automaton 
the final state is an acceptation or negation state

## Usage:

levenshtein :: String -> Int -> [String] -> [Bool] 

To do a levenshtein calculation, initially, enter the reference word w and the 
maximun distance n to consider, and after that, give a list of words to evaluate


* GHC, version 8.0.2
* Haskell Platform 8.0.2
