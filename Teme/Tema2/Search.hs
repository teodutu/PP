{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Ord

{-
    Un nod poate fi ori nul (Nil), cum e parintele radacinii sau poate contine
    o stare, actiunea-cauza, parintele nodului, adancimea si succesorii sai.
-}
data Node s a = Nil | StateNode { nodeState :: s,
                                  lastAction :: Maybe a,
                                  parentNode :: Node s a,
                                  crtDepth :: Int,
                                  succs :: [Node s a] }
                                  deriving (Eq)

{-
    Impreuna cu `createStateSpace`, rezolva problema "tying the knot" pentru
    spatiul starilor.

    makeStates :: stare curenta -> actiune precedenta -> parinte -> adancime
                 -> nodul creat

    createStateSpace :: stare initiala -> radacina spatiului starilor
-}
makeStates :: (ProblemState s a) => s -> a -> Node s a -> Int -> Node s a
makeStates s a parent depth = node
  where
    node     = StateNode s (Just a) parent depth newSuccs
    newSuccs = map (\(st, act) -> makeStates act st node $ depth + 1) nexts
    nexts    = successors s

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace s = node
  where
    node     = StateNode s Nothing Nil 0 newSuccs
    newSuccs = map (\(st, act) -> makeStates act st node 1) $ successors s

{-
    Ordoneaza spatiul starilor in functie de euristica implementata in
    `Bloxorz.hs`.

    orderStateSpace :: radacina initiala -> radacina starilor ordonate
-}
orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace Nil = Nil
orderStateSpace (StateNode state action parent depth nexts) = newNode
  where
    newNode     = StateNode state action parent depth newSuccs
    newSuccs    = map orderStateSpace sortedSuccs
    sortedSuccs = L.sortBy (comparing (heuristic . nodeState)) nexts

{-
    Executa o parcurgere DFS facuta iterativ, cu recursivitate pe coada, pana la
    o adancime maxima prestabilita, folosindu-se de un `Set` pentru a retine
    starile vizitate, iar cand toti copiii unui nod au fost vizitati, se
    intoarce la parintele respectivului nod.

    limitedDFS :: lista de noduri vizitate la fiecare apel-> stari vizitate
                  -> adancime maxima -> lista de noduri vizitate

    limitedDfs :: nod radacina -> adancime maxima -> lista de noduri parcurse
-}
limitedDFS :: (ProblemState s a, Ord s) =>
              [Node s a] -> S.Set s -> Node s a -> Int -> [Node s a]
limitedDFS _ _ Nil _ = []
limitedDFS nodes vis (StateNode _ _ parent depth succNodes) maxDepth
  | depth == 0 && null notVis        = nodes
  | depth == maxDepth || null notVis = limitedDFS nodes vis parent maxDepth
  | otherwise                        = limitedDFS newNodes newVis next maxDepth
    where
      newNodes = nodes ++ [next]
      newVis   = S.insert (nodeState next) vis
      notVis   = filter (\n -> not $ S.member (nodeState n) vis) succNodes
      next     = head notVis

limitedDfs :: (ProblemState s a, Ord s) => Node s a -> Int -> [Node s a]
limitedDfs root maxDepth = root : (limitedDFS [] vis root maxDepth)
  where
    vis = S.insert rootState S.empty
    rootState = nodeState root

{-
    Aplica DFS cu adancimi maxime din ce in ce mai mari, pana ajunge la un nod
    tinta, pe care-l returneaza impreuna cu numarul de noduri parcurse pana
    la acesta.

    iterativeDeepening :: nod radacina -> pereche de nod tinta si numar de
                          noduri parcurse pana la el
-}
iterativeDeepening :: (ProblemState s a, Ord s) => Node s a -> (Node s a, Int)
iterativeDeepening root = (goalNode, numNodes + nodesToGoal)
  where
    allDFSs     = map (limitedDfs root) [0..] 
    nodes       = head $ filter (any (isGoal . nodeState)) allDFSs
    nodesToGoal = length $ takeWhile (not . isGoal . nodeState) nodes
    noGoalLists = takeWhile (not . any (isGoal . nodeState)) allDFSs
    numNodes    = length $ concat noGoalLists
    goalNode    = fromJust (L.find (isGoal . nodeState) nodes)

{-
    Returneaza calea de la un nod la radacina grafului, cu exceptia acesteia
    din urma, impreuna cu actiunile care generaza starile.

    extractPath :: nod de start -> lista de perechi de actiuni-cauza si stari
-}
extractPath :: Node s a -> [(a, s)]
extractPath node = reverse path
  where
    parents = takeWhile (\n -> crtDepth n > 0) $ iterate parentNode node
    path    = map (\n -> (fromJust (lastAction n), nodeState n)) parents

{-
    Pornind de la o stare initiala, se returneaza o lista de perechi
    (actiune determinanta, stare) de la starea initiala la cea dorita.

    solve :: stare initiala -> se foloseste euristica?
             -> lista de stari si actiuni pana la tinta
-}
solve :: (ProblemState s a, Ord s) => s -> Bool -> [(a, s)]
solve initial useHeuristic = extractPath $ fst $ iterativeDeepening $ states
  where
    initialStates = createStateSpace initial
    states
      | useHeuristic = orderStateSpace initialStates
      | otherwise    = initialStates
