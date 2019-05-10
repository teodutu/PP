{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A
import Data.List
import Data.Maybe

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc.
-}
hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezentarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}
type Position = (Int, Int)

{-
    Direcțiile în care se poate mișca blocul de pe tablă
-}
data Directions = North | South | West | East
    deriving (Eq, Ord, Show)

{-
    Constructori pentru celule, de tipurile prezente in joc.
    Switchurile retin lista de pozitii pe care le modifica si daca sunt activate
    sau nu
-}
data Cell = EmptyCell | HardCell | SoftCell | BlockCell | WinCell |
            SwtichCell { changedCells :: [Position], activated :: Bool }
    deriving (Eq, Ord)

instance Show Cell where
    show cell = case cell of
      HardCell         -> [hardTile]
      SoftCell         -> [softTile]
      BlockCell        -> [block]
      (SwtichCell _ _) -> [switch]
      EmptyCell        -> [emptySpace]
      WinCell          -> [winningTile]

{-
    Constructorii pentru Level.
    Sunt retinute pozitiile blocului (crtPos1 fiind mai apropiada de (0, 0)) si
    celulele ca un Array.
-}
data Level = EmptyLevel | Lv { crtPos1 :: Position,
                               crtPos2 :: Position,
                               lvlCells :: (A.Array Position Cell) }
    deriving (Eq, Ord)

{-
    Se instantiaza Level pe Show afisandu-se harta jocului folosind caracterele
    de mai sus.
    In cazul in care jocul este terminat (pierdut sau castigat), se adauga in
    Stringul returnat si mesajul corespunzator.

    Functia `showCell` returneaza fie `show` aplicat unei celule, fie `block`,
    daca este apelata pe o pozitie pe care se gaseste blocul.

    showCell :: (pozitie curenta, celula) -> pozitie a blocului
                -> pozitie a blocului -> String cu caracterul de pe pozitia
                curenta
-}
showCell :: (Position, Cell) -> Position -> Position -> String
showCell mapCell block1 block2
  | fst mapCell == block1 || fst mapCell == block2 = [block]
  | otherwise                                      = show (snd mapCell)

instance Show Level where
    show EmptyLevel = ""
    show lvl@(Lv pos1 pos2 cells) = foldl printCell "" cellList ++ crtState
      where
        cellList = A.assocs cells
        crtState
          | isGoal lvl   = "\nCongrats! You won!\n"
          | lostLevel lvl = "\nGame Over\n"
          | otherwise    = "\n"
        printCell out crtCell
          | snd (fst crtCell) == 0 = out ++ "\n" ++ cellToString
          | otherwise              = out ++ cellToString
            where
              cellToString = showCell crtCell pos1 pos2

{-
    Functia creeaza un nivel ce contine doar un bloc si, in rest, celule goale.

    emptyLevel :: limita dreapta-jos a matricei -> pozitia de start -> nivel
-}
emptyLevel :: Position -> Position -> Level
emptyLevel (l, c) start = Lv start start justABlock
  where
    cells =
      A.array ((0, 0), (l, c)) [((i, j), EmptyCell) | i <- [0..l], j <- [0..c]]
    justABlock = cells A.// [(start, BlockCell)]

{-
    Functia adauga o noua celula in nivel.

    addTile :: caracter simbolizand celula -> pizitia celulei -> nivel initial
               -> nivel dupa inserare
-}
addTile :: Char -> Position -> Level -> Level
addTile _ _ EmptyLevel                    = EmptyLevel
addTile cellType pos (Lv pos1 pos2 cells) = Lv pos1 pos2 newCells
  where
    newCells = cells A.// [(pos, newCell)]
    newCell  = case cellType of
      'H' -> HardCell
      'S' -> SoftCell
      'W' -> WinCell
      _   -> EmptyCell

{-
    Adauga un swtich in nivelul curent.
    
    addSwitch :: pozitia switchului -> pozitiile comandate de acesta
                 -> nivel initial -> nivel dupa adaugare
-}
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch _ _ EmptyLevel                   = EmptyLevel
addSwitch pos changed (Lv pos1 pos2 cells) = Lv pos1 pos2 newCells
  where
    newCells = cells A.// [(pos, (SwtichCell changed False))]

{-
    Modifica in harta celulele gestionate de acest switch tinand seama de
    starea sa (apasat sau nu).
    Cand celula nu este switch, nu se modifica nimic

    activate :: celula -> nivel initial -> nivel final
-}
activate :: Cell -> Level -> Level
activate (SwtichCell poss activ) (Lv pos1 pos2 cells)
  | activ == False = Lv pos1 pos2 (makeCells HardCell)
  | otherwise      = Lv pos1 pos2 (makeCells EmptyCell)
    where
      makeCells cellType = cells A.// (map (\ pos -> (pos, cellType)) poss)
activate _ lvl = lvl

{-
    Returneaza daca o celula este switch sau nu.
-}
isSwitch :: Cell -> Bool
isSwitch (SwtichCell _ _) = True
isSwitch _                = False

{-
    Inverseaza starea unui switch.
    Se aplica doar dupa ce un switch a fost activat.

    update :: pozitia switchului -> nivel initial -> nivel modificat
-}
update :: Position -> Level -> Level
update _ EmptyLevel = EmptyLevel
update pos lvl@(Lv pos1 pos2 cells )
  | isSwitch crtCell = Lv pos1 pos2 $ cells A.// [(pos, newSwitch)]
  | otherwise        = lvl
    where
      crtCell   = cells A.! pos
      newSwitch = SwtichCell (changedCells crtCell) (not $ activated crtCell)

{-
    Executa mutarea efectiva a blocului, activand noile celule pe care acesta
    se pozitioneaza si updatand si switchurile de pe ele.
    Se foloseste de o serie de deplasamente ale liniilor si coloanelor fata de
    cele initiale.

    moveBlock :: pozitie initiala -> pozitie initiala -> celule
                 -> deplasanent linie -> deplasanent coloana
                 -> deplasanent linie -> deplasanent coloana -> nivel nou
-}
moveBlock :: Position -> Position -> A.Array Position Cell -> Int -> Int
             -> Int -> Int -> Level
moveBlock (l1, c1) (l2, c2) cells dl1 dc1 dl2 dc2
  | p1 == p2  = update p1 $ activate s1 $ newLevel
  | otherwise = update p1 $ activate s1 $ update p2 $ activate s2 $ newLevel
    where
      p1 = (l1 + dl1, c1 + dc1)
      p2 = (l2 + dl2, c2 + dc2)
      newLevel = Lv p1 p2 cells
      s1 = cells A.! p1
      s2 = cells A.! p2

{-
    Muta blocul in directia primita ca parametru si mentine prima pozitie a
    acestuia mai apropiata de (0, 0) decat cea de-a doua.
    Apeleaza moveBlock cu cele 2 pozitii, celulele si deplasamentele
    corespunzatoare fiecarui caz.

    move -> directie -> nivel initial -> nivel cu blocul mutat
-}
move :: Directions -> Level -> Level
move _ EmptyLevel = EmptyLevel
move dir lvl@(Lv pos1 pos2 cells)
  | continueGame lvl == False = lvl
  | pos1 == pos2 = case dir of        -- bloc vertical
    North -> moveBlock pos2 pos1 cells (-2) 0 (-1) 0
    South -> moveBlock pos1 pos2 cells 1 0 2 0
    East  -> moveBlock pos1 pos2 cells 0 1 0 2
    West  -> moveBlock pos2 pos1 cells 0 (-2) 0 (-1)
  | snd pos1 < snd pos2 = case dir of  -- bloc orizontal paralel cu Ox
    North -> moveBlock pos1 pos2 cells (-1) 0 (-1) 0
    South -> moveBlock pos1 pos2 cells 1 0 1 0
    East  -> moveBlock pos1 pos2 cells 0 2 0 1
    West  -> moveBlock pos1 pos2 cells 0 (-1) 0 (-2)
  | otherwise = case dir of            -- bloc orizontal paralel cu Oy
    North -> moveBlock pos1 pos2 cells (-1) 0 (-2) 0
    South -> moveBlock pos1 pos2 cells 2 0 1 0
    East  -> moveBlock pos1 pos2 cells 0 1 0 1
    West  -> moveBlock pos1 pos2 cells 0 (-1) 0 (-1)

{-
    Returneaza daca un nivel este pierdut sau nu.
-}
lostLevel :: Level -> Bool
lostLevel EmptyLevel = False
lostLevel (Lv pos1 pos2 cells)
  | cells A.! pos1 == EmptyCell || cells A.! pos2 == EmptyCell    = True
  | numCorrect /= 2 && pos1 /= pos2                               = True
  | numCorrect == 1 && pos1 == pos2 && cells A.! pos1 == SoftCell = True
  | otherwise                                                     = False
    where
      indexes    = A.indices cells
      correctPos = filter (\i -> i == pos1 || i == pos2) indexes
      numCorrect = length correctPos

{-
    Returneaza True daca jocul nu este nici castigat nici pierdut si False
    altfel.
-}
continueGame :: Level -> Bool
continueGame EmptyLevel = False
continueGame lvl = not (lostLevel lvl) && not (isGoal lvl)

{-
    Se instantiaza ProblemState pe Level si Directions.

    Se implementeaza `successors` pentru a returna lista tuturor mutarilor care
    nu duc la "Game Over" ce se pot face din starea curenta.

    `isGoal` returneaza True daca blocul este vertical pe un `WinCell`.

    Functia `heuristic` returneaza minimul dintre distantele Manhattan de la
    cele 2 pozitii ale blocului la `WinCell`.
-}
instance ProblemState Level Directions where
    successors lvl
      | isGoal lvl = []
      | otherwise  = filter (\state -> not (lostLevel (snd state))) dirs
        where
          northLevel = (North, move North lvl)
          southLevel = (South, move South lvl)
          eastLevel  = (East, move East lvl)
          westLevel  = (West, move West lvl)
          dirs = [northLevel, southLevel, eastLevel, westLevel]

    isGoal EmptyLevel = False
    isGoal (Lv pos1 pos2 cells) = pos1 == pos2 && cells A.! pos1 == WinCell

    heuristic EmptyLevel = maxBound :: Int
    heuristic (Lv (l1, c1) (l2, c2) cells) = min dist1 dist2
      where
        dist1 = abs (l1 - winL) + (c1 - winC)
        dist2 = abs (l2 - winL) + (c2 - winC)
        (winL, winC) = fst $ fromJust $ find (\c -> snd c == WinCell) cellList
        cellList = A.assocs cells
