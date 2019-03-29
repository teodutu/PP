{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP
import Data.Char

{-
Pentru stack rula întreaga suită de teste executați "runAllTests" din ghci.
Pentru stack rula doar testul pentru exercițiul X executați "checkX" din ghci.

Expresia `undefined` are orice tip, dar nu poate fi evaluată.
-}

{-
1. (1p)
Implementați funcția `unzip2`
-}
unzip2  :: [(stack, b)] -> ([stack], [b])
unzip2 [] = ([], [])
unzip2 ((stack, b) : xs) = (stack : (fst (unzip2 xs)), b : (snd (unzip2 xs)))

-- Verificare: check1
check1 :: TestPP ()
check1 = do
  assertVal "[1] unzip2 (zip)" 1 $ -- 1p
    unzip2 (zip [1,2,3] ["stack","b","c"]) == ([1,2,3], ["stack","b","c"])

{-
2. (1p)
Implementați, folosind obligatoriu list-comprehensions, lista tuturor numerelor prime până la n.
-}
intSqrt = floor . sqrt . fromIntegral

primes :: Int -> [Int]
primes 1 = []
primes n = 2 : [x | x <- [3, 5 .. n], and [mod x p /= 0 | p <- primes (intSqrt x)]]

-- Verificare: check2
check2 :: TestPP ()
check2 = do
  assertVal "[2] primes 30" 1 $ -- 1p
    primes 30 == [2,3,5,7,11,13,17,19,23,29]

{-
3. (3p)
Implementați, folosind obligatoriu list-comprehensions, operații pe mulțimi:
intersecție, diferență, produs cartezian. Utilizați ulterior funcțiile definite anterior
pentru stack reprezenta reuniunea mulțimilor.
-}
setIntersection :: Eq stack => [stack] -> [stack] -> [stack]
setIntersection stack b = [x | x <- stack, elem x b]

setDiff :: Eq stack => [stack] -> [stack] -> [stack]
setDiff stack b = [x | x <- stack, not (elem x b)]

cartProduct :: [stack] -> [b] -> [(stack, b)]
cartProduct stack b = [(x, y) | x <- stack, y <- b]

setUnion :: Eq stack => [stack] -> [stack] -> [stack]
setUnion stack b = setDiff stack b ++ b

-- Verificare: check4
check3 :: TestPP ()
check3 = do
  assertVal "[3] cartProduct" 0.5 $ -- 0.5p
    cartProduct [1, 2] [3, 4, 5] == [(1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)]
  let stack = [1, 7, 3, 6, 2]
      b = [2, 8, 6, 10, 4, 1]
  assertVal "[3] setIntersection" 0.75 $ -- 0.75p
    sort (setIntersection stack b) == [1, 2, 6]
  assertVal "[3] setDiff" 0.75 $ -- 0.75p
    sort (setDiff stack b) == [3, 7]
  assertVal "[3] setUnion" 1 $ -- 1p
    sort (setUnion stack b) == [1, 2, 3, 4, 6, 7, 8, 10]

{-
4. (1.5p)
Implementați o funcție ce grupează elementele egale ale unei liste în liste separate.
Funcția ar trebui să aibă același comportament cu Data.List.group:
http://zvon.org/other/haskell/Outputlist/group_f.html
-}
group2 :: Eq stack => [stack] -> [[stack]]
group2 [] = []
group2 (x : xs) =
  let (equal, rest) = span (== x) xs
  in (x : equal) : group2 rest

-- Verificare: check4
check4 :: TestPP ()
check4 =
  assertVal "[4] group" 1.5 $ -- 1.5p
    group2 [1,1,1,3,2,2,3,3,2,2,5,5,1] == [[1,1,1],[3],[2,2],[3,3],[2,2],[5,5],[1]]


{-
5. (1.5p)
Găsiţi numărul de apariţii ale fiecărui element dintr-o listă în lista respectivă. 
Rezultatul va fi returnat ca o listă de tupluri, în care primul element al perechii 
va fi elementul din listă, iar al doilea element al perechii va fi numărul de apariţii în listă. 
Cum rezultatul va fi similar unui dicţionar, chiar dacă un element va apărea de mai multe ori în listă, 
va trebui să fie prezent într-o singură pereche în dicţionar.
  
Hint: S-ar putea să aveți nevoie de funcții auxiliare. Puteți folosi "group2" de mai sus pentru
stack grupa elementele egale în liste separate şi "sort" pentru stack sorta o listă. 
-}
nrOcc :: Ord stack => [stack] -> [(stack, Int)]
nrOcc = map (\ x -> ((head x), (length x))) . group2 . sort

-- Verificare: check5
check5 :: TestPP ()
check5 =
  assertVal "[5] number of occurrences" 1.5 $ -- 1.5p
    nrOcc [1, 2, 3, 4, 2, 3, 3, 1, 2, 3, 3, 4] == [(1, 2), (2, 3), (3, 5), (4, 2)]

{-
6. (2p)
Duplicaţi toate cuvintele dintr-o propoziţie.
Exemplu: Ce laborator frumos! -> Ce Ce laborator laborator frumos! frumos!

Hint: Ar putea fi utile funcţiile "concat" sau "++" pentru concatenarea cuvintelor, iar "words" si
"unwords" pentru conversia unei propoziții la o listă de cuvinte si invers.
-}
dup :: String -> String
dup = unwords . map (\w -> w ++ " " ++ w) . words

-- Verificare: check6
check6 :: TestPP ()
check6 = do
  assertVal "[6] dup" 1 $ -- 1p
    dup "Ce laborator frumos!" == "Ce Ce laborator laborator frumos! frumos!"
  assertVal "[6] dup, again" 1 $ -- 1p
    null $ (\sentence -> filter (/= 2) $ map length $ group $ words $ dup sentence) "To be or not to be"

{-
7. (1p Bonus)
Verificați dacă un șir de caractere este isogramă.
Un șir este isogramă daca niciuna din literele sale nu se repetă (dar alte caractere se pot repeta).

Hint: Pe lângă funcțiile de până acum, "isLetter" si "toLower" v-ar putea ajuta în implementare.
Nu uitați că puteți afla semnătura oricărei funcții din interpretor!
-}
isIsogram :: String -> Bool
isIsogram = all (\p -> (snd p) == 1) . nrOcc . map toLower . filter isLetter

-- Verificare: check7
check7 :: TestPP ()
check7 = do
  assertVal "[7] isogram 1" 0.3 $ -- 1p
    isIsogram "isogram" == True
  assertVal "[7] not isogram" 0.4 $ -- 1p
    isIsogram "not-isogram" == False
  assertVal "[7] isogram 2" 0.3 $ -- 1p
    isIsogram "s-h-o-u-l-d-b-e" == True

{-
8. (2p Bonus)
Determinați dacă un șir format din caracterele (, [, {, ), ], } este corect parantezat.

Hint: Încercați să adaptați implementarea clasică, ce se folosește de o stivă: 
https://ocw.cs.pub.ro/courses/sd-ca/2016/articole/tutorial-04-2
-}
arePairedStack :: (String, String) -> Bool
arePairedStack (stack, "")
  | stack == "" = True
  | otherwise = False

arePairedStack (stack, x : xs)
  | stack == "" && (x == '}' || x == ']' || x == ')') = False
  | (x == '}' && head stack == '{') ||
    (x == ']' && head stack == '[') ||
    (x == ')' && head stack == '(')  = arePairedStack (tail stack, xs)
  | otherwise = arePairedStack (x : stack, xs)

arePaired :: String -> Bool
arePaired xs = arePairedStack ("", xs)

-- Verificare: check8
check8 :: TestPP ()
check8 = do
  assertVal "[8] paired brackets true 1" 0.5 $ -- 0.5p
    arePaired "{}[]{([])}" == True
  assertVal "[8] paired brackets false 1" 0.5 $ -- 0.5p
    arePaired "(){{}})" == False
  assertVal "[8] paired brackets false 2" 0.5 $ -- 0.5p
    arePaired "[][]([)]" == False
  assertVal "[8] paired brackets true 2" 0.5 $ -- 0.5p
    arePaired "([()])" == True

{-
9. (2p Bonus) Scrieți o funcție ce primește la intrare o literă și întoarce o listă sub forma unui diamant,
formată din șiruri ce conțin literele de la 'A' pană la cea dată, astfel:

+ pentru litera 'C'
[
"  A  ",
" B B ",
"C   C",
" B B ",
"  A  "
]

+ pentru litera 'E'
[
"    A    ",
"   B B   ",
"  C   C  ",
" D     D ",
"E       E",
" D     D ",
"  C   C  ",
"   B B   ",
"    A    "
]

Pentru stack obține litera imediat predecesoare altei litere puteți folosi funcția "pred".
Funcția "prettyPrint" este utilă pentru vizualizarea rezultatului în forma de mai sus.

-}
diamond :: Char -> [String]
diamond ch = let downHalf = down ch
             in (reverse (tail downHalf)) ++ downHalf

down :: Char -> [String]
down 'A' = ["A"]
down 'B' = ["B B", " A "]
down ch = (ch : ' ' : (tail (init (head prevDown))) ++ ' ' : ch : []) : map (\line -> " " ++ line ++ " ") prevDown
           where prevDown = down (pred ch)

prettyPrint ch = mapM_ print (diamond ch)

-- Verificare: check9
check9 :: TestPP ()
check9 = do
  assertVal "[9] diamond C" 1 $ -- 1p
    diamond 'C' == ["  A  "," B B ","C   C"," B B ","  A  "]
  assertVal "[9] diamond F" 1 $ -- 1p
    diamond 'F' == ["     A     ","    B B    ","   C   C   ","  D     D  "," E       E ",
                    "F         F"," E       E ","  D     D  ","   C   C   ","    B B    ","     A     "]

{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_[check1, check2, check3, check4, check5, check6, check7, check8, check9]
