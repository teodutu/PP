# Lab7 functionale, legare, streamuri

- cam ce e si in labul trecut;


## 1. myCycle :: [a] -> [a]

- se reimplementeaza `cycle`.


## 2.1. arithmetic :: Num a => a -> a -> [a]

- generaza o progresie aritmetica.


## 2.2. geometric :: Num a => a -> a -> [a]

- generaza o progresie geometrica.


## 3. randoms :: Int -> [Word8]

- genereaza un stream de numere random, pornind de la un seed;

- se foloseste `mkStdGen` din *System.Random*.


## 4. wordToAlpha :: Word8 -> Char

- converteste un numar din intervalul `[0, 255]` la o minuscula;

- "overflowurile" se trateaza prin `mod`.


## 5. randomAlphaKey :: Int -> String

- ca `randoms`, doar ca genereaza litere;

- implementat pe baza lui `randoms`, cu un `map`...


## 6.1. tableToFunc :: [(Char, Char)] -> Char -> Char

- returneaza o functie care realizeaza asocierile dintre caracterele din lista pe care o primeste ca parametru;

- implementata (degeaba) cu `where`.


## 6.2 substCrypt :: [(Char, Char)] -> String -> String

- aplica `tableToFunc`  pe un *String* pentru a-l cripta.


## 7. genRotTable :: Int -> [(Char, Char)]

- se creeaza o tabela `caracter_clar - caracter_criptat` folosind `zip`;

- criptarea presupune rotatia cu un `offset` dat ca parametru.


## 8. encryptVigenere :: String -> String -> String

- aplica o criptare *Vigenere* unui *String* cu o cheie, ambele date ca parametri;

- se foloseste un `zipWith` cu o functia `tableToFunc` si o tabela de criptare construita local;

- tabela se obtine prin `genRotTable` cu offseturile fiecarei cifre fata de `'a'`, ciclata la infinit.

## 9. xorStrings :: [Char] -> [Char] -> [Char]

- sirurile pot avea lungimi diferite, deci cel mai scurt este extins prin ciclare, din care se extrage un sir de lungimea celui lung;

- apoi, se aplica `xor` pe sirurile convertite in numere prin intermediul functionalei `zipWith`.

## 10. rollingXor :: [Char] -> [Char] -> [Char]

- la fel ca `xorStrings`...
