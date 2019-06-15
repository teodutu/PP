# Lab6 - introducere in Haskell

- introducere in Haskell;

- functionale;

- evaluare lenesa;

- constrangeri de tip;

- list comprehensions;

- `$` si `.`;

- stringuri.

## 1. unzip2  :: [(a, b)] -> ([a], [b])

- se foloseste *pattern matching* si se adauga la lista `[a]` primul element din valoarea de retur
a apelului recursiv al lui `unzip` si la `[b]` cel de-al doilea element.

## 2. primes :: Int -> [Int]

- folosind *list comprehensions*, se calculeaza si se returneaza numerele prime prin verificarea pentru
fiecare numar `x` numerelor prime anterioare, mai mici decat `floor $ sqrt $ fromIntegral x`;

## 3. operatii pe multimi cu list comprehensions

- `setIntersection :: Eq a => [a] -> [a] -> [a]`;

- `setDiff :: Eq a => [a] -> [a] -> [a]`;

- `cartProduct :: [a] -> [b] -> [(a, b)]`;

- `setUnion :: Eq a => [a] -> [a] -> [a]`;

## 4. group2 :: Eq a => [a] -> [[a]]

- se foloseste `span` pentru a separa elementele listei egale cu `head xs` de restul, apoi se apeleaza
recursiv pana la `[]`;

- `let` si nu `where` pentru ca ramasite de la *Racket*.

## 5. nrOcc :: Ord a => [a] -> [(a, Int)]

- se determina numarul de aparitii ale fiecarui caracter intr-o lista;

- se sorteaza lista, apoi se apeleaza `group2` pe noua lista, dupa care un `map` a carui functie
returneaza o pereche `(element, lungimeListaElement)`.

## 6. dup :: String -> String

- se converteste propozitia la o lista de cuvinte prin `words`;

- fiecare astfel de lista este "dublata" facandu-i-se append cu ea insasi;

- la final se transforma listele rezultate inapoi intr-o propozitie cu `unwords`.

## 7. isIsogram :: String -> Bool

- se pastreaza din cuvant doar ceea ce este litera prin `filter isLetter`;

- se aduc toate caracterele din noul cuvant la litere mici prin `map toLower`;

- se numara aparitiile acestora prin `nrOcc` si se returneaza True doar daca toate apar o singura data.

## 8. arePaired :: String -> Bool

- se verifica daca un sir este corect parantezat folosind abordarea clasica, aceeac cu o stiva
(implementata ca o lista);

se utilizeaza functia aditionala `arePairedStack :: (String, String) -> Bool` care mai primeste ca
parametru si stiva.

## 9. diamond :: Char -> [String]

- se genereaza mai intai jumatatea inferioara si apoi se adauga rasturnatul sau (jumatatea superioara);

- se foloseste functia auxiliara `down :: Char -> [String]`.
