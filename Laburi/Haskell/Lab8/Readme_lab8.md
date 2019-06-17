# Lab8 - tipuri de date

- se vor defini si implementa functii pe urmatoarele tipuri de date:

	- `Vector` (in sens matematic);

    - `SList` (lista de liste);

    - `BST`;

    - `Tree`;

    - `DList` (lista partial construita.

## Vector

### lengthV :: Vector -> Double

- calculeaza lungimea unui vector (Pitagora adica).

### normalizeV :: Vector -> Vector

- normeaza vectorul cu norma data de lungimea sa.

### dotV :: Vector -> Vector -> Double

- calculeaza produsul scalar a 2 vectori.

### crossV :: Vector -> Vector -> Vector

- calculeaza produsul vectorial a 2 vectori.

### addV :: Vector -> Vector -> Vector

- aduna 2 vectori.

### subV :: Vector -> Vector -> Vector

- scade 2 vectori.

### orthogonalV :: [Vector] -> Bool

- verifica daca o lista de vectori sunt ortogonali;

- aplica `dotV` pe oricare 2 vectori folosind `map`

- daca toate aceste produse scalare sunt 0, vectorii sunt ortogonali, altfel nu.

## SList

- implementeaza functionalitatile obisnuite ale listelor dar pentru liste imbricate pe oricate
niveluri;

### emptySList :: SList a

- creeaza o lista vida

### consElem :: a -> SList a -> SList a

- adauga un element la un `SList`;

- elementul se va adauga sub forma unui *atom* (`Elem`) la inceputul listei.

### consList :: SList a -> SList a -> SList a

- similar cu `consElem`, dar adauga o noua lista imbricata la o lista deja existenta;

- daca lista la care se adauga contine un singur element (`Elem`), lista adaugata este pur si simplu
inserata inainte de acesta;

- daca trebuie adaugat un `SList` la altul, primul `SList` se concateneaza la prima lista din cel
de-al doilea.

### headSList :: SList a -> SList a

- extrage prima *chestie* din `SList`: fie un `Elem` fie o alta lista.

### tailSList :: SList a -> SList a

- similar cu `tail`.

### deepEqual :: Eq a => SList a -> SList a -> Bool

- verifica daca 2 liste imbricate sunt egale pana la nivel de element.

### flatten :: SList a -> [a]

- aduce toate listele din `SList` la acelasi nivel, rezultand o lista simpla.

## data BST

- implementeaza functiile clasice ale unui *Arbore Binar de Cautare*:

    - `insertElem :: (Ord a, Eq a) => BST a -> a -> BST a`;

    - `findElem :: (Ord a, Eq a) => BST a -> a -> Maybe a`;

    - `subTree :: (Ord a, Eq a) => BST a -> a -> a -> Maybe (BST a)`;

    - `inorder :: BST a -> [a]`.
