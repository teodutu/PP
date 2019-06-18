# Lab9 - clase Haskell

- se implementeaza un *Prioroty Queue* in 2 moduri:

    - printr-o lista;

    - printr-un [Leftist Tree](https://en.wikipedia.org/wiki/Leftist_tree)

# PQueue

- expune metodele tipice ale unei cozi cu prioritati:

    - `empty :: pq a`;

    - `isEmpty :: pq a -> Bool`;

    - `insert :: (Prio, a) -> pq a -> pq a`;

    - `top :: pq a -> Maybe (Prio, a)`;

    - `pop :: pq a -> pq a`;

    - `fromList :: [(Prio, a)] -> pq a`;

    - `toList :: pq a -> [(Prio, a)]`;
    
    - `size :: pq a -> Int`;

- dintre acestea, ultimele 3 sunt implementate direct in clasa, restul fiind nevoie sa se implementeze
la instantieri.

## ListPQ

- construieste o coada de prioritati sub forma unei liste de perechi `(prioritate, element)`;

- `O(n)` spatiu;

- timp `(O(n))` pentru toate operatiile.

## LeftistPQ

- foloseste un *Leftist Heap*;

- similar cu heapul ca principiu de functionare, cu precizarea ca intr-un *Leftist Heap*, copilul
stang al unui nod este mai lung decat cel drept;

- structura de date se preteaza mai bine limbajelor functionale, toate operatiile fiind bazate pe
`merge` intre 2 heapuri *Leftist*.

### insert

- se face `merge` intre heapul vechi si un heap ce contine doar elementul ce trebuie inserat.

### pop

- se aplica `merge` pe copiii nodului din varful heapului.
