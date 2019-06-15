# Lab5 - fluxuri

- se implmenteaza fluxuri (liste infinite) pe baza evaluarii lenese;

- se folosesc functiile predefinite ale *Racket* pentru streamuri:

    - `empty-stream`
    
    - `stream-cons`
    
    - `stream-first`
    
    - `stream-rest`
    
    - `stream-map`

## 1.(list->stream L)

- folosind `empty-stream` si `stream-cons` se formeaza un stream cu elementele listei `L`.

## 2.(stream-zip-with f s1 s2)

- se implementeaza functia `zipWith` din *Haskell* folosind API-ul de streamuri al *Racket*;

- se construieste recursiv un stream (folosind `stream-cons`) cu `f` aplicat pe primele elemente
(`stream-first`) ale fiecarui stream;

- streamul creat se termina cand se termina unul dintre streamurile `s1` sau `s2`.

## 3.(repeat a b)

- se construieste streamul `a, b, a, b...` adaugand `a` la streamul returnat de apelul functiei
`repeat` cu parametrii `a` si `b` inversati.

## 4.leibniz-series

- se formeaza un stream cu [seria lui Leibniz](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)

### leibniz-series-1 si (leibniz-1 k sign)

- leibniz-series-1 apeleaza `(leibniz-1 0 1)`;

- `leibniz-1` cnstruieste efectiv seria adaugand `1 / (2 * k + 1)` la streamul returnat de apelul
aceleiasi functii cu paramaetrii `k + 1` si `sign * -1`;

### leibniz-series-2

- foloseste `stream-map` aplicat pe fluxul numerelor naturale (`naturals`) ca sa formeze fiecare
termen al seriei.

## 5.(play x f1 f2)

- se va aplica folosind `stream-zip-with` implementat anterior rezultatul repetarii (`repeat`) lui `f1` si `f2`
pe un flux format din aplicarea recursiva a lui `play` - foarte autist.

## 6.(winner n)

- se foloseste un *named let* care defineste functia `(find-winner player play-stream)` ;

- aceasta functie returneaza `player` cand prima valoare din `play-stream` (prima mutare disponibila) este `<= 0`;

- in rest, continua sa se apeleze reciproc alternand jucatorii si eliminand la fiecare apel prima valoare din stream.

## 7.(not-prime? n)

- se verifica daca `n` este sau nu prim incercand prin calcularea resturilor impartirii lui `n` la numerele prime
din fluxul`primes` pana la `(sqrt n)`.

## 8.(rotations n)

- se foloseste de functia auxiliara `(rotate n len step)` si de tansformari din numar in string pentru a putea
manipula mai usor cifrele.

### (rotate n len step)

- se apeleaza recursiv crescand `step` (numarul de cifre rotite) cu 1 pana cand acesta ajunge egal cu `len`
(s-a ajuns din nou la numarul initial).

## 9.(circular-prime? n)

- foloseste `stream-andmap` impreuna cu o functie *lambda* care neaga rezultatul lui `not-prime?` pe
fluxul rotatiilor lui `n`, generat cu functia anterioara.

## 10.(sorted-lists D)

- se face un stream cu listele de numere formate cu cifrele din `D` folosind functia `(make-next-list D L res)`.

### (make-next-list D L res)

- genereaza o lista de numere prin adaugarea fiecarei cifre din `D` la fiecare numar din `L` si
salveaza rezultatul in `res`.
