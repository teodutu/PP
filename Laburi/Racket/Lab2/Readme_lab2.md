# Lab2

## 1.(rev-iter L R)

- se inverseaza o lista folosind recursivitate pe coada;

- `R` desemneaza lista inversata pana la un anumit moment care va fi returnata cand `L` devine nula.

## 2.(lesser images height)

- se pasteraza din lista `images` doar acele imagini cu inaltimea mai mica decat `height`;

- se utilizeaza functia predefinita in *Racket* `image-height` in functie de rezultatul careia se adauga imaginea curenta la rezultat sau nu.

## 3.(greater images height)

- opusul functiei anterioare, doar ca se pastreaza imaginilie cu inaltimi **mai mari sau egale** cu `height`.

## 4.(remove-dup L R)

- se elimina duplicatele din lista `L` parcursa de la stanga la dreapta folosind recursivitatea pe coada;

- rezultatul se retine in R si se foloseste functia `member` pentru a se determina daca un element mai exista in restul listei de la un anumit punct.

## 5.(remove-duplicates-right L)

- acum se elimina duplicatele mergand de la dreapta la stanga folosind recursivitate pe stiva.

## 6.(common-prefix L1 L2)

- se determina prefixul comun a 2 liste.

## 7.(check-suffix L1 L2 res)

- sufixul comun se calculeaza in `O(n)` eliminand initial din lista mai lunga elementele in plus;

- apoi, se verifica daca capetele celor 2 liste sunt egale, caz in care se adauga in resultatul `res`;

- in caz contrar, `res` devine o lista nula.

## 8.(overlay-> initial images)

- se aplica functia `overlay` pe intre fiecare capat de lista si vechea imagine initiala.

## 9.(mergesort-interv L last)

- algoritmul *MergeSort* care aplica functia `mergesort-interv` pe jumatatile unei liste si apoi `merge` pe ce returneaza aceste funtii.

### (merge L1 L2)

- realizeaza interclasarea celor 2 liste date in primul rand in functie de inaltime si apoi de latime.

## 10.(image-subsets images k)

- aplica `overlay-lists` pe fiecare submultime returnata de `make-subsets`;

### (overlay-lists images-list)

- aplica functia `overlay->` pe fiecare lista din setul `images-list` si returneaza o lista cu toate aceste imagini din subliste suprapuse;

- pentru a sorta imaginile, se foloseste *MergeSortul* implementat anterior;

### (make-subsets L subs subL crt-len target-len)

- genereaza toate submultimile lui `L` si le pastreaza doar pe cele de lungime `target-len` (un tractor)...
