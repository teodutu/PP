# Lab3

- functionale

## 1.(uncurry->curry f)

- se transforma o functie uncurry `f` intr-una curry.

## 2.(curry->uncurry f)

- se transforma o functie curry `f` intr-una uncurry.

## 3.(remove-duplicates-left L)

- se elimina duplicatele pastrand doar prima aparitie, folosind `foldl`.

## 3.(remove-duplicates-right L)

- se elimina duplicatele pastrand doar ultima aparitie, folosind `foldr`.

## 3.(overlay<- initial images)

- se foloseste `fold` ca sa se suprapuna imaginile de la stanga la dreapta.

## 3.(overlay<- initial images)

- ca mai sus, dar se foloseste `foldr` ca sa preia imaginile de la coada la cap.

## 4.(slim-horizontal matrix)

- se iau liniile din matrice cu `map` si se aplica `overlay->` pe ele.

## 5.(matrix-to-image matrix)

- se iau liniile matricei cu `map` si li se aplica `beside` rezultand linii cu o singura imagine care este concatenarea celor intiale;

- apoi, se iau aceste noi linii cu `apply` si se combina folosind `above`.

## 6.(mirror matrix)

- se iau liniile din `matrix` cu `map` si se adauga elementele in ordine inversa.

## 7.(transpose matrix)

- printr-un `map` ce aplica `list` se transforma liniile in coloane;

- liniile sunt procesate prin `apply`.

# 8.(slim-vertical matrix)
`
- pentru a se face `overlay` pe coloane, se transpune matricea folosind `transpose` de mai sus;

- apoi, se aplica `overlay<-` pe fiecare linie (fosta coloana) printr-un `lambda` data ca paramtetru unui `map` pentru a lua toate liniile.

## 9.(rotate-diag L min-height)

- se retin doar imaginile cu inaltimea mai mare decat `min-height`;

- pe aceste imagini (retinute ca lista) se aplica `rotate-images`.

### (rotate-images L line)

- se adauga cate o linie in care elementul de pe diagonala este rotit corespunzator si se returneaza matricea rezultata.

## 10.(scale&overlay L times)

- se elimina duplicatele din `L` prin `remove-duplicates-left`;

- aceste elemente sunt scalate cu `map`;

- lista rezultata e sortata cresccator dupa inaltime;

- apoi se face `overlay<-` pe aceasta noua lista.
