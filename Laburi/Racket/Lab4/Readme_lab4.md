# Lab 4

- legari de variabile si inchideri functionale

## 1.(compute-square-area get-length)

- se creeaza variabila `l` ce pastreaza rezultatul lui `get-length` si se ridica la patrat.

## 2.(compute-length get-line-segment get-start-point get-end-point)

- parametrii sunt toti functii ale caror rezultate sunt salvate succesiv in variabile printr-un `let*`, pana la punctele de pe `Ox` si `Oy` cu care se calculeaza lungimea.

## 3.(distance p1 p2)

- calculeaza distanta dintre punctele date folosind `compute-length`;

- pentru aceasta, se definesc niste `lambdauri` care reprezinta functiile date ca parametri lui `compute-length`.

## 4.(compute-f-with-step f a b step)

- returneaza o lista cu `f(a), f(a + step), ..., f(b)`;

- cu un `named let` se defineste functia recursiva `calculate` care va aplica f pe toate valorile din intervalul `[a, b]` cu pasul `step`.

## 5.(num-concat x y)

- se redefineste functia `+` care acum devine un `lambda` care apeleaza `string-append` pe transformarile in stringuri a celor 2 nr `x` si `y`;

- apoi, rezultatul este retransformat in numar cu `string->number`.

## 6.(compute-perimeter points)

- cu `named let`, functia `perim` returneaza perimetrul cerut, primind lista de puncte ramase ca paramteru in `current-points`;

- apoi, calculeaza distantele intre 2 puncte adiacente cu `distance` si le aduna.

## 7.(3-sequence-max numbers separator)

- `numbers` este o lista de 3 secvente de numere separate prin `separator` si se doreste suma maxima a acestor secvente;

- se creeaza variabile `secvk`, `k = 1:3` pentru secventele dorite si apoi se returneaza maximul cerut, folosind functia auxiliara `sum-list` care returneza suma unei liste folosind `apply`.

## 8.(find-all-suffixes number)

- initial, se transforma numarul dat `number` in lista cu `number->list`, iar rezultatul se salveaza in variabila `num-list`;

- apoi, se aplica functia definita local, `add-suffix` pe toate sufixele listei `num-list`, rezultatul fiind adaugat intr-o noua lista, folosind o functie nou-definita: `list-num-concat`;

### (list-num-concat numbers)

- similar cu `num-concat` dar functioneaza pe oricate numere (date prin lista `numbers`);

- din acest motiv, se foloseste `foldr`, cu acumulatorul `0`, iar rezultatul se imparte la 10 pentru a elimina acest acumulator.

## 9.(run initial-state final-state next)

- simuleaza un automat finit care aplica functia `next` recursiv de la starea `initial-state` pana ajunge la `final-state`;

- returneaza o lista cu toate starile de la `initial-state` la `final-state`.

## 10.(generate-number k x)

- trecerile de la o cifra la alta pot fi vazute ca un automati finit ce merge de la `x` la `x + k * (k - 1)`;

- asadar se foloseste `run` pentru a crea o lista cu starile, ale carei elemente se concateneaza intr-un numar folosind `list-num-concat`.
