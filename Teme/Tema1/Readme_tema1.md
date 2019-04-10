# Tema 1 - Interpretor de bytecode Python

- conform [documentatiei Racket](https://docs.racket-lang.org/style/Textual_Matters.html#%28part._.Line_.Width%29), "A line in a Racket file is at most 102 characters wide.";

- asadar, liniile vor avea cel mult 102 caractere (nu am idee de ce fix 102... 
csf? ncsf).

## Reprezentare

### Stiva

- `stack` este reprezentata ca o lista in care varful este primul
element;

- functiile de `push`, `top` si `pop` pe si de pe stiva de executie se vor face
direct, in functiile in care sunt necesare si nu prin wrapperele date in schelet;

- totusi, acestea sunt definite ca fiind `cons`, `car`, respectiv `cdr`, pentru
a opera pe stiva intr-un mod mai... _autentic_;

- `update-stack-machine` a fost modificat pentru a putea primi o lista de
noi campuri ale masinii-stiva;

- pentru a crea noua masina, se apeleaza `update-stack-machine-tail`, care va
construi respectiva masina-stiva folosind recursivitate pe coada.

### Masina-stiva

- `stack-machine` va fi tot o lista ale carei 6 componente sunt
`stack`, `co-varnames`, `co-consts`, `co-names`, `co-code`, `IC`;

- pentru a obtine o anumita componenta a masinii-stiva se va extrage de la
pozitia corespunzatoare din `stack-machine`.

## Functionare

- `run-stack-machine` salveaza in `current-instr` instructiunea de la `IC`ul
curent si din aceasta separa instructiunea efectiva (retinuta in `instruction`)
si parametrul (retinut in `param`);

- in functie de tipul instructiunii, `run-stack-machine` se apeleaza recursiv,
primind ca parametru rezultatul unei functii auxiliare care corespunde unei
anumite instructiuni sau unui grup de instructiuni similare;

- toate functiile auxiliare returneaza o noua masina-stiva plecand de la cea
veche, folosindu-se de `update-stack-machine`;

- toate instructiunile care opereaza cu `TOS` si `TOS1` sunt
apelate prin `(generic-binary stack-machine op)`, in care parametrul `op`
contine functia ce va fi aplicata intre `TOS1` si `TOS`;

- la fel, _jumpurile_ conditionate sunt gestionate prin
`jump-if stack-machine IC bool)`, conditia de jump fiind specificata prin
parametrul `condition` (`#t` sau `#f`);

- recursivitatea functiei `run-stack-machine` se incheie cand se ajunge la o
instructiune de tipul `RETURN_VALUE`, moment in care masina-stiva este returnata.

## Bonus

- se iau de pe stiva argumentele in ordine, se stocheaza intr-o lista (`argv`)
si se aplica functia necesara obtinuta pe baza hashului din `opcodes.rkt`
pe aceasta lista.
