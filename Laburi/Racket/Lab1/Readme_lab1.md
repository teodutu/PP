# Lab1

## (num->base n b)

- se transforma un numar dat intr-o baza data tot ca parametru.

## (rev L)

- implementarea lui `reverse`.

## (palindrome? L)

- se verifica daca o lista este palindrom, adica daca este egala cu inversa sa.

## (all-palindromes? n B)

- se verifica daca `n` este palindrom in toate bazele din lista B;

- se iau bazele pe rand, se converteste numarul in fiecare baza cu `num->base` si se verifica daca numarul lista rezultata este palindrom cu `palindrome?`.

## 5.(palindromes-to-n n Bases)

- se cer toate numerele mai mici sau egale cu `n` care sunt palindroame in toate bazele din `bases`;

- se pleaca de la `n` si se decrementeaza primul parametru, verificand la fiecare apel daca este palindrom in toate bazele `Bases`.

## (first-b-pal start b)

- se doreste primul numar mai mare decat `start` care sa fie palindrom in minimum `b` baze dintre bazele `'(2 3 4 5 6 7 8 9 10)`;

- se defineste functia `(count-palindrome-bases n base found target)` care daca a gasit `target` baze (retinute in parametrul `found`) returneaza `#t`, iar daca a depasit baza `10`, returneaza `#f`;

- pentru a verifica daca un numar este palindrom, aplica functia `palindrome?` listei obtinuteprin `num->base` cu baza curenta.

## (longest-palindrome n)

- se cauta cea mai lunga portiune din `n` care e ppalindrom;

- se transforma n in lista cu `(num->list n)` si se ia maximul dintre cel mai lung palindrom obtinut pastrand partea dreapta a numarului, si cel mai lung folosind partea stanga.
