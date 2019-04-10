#lang racket

;; Teodor-Stefan Dutu 325 CA

(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; Stiva este o lista iar operatiile efectuate pe aceasta sunt:
(define empty-stack '())
(define (make-stack) empty-stack)
(define (push element stack) (cons element stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))


;; Masina-stiva este o lista de liste, fiecare dintre listele interioare fiind un camp dintre cele
;; citite + `instruction-counter`ul.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Asadar, getterii acceseaza elemente de la anumiti indecsi ai masinii-sitva.
(define (get-varnames stack-machine) (second stack-machine))
(define (get-consts stack-machine) (third stack-machine))
(define (get-names stack-machine) (fourth stack-machine))
(define (get-code stack-machine) (fifth stack-machine))
(define (get-stack stack-machine) (first stack-machine))
(define (get-IC stack-machine) (sixth stack-machine))


;; Functia creeaza efectiv noua masina pe coada.
(define (update-stack-machine-tail items res stack-machine)
  (cond
    [(null? items) (reverse res)]
    [(null? (car items)) (update-stack-machine-tail (cdr items)
                                                    (cons (car stack-machine) res)
                                                    (cdr stack-machine))]
    [else (update-stack-machine-tail (cdr items) (cons (car items) res) (cdr stack-machine))]))

;; Acum functia primeste o lista de noi componente ale masinii pe care le va modifica.
;; Componentele ce raman neschimbate sunt date ca fiind '().
(define (update-stack-machine items stack-machine)
  (update-stack-machine-tail items '() stack-machine))


;; Se creeaza o noua masina a carei stiva contine stiva veche careia i s-a aduagat
;; `co-consts[const-index]`.
(define (load-const stack-machine const-index)
  (let* ([crt-const (hash-ref (get-consts stack-machine) const-index)]
         [new-stack (push crt-const (get-stack stack-machine))]
         [new-IC (add1 (get-IC stack-machine))])
    (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine)))

;; Functia returneaza o masina din a carei stiva s-a eliminat varful si care are
;; `co-varnames[varname-index] = TOS_vechi`.
(define (store-fast stack-machine varname-index)
  (let* ([stack (get-stack stack-machine)]
         [new-varnames (hash-set (get-varnames stack-machine) varname-index (top stack))]
         [new-stack (pop stack)]
         [new-IC (add1 (get-IC stack-machine))])
    (update-stack-machine (list new-stack new-varnames '() '() '() new-IC) stack-machine)))

;; Functia returneaza o masina a carei stiva contine `co-varnames[varname]` la varf.
(define (load-fast stack-machine varname)
  (let* ([var (hash-ref (get-varnames stack-machine) varname)]
         [new-stack (push var (get-stack stack-machine))]
         [new-IC (add1 (get-IC stack-machine))])
    (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine)))


;; Functia aplica operatia data ca parametru (`op`) intre `TOS1` si `TOS`, pe care le elimina de pe
;; stiva, iar rezultatul este pus inapoi pe stiva.
;; Functia e folosita la operatiile matematice si la comparari.
(define (generic-binary stack-machine op)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)]
         [TOS1 (top (pop stack))]
         [new-stack (push (op TOS1 TOS) (pop (pop stack)))]
         [new-IC (add1 (get-IC stack-machine))])
    (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine)))


;; Executa un jump conditionat de parametrul boolean `bool`.
;; Daca `TOS` este `bool`, se sare la `IC`, altfel, se trece la urmatoarea instructiune.
(define (jump-if stack-machine IC condition)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)]
         [new-stack (pop stack)])
    (if (equal? TOS condition)
        (let ([new-IC (quotient IC 2)])
          (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine))
        (let ([new-IC (add1 (get-IC stack-machine))])
          (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine)))))

;; Executa un jump neconditionat la IC.
(define (jump-absolute stack-machine IC)
  (update-stack-machine (list '() '() '() '() '() (quotient IC 2)) stack-machine))


;; Se definesc operatii pe iteratori:
(define current car)
(define next cdr)

;; Se scoate un element din iterator si se pune pe stiva pana se goleste iteratorul.
;; Cand se intampla acest lucru, se sare la `IC + delta / 2 + 1`.
(define (for-iter stack-machine delta)
  (let* ([stack (get-stack stack-machine)]
         [iterator (top stack)])  ; iteratorul
    (if (null? iterator)  ; daca iteratorul e nul, se sare peste for si daca nu, se continua iterarea
        (let ([next-stack (pop stack)]
              [new-IC (+ (get-IC stack-machine) (quotient delta 2) 1)])
          (update-stack-machine (list next-stack '() '() '() '() new-IC) stack-machine))
        (let ([next-stack (push (current iterator) (push (next iterator) (pop stack)))]
              [new-IC (add1 (get-IC stack-machine))])
          (update-stack-machine (list next-stack '() '() '() '() new-IC) stack-machine)))))


;; Incarca pe stiva o functie preluata ca string prin intermediul hashtable-ului "functions"
(define (load-global stack-machine index)
  (let* ([f (hash-ref (get-names stack-machine) index)]
         [new-stack (push f (get-stack stack-machine))]
         [new-IC (add1 (get-IC stack-machine))])
    (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine)))

;; Daca functia are un singur argument se va apela doar cu acesta.
;; Functiile cu mai multi parametri ii vor primi sub forma unei liste.
;; In ambele cazuri, rezultatele vor fi stocate pe stiva.
(define (call-function stack-machine argc)
  (let get-args ([stack (get-stack stack-machine)] [argv '()] [argc argc])
    (if (zero? argc)  ; daca s-au adaugat toti parametrii in lista, se apeleaza functia
        (let ([new-stack (push (apply (get-function (top stack)) argv) (pop stack))]
              [new-IC (add1 (get-IC stack-machine))])
          (update-stack-machine (list new-stack '() '() '() '() new-IC) stack-machine))
        ; altfel, se construieste in continuare lista parametrilor
        (get-args (pop stack) (cons (top stack) argv) (sub1 argc)))))

;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

;; Fiecare instructiune (sau grup de instructiuni cu functionalitati similare/identice)
;; din bytecode are cate o functie care este apelata in momentul in care se ajunge la aceasta
;; instructiune.
(define (run-stack-machine stack-machine)
  (let* ([current-instr (list-ref (get-code stack-machine) (get-IC stack-machine))]
         [instruction (car current-instr)]
         [param (cdr current-instr)])
    (case instruction
      [(POP_TOP)
       (let ([new-stack (pop (get-stack stack-machine))]
             [new-IC (add1 (get-IC stack-machine))])
         (run-stack-machine (update-stack-machine (list new-stack '() '() '() '() new-IC)
                                                  stack-machine)))]
      [(LOAD_CONST)
       (run-stack-machine (load-const stack-machine param))]
      [(STORE_FAST)
       (run-stack-machine (store-fast stack-machine param))]
      [(LOAD_FAST)
       (run-stack-machine (load-fast stack-machine param))]
      [(BINARY_ADD INPLACE_ADD)
       (run-stack-machine (generic-binary stack-machine +))]
      [(BINARY_SUBTRACT INPLACE_SUBTRACT)
       (run-stack-machine (generic-binary stack-machine -))]
      [(BINARY_MODULO INPLACE_MODULO)
       (run-stack-machine (generic-binary stack-machine remainder))]
      [(COMPARE_OP)
       (run-stack-machine (generic-binary stack-machine (get-cmpop param)))]
      [(POP_JUMP_IF_FALSE)
       (run-stack-machine (jump-if stack-machine param #f))]
      [(POP_JUMP_IF_TRUE)
       (run-stack-machine (jump-if stack-machine param #t))]
      [(JUMP_ABSOLUTE)
       (run-stack-machine (jump-absolute stack-machine param))]
      [(FOR_ITER)
       (run-stack-machine (for-iter stack-machine param))]
      [(CALL_FUNCTION)
       (run-stack-machine (call-function stack-machine param))]
      [(LOAD_GLOBAL)
       (run-stack-machine (load-global stack-machine param))]
      [(SETUP_LOOP POP_BLOCK GET_ITER)
       (let ([new-IC (add1 (get-IC stack-machine))])
         (run-stack-machine (update-stack-machine (list '() '() '() '() '() new-IC) stack-machine)))]
      [(RETURN_VALUE) stack-machine])))
