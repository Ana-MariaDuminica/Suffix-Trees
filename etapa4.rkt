#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (iter w1 w2 '()))

(define (iter w1 w2 result)
  (if (null? w1)
      (collection-cons (reverse result) (cons w1 w2))
      (if (null? w2)
          (collection-cons (reverse result) (cons w1 w2))
          (if (char=? (car w1) (car w2))
              (iter (cdr w1) (cdr w2) (cons (car w1) result))
              (collection-cons (reverse result) (cons w1 w2))))))


(define (longest-common-prefix-of-collection words)
  (iter2 (collection-first words) (collection-rest words)))

(define (iter2 result words)
  (if (collection-empty? words)
      result
      (iter2 (collection-first (longest-common-prefix result (collection-first words))) (collection-rest words))))

(define (match-pattern-with-label st pattern)
  (if (collection-empty? st)
      (collection-cons #f empty-collection)
      (if (char=? (car pattern) (car (get-branch-label (collection-first st))))
          (if (equal? pattern (collection-first (longest-common-prefix pattern (get-branch-label (collection-first st)))))
              #t
            (function st pattern '() (get-branch-subtree (collection-first st))))
          (match-pattern-with-label (collection-rest st) pattern))))


(define (function st pattern result subtree)
  (define prefix (longest-common-prefix (get-branch-label (collection-first st)) (collection-first (longest-common-prefix (collection-first (collection-rest (longest-common-prefix pattern result))) (get-branch-label (collection-first st))))))

  (if (collection-empty? (collection-first (collection-rest prefix)))

          (if (equal? pattern (append result (collection-first prefix)))
              (cons result (cons (collection-first prefix) subtree))
                   
              (if (collection-empty? (get-branch-subtree (collection-first st)))
                  (cons result (collection-first prefix))
                  (function (get-branch-subtree (collection-first st))  pattern  (append result (collection-first prefix)) subtree)))
          
          (if (null? result)
              (if (collection-empty? (collection-rest (collection-first st)))
                  (if (equal? pattern (append result (collection-first prefix)))
                      (cons result (cons (collection-first prefix) subtree))
                      (cons #f (cons result (collection-first prefix))))
                  (function (get-branch-subtree st) pattern result subtree))
              
              (if (collection-empty? (collection-rest st))
                  (if (equal? pattern (append result (collection-first prefix)))
                      (cons result (cons (collection-first prefix) subtree))
                      (cons #f (cons result (collection-first prefix))))
                (function (collection-rest st) pattern result subtree)))))



(define (st-has-pattern? st pattern)
  (if (stream? (match-pattern-with-label st pattern))
      (if (equal? #f (collection-first (match-pattern-with-label st pattern)))
          #f
          #t) #t))

(define (get-suffixes text)
  (if (null? text)
      '()
      (collection-cons text (get-suffixes (cdr text)))))


(define (get-ch-words words ch)
  (collection-filter (lambda (word) (and (not (null? word)) (char=? (car word) ch))) words))


(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes))) (collection-map (lambda (word) (collection-rest word)) suffixes)))


(define (cst-func suffixes)
  (define prefix (longest-common-prefix-of-collection suffixes))
  (cons prefix
        (collection-map (lambda (word)
               (collection-first (collection-rest (longest-common-prefix word prefix)))) suffixes)))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)

(define (suffixes->st labeling-func suffixes alphabet)
  (define initial-stream-words (collection-map (lambda (ch) (get-ch-words suffixes ch)) alphabet)) ; sortez fluxul care incep cu acelasi caracter
  (define streamwords (collection-filter (lambda (x) (not (collection-empty? x))) initial-stream-words))
 
   (collection-map (lambda (stream-suffixes)
         (if (collection-empty? stream-suffixes)
             empty-collection
             (let* (
                    (label (get-branch-label (labeling-func stream-suffixes))) ; eticheta
                    (suffixes (get-branch-subtree (labeling-func stream-suffixes))) ; sufixele
                    )
               (cons label (suffixes->st labeling-func suffixes alphabet))
            ;  construiesc o pereche cu eticheta listei si apelez recursiv functia cu restul elementelor din fluxul fara eticheta
             ))) streamwords))


; nu uitați să convertiți alfabetul într-un flux


(define text->st
  (lambda (labeling-func)
    (lambda (word)
           ( let* (
            (alphabet (list->stream (sort (remove-duplicates (append word (list #\$))) char<?))) ; gasesc toate caracterele cuvantului, inclusiv $ si le sortez
            (suffixes (get-suffixes (append word (list #\$)))) ; construiesc fluxul de sufixe
            )
            (suffixes->st labeling-func suffixes alphabet) ; apelez functia cu sufixele si alfabetul gasit si functia corespunzatoare
           ))))


(define (text->ast word)
    ((text->st ast-func) word))


(define (text->cst word)
    ((text->st cst-func) word))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))

;(substring? (string->list "banana") (string->list "na"))
; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
 
  (let* ((st (text->cst text))  ; arbore compact
         (initial-len len)) ; lungimea initiala
 
  (define (search-len st len acc)
    (if (collection-empty? st)
        #f  
        (if (collection-empty? (get-branch-subtree (collection-first st))) ; daca subarborele ramurii curente este null
            (if (collection-empty? (collection-rest st)) ; ma duc pe urmatoarea ramura 
                (if (= initial-len len) ; daca nu am gasit momentan nimic, trec la urmatorea ramura
                    (search-len (collection-rest st) len acc)
                    #f)
                (search-len (collection-rest st) len acc)) 
            (if (>= (length (get-branch-label (collection-first st))) len)
                ; daca lungimea etichetei ramurii este mai mare decat lungimea curenta, o adaug la acc
                (append acc (take (get-branch-label (collection-first st)) len))
                (let* ((new-len (- len (length (get-branch-label (collection-first st))))) ; scad din lungimea curenta lungimea etichetei
                       (new-acc (append acc (get-branch-label (collection-first st))))) ; adaug la acc eticheta
                  (or (search-len (get-branch-subtree (collection-first st)) new-len new-acc)
                      ; daca nu gasesc nicio eticheta buna in subarbore, trec la urmatorea ramura
                      (search-len (collection-rest st) len acc)))))))

  
  (search-len st len '())))