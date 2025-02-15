#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  (iter w1 w2 '() )
)

(define (iter w1 w2 result)

  (if (null? w1)

      (append (list (reverse result)) (list w1) (list w2))

      (if (null? w2)
          (append (list (reverse result)) (list w1) (list w2))
          (if (char=? (car w1) (car w2))
              ( iter (cdr w1) (cdr w2) (cons (car w1) result) )
              (append (list (reverse result)) (list w1) (list w2) )
  
          )
      )
  )
)


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)

  (iter2 (car words) words)
  
  )

(define (iter2 result words)
         
  (if (null? words)

      result

      (iter2 (car (longest-common-prefix result (car words))) (cdr words))

   )

 )


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (match-pattern-with-label st pattern)

  (if (st-empty? st)
      (append (list #f) (list '()))
      (if (char=? (car pattern) (car (get-branch-label (first-branch st))))
          (if (equal? pattern (car (longest-common-prefix pattern (get-branch-label (first-branch st)))) )
              #t
            (function st pattern '() (cdr (first-branch st)))
           )
          (match-pattern-with-label (cdr st) pattern)
      )
   )
)

(define (function st pattern result subtree)
  (define prefix (longest-common-prefix (get-branch-label (car st)) (car (longest-common-prefix (car (cdr (longest-common-prefix pattern result))) (get-branch-label (car st))))))

  (if (null? (car(cdr prefix)))

          (if (equal? pattern (append result (car prefix) ) )
              (append (list result) (list (car prefix)) (list subtree))
                   
              (if (null? (cdr (car st)))
                  (append result (car prefix))
                  (function (cdr (car st))  pattern  (append result (car prefix)) subtree)
              )
           )
          (if (null? result)
              (if (null? (cdr (car st)))
                  (if (equal? pattern (append result (car prefix) ) )
                      (append (list result) (list (car prefix)) (list subtree))
                      (append (list #f) (list (append result (car prefix) )))
                   )
                  (function (cdr st) pattern result subtree)
               )
              (if (null? (cdr st))
                  (if (equal? pattern (append result (car prefix) ) )
                      (append (list result) (list (car prefix)) (list subtree))
                      (append (list #f) (list (append result (car prefix) )))
                   )
                (function (cdr st) pattern result subtree)
              )
           )
   )
  )





; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)

  (if (list? (match-pattern-with-label st pattern))
      (if (equal? #f (car (match-pattern-with-label st pattern)))
          #f
          #t
       )
      #t
   )
  )