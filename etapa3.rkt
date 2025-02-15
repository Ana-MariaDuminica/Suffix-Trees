#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

(define (longest-common-substring text1 text2)
  
  (let* ((st1 (text->ast text1))  ;arborele de sufixe ST1
         (st2 (get-suffixes text2)))  ;lista de sufixe pentru al doilea cuvant
    
    (let iter ((st2 st2) (result '()))
      (if (null? st2)
          result
          (let ((primul-rezultat (match-pattern-with-label st1 (car st2))))   ;cel mai lung sir comun intre arbore si sufix
            (if (list? primul-rezultat)  ; daca este o lista inseamna ca intram pe cazurile doi si trei
                  
                (if (equal? #f (car primul-rezultat))  ; am gasit doar o parte din sufix
                    (if (null? (cdr primul-rezultat))
                        (iter (cdr st2) result)
                        (if (> (length (cadr primul-rezultat)) (length result))
                            (iter (cdr st2) (cadr primul-rezultat))  ; subsirul este al doilea element din lista
                            (iter (cdr st2) result)))
                      
                    (if (> (length (append (car primul-rezultat) (cadr primul-rezultat))) (length result))
                        ; cazul cand subsirul este format din eticheta si restul din subarbore
                        (iter (cdr st2) (append (car primul-rezultat) (cadr primul-rezultat)))
                        (iter (cdr st2) result)))
                  
                (if (> (length (car st2)) (length result))  ;cel mai lung subsir este chiar sufixul curent din al doilea cuvant
                    (iter (cdr st2) (car st2))      ; il comparam cu subsirul curent cel mai lung si daca e mai mare il inlocuim
                    (iter (cdr st2) result))))))))


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

(define (repeated-substring-of-given-length text len)
 
  (let* ((st (text->cst text))  ; arbore compact
         (initial-len len)) ; lungimea initiala
 
  (define (search-len st len acc)
    (if (st-empty? st)
        #f  
        (if (st-empty? (get-branch-subtree (first-branch st))) ; daca subarborele ramurii curente este null
            (if (st-empty? (other-branches st)) ; ma duc pe urmatoarea ramura 
                (if (= initial-len len) ; daca nu am gasit momentan nimic, trec la urmatorea ramura
                    (search-len (other-branches st) len acc)
                    #f)
                (search-len (other-branches st) len acc)) 
            (if (>= (length (get-branch-label (first-branch st))) len)
                ; daca lungimea etichetei ramurii este mai mare decat lungimea curenta, o adaug la acc
                (append acc (take (get-branch-label (first-branch st)) len))
                (let* ((new-len (- len (length (get-branch-label (first-branch st))))) ; scad din lungimea curenta lungimea etichetei
                       (new-acc (append acc (get-branch-label (first-branch st))))) ; adaug la acc eticheta
                  (or (search-len (get-branch-subtree (first-branch st)) new-len new-acc)
                      ; daca nu gasesc nicio eticheta buna in subarbore, trec la urmatorea ramura
                      (search-len (other-branches st) len acc)))))))

  
  (search-len st len '())))