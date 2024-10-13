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
      (longest-common-prefix2 w1 w2 '())
  )
(define (longest-common-prefix2 w1 w2 acc)
  (if (or (null? w1) (null? w2) (not (equal? (car w1) (car w2))))
      (list acc w1 w2)
      (longest-common-prefix2 (cdr w1) (cdr w2) (append acc (list (car w1))))))
      

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (if (collection-empty? (collection-rest words))
      (collection-first words)
      (let* ((pref-2 (longest-common-prefix (collection-first words) 
                                            (collection-first (collection-rest words))))
             (pref-aux (car pref-2)))
        (if (collection-empty? (collection-rest (collection-rest words)))
            pref-aux
            (longest-common-prefix-of-collection (collection-cons pref-aux 
                                                                  (collection-rest (collection-rest words))))))))
(define (longest-common-prefix-of-list words)
  (if (null? (cdr words))
             (car words)
             (let*
                ((pref-2 (longest-common-prefix (car words) (cadr words)))
                (pref-aux (car pref-2)))
             (if (null? (cddr words))
                 pref-aux
                 (longest-common-prefix-of-list (cons pref-aux (cddr words)))))))
             



(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern))))
    (if branch
        (let* ((sub-t (get-branch-subtree branch))
               (label (get-branch-label branch))
               (pref-comp (longest-common-prefix-of-list (list label pattern))))
          (if (equal? pref-comp label)
              (if (equal? label pattern)
                  true
                  (list label (drop-n-elements (length label) pattern) sub-t))
              (if (equal? pref-comp pattern)
                  true
                  (list false pref-comp))))
        (list false '()))))

(define (drop-n-elements n lst)
  (if (zero? n)
      lst
      (drop-n-elements (- n 1) (cdr lst))))

(define (st-has-pattern? st pattern)
  (let* ((result (match-pattern-with-label st pattern)))
    (cond
      ((equal? result true) true)
      ((and (>= (length result) 3))
       (st-has-pattern? (caddr result) (cadr result)))
      (else false))))



(define (get-suffixes text)
  (if (collection-empty? text)
      collection-empty
      (collection-cons text (get-suffixes (collection-rest text)))))


(define (get-ch-words words ch)
  (collection-filter (lambda (word) (and (not (collection-empty? word)) (char=? (car word) ch))) words))


(define (ast-func suffixes)
  (let* ((label (list (collection-first (collection-first suffixes))))
         (new-suffixes (collection-map (lambda (suffix) (collection-rest suffix)) suffixes)))
    (cons label new-suffixes)))

(define (cst-func suffixes)
  (if (and (not (collection-empty? suffixes))
           (collection-list? (collection-first suffixes)))
      (let* ((label (longest-common-prefix-of-collection suffixes)) 
             (new-suffixes (collection-map (lambda (suffix)
                                             (if (collection-empty? suffix)
                                                 collection-empty
                                                 (drop  suffix (length label))))
                                           suffixes)))
        (cons label new-suffixes))
      collection-empty))
 
(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-empty? suffixes) '()
      (let* ((branches (collection-map (lambda (ch)
                                (if (collection-empty? (get-ch-words suffixes ch)) 
                                    '()
                                    (let* ((labeled-res (labeling-func (get-ch-words suffixes ch)))
                                           (label (list (car labeled-res)))
                                           (suffs-rest (cdr labeled-res)))
                                      (append label (suffixes->st labeling-func suffs-rest alphabet)))))
                            alphabet)))
        (collection-filter (lambda (branch) (not (null? branch))) branches))))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (suffixes->st labeling-func (get-suffixes (append text '(#\$))) (list->stream(sort (remove-duplicates (append text '(#\$))) char<?))  
  ))))

(define text->ast
  (lambda (text)
    ((text->st text) ast-func)))



(define text->cst
  (lambda (text)
    ((text->st text) cst-func)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((cst (text->ast text)))
    (st-has-pattern? cst pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let* ([cst (text->cst text)])
    (let loop ([tree cst] [len len])
      (cond
        ((st-empty? tree) #f)
       
        ((not (st-empty? (get-branch-subtree (first-branch tree))))
         (let* ((branch (first-branch tree))
                (label (get-branch-label branch)) 
                (subtree-aux (get-branch-subtree branch))) 
           (if (< (length label) len)
               (let ((search (loop subtree-aux (- len (length label)))))
                 (if (equal? #f search)
                     (loop (other-branches tree) len)
                     (append label search)))
               (take label len))))
        (else (loop (other-branches tree) len))))))
