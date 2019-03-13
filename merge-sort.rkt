;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; merge-sort
;; (listof Number) -> (listof Number)
;; produces a list of numbers sorted in ascending order

(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 1)) (list 1))
(check-expect (merge-sort (list 1 2)) (list 1 2))
(check-expect (merge-sort (list 7 1 4 5 8 2 6 3)) (list 1 2 3 4 5 6 7 8))


(define (merge-sort lon)
  (cond[(empty? lon) empty]
       [(empty? (rest lon)) lon]
       [else
        (merge (merge-sort (firstpart lon))
               (merge-sort (secondpart lon)))]))

;; Template
#; 
(define (genrec-fn d)
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))


;; (listof Number) -> (listof Number)
;; produces the same list it is given till the kth element where k is the quotient of n and 2 (n is the total number of elements in
;; the list)

(check-expect (firstpart empty) empty)
(check-expect (firstpart (list 1)) (list 1))
(check-expect (firstpart (list 1 2)) (list 1))
(check-expect (firstpart (list 1 2 3 4 5 6)) (list 1 2 3))
(check-expect (firstpart (list 4 5 6 2 4 6 3)) (list 4 5 6))

(define (firstpart lon0)
  (local [(define (firstpart lon position)
            
            (cond[(empty? lon) empty]
                 [(empty? (rest lon)) lon]
                 [(empty? (rest (rest lon))) (list (first lon))]
                 [else
                  (if (>= (quotient (length lon0) 2) position)
                      (cons (first lon) (firstpart (rest lon) (add1 position)))
                      empty)]))]
    (firstpart lon0 1)))

;; (listof Number) -> (listof Number)
;; produces the list it is given from the k+1nth number where k is the quotient of n and 2 (n is the total number of elements in
;; the list)

(check-expect (secondpart empty) empty)
(check-expect (secondpart (list 1)) (list 1))
(check-expect (secondpart (list 1 2)) (list 2))
(check-expect (secondpart (list 1 2 3 4 5 6)) (list 4 5 6))
(check-expect (secondpart (list 4 5 6 2 4 6 3)) (list 2 4 6 3))

(define (secondpart lon0)
  (local [(define (secondpart lon original-list)
            (cond[(empty? lon) empty]
                 [(empty? (rest lon)) lon]
                 [else
                  (if (member? (first lon) (firstpart original-list))
                      (secondpart (rest lon) original-list)
                      lon)]))]
    (secondpart lon0 lon0)))
      

;; (listof Number) (listof Number) -> (listof Number)
;; merges one list into the other in such a way that the produced list has numbers in ascending order
;; ASSUME: both the given lists already have numbers arranged in ascending order


(check-expect (merge empty empty) empty)
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge empty (list 2)) (list 2))
(check-expect (merge (list 2) (list 1)) (list 1 2))
(check-expect (merge (list 1 2) (list 3)) (list 1 2 3))
(check-expect (merge (list 1 2) (list 3 4)) (list 1 2 3 4))
(check-expect (merge (list 6 8 12) (list 1 2 3)) (list 1 2 3 6 8 12))


(define (merge lon1 lon2)
  (cond[(empty? lon1) lon2]
       [(empty? lon2) lon1]
       [else
        (if (> (first lon1) (first lon2))
            (cons (first lon2) (merge lon1 (rest lon2)))
            (cons (first lon1) (merge lon2 (rest lon1))))]))
   
  




  