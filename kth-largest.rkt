;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname kth-largest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; (listof Number) Natural -> Number OR false
;; produces the kth largest natural number in the list - where k is the number given
;; produces false if the kth largest number doesn't exist

(check-expect (kthlargest empty 5) false)
(check-expect (kthlargest (list 1 2 3) 5) false)
(check-expect (kthlargest (list 1 2 3) 3) 1)
(check-expect (kthlargest (list 1 2 3 4) 1) 4)


(define (kthlargest lon k)
  
  (cond[(or (empty? lon) (> k (count lon)) (< k 0)) false]
       [else
        (list-ref (sortdec lon) (sub1 k))]))

;; (listof Number) -> Natural
;; produces the number of elements in a list

(define (count lon0)
  ;; ctn is count till now - the numbers counted till now
  (local[(define (count lon ctn)
           (cond[(empty? lon) ctn]
                [else
                 (count (rest lon) (add1 ctn))]))]
    (count lon0 0)))
         

;; (listof Number) -> (listof Number)
;; produces a list of numbers sorted in descending order

(check-expect (sortdec empty) empty)
(check-expect (sortdec (list 1 2 3 4)) (list 4 3 2 1))
(check-expect (sortdec (list 1 9 3 0 4)) (list 9 4 3 1 0))

(define (sortdec lon)
  
  (cond[(empty? lon) empty]
       [else
        (insert (first lon) (sortdec (rest lon)))]))

;; Number (listof Number) -> (listof Number)
;; produces a list of numbers with the given number inserted such that the entire list
;; is in descending order
;; ASSUME: the list of numbers given is already sorted in descending order

(define (insert n lon)
  (cond[(empty? lon) (list n)]
       [else
        (if (>= (first lon) n)
            (cons (first lon) (insert n (rest lon)))
            (cons n lon))]))
       

(kthlargest (list 1 3 12 10 5 7 2) 1000)                
