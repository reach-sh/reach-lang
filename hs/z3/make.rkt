#lang racket/base
(module+ main
  (define WIDTH (+ 9 9 1))

  (define (make-expt n)
    (if (zero? n)
      `1
      `(* ,(make-expt (sub1 n))
          (ite (= e ,n) b 1))))
  (write `(define-fun expt ((b Int) (e Int)) Int
            ,(make-expt WIDTH)))

  (define (make-band n)
    (if (negative? n)
      `0
      `(let ((rec (* 2 rec)))
         (+ (ite (and (= (div x rec) 1)
                      (= (div y rec) 1))
                 rec
                 0)
            ,(make-band (sub1 n))))))
  (write `(define-fun band ((x Int) (y Int)) Int
            (let ((rec 1))
              ,(make-band WIDTH))))

  (define (make-bior n)
    (if (negative? n)
      `0
      `(let ((rec (* 2 rec)))
         (+ (ite (or (= (div x rec) 1)
                     (= (div y rec) 1))
                 rec
                 0)
            ,(make-bior (sub1 n))))))
  (write `(define-fun bior ((x Int) (y Int)) Int
            (let ((rec 1))
              ,(make-bior WIDTH))))

  (define (make-bxor n)
    (if (negative? n)
      `0
      `(let ((rec (* 2 rec)))
         (+ (ite (xor (= (div x rec) 1)
                      (= (div y rec) 1))
                 rec
                 0)
            ,(make-bior (sub1 n))))))
  (write `(define-fun bxor ((x Int) (y Int)) Int
            (let ((rec 1))
              ,(make-bxor WIDTH)))))
