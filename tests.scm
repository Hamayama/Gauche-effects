(add-load-path "." :relative)
(use gauche.test)
(use effects)

;; for debug
(define *dbg-on* #f)
(define dbg-disp (if *dbg-on* display (lambda args)))

;; Racket's definition
(define null '())
(define foldr fold-right)
(define-syntax range
  (syntax-rules ()
    [(_ end)
     (range 0 end 1)]
    [(_ start end)
     (range start end 1)]
    [(_ start end step)
     (if (or (= start end)
             (= step 0)
             (and (< start end) (< step 0))
             (and (> start end) (> step 0)))
       '()
       (iota (ceiling (/ (- end start) step)) start step))]))
(define-syntax check-equal?
  (syntax-rules ()
    [(_ expr expected description)
     (test-handler description expected expr)]))

(check-equal?
 (handle-with* ((make-handler
                  [value x (dbg-disp "[a01]") (+ x (expt 10 1))]
                  [finally x (dbg-disp "[a02]") (+ x (expt 10 2))]
                  [effect e k [(eq? e 'A) (dbg-disp "[a03]") (k 1)]])
                (make-handler
                  [value x (dbg-disp "[b01]") (+ x (expt 10 3))]
                  [finally x (dbg-disp "[b02]") (+ x (expt 10 4))])
                (make-handler
                  [value x (dbg-disp "[c01]") (+ x (expt 10 5))]
                  [finally x (dbg-disp "[c02]") (+ x (expt 10 6))]))
   (perform 'A))
 (foldr (lambda (m n) (+ (expt 10 m) n)) 1 (range 1 7))
 "value and finally handlers chain")

(check-equal?
 (handle ([value x (dbg-disp "[a01]") (+ x (expt 10 1))]
          [finally x (dbg-disp "[a02]") (+ x (expt 10 2))]
          [effect e k
            [(eq? e 'A)
             (dbg-disp "[a03]") 
             (handle ([value x (dbg-disp "[b01]") (+ x (expt 10 3))]
                      [finally x (dbg-disp "[b02]") (+ x (expt 10 4))]
                      [effect e k [(eq? e 'B) (dbg-disp "[b03]") (k 1)]])
               (k null))]])
   (perform 'A)
   (perform 'B))
 (foldr (lambda (m n) (+ (expt 10 m) n)) 1 (range 1 5))
 "handle in effect handler")
