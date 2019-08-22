;;
;; emu-dynamic.scm
;; 2019-8-22 v3.18
;;
;; Emulate dynamic-wind and reset/shift on Gauche
;;
;;  (emu-dynamic-wind before thunk after)
;;    before is (^[] expr ...)
;;    thunk  is (^[] expr ...)
;;    after  is (^[] expr ...)
;;
;;  (emu-call/cc proc)
;;    proc is (^[k] expr ...) and k is full continuation
;;
;;  (emu-call/pc proc)
;;    proc is (^[k] expr ...) and k is partial continuation
;;
;;  (emu-reset expr ...)
;;    make a boundary of partial continuation
;;
;;  (emu-shift k expr ...)
;;    this is equivalent to (emu-call/pc (^[k] expr ...))
;;
;;  (emu-parameterize ((param val) ...) body ...)
;;    param is parameter
;;

(define-module emu-dynamic
  (use gauche.partcont)
  (use gauche.parameter)
  (use gauche.test)
  (export
    emu-dynamic-wind
    emu-call/cc emu-call/pc
    emu-reset   emu-shift
    emu-parameterize
    ;testA
    ))
(select-module emu-dynamic)

(define-class <dynamic-winder> ()
  ((before        :init-keyword :before)
   (after         :init-keyword :after)
   (reset-chain   :init-form    *reset-chain*)
   (dbg-name      :init-keyword :dbg-name)))
(define-class <reset-info> ()
  ((dynamic-chain :init-form    *dynamic-chain*)
   (dbg-name      :init-keyword :dbg-name)))

(define *dynamic-chain* '())
(define *reset-chain*   (list (make <reset-info> :dbg-name "root")))

;; for debug
(define *dbg-level* 1) ; (bitwise setting (e.g. 3 is warning+info)
;                      ;   =0:none, =1:warning, =2:info, =8:special)
(define (dbg-print dbg-level . args)
  (when (logtest *dbg-level* dbg-level)
    (apply format (current-error-port) args)))
(define (dbg-print-chain dbg-level)
  (dbg-print dbg-level " d-chain=~s~%" (map (^[dp] (~ dp 'dbg-name)) *dynamic-chain*))
  (dbg-print dbg-level " r-chain=~s~%" (map (^[rp] (~ rp 'dbg-name)) *reset-chain*)))

(define (emu-dynamic-wind before thunk after :optional (dbg-name "emu-dynamic-wind"))
  (let* ([count1  0]
         [count2  0]
         [dbg-id  (gensym)]
         [before1 (^[]
                    (inc! count1)
                    (dbg-print 2 "~a ~s before ~d~%" dbg-name dbg-id count1)
                    (before))]
         [after1  (^[]
                    (inc! count2)
                    (dbg-print 2 "~a ~s after  ~d~%" dbg-name dbg-id count2)
                    (when (> count2 count1)
                      (dbg-print 1 "warning: emu-dynamic-wind calls 'after' without 'before'. ~a (b=~d, a=~d)~%" dbg-name count1 count2)
                      (dbg-print-chain 1))
                    (after))]
         [winder  (make <dynamic-winder> :before before1 :after after1 :dbg-name dbg-name)])
    ;; run before -> thunk -> after
    (before1)
    (push! *dynamic-chain* winder)
    (receive ret (thunk)
      (pop! *dynamic-chain*)
      (after1)
      (apply values ret))))

;; get a common tail of two lists
(define (%common-tail x y)
  (let ([lx (length x)] [ly (length y)])
    (let loop ([x (if (> lx ly) (list-tail x (- lx ly)) x)]
               [y (if (> ly lx) (list-tail y (- ly lx)) y)])
      (if (eq? x y)
        x
        (loop (cdr x) (cdr y))))))

;; travel dynamic-chain
(define (%travel dp-from dp-to)
  (let ([tail (%common-tail dp-from dp-to)])
    ;; call afters and update *dynamic-chain*
    (let loop ([dp dp-from])
      (unless (eq? dp tail)
        (set! *dynamic-chain* (cdr dp))
        (dbg-print 2 "travel after:  ")
        ((~ (car dp) 'after))
        (loop (cdr dp))))
    ;; call befores and update *dynamic-chain*
    (let loop ([dp dp-to])
      (unless (eq? dp tail)
        (loop (cdr dp))
        (dbg-print 2 "travel before: ")
        ((~ (car dp) 'before))
        (set! *dynamic-chain* dp)))))

;; cut dynamic-chain
(define (%dc-cut dp-from dp-to)
  (take dp-to (- (length dp-to)
                 (length (%common-tail dp-from dp-to)))))

(define (emu-call/cc proc)
  (let ([dp-cc  *dynamic-chain*]
        [rp-cc  *reset-chain*]
        [dbg-id (gensym)])
    (dbg-print 2 "emu-call/cc ~s~%" dbg-id)
    (call/cc
     (^[real-k]
       (let ([emu-k (^ args
                       (dbg-print 2 "emu-cc-k ~s~%" dbg-id)
                       (%travel *dynamic-chain* dp-cc)
                       (set! *reset-chain* rp-cc)
                       (apply real-k args))])
         (proc emu-k))))))

(define (%emu-reset thunk :optional (dbg-name ""))
  (dbg-print 2 "%emu-reset ~a~%" dbg-name)
  (push! *reset-chain* (make <reset-info> :dbg-name dbg-name))
  (receive ret (reset (thunk))
    (pop! *reset-chain*)
    (apply values ret)))

(define-syntax emu-reset
  (syntax-rules (:name)
    [(_ :name dbg-name expr ...)
     (%emu-reset (^[] expr ...) dbg-name)]
    [(_ expr ...)
     (%emu-reset (^[] expr ...))]))

(define (emu-call/pc proc)
  (let* ([dp-reset  (~ (car *reset-chain*) 'dynamic-chain)]
         [dp-pc     *dynamic-chain*]
         [dp-reset2 (if (null? dp-pc)
                      '()
                      (~ (car (~ (car dp-pc) 'reset-chain)) 'dynamic-chain))]
         [dc-part   (%dc-cut dp-reset2 dp-pc)]
         [dbg-id    (gensym)])
    (dbg-print 2 "emu-call/pc ~s~%" dbg-id)
    ((with-module gauche.internal %call/pc)
     (^[real-k]
       (let ([emu-k (^ args
                       (let ([dp-k *dynamic-chain*])
                         (dbg-print 2 "emu-pc-k ~s~%" dbg-id)
                         ;(%travel dp-k dp-pc)
                         (%travel dp-k (append dc-part dp-k))
                         (receive ret (emu-reset :name "emu-reset-1"
                                                 (apply real-k args))
                           (%travel *dynamic-chain* dp-k)
                           (apply values ret))))])
         ;; travel must be done before calling proc
         (%travel dp-pc dp-reset)
         (proc emu-k))))))

(define-syntax emu-shift
  (syntax-rules ()
    [(_ k expr ...)
     (emu-call/pc (^[k] expr ...))]))

(define-syntax emu-parameterize
  (syntax-rules ()
    [(_ ((param val) ...) body ...)
     (let ([params (list param ...)]
           [vals1  (list val ...)]
           [vals2  (list val ...)])
       (emu-dynamic-wind
        (^[] (set! vals2 (map (^[p v] (p v)) params vals1)))
        (^[] body ...)
        (^[] (set! vals1 (map (^[p v] (p v)) params vals2)))
        "emu-parameterize"))]))


;; ***** test tool *****
(define-syntax testA
  (syntax-rules ()
    [(_ description expected expr)
     (begin
       (test* description expected expr)
       (dbg-print-chain 8))]
    [(_ description expected expr check)
     (begin
       (test* description expected expr check)
       (dbg-print-chain 8))]))


(define (main args)
  ;; ***** test - parameterize *****
  (testA "parameterize 1"
         "[p01][p02][p01]"
         (with-output-to-string
           (^[]
             (define p (make-parameter "[p01]"))
             (display (p))
             (emu-parameterize ([p "[p02]"])
               (display (p)))
             (display (p)))))

  ;; ***** test - reset/shift *****
  (testA "reset/shift 1"
         10
         (+ 1 (emu-reset
               (+ 2 (emu-shift k (+ 3 (k 4)))))))

  (testA "reset/shift 2"
         '(1 2)
         (emu-reset
          (emu-shift k1 (cons 1 (k1)))
          (emu-shift k2 (cons 2 (k2)))
          '()))

  (testA "reset/shift 3"
         1000
         (begin
           (define k1 #f)
           (emu-reset
            (emu-shift k (set! k1 k))
            (emu-shift k 1000))
           (k1)))

  (testA "reset/shift + parameterize 1"
         "010"
         (with-output-to-string
           (^[]
             (define p (make-parameter 0))
             (display (p))
             (emu-reset
              (emu-parameterize ([p 1])
                (display (p))
                ;; 'shift' escapes from 'reset' before done
                (emu-shift k (display (p))))))))

  (testA "reset/shift + call/cc 1"
         "[r01][r02][r02][r03]()(root)"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (define done #f)
             (emu-call/cc
              (^[k0]
                (emu-reset
                 (display "[r01]")
                 (emu-shift k (set! k1 k))
                 (display "[r02]")
                 (unless done
                   (set! done #t)
                   (k0))
                 (display "[r03]"))))
             (k1)
             (display (map (^[dp] (~ dp 'dbg-name)) *dynamic-chain*))
             (display (map (^[rp] (~ rp 'dbg-name)) *reset-chain*))
             )))

  (testA "dynamic-wind + reset/shift 1"
         "[d01][d02][d03][d04]"
         ;"[d01][d02][d04][d01][d03][d04]"
         (with-output-to-string
           (^[]
             (emu-reset
              (emu-shift
               k
               (emu-dynamic-wind
                (^[] (display "[d01]"))
                (^[] (display "[d02]")
                     (k)
                     (display "[d03]"))
                (^[] (display "[d04]"))))))))

  (testA "dynamic-wind + reset/shift 2"
         "[d01][d02][d04][d01][d03][d04]"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (emu-reset
              (emu-dynamic-wind
               (^[] (display "[d01]"))
               (^[] (display "[d02]")
                    (emu-shift k (set! k1 k))
                    (display "[d03]"))
               (^[] (display "[d04]"))))
             (k1))))

  (testA "dynamic-wind + reset/shift 3"
         "[d01][d02][d01][d02][d01][d02][d01][d02]"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (define k2 #f)
             (emu-reset
              (emu-dynamic-wind
               (^[] (display "[d01]"))
               (^[] (emu-shift k (set! k1 k))
                    (emu-shift k (set! k2 k)))
               (^[] (display "[d02]"))))
             (k1)
             (k2)
             (k2))))

  (testA "dynamic-wind + reset/shift 4"
         "[d01][d11][d12][d02][d11][d12]"
         ;"[d01][d11][d12][d02][d01][d11][d12][d02]"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (emu-reset
              (emu-dynamic-wind
               (^[] (display "[d01]"))
               (^[] (emu-reset
                     (emu-dynamic-wind
                      (^[] (display "[d11]"))
                      (^[] (emu-shift k (set! k1 k)))
                      (^[] (display "[d12]")))))
               (^[] (display "[d02]"))))
             (k1))))

  (testA "dynamic-wind + reset/shift 5"
         "[d01][d02][d01][d11][d12][d02][d11][d12][d11][d12]"
         ;"[d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (define k2 #f)
             (define k3 #f)
             (emu-reset
              (emu-dynamic-wind
               (^[] (display "[d01]"))
               (^[] (emu-shift k (set! k1 k))
                    (emu-reset
                     (emu-dynamic-wind
                      (^[] (display "[d11]"))
                      (^[] (emu-shift k (set! k2 k))
                           (emu-shift k (set! k3 k)))
                      (^[] (display "[d12]")))))
               (^[] (display "[d02]"))))
             (k1)
             (k2)
             (k3))))

  (testA "dynamic-wind + reset/shift 6"
         "[d01][d02][d11][d12][d13][d14][d03][d04]"
         ;"[d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]"
         (with-output-to-string
           (^[]
             (emu-reset
              (emu-shift
               k
               (emu-dynamic-wind
                (^[] (display "[d01]"))
                (^[] (display "[d02]")
                     (emu-dynamic-wind
                      (^[] (display "[d11]"))
                      (^[] (display "[d12]")
                           (k)
                           (display "[d13]"))
                      (^[] (display "[d14]")))
                     (display "[d03]"))
                (^[] (display "[d04]"))))))))

  (testA "dynamic-wind + reset/shift 7"
         "[d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (emu-reset
              (emu-dynamic-wind
               (^[] (display "[d01]"))
               (^[] (display "[d02]")
                    (emu-dynamic-wind
                     (^[] (display "[d11]"))
                     (^[] (display "[d12]")
                          (emu-shift k (set! k1 k))
                          (display "[d13]"))
                     (^[] (display "[d14]")))
                    (display "[d03]"))
               (^[] (display "[d04]"))))
             (k1))))

  (testA "dynamic-wind + reset/shift 8"
         "[d01][d02][d04][d11][d12][d01][d03][d04][d13][d14]"
         ;"[d01][d02][d04][d11][d12][d14][d01][d03][d04][d11][d13][d14]"
         (with-output-to-string
           (^[]
             (define k1 #f)
             (emu-reset
              (emu-dynamic-wind
               (^[] (display "[d01]"))
               (^[] (display "[d02]")
                    (emu-shift k (set! k1 k))
                    (display "[d03]"))
               (^[] (display "[d04]"))))
             (emu-dynamic-wind
              (^[] (display "[d11]"))
              (^[] (display "[d12]")
                   (k1)
                   (display "[d13]"))
              (^[] (display "[d14]"))))))

  ;; ***** test - call/cc *****
  (testA "dynamic-wind"
         "[d01][d02][d03][d04]"
         (with-output-to-string
           (^[]
             (emu-dynamic-wind
              (^[] (display "[d01]"))
              (^[] (display "[d02]")
                   (display "[d03]"))
              (^[] (display "[d04]"))))))

  (testA "dynamic-wind + call/cc 1"
         "[d01][d02][d04]"
         (with-output-to-string
           (^[]
             (emu-call/cc
              (^[k]
                (emu-dynamic-wind
                 (^[] (display "[d01]"))
                 (^[] (display "[d02]")
                      (k)
                      (display "[d03]"))
                 (^[] (display "[d04]"))))))))

  (testA "dynamic-wind + call/cc 2"
         "[d01][d02][d03][d04][d01][d03][d04]"
         (with-output-to-string
           (^[]
             (define k1         #f)
             (define first-flag #t)
             (emu-dynamic-wind
              (^[] (display "[d01]"))
              (^[] (display "[d02]")
                   (emu-call/cc (^[k] (set! k1 k)))
                   (display "[d03]"))
              (^[] (display "[d04]")))
             (when first-flag
               (set! first-flag #f)
               (k1)))))

  (testA "dynamic-wind + call/cc 3"
         "[d01][d02][d11][d12][d14][d04]"
         (with-output-to-string
           (^[]
             (emu-call/cc
              (^[k]
                (emu-dynamic-wind
                 (^[] (display "[d01]"))
                 (^[] (display "[d02]")
                      (emu-dynamic-wind
                       (^[] (display "[d11]"))
                       (^[] (display "[d12]")
                            (k)
                            (display "[d13]"))
                       (^[] (display "[d14]")))
                      (display "[d03]"))
                 (^[] (display "[d04]"))))))))

  (testA "dynamic-wind + call/cc 4"
         "[d01][d02][d11][d12][d13][d14][d03][d04][d01][d11][d13][d14][d03][d04]"
         (with-output-to-string
           (^[]
             (define k1         #f)
             (define first-flag #t)
             (emu-dynamic-wind
              (^[] (display "[d01]"))
              (^[] (display "[d02]")
                   (emu-dynamic-wind
                    (^[] (display "[d11]"))
                    (^[] (display "[d12]")
                         (emu-call/cc (^[k] (set! k1 k)))
                         (display "[d13]"))
                    (^[] (display "[d14]")))
                   (display "[d03]"))
              (^[] (display "[d04]")))
             (when first-flag
               (set! first-flag #f)
               (k1)))))

  (testA "dynamic-wind + call/cc 5"
         "[d01][d02][d03][d04][d11][d12][d14][d01][d03][d04][d11][d12][d13][d14]"
         (with-output-to-string
           (^[]
             (define k1         #f)
             (define first-flag #t)
             (emu-dynamic-wind
              (^[] (display "[d01]"))
              (^[] (display "[d02]")
                   (emu-call/cc (^[k] (set! k1 k)))
                   (display "[d03]"))
              (^[] (display "[d04]")))
             (emu-dynamic-wind
              (^[] (display "[d11]"))
              (^[] (display "[d12]")
                   (when first-flag
                     (set! first-flag #f)
                     (k1))
                   (display "[d13]"))
              (^[] (display "[d14]"))))))

  ;; summary
  (format (current-error-port) "~%~a" ((with-module gauche.test format-summary)))
  )

