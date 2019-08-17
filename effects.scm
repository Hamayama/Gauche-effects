;;
;; effects.scm
;; 2019-8-18 v1.03
;;
;; modified for Gauche ( https://github.com/Hamayama/Gauche-effects )
;; ( the original is https://github.com/ayatoy/racket-effects )
;;
(define-module effects
  (use gauche.partcont)
  (use gauche.parameter)
  (use gauche.record)
  (use gauche.test)
  (use emu-dynamic)
  (export
    make-handler define-handler
    perform with-handler handle-with handle-with* handle
    testA))
(select-module effects)

;; prepare Gauche's reset/shift
(define *use-native-reset* #f)
(cond
 [*use-native-reset*
  ;; use native reset/shift
  (define testA test*)]
 [else
  ;; use emulator of reset/shift
  (define dynamic-wind emu-dynamic-wind)
  (define call/cc      emu-call/cc)
  (define call/pc      emu-call/pc)
  (define reset        emu-reset)
  (define shift        emu-shift)
  (define parameterize emu-parameterize)
  (define testA (with-module emu-dynamic testA))])

;; Racket's definition
(define null '())
(define findf find)

(define *current-handler* (make-parameter #f))
;(define *tag* (make-continuation-prompt-tag))
(define-record-type <handler> handler handler?
  (value    handler-value)
  (effect   handler-effect)
  (finally  handler-finally))
(define-record-type <%effect> %effect %effect?
  (value    %effect-value)
  (continue %effect-continue))

(define-syntax handler-clause
  (syntax-rules (value effect finally else)
    [(_ (value value-id body ...))
     (cons 'value
           (lambda (value-id) body ...))]
    [(_ (effect value-id cont-id (test expr ...) ...))
     (cons 'effect
           (lambda (value-id cont-id)
             (call/cc
              (lambda (break)
                (cond [test (break (begin expr ...))] ...)
                (perform/pc value-id cont-id)))))]
    [(_ (finally value-id body ...))
     (cons 'finally
           (lambda (value-id) body ...))]))

(define-syntax handler-clauses
  (syntax-rules (value effect finally else)
    [(_) null]
    [(_ clause rest ...)
     (cons (handler-clause clause)
           (handler-clauses rest ...))]))

(define-syntax make-handler
  (syntax-rules (value effect finally else)
    [(_ clause ...)
     (let ([handlers (handler-clauses clause ...)]
           [find-handler
            (lambda (type handlers)
              (let ([handler (findf (lambda (h) (eq? type (car h)))
                                    handlers)])
                (and handler (cdr handler))))])
       (handler
        (or (find-handler 'value handlers) identity)
        (or (find-handler 'effect handlers) (lambda (x k) (perform/pc x k)))
        (or (find-handler 'finally handlers) identity)))]))

(define-syntax define-handler
  (syntax-rules (value effect finally else)
    [(_ name clause ...)
     (define name (make-handler clause ...))]))

(define (perform/pc value k1)
  (let ([handler (*current-handler*)])
    (if (not handler)
        (error "uncaught effect" value)
        ;(control-at *tag* k2
        (shift k2
          (%effect value
                   (lambda (x)
                     (%with-handler
                      handler
                      (lambda ()
                        (if (not k1)
                            (k2 x)
                            (k2 (k1 x)))))))))))

(define (perform value)
  (perform/pc value #f))

(define (%with-handler handler thunk)
  ;(let ([result (prompt-at *tag*
  (let ([result (reset
                  (parameterize ([*current-handler* handler])
                    (thunk)))])
    (cond [(%effect? result)
           ((handler-effect handler)
            (%effect-value result)
            (%effect-continue result))]
          [else ((handler-value handler) result)])))

(define (with-handler handler thunk)
  ((handler-finally handler) (%with-handler handler thunk)))

(define-syntax handle-with
  (syntax-rules ()
    [(_ handler body ...)
     (with-handler handler (lambda () body ...))]))

(define-syntax handle-with*
  (syntax-rules ()
    [(_ (handler) body ...)
     (handle-with handler body ...)]
    [(_ (handler rest ...) body ...)
     (handle-with handler
       (handle-with* (rest ...) body ...))]))

(define-syntax handle
  (syntax-rules (value effect finally else)
    [(_ (clause ...) body ...)
     (handle-with (make-handler clause ...) body ...)]))
