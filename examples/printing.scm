(add-load-path ".." :relative)
(use gauche.record)
(use effects)

;; Racket's definition
(define null '())

(define-record-type <print> print print? (message print-message))

(define-handler default
  [effect e k
    [(print? e)
     (display (print-message e))
     (k null)]])

;;
;; perform (Print "Hello, world!\n")
;;
(test-handler
 "print 1"
 "Hello, world!\n"
 (with-output-to-string
  (^[]
    (handle-with default
      (perform (print "Hello, world!\n"))))))

;;
;; handle
;;   perform (Print "A");
;;   perform (Print "B");
;;   perform (Print "C");
;;   perform (Print "D")
;; with
;;   | effect (Print msg) k ->
;;     perform (Print ("I see you tried to print " ^ msg ^ ". Not so fast!\n"))
;;
(test-handler
 "print 2"
 "I see you tried to print A. Not so fast!\n"
 (with-output-to-string
  (^[]
    (handle-with default
      (handle ([effect e k
                 [(print? e)
                  (perform (print (string-append
                                   "I see you tried to print "
                                   (print-message e)
                                   ". Not so fast!\n")))]])
        (perform (print "A"))
        (perform (print "B"))
        (perform (print "C"))
        (perform (print "D")))))))

;;
;; handle
;;   perform (Print "A");
;;   perform (Print "B");
;;   perform (Print "C");
;;   perform (Print "D")
;; with
;;   | effect (Print msg) k ->
;;     perform (Print ("I see you tried to print " ^ msg ^ ". Okay, you may.\n"));
;;     continue k ()
;;
(test-handler
 "print 3"
 (string-append
  "I see you tried to print A. Okay, you may.\n"
  "I see you tried to print B. Okay, you may.\n"
  "I see you tried to print C. Okay, you may.\n"
  "I see you tried to print D. Okay, you may.\n")
 (with-output-to-string
  (^[]
    (handle-with default
      (handle ([effect e k
                 [(print? e)
                  (perform (print (string-append
                                   "I see you tried to print "
                                   (print-message e)
                                   ". Okay, you may.\n")))
                  (k null)]])
        (perform (print "A"))
        (perform (print "B"))
        (perform (print "C"))
        (perform (print "D")))))))

;;
;; let collect = handler
;;   | x -> (x, "")
;;   | effect (Print msg) k ->
;;     let (result, msgs) = continue k () in
;;       (result, msg ^ msgs)
;;
(define-handler collect
  [value x (vector x "")]
  [effect e k
    [(print? e)
     (let ([v (k null)])
       (vector (vector-ref v 0)
               (string-append (print-message e)
                              (vector-ref v 1))))]])

;;
;; with collect handle
;;   perform (Print "A");
;;   perform (Print "B");
;;   perform (Print "C");
;;   perform (Print "D")
;;
(test-handler
 "collect"
 #(() "ABCD")
 (handle-with collect
   (perform (print "A"))
   (perform (print "B"))
   (perform (print "C"))
   (perform (print "D"))))
