(add-load-path ".." :relative)
(use gauche.record)
(use effects)

;; Racket's definition
(define null '())

(define-record-type <print> print print? (message print-message))

(define-handler display-message
  [effect e k
    [(print? e)
     (k (begin (display (print-message e))
               (newline)
               null))]])

;;
;; effect Yield : unit
;; effect Spawn : (unit -> unit) -> unit
;; effect Get_next : (unit -> unit) option
;; effect Add_to_queue : (unit -> unit) -> unit
;;
(define-record-type <yield> yield yield?)
(define-record-type <spawn> spawn spawn? (thunk spawn-thunk))
(define-record-type <get-next> get-next get-next?)
(define-record-type <add-to-queue> add-to-queue add-to-queue? (thunk add-to-queue-thunk))

;;
;; let queue initial = handler
;;   | effect Get_next k ->
;;     ( fun queue -> match queue with
;;         | [] -> (continue k None) []
;;         | hd::tl -> (continue k (Some hd)) tl )
;;   | effect (Add_to_queue y) k -> ( fun queue -> (continue k ()) (queue @ [y]))
;;   | x -> ( fun _ -> x)
;;   | finally x -> x initial
;;
(define (queue initial)
  (make-handler
   [effect e k
     [(get-next? e)
      (lambda (queue)
        (if (null? queue)
            ((k null) '())
            ((k (car queue)) (cdr queue))))]
     [(add-to-queue? e)
      (lambda (queue)
        ((k null) (append queue (list (add-to-queue-thunk e)))))]]
   [value x (lambda (_) x)]
   [finally x (x initial)]))

;;
;; let round_robin =
;;   let enqueue t =
;;     perform (Add_to_queue t)
;;   in
;;   let dequeue () =
;;     match perform Get_next with
;;     | None -> ()
;;     | Some t -> t ()
;;   in
;;   let rec rr_handler () = handler
;;     | effect Yield k -> enqueue k; dequeue ()
;;     | effect (Spawn t) k -> enqueue k; with rr_handler () handle t ()
;;     | () -> dequeue ()
;;   in
;;   rr_handler ()
;;
(define round-robin
  (let ([enqueue (lambda (t) (perform (add-to-queue t)))]
        [dequeue (lambda (_)
                   (let ([t (perform (get-next))])
                     (if (null? t) null (t null))))])
    (letrec ([rr-handler
              (lambda (_)
                (make-handler
                 [effect e k
                   [(yield? e) (enqueue k) (dequeue null)]
                   [(spawn? e)
                    (enqueue k)
                    (handle-with (rr-handler null)
                                 ((spawn-thunk e) null))]]
                 [value _ (dequeue null)]))])
      (rr-handler null))))

;;
;; with queue [] handle
;; with round_robin handle
;;   perform (Spawn (fun _ ->
;;     iter (fun x -> perform (Print x); perform Yield) ["a"; "b"; "c"; "d"; "e"]
;;     ));
;;   perform (Spawn (fun _ ->
;;     iter (fun x -> perform (Print x); perform Yield) ["A"; "B"; "C"; "D"; "E"]
;;     ))
;;
(test-handler
 "round-robin"
 "a A b B c C d D e E "
 (regexp-replace-all*
  (with-output-to-string
   (^[]
     (handle-with* (display-message (queue '()) round-robin)
       (perform (spawn (lambda (_)
                         (for-each (lambda (x)
                                     (perform (print x))
                                     (perform (yield)))
                                   '("a" "b" "c" "d" "e")))))
       (perform (spawn (lambda (_)
                         (for-each (lambda (x)
                                     (perform (print x))
                                     (perform (yield)))
                                   '("A" "B" "C" "D" "E"))))))))
  #/\n/ " "))

;;
;; let rec fractions d e =
;;   let rec find_fractions n =
;;     if gcd n d = 1 then
;;       perform (Print (to_string n ^ "/" ^ to_string d ^ ", ")); perform Yield
;;     else ();
;;     if d > n then
;;       find_fractions (n+1)
;;     else ()
;;   in
;;   (if d < e then
;;      perform (Spawn (fun _ -> perform Yield; fractions (d + 1) e)) else ()) ;
;;   find_fractions 1
;;
(define (fractions d e)
  (letrec ([find-fractions
            (lambda (n)
              (if (= 1 (gcd n d))
                  (begin (perform (print (string-append (number->string n)
                                                        "/"
                                                        (number->string d))))
                         (perform (yield)))
                  null)
              (if (> d n)
                  (find-fractions (+ n 1))
                  null))])
    (if (< d e)
        (perform (spawn (lambda (_)
                          (perform (yield))
                          (fractions (+ d 1) e))))
        null)
    (find-fractions 1)))

;;
;; with queue [] handle
;; with round_robin handle
;;   fractions 1 10
;;
(test-handler
 "fractions"
 "1/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 1/6 3/5 5/6 1/7 4/5 2/7 1/8 3/7 3/8 1/9 1/10 4/7 5/8 2/9 3/10 5/7 7/8 4/9 7/10 6/7 5/9 9/10 7/9 8/9 "
 (regexp-replace-all*
  (with-output-to-string
   (^[]
     (handle-with* (display-message (queue '()) round-robin)
       (fractions 1 10))))
  #/\n/ " "))
