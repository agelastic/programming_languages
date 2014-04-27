;; Programming Languages, Homework 5

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist x) ;  assumes list values are MUPL expressions
  (cond [(null? x) (aunit)]
        [(pair? x) (apair (car x) (racketlist->mupllist (cdr x)))]
        [#t x]))

(define (mupllist->racketlist x) ; assumes list values are MUPL expressions
  (cond [(aunit? x) '()]
        [(apair? x) (cons (apair-e1 x) (mupllist->racketlist (apair-e2 x)))]
        [#t x]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) ; variable lookup
         (envlookup env (var-string e))]
        [(add? e) ; addition
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; homework code starts here
        [(or (int? e) (closure? e) (aunit? e)) e] ; values
        [(fun? e) ; function definition
         (closure env e)] 
        [(ifgreater? e) ; conditional expression
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2)) 
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL conditional applied to non-number")))]
        [(mlet? e) ; mlet
         (letrec ([v1 (eval-under-env (mlet-e e) env)]
                  [env1 (cons (cons (mlet-var e) v1) env)])
           (eval-under-env (mlet-body e) env1))]
        [(call? e) ; function call
         (let ([v1 (eval-under-env (call-funexp e) env)] ; evaluate function
               [v2 (eval-under-env (call-actual e) env)]) ; evaluate argument
           (if (closure? v1)
               (letrec ([cl-fun (closure-fun v1)] ; extract function 
                        [cl-en (closure-env v1)] ;extract closure environment
                        [env1 (if (fun-nameopt cl-fun) ; function name is not #f
                                  (cons (cons (fun-nameopt cl-fun) v1) cl-en) 
                                  ; extend closure environment to map function name to the closure
                                  cl-en)]
                        [env2 (cons (cons (fun-formal cl-fun) 
                                          ; extend with argument name bound to its actual value
                                          v2) env1)])
                 (eval-under-env (fun-body cl-fun) env2)) ; evaluate function body in the new environment
               (error "MUPL call applied to non-closure")))]
        [(apair? e) ; pair
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e) ; first element of a pair
         (let ([val (eval-under-env (fst-e e) env)])
           (if (apair? val)
               (apair-e1 val)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e) ; second element of a pair
         (let ([val (eval-under-env (snd-e e) env)])
           (if (apair? val)
               (apair-e2 val)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e) ; test for unit value
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1) (int 1) (int 0)))]        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [(pair? (car lstlst)) 
         (mlet (caar lstlst) 
               (cdar lstlst) 
               (mlet* (cdr lstlst) e2))]
        [#t (error "MLET* called with bad list")]))

(define (ifeq e1 e2 e3 e4) ; e1 = e2 if neither e1 > e2 nor e2 > e1
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) 
         (ifgreater (var "_x") (var "_y") e4 
                    (ifgreater (var "_y") (var "_x") 
                               e4 
                               e3))))
; 

;; Problem 4

(define mupl-map
  (fun #f "f" 
       (fun "g" "xs" 
            (ifaunit (var "xs")
                     (aunit)
                     (apair
                      (call (var "f") (fst (var "xs")))
                      (call (var "g") (snd (var "xs"))))))))


(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" 
             (call (var "map") 
                   (fun #f "y" 
                        (add (var "y") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([getvars 
            ; The helper function can take and return a pair of expression and vars, 
            ; collapsing it with the main cond. 
            ; This will save a lot of unnecessary re-computation, but the code gets ugly. 
            ; There should exist a more elegant solution. Maybe memoization.
            (lambda (e) 
              (cond
                [(var? e) (set (var-string e))]
                [(add? e) (set-union (getvars (add-e1 e)) (getvars (add-e2 e)))]
                [(ifgreater? e) (set-union (getvars (ifgreater-e1 e)) 
                                           (getvars (ifgreater-e2 e)) 
                                           (getvars (ifgreater-e3 e)) 
                                           (getvars (ifgreater-e4 e)))]
                [(mlet? e) (set-remove (getvars (mlet-body e)) (mlet-var e))] ; a variable is bound
                [(apair? e) (set-union (getvars (apair-e1 e)) (getvars (apair-e2 e)))]
                [(fst? e) (getvars (fst-e e))]
                [(call? e) (set-union (getvars (call-actual e)) (getvars (call-funexp e)))] 
                ; need vars from both expressions - this was the last bug I've been hunting for.
                [(snd? e) (getvars (snd-e e))]
                [(isaunit? e) (getvars (isaunit-e e))]
                [(fun? e) (set-remove (set-remove (getvars (fun-body e)) (fun-formal e)) (fun-nameopt e))] 
                ; Bind the argument and the function name.
                [#t (set)]))
            ])
    (cond
      [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
      [(ifgreater? e) (ifgreater 
                       (compute-free-vars (ifgreater-e1 e)) 
                       (compute-free-vars (ifgreater-e2 e)) 
                       (compute-free-vars (ifgreater-e3 e)) 
                       (compute-free-vars (ifgreater-e4 e)))]
      [(mlet? e) (mlet (mlet-var e) 
                       (compute-free-vars (mlet-e e)) 
                       (compute-free-vars (mlet-body e)))]
      [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
      [(fst? e) (fst (compute-free-vars (fst-e e)))]
      [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
      [(snd? e) (snd (compute-free-vars (snd-e e)))]
      [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
      [(fun? e) (fun-challenge (fun-nameopt e) 
                               (fun-formal e) 
                               (compute-free-vars (fun-body e))
                               (getvars e))]
      [#t e])))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes(define (compute-free-vars e) 

;; solution from the course here: (not mine)

(struct res (e fvs)) ; result type of f (could also use a pair)
(define (f e) 
  (cond [(var? e) (res e (set (var-string e)))]
        [(int? e) (res e (set))]
        [(add? e) (let ([r1 (f (add-e1 e))]
                        [r2 (f (add-e2 e))])
                    (res (add (res-e r1) (res-e r2))
                         (set-union (res-fvs r1) (res-fvs r2))))]
        [(ifgreater? e) (let ([r1 (f (ifgreater-e1 e))]
                              [r2 (f (ifgreater-e2 e))]
                              [r3 (f (ifgreater-e3 e))]
                              [r4 (f (ifgreater-e4 e))])
                          (res (ifgreater (res-e r1) (res-e r2) (res-e r3) (res-e r4))
                               (set-union (res-fvs r1) (res-fvs r2) (res-fvs r3) (res-fvs r4))))]
        [(fun? e) (let* ([r (f (fun-body e))]
                         [fvs (set-remove (res-fvs r) (fun-formal e))]
                         [fvs (if (fun-nameopt e) (set-remove fvs (fun-nameopt e)) fvs)])
                    (res (fun-challenge (fun-nameopt e) (fun-formal e) (res-e r) fvs)
                         fvs))]
        [(call? e) (let ([r1 (f (call-funexp e))]
                         [r2 (f (call-actual e))])
                     (res (call (res-e r1) (res-e r2))
                          (set-union (res-fvs r1) (res-fvs r2))))]
        [(mlet? e) (let* ([r1 (f (mlet-e e))]
                          [r2 (f (mlet-body e))])
                     (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                          (set-union (res-fvs r1) (set-remove (res-fvs r2) (mlet-var e)))))]
        [(apair? e) (let ([r1 (f (apair-e1 e))]
                          [r2 (f (apair-e2 e))])
                      (res (apair (res-e r1) (res-e r2))
                           (set-union (res-fvs r1) (res-fvs r2))))]
        [(fst? e) (let ([r (f (fst-e e))])
                    (res (fst (res-e r))
                         (res-fvs r)))]
        [(snd? e) (let ([r (f (snd-e e))])
                    (res (snd (res-e r))
                         (res-fvs r)))]
        [(aunit? e) (res e (set))]
        [(isaunit? e) (let ([r (f (isaunit-e e))])
                        (res (isaunit (res-e r))
                             (res-fvs r)))]))
(res-e (f e)))

(define (eval-under-env-c e env) 
  (cond 
    [(fun-challenge? e)
     (closure (set-map (fun-challenge-freevars e)
                       (lambda (s) (cons s (envlookup env s))))
              e)]
    ; call case uses fun-challenge as appropriate
    ; all other cases the same
    ...)
  
  (define (eval-exp-c e)
    (eval-under-env-c (compute-free-vars e) null))
  
  
  
  ;; Do NOT change this
  (define (eval-exp-c e)
    (eval-under-env-c (compute-free-vars e) null))
  