(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

(define getlast
  (lambda (arg)
    (if (null? (cdr arg))
        (car arg)
        (getlast (cdr arg)))))

(define pbegin
  (lambda (arg)
    (if (eqv? (car arg) 'begin-exp)
        (getlast (cadr arg))
        arg)))
        


(define contains?
  (lambda (v1 l2)
    (if (null? l2) #f
        (if (equal? v1 (car l2))
            #t
            (contains? v1 (cdr l2))
            )
        )))

;fromA6Q8
;8

(define lambdacons
  (lambda (args)    (cons 'lambda (list (map car (car (cdr args)))))))





(define consarg
  (lambda (args)
    (append (lambdacons args) (cdr (cdr args)))))

(define consmap
  (lambda (args)
    (append (list(consarg args)) (map cadr (car (cdr args))))))


(define let->application
  (lambda (args)
    (consmap args)))





;17a
;Joe Krisciuna :(                                                                                           )
;Russel Staples
;Nick Pisciotta
(define appendcheck
  append)
(define pid
  (lambda (args)
    (if (not (pair?  args))
        (list)
        (cons (car args) (pid (cdr args))))))

(define pvarid
  (lambda (args)
    (if (not (pair? args))
        args
        (pvarid (cdr args)))))

(define symbolist?
  (lambda (arg)
    (if (list? arg)
        (or (null? arg) (list-of symbol? arg))
    (or (improperl? arg) (symbol? arg)))))

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.
(define deforexp?
  (lambda (ls)
    (or (expression? ls) (definition? ls))))
(define listexpl?
  (lambda (ls)
    (or (expression? ls) (list-of expression? ls))))
(define condpred?
  (lambda (pred)
    (or (equal? pred 'else) (expression? pred))))
(define validassignment?
  (lambda (ls)
    (let [(tocheck (map car ls))]
      (andmap symbol? tocheck))))
(define improperl?
  (lambda (ls)
    (if (null? ls) #f
    (if (not (list? ls)) #t
    (ormap improper? ls)))))
(define invalidargs?
  (lambda (ls)
    (if (not (list? ls)) #f
    (if (null? ls) #f
        (if (not (symbol? (car ls))) #t (invalidargs? (cdr ls)))))))
(define improper?
  (lambda (ls)
    (and (not (list? ls)) (pair? ls))))
(define litcheck?
  (lambda (lit)
    (or (list? lit) lit (not lit) (symbol? lit) (integer? lit) (string? lit))))
(define invalidletargs?
  (lambda (ls)
    (if (not (list? ls))
        #t
        (if (null? ls) #f
            (if (not (eqv? 2 (length (car ls)))) #t
                (invalidletargs? (cdr ls)))))))




;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression.  You'll probably want to replace this 
; code with your expression datatype from A11b


(define idcheck?
  (lambda (arg)
    (if (null? arg)
        #t
    (listexpl? arg))))
(define varidcheck?
  (lambda (arg)
    (if (null? arg)
        #t
     (symbol? (car arg)))))


(define-datatype definition definition?
  [define-def
    (sym symbol?)
    (body expression?)]
    
  [begin-def
    (body (list-of deforexp?))])


(define-datatype expression expression?

  [while-exp
    (loopcond expression?)
    (body (list-of expression?))]
  [begin-exp 
    (body listexpl?)]
  [cond-exp 
    (predicates (list-of condpred?))
    (consequents (list-of expression?))]
  [if-exp-two
    (test-exp expression?)
    (then-exp listexpl?)
    (else-exp listexpl?)
    ]
	[if-exp-one
    (test-exp expression?)
    (then-exp listexpl?)
    ]
  [var-exp
   (id symbol?)]
  [lambda-exp
   (id idcheck?)
    (varid varidcheck?)
   (body listexpl?)]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))]
  [set!-exp
    (toset expression?)
    (value expression?)]
  [let-exp
    (assigned (list-of symbol?))
    (defined listexpl?)
    (body listexpl?)]
  [let*-exp
     (assigned listexpl?)
    (defined listexpl?)
    (body listexpl?)]
  [letrec-exp 
			(proc-names (list-of symbol?))
			(idss (list-of (list-of symbol?)))
			(bodiess (list-of (list-of expression?)))
			(letrec-bodies (list-of expression?))]
  [named-let-exp 
    (name symbol?)
    (assigned (list-of symbol?))
    (defined listexpl?)
    (body listexpl?)]
  [lit-exp
    (body litcheck?)]
  
  
  [case-exp
    (con listexpl?)
    (cases symbolist?)
    (results symbolist?)]
    )


	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define deref unbox)

(define set-ref! set-box!)

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms symbolist?)
   (vals (list-of box?))
   (env environment?)]
  [recursively-extended-env-record
	(proc-names (list-of symbol?))
	(idss (list-of (list-of symbol?)))
	(bodiess (list-of (list-of expression?)))
	(old-env environment?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.


(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (ids idcheck?)
    (bodies (list-of expression?))
    (env environment?)])

  
;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
; Again, you'll probably want to use your code form A11b
(define top-level-parse
  (lambda (datum)
  (if (and (pair? datum) (or (eqv? (1st datum) 'begin) (eqv? (1st datum) 'define)))
  (parse-def datum)
  (parse-exp datum)
  )))
    
  (define parse-def 
    (lambda (datum)
    (cond
      [(eqv? (1st datum) 'define) 
        (define-def (2nd datum) (parse-exp (3rd datum)))]

      [(eqv? (1st datum) 'begin)
         (cond 
          [(> 2 (length datum))
                                          (eopl:error 'top-level-parse "incorrect begin length ~s" datum)]
         [else (begin-def  (map top-level-parse (cdr datum)))])]  

      [else (eopl:error 'parse-def "bad definition: ~s" datum)])))
    

(define parse-exp         
  (lambda (datum)
    (cond
      [(null? datum) (list)]
       [(improper? datum)
         (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
     [(symbol? datum) (if (equal? datum 'else) (lit-exp datum) (var-exp datum))]
     [(number? datum) (lit-exp datum)]
     [(not (list? datum)) (lit-exp datum)]
     [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
     [(list? datum)
      (cond
        [(eqv? (car datum) 'case)
          (case-exp (parse-exp (2nd datum)) (map car (cddr datum)) (map parse-exp (map cadr (cddr datum))))]
        
              
       [(eqv? (car datum) 'lambda)
        (cond
        [(null? (cddr datum))
            (eopl:error 'parse-exp "lambda-expression: incorrect length: ~s" datum)]
            [(invalidargs? (2nd datum))
                            (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" datum)]
                
	[else (if (list? (2nd datum))
           (lambda-exp (2nd datum) (list) (map parse-exp (cddr datum)))
           (if (symbol? (2nd datum))
               (lambda-exp (list) (list (2nd datum)) (map parse-exp (cddr datum)))
               (lambda-exp (pid (2nd datum)) (list(pvarid (2nd datum))) (map parse-exp (cddr datum)))
               ))])]
           
        [(eqv? (1st datum) 'while)
         (cond 
          [(> 2 (length datum))
                                          (eopl:error 'parse-exp "incorrect while length ~s" datum)]
         [else (while-exp (parse-exp (2nd datum)) (map parse-exp (reverse (cddr datum))))])]

        [(eqv? (1st datum) 'begin)
         (cond 
          [(> 2 (length datum))
                                          (eopl:error 'parse-exp "incorrect begin length ~s" datum)]
         [else (begin-exp  (map parse-exp (cdr datum)))])]   

        [(eqv? (1st datum) 'cond)
         (cond 
          [(> 2 (length datum))
                                          (eopl:error 'parse-exp "incorrect cond length ~s" datum)]
         [else (cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum))))])]  


        [(eqv? (car datum) 'set!)
         (cond
           [(not (eqv? 3 (length datum)))
                                          (eopl:error 'parse-exp "incorrect set! length ~s" datum)]
              
         [else (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
           )]
         [(eqv? (car datum) 'if)
         (cond
           [(> 3 (length datum))
                                        (eopl:error 'parse-exp "incorrect if expression length" datum)]
			; For one armed
			[(= 3 (length datum))
			(if-exp-one  (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
        [else (if-exp-two  (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))])]
          
        [(eqv? (car datum) 'let)
         (cond
           [(symbol? (2nd datum)) (named-let-exp (2nd datum) (map car (3rd datum)) (map parse-exp (map cadr (3rd datum))) (map parse-exp (cdddr datum)))]

            [(improperl? (2nd datum))
                                                    (eopl:error 'parse-exp "incorrect let expression length" datum)]
           [(invalidletargs? (2nd datum))
                                                                 (eopl:error 'parse-exp "incorrect let expression arguments" datum)]
          
           [(> 3 (length datum))
                                                    (eopl:error 'parse-exp "incorrect let expression length" datum)]
            
            [(not (validassignment? (2nd datum)))
                                                                           (eopl:error 'parse-exp "incorrect let expression assignment" datum)]
         
           [else (let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))])]
        [(eqv? (car datum) 'let*)
         (cond
           [(improperl? (2nd datum))
                                                    (eopl:error 'parse-exp "incorrect let expression length" datum)]
           [(> 3 (length datum))
                                                    (eopl:error 'parse-exp "incorrect let expression length" datum)]
           [(invalidletargs? (2nd datum))
                                                                 (eopl:error 'parse-exp "incorrect let expression length" datum)]
         [(not (validassignment? (2nd datum)))
                                                                           (eopl:error 'parse-exp "incorrect let expression assignment" datum)]
         [else (let*-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))])]
      [(eqv? (car datum) 'letrec)
         (cond
           [(improperl? (2nd datum))
			(eopl:error 'parse-exp "incorrect let expression length" datum)]
           [(> 3 (length datum))
			(eopl:error 'parse-exp "incorrect let expression length" datum)]
           [(invalidletargs? (2nd datum))
			(eopl:error 'parse-exp "incorrect let expression arguments" datum)]                                                 
           [(not (validassignment? (2nd datum)))
            (eopl:error 'parse-exp "incorrect let expression assignment" datum)]
		   [else 
			; (proc-names idss bodiess letrec-bodies)
			(letrec-exp 
			(map car (2nd datum)) (map cadr (map cadr (2nd datum))) 
			(map (lambda (x) (map parse-exp x)) (map cddr (map cadr (2nd datum)))) (map parse-exp (cddr datum)))])]
      [else (app-exp (parse-exp (1st datum))
		     (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))







;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))

(define multivalsetter
  (lambda (args len)
    (if (eq? len 1)
        (cons args (list))
        (cons (car args) (multivalsetter (cdr args) (- len 1))))))
    
	
(define apply-env
	(lambda (env var)
	(deref (apply-env-ref env var))))
    
(define apply-env-ref
 (lambda (env sym) 
   (cases environment env 
    [empty-env-record ()      
     (apply-global-env-ref sym)] 
  [extended-env-record (syms vals env)
   ;addedcondhereformultivar
       (cond
         [(< (length syms) (length vals))
           (set! vals (multivalsetter vals (length syms)))
		(let ((pos (list-find-position sym syms)))
     	  (if (number? pos)
	     (list-ref vals pos)
	      (apply-env-ref env sym)))]
          [else (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (list-ref vals pos)
	      (apply-env-ref env sym)))]
          
          )]
	[recursively-extended-env-record
		(procnames idss bodiess old-env)
		(let ([pos (list-find-position sym procnames)])
		(if (number? pos)
			(box (closure (list-ref idss pos)
					 (list-ref bodiess pos)
					 env))
			(apply-env-ref old-env sym)))]
		  
		  )))



;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define top-level-synexpand
  (lambda (ls)
  (if (expression? ls)
    (syntax-expand ls)
    (synexpand-def ls))))

(define synexpand-def
  (lambda (def)
      (cases definition def
        [define-def (sym body)
          (define-def sym (syntax-expand body))]

        [begin-def (body)
          (begin-def (map top-level-synexpand body))]
        
        [else (eopl:error 'synexpand-def "def not defined in synexpand-def: ~s")])))

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [lit-exp (lit) exp]
      [var-exp (var) exp]
      [case-exp (con cases results)
        (cond
          [(eqv? (car cases) 'else)
           (car results)]
          [else
            (cond
              [(list? (car cases))
               (syntax-expand(if-exp-two (app-exp (var-exp 'contains?) (list con (lit-exp (car cases))))  (car results) (syntax-expand (case-exp con (cdr cases) (cdr results)))))]
              
           [else (if-exp-two (app-exp (var-exp 'eqv?) (list con (lit-exp (car cases))))  (car results) (syntax-expand (case-exp con (cdr cases) (cdr results))))] 
      )]
          )]
          
	[set!-exp (var-exp lit-exp)
	(set!-exp (syntax-expand var-exp) lit-exp)]

      [if-exp-one (if-exp then-exp)
        (if-exp-one (syntax-expand if-exp) (syntax-expand then-exp))]
      [if-exp-two (if-exp then-exp else-exp)
        (if-exp-two (syntax-expand if-exp) (syntax-expand then-exp) (syntax-expand else-exp))]
      [app-exp (rator rand)
        (cond
          
          
             
          [(eqv? (cadr rator) 'or)
           (if (null? rand) (lit-exp '#f)
           (let ([ccon (pbegin (car rand))])
           (cond
             [(null? rand) (lit-exp '#f)]
             [(eqv? (length rand) 1) (syntax-expand (if-exp-two (car rand) ccon (lit-exp '#f)))]
             [else (syntax-expand (if-exp-two (car rand) ccon (syntax-expand (app-exp (var-exp 'or) (cdr rand)))))]
             
             )))]
          
          
          [(eqv? (cadr rator) 'and)
           (cond
             
            [(eqv? (length rand) 1) (lit-exp '#f)] 
           [(eqv? (length rand) 2)
               (syntax-expand(if-exp-two (car rand) (cadr rand) (lit-exp '#f)))]
               
           [else (syntax-expand (if-exp-two (car rand) (syntax-expand (app-exp (var-exp 'and) (cdr rand))) (lit-exp '#f)))]
          )]
         
          
          
          [else exp])]

             [cond-exp (preds bodies)
        (cond 
          [(eq? preds '()) (app-exp (var-exp 'void) '()) ]
          [(equal? (car preds) 'else) (syntax-expand (car bodies))]
          [else (if-exp-two (syntax-expand (car preds)) (syntax-expand (car bodies)) (syntax-expand (cond-exp (cdr preds) (cdr bodies))))])]    
      
       [let*-exp (assigned defined body)
        (cond
          [(eqv? (length assigned) 1) (syntax-expand (let-exp (list(car assigned)) (list(car defined)) body))]
        [else (syntax-expand (let-exp (list (car assigned)) (list (car defined)) (list (let*-exp (cdr assigned) (cdr defined) body))))] 
          
           )]
          
           

  
      [begin-exp (body)
        (begin-exp (map syntax-expand  body))]



      [let-exp (assigned defined body)

       (app-exp (lambda-exp assigned (list)  (map syntax-expand body))  defined)]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (letrec-exp proc-names idss (map (lambda (x) (map syntax-expand x)) bodiess) (map syntax-expand letrec-bodies))]
      [named-let-exp (name assigned defined body)
        (letrec-exp (list name) (list assigned) (list (map syntax-expand body)) (list (app-exp (var-exp name) (map syntax-expand defined))))]
      [lambda-exp (id varid body) (lambda-exp id varid (if (expression? body) (syntax-expand body) (map syntax-expand body)))]
      [else (eopl:error 'syntax-expand "not defined in syntax-expand")]
      )))
;--------------------------------------+
;                                      |
;   CONTINUATION DATATYPE and APPLY-K  |
;                                      |
;--------------------------------------+

; To be added in assignment 18a.


;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+


; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    (if (expression? form)
      [eval-exp form (empty-env)]
      [eval-def form (empty-env)])))

(define extend-env-recursively
	(lambda (proc-names idss bodiess old-env)
	(recursively-extended-env-record
	proc-names idss bodiess old-env)))

; eval-exp is the main component of the interpreter

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin
          (eval-exp (car bodies) env)
          (eval-bodies (cdr bodies) env)))))

(define top-level-eval-bodies
  (lambda (bodies)
    (if (null? (cdr bodies))
        (top-level-eval (car bodies))
        (begin
          (top-level-eval (car bodies))
          (top-level-eval-bodies (cdr bodies))))))

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
    (apply-env env id)]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (if (integer? proc-value) proc-value
          
               (apply-proc proc-value args)))]
      [let-exp (assigned defined body)
        (eval-bodies body (extend-env assigned (eval-rands defined env) env))]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (eval-bodies letrec-bodies (extend-env-recursively proc-names idss bodiess env))] ; Bodies shouldnt have lambda
		
	; For set!
	[set!-exp (id exp)
	(set-ref!
	(apply-env-ref env (cadr id))
	(eval-exp exp env))]

      [if-exp-two (test-exp then-exp else-exp)
	  ; Two armed if
        (if (eval-exp test-exp env)
            (eval-exp then-exp env)
            (eval-exp else-exp env))]
		; One armed if
      [if-exp-one (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
      [while-exp (loopcond body)
        (if (eval-exp loopcond env)
            (begin (eval-rands body env)
                (eval-exp exp env))
            '())]
      [begin-exp (body)
        (apply-proc (closure '() body env) '())]
      [lambda-exp (id varid bodies)
        (cond
          [(and (null? id) (null? varid)) (eval-bodies bodies env)]
          [(null? id) (closure varid bodies env)]
          [(null? varid) (closure id bodies env)]
          [else 
        (closure (appendcheck id varid) bodies env)])]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define eval-def
  (lambda (def env)
    (cases definition def
      [begin-def (body) (top-level-eval-bodies body)]
      [define-def (sym body) (set! init-env (extend-env (list sym) (list (eval-exp body env)) init-env))]
      [else (eopl:error 'eval-def "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (e)
           (eval-exp e env))
      rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (ids bodies env)
        (eval-bodies bodies
            (extend-env ids args env))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(contains? list-tail void map eqv? apply + - * / add1 sub1 quotient zero? not = < > <= >= append cons car cdr list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar cddr caaar caadr cdaar cadar caddr cdadr cddar cdddr))




(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))
(define global-env init-env)
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
	; Map and Apply
	[(map) (apply map (lambda x (apply-proc (1st args) x)) (cdr args))]
	[(apply) (apply-proc (1st args) (2nd args))]
	; Basic math
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
	    [(/) (apply / args)]
      [(quotient) (quotient (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
	; List/Vector stuff
	  [(car) (car (1st args))]
	  [(cdr) (cdr (1st args))]
    [(append) (apply append args)]
	  [(list->vector) (list->vector (1st args))]
	  [(vector->list) (vector->list (1st args))]
      [(cons) (cons (1st args) (2nd args))]
	  [(list)  (apply list args)]
	  [(vector) (apply vector args)]
	  [(make-vector) (make-vector (1st args) (2nd args))]
	  [(length) (length (1st args))]
	  [(vector-ref) (vector-ref (1st args) (2nd args))]
	  [(assq) (assq (1st args) (2nd args))]
	  [(set-car!) (set-car! (1st args) (2nd args))]
	  [(set-cdr!) (set-cdr! (1st args) (2nd args))]
	  [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
	; Boolean stuff
	  [(=) (= (1st args) (2nd args))]
	  [(<) (< (1st args) (2nd args))]
	  [(<=) (<= (1st args) (2nd args))]
	  [(>) (> (1st args) (2nd args))]
	  [(>=) (>= (1st args) (2nd args))]
	  [(equal?) (equal? (1st args) (2nd args))]
	  [(eq?) (eq? (1st args) (2nd args))]
      [(eqv?) (eq? (1st args) (2nd args))]
      [(contains?) (contains? (1st args) (2nd args))]
      [(list-tail) (list-tail (1st args) (2nd args))]
      
	  [(atom?) (atom? (1st args))]
	  [(not) (not (1st args))]
	  [(zero?) (zero? (1st args))]
	  [(symbol?) (symbol? (1st args))]
	  [(null?) (null? (1st args))]
	  [(number?) (number? (1st args))]
	  [(pair?) (pair? (1st args))]
	  [(list?) (list? (1st args))]
	  [(vector?) (vector? (1st args))]
	  [(procedure?) (if (list? (1st args))
                     (or (eqv? (1st (1st args)) 'closure)
                         (eqv? (1st (1st args)) 'prim-proc))
                     #f)]
	; c**r and c***r
	  [(caar) (caar (1st args))]
	  [(cadr) (cadr (1st args))]
	  [(cdar) (cdar (1st args))]
	  [(cddr) (cddr (1st args))]
	  [(caaar) (caaar (1st args))]
	  [(caadr) (caadr (1st args))]
	  [(cdaar) (cdaar (1st args))]
	  [(cadar) (cadar (1st args))]
	  [(caddr) (caddr (1st args))]
	  [(cdadr) (cdadr (1st args))]
	  [(cddar) (cddar (1st args))]
	  [(cdddr) (cdddr (1st args))]
	; Misc.
	  [(display) (display (1st args))]
	  [(newline) (newline)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define reset-global-env
  (lambda ()
    (set! init-env global-env)))

(define apply-global-env-ref
  (lambda (sym)
    (apply-global-env-ref-helper sym init-env)))

(define apply-global-env-ref-helper
  (lambda (sym env)
    (cases environment env
      [extended-env-record (syms vals env)
        (let ([pos (list-find-position sym syms)])
          (if (number? pos)
              (list-ref vals pos)
              (apply-global-env-ref-helper sym env)))]
      [empty-env-record ()
        (eopl:error 'global-env "symbol ~s is not bound in global env" sym)]
		[recursively-extended-env-record
		(procnames idss bodiess old-env)
		(let ([pos (list-find-position sym procnames)])
		(if (number? pos)
			(closure (list-ref idss pos)
					 (list-ref bodiess pos)
					 env)
			(apply-env-ref old-env sym)))])))

(define closeprocchecker
  (lambda (args)
    (if (null? args)
        (list)
    (if (list? args)
        (if (list? (car args))
            (if (or (eqv? (car (car args)) 'prim-proc) (eqv? (car (car args)) 'closure))
                (cons '<interpreter-procedure> (closeprocchecker (cdr args)))
                (cons (car args) (closeprocchecker (cdr args))))
            (cons (car args) (closeprocchecker (cdr args))))
        args))))
            



(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (top-level-parse (read)))])
      
      (eopl:pretty-print (closeprocchecker answer)) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp ; youll add a local environment argument.
 (lambda (exp) (top-level-eval (top-level-synexpand (top-level-parse exp)))))


