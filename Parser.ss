;; Name: Khaled Alfayez
;; Assignment: 11
; Helper methods are in the end of the document.
; -------------------

(load "chez-init.ss")

; Problem 4:
;	letrec, (named) let, let*, set!, if.
(define-datatype expression expression?
	; Variables
	[var-exp
	   (id symbol?)]
	; Primitives and whatever returns itself
	[lit-exp
		(id datatype?)]
	; Lambdas
	[lambda-exp
		(formals (lambda (formals)
				(cond	[(symbol? formals)]
						[(andmap symbol? formals)])))
	   (body (lambda (bodies) (andmap expression? bodies)))]
	; If expressions
	[if-exp
		(condition
			expression?)
		(true-case expression?)]
	[complete-if-exp
		(condition
			expression?)
		(true-case expression?)
		(false-case expression?)]
	; Let expressions
	[let-exp
		(formals list-of-expressions?)
		(body (lambda (bodies) (andmap expression? bodies)))]
	; Let sequential expressions
	[let*-exp
		(formals list-of-expressions?)
		(body (lambda (bodies) (andmap expression? bodies)))]
	; Named let expression
	[named-let-exp
		(name symbol?)
		(formals list-of-expressions?)
		(body (lambda (bodies) (andmap expression? bodies)))]
	; Let recursive expressions
	[letrec-exp
		(formals list-of-expressions?)
		(body (lambda (bodies) (andmap expression? bodies)))]
	; Other procedures with oprators and oprands
	[app-exp
	   (rator expression?)
	   (rand (lambda (operands) (andmap expression? operands)))])

(define (parse-let-formals formals)
	(cond	[(null? formals)]
			[(list? (car formals))
				(and	(parse-exp (2nd (car formals)))
						(parse-let-formals (cdr formals)))]
			[else #f])
)

(define (list-of-expressions? lst)
	(andmap expression? lst)
)

(define (check-proper-list lst)
	(and
		(pair? lst)
		(list? lst)
		(list? (cdr lst)))
)

(define (not-symbol-formals? lst)
	(not (andmap (lambda (element) (symbol? (car element))) lst))
)

(define (check-legal-list-and-not-len-2-formals lst type)
	(not (andmap (lambda (element) 
			(cond	[(not (check-proper-list lst))
						(display "wot")
						(lets-error "in ~s not all proper lists: ~s" type lst)]
					[(not (= 2 (length element)))
						(lets-error "in ~s not all length 2: ~s" type lst)]
					[else
						#t]))
		lst))
)

(define (datatype? datum)
	(or
		(boolean? datum)
		(char? datum)
		(pair? datum)
		(null? datum)
		(string? datum)
		(number? datum)
		(vector? datum))
)
   
(define (lets-error text type datum)
	(eopl:error 'parse-exp text type datum)
)   

(define named-let? symbol?)

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
		[(pair? datum)
		  (cond
			[(eqv? (car datum) 'lambda)
				(cond	[(> 3 (length datum))
							(eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
						[(symbol? (2nd datum))
							(lambda-exp (2nd  datum)
								(map parse-exp (cddr datum)))]
						[(not (andmap symbol? (2nd datum)))
							(eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (2nd datum))]
						[else
							(lambda-exp (2nd  datum)
								(map parse-exp (cddr datum)))])]
			[(eqv? (car datum) 'let)
				(cond	[(> 3 (length datum))
							(lets-error "~s-expression has incorrect length ~s" 'let datum)]
						[(named-let? (2nd datum))
							(cond	[(not (list? (3rd datum)))
										(lets-error "formals declaration in ~s-expression not a list: ~s" 'let datum)]
									[(not (andmap check-proper-list (3rd datum)))
										(lets-error "in ~s-expression not all proper lists: ~s" 'let (3rd datum))]
									[(check-legal-list-and-not-len-2-formals (3rd datum) 'let)]
									[(not-symbol-formals? (3rd datum))
										(lets-error "vars in ~s-exp must be symbols ~s" 'let (3rd datum))]
									[else
										(named-let-exp (2nd datum)
												(map parse-exp (3rd datum))
												(map parse-exp (cdddr datum)))])]
						[else
							(cond	[(not (list? (2nd datum)))
										(lets-error "formals declaration in ~s-expression not a list: ~s" 'let datum)]
									[(not (andmap check-proper-list (2nd datum)))
										(lets-error "in ~s-expression not all proper lists: ~s" 'let (2nd datum))]
									[(check-legal-list-and-not-len-2-formals (2nd datum) 'let)]
									[(not-symbol-formals? (2nd datum))
										(lets-error "vars in ~s-exp must be symbols ~s" 'let (2nd datum))]
									[else
										(let-exp (map parse-exp (2nd datum))
											(map parse-exp (cddr datum)))])
							])]
			[(eqv? (car datum) 'let*)
				(cond	[(> 3 (length datum))
							(lets-error  "~s-expression has incorrect length ~s" 'let* datum)]
						[(not (list? (2nd datum)))
							(lets-error "formals declaration in ~s-expression not a list: ~s" 'let* datum)]
						[(not (andmap check-proper-list (2nd datum)))
							(lets-error "in ~s-expression not all proper lists: ~s" 'let (2nd datum))]
						[(check-legal-list-and-not-len-2-formals (2nd datum) 'let*)]
						[(not-symbol-formals? (2nd datum))
							(lets-error "vars in ~s-exp must be symbols ~s" 'let* (2nd datum))]
						[else
							(let*-exp	(map parse-exp (2nd datum))
								(map parse-exp (cddr datum)))])]
			[(eqv? (car datum) 'letrec)
				(cond	[(> 3 (length datum))
							(lets-error  "~s-expression has incorrect length ~s" 'letrec datum)]
						[(not (list? (2nd datum)))
										(lets-error "formals declaration in ~s-expression not a list: ~s" 'letrec datum)]
						[(not (andmap check-proper-list (2nd datum)))
							(lets-error "in ~s-expression not all proper lists: ~s" 'let (2nd datum))]
						[(check-legal-list-and-not-len-2-formals (2nd datum) 'letrec)]
						[(not-symbol-formals? (2nd datum))
							(lets-error "vars in ~s-exp must be symbols ~s" 'letrec (2nd datum))]
						[else
							(letrec-exp	(map parse-exp (2nd datum))
								(map parse-exp (cddr datum)))])]
			[(eqv? (car datum) 'if)
				(cond	[(> 3 (length datum))
							(eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" (2nd datum))]
						[(eq? 3 (length datum))
							(if-exp (parse-exp (2nd datum))
									(parse-exp (3rd datum)))]
						[(eq? 4 (length datum))
							(complete-if-exp	(parse-exp (2nd datum))
												(parse-exp (3rd datum))
												(parse-exp (4th datum)))]
						[else
							(eopl:error 'parse-exp "if-expression: incorrect length ~s" datum)])]
			[else
				(cond	[(and (pair? datum) (not (list? datum)))
							(eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
						[else
							(cond	[(equal? 'set! (car datum))
										(cond	[(> 3 (length datum))
													(eopl:error 'parse-exp
														"set! expression ~s does not have (only) variable and expression"
														datum)]
												[(< 3 (length datum))
													(eopl:error 'parse-exp
														"in set! Too many parts: ~s"
														datum)]
												[else
													(app-exp (parse-exp (1st datum))
													(map parse-exp (cdr datum)))])
										]
									[else
										(app-exp (parse-exp (1st datum))
										(map parse-exp (cdr datum)))])])])]
		 [(symbol? datum) (var-exp datum)]
		 [(datatype? datum) (lit-exp datum)]
		 [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (unparse-exp expr)
	(cases	expression expr
			[var-exp (id)
				id]
			[lit-exp (id)
				id]
			[lambda-exp (formals body)
				(apply list 'lambda formals (map unparse-exp body))]
			[let-exp (formals body)
				(apply list 'let
					(map unparse-exp formals)
					(map unparse-exp body))]
			[named-let-exp (name formals body)
				(apply list 'let name
					(map unparse-exp formals)
					(map unparse-exp body))]
			[if-exp (condition true-case)
				(list 'if (unparse-exp condition)
					(unparse-exp true-case))]
			[complete-if-exp (condition true-case false-case)
				(list 'if (unparse-exp condition)
					(unparse-exp true-case)
					(unparse-exp false-case))]
			[app-exp (rator rand)
				(cons	(unparse-exp rator)
						(map unparse-exp rand))]
			[let*-exp (formals body)
				(apply list 'let*
				(map unparse-exp formals)
				(map unparse-exp body))]
			[letrec-exp (formals body)
				(apply list 'letrec
				(map unparse-exp formals)
				(map unparse-exp body))]
			[else '()])
)