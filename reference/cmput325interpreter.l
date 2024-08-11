
;; Question 3: Interpreter

;; implements an interpreter for the "FL" lanaguage as specified in the assignment
;; FL evaluations consists of an expression (parameter E) and a list of defined functions (parameter P)
;; E is evaluated in the context of the defined functions in P
;; note that the implementation uses an internal environment to keep track of parameters
;; since binding parameters to function bodies is a scoping mechanism
;; the environment contains the binding of 'T and T to start with
;; fl-interp: Expression Program -> Expression
(defun fl-interp (E P) 
  (apply-hof (fl-eval E P (list (list 'T T))) P (list (list 'T T))))

;; helper function: fl-eval
;; fl-eval: (listof fl-exp) -> Value
;; evaluates an expression in the context of a list of defined functions and the environment
;; main recursive function of the interpreter
;; dispatches the appropriate action based on the type of the expression
(defun fl-eval (exp P env) 
  (cond 
    ((atom exp) (if (numberp exp) exp 
      (let ((val (lookup exp env))) 
        (if val (cadr val) exp))))
    ((primitive? (car exp)) 
      (apply-primitive-fn (car exp) (cdr exp) P env))
    ;; I think this can just be "true" instead of needing to be checked
    ((defined? (car exp) P) 
      (apply-defined-fn (car exp) (cdr exp) P env))
    (T (mapcar (lambda (x) (fl-eval x P env)) exp))))  

;; helper function: primitive? 
;; primitive?: function-name -> Boolean
;; checks if a function is a primitive function from the list
(defun primitive? (fn) 
  (my-member fn '(+ - * > < = not and or cons equal car cdr number if null atom eq)))

;; helper function: my-member 
;; my-member: Value (listof Value) -> Boolean
;; checks if a value is in a list
;; "member" wasn't in the list of allowed functions; why? 
(defun my-member (val L) 
  (cond 
    ((null L) nil)
    ((eq val (car L)) T)
    (T (my-member val (cdr L)))))

;; helper: apply-primitive-fn
;; applies a primitive function to its arguments
;; if I had time to restructure the program, I would have found a way to clean up the "if" special case
;; possibly by applying all the primitive functions using a cond tree, like the original implementation I "improved"
(defun apply-primitive-fn (fn args P env) 
;; special case for if-statmenets, since they shouldn't evaluate all their arguments
  (if 
    (eq fn 'if) 
    (if 
      (fl-eval (car args) P env) 
      (fl-eval (cadr args) P env) 
      (fl-eval (caddr args) P env)) 
    (apply 
      (check-primitive-fn fn) 
      (mapcar (lambda (x) (fl-eval x P env)) args))))

; helper function for apply-primitive-fn
; check-primitive-fn: function-name -> function
; checks if a function is a special primitive function and returns the corresponding function
; if the function is not special, it returns the function itself
; all boolean functions are special due to the "returning T" requirement
; I am aware that this function epitomizes "jank"
(defun check-primitive-fn (fn) 
  (cond 
    ((eq fn 'number) (lambda (x) (numberp x)))
    ((eq fn 'and) (lambda (x y) (if (and x y) T nil)))
    ((eq fn 'or) (lambda (x y) (if (or x y) T nil)))
    ((eq fn 'not) (lambda (x) (if (not x) T nil)))
    ((eq fn 'null) (lambda (x) (if (null x) T nil)))
    ((eq fn 'atom) (lambda (x) (if (atom x) T nil)))
    ((eq fn 'eq) (lambda (x y) (if (eq x y) T nil)))
    ((eq fn 'equal) (lambda (x y) (if (equal x y) T nil)))
    ((eq fn '> ) (lambda (x y) (if (> x y) T nil)))
    ((eq fn '<) (lambda (x y) (if (< x y) T nil)))
    ((eq fn '=) (lambda (x y) (if (= x y) T nil)))
    ((eq fn 'if) (lambda (x y z) (if x y z)))
    (T fn)))

;; helper function: defined?
;; defined?: function-name -> Boolean
;; checks if a function is a defined function in the program P
(defun defined? (fn P) (mapcar 'car P)
  (my-member fn (mapcar 'car P)))

; helper function: apply-defined-fn
; applies a defined function to its arguments
; this is done by adding the values (arguments) of all the parameters to the environment
(defun apply-defined-fn (fn args P env) 
  (fl-eval 
    (cadddr (lookup-defined fn (length args) P)) 
    P 
    (bind-params 
      (cadr (lookup-defined fn (length args) P)) 
      (mapcar (lambda (x) (fl-eval x P env)) args) 
      env)))

;; helper function: lookup
;; looks up a value in an association list
(defun lookup (val L) 
  (cond 
    ((null L) nil)
    ((eq (caar L) val) (car L))
    (T (lookup val (cdr L)))))

;; helper function: lookup-defined
;; looks up a defined function in the list of defined functions
;; differs from lookup in that it looks for a function with a specific arity
;; as per the "method signature" requirements of the FL langauge
(defun lookup-defined (fn arity P) 
  (cond 
    ((and (eq (caar P) fn) (= arity (length (cadar P)))) (car P))
    (T (lookup-defined fn arity (cdr P)))))

;; helper function: bind-params
;; binds parameters to their values, creating a new environment
;; this is done by adding the values (arguments) of all the parameters to the environment
;; I know the interpreter didn't require an environment, but it turns out the idea is just that useful! who knew?
(defun bind-params (params args env) 
  (cond 
    ((null params) env)
    (T (cons 
      (list (car params) (car args)) 
      (bind-params (cdr params) (cdr args) env)))))

;; helper function: apply-hof
;; looks at the "final" evaluation of the program, and applies any higher-order functions
;; that weren't caught by main evaluation function
;; this is done by applying the fl-eval function to the expression
;; yes, this is janky as well
;; it kills me that altohugh it works, it destroys the elegance that makes interpreters such a fun area
;; instead we get "imperative nesting" to "run" this "procedure" on the "finished" expression
;; gross
(defun apply-hof (exp P env) 
  (cond 
    ((atom exp) exp)
    ((atom (car exp)) 
      (if 
        (defined? (car exp) P) 
        (fl-eval exp P env) 
        exp))
    (T (mapcar (lambda (x) (apply-hof x P env)) exp))))