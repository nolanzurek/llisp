
; evaluate a list of expressions: simple recursion
(defun eval-expr-list (exprs env)
    (if (null exprs) 
        nil 
        (cons 
            (eval-expr (car exprs) env) 
            (eval-expr-list (cdr exprs) env))))

; apply function
; we assume that the args have already been evaluated
(defun apply-lambda (l-fn args env) 
    (eval-expr (caddr l-fn) (append (pairlis (cadr l-fn) args) env)))

; handle symbol
; if the symbol means something in the environment, we retrieve it
; otherwise we assume the client wanted to use a symbol
; this needs to implement the more efficient data structure for the env later
(defun handle-symbol (symb env) 
    (let ((val (cdr (assoc symb env))))
        (if val val symb)))

; check if a function is primitive
(defun primitive-fn? (fn) 
  (member fn '(+ - * > < / = not and or cons equal car cdr number if null atom eq)))

; check if a function is a lambda function
(defun lambda-ap? (expr) (if (and (not (atom (car expr))) (eq (car (car expr)) 'lambda)) T nil))

; defers to lisp to give the results from the primitive functions
(defun apply-primitive (fn args) 
    (cond 
        ((eq fn '+) (+ (car args) (cadr args)))
        ((eq fn '-) (- (car args) (cadr args)))
        ((eq fn '*) (* (car args) (cadr args)))
        ((eq fn '/) (/ (car args) (cadr args)))
        ((eq fn '>) (> (car args) (cadr args)))
        ((eq fn '=) (= (car args) (cadr args)))
        ((eq fn 'not) (if (not (car args)) T nil))
        ((eq fn 'and) (if (and (car args) (cadr args)) T nil))
        ((eq fn 'or) (if (or (car args) (cadr args)) T nil))
        ((eq fn 'cons) (cons (car args) (cadr args)))
        ((eq fn 'equal) (if (equal (car args) (cadr args)) T nil))
        ((eq fn 'car) (car (car args)))
        ((eq fn 'cdr) (cdr (car args)))
        ((eq fn 'number) (numberp (car args)))
        ((eq fn 'null) (if (null (car args)) T nil))
        ((eq fn 'atom) (if (atom (car args)) T nil))
        ((eq fn 'eq) (if (eq (car args) (cadr args)) T nil))
        (T (error "primitive function not defined"))))