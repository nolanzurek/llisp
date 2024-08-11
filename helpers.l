
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
(defun primitive-fn? (fn) (if (eq fn '+) T nil))

; check if a function is a lambda function
(defun lambda-ap? (expr) (if (and (not (atom (car expr))) (eq (car (car expr)) 'lambda)) T nil))

; defers to lisp to give the results from the primitive functions
(defun apply-primitive (fn args) 
    (cond 
        ((eq fn '+) (+ (car args) (cadr args)))
        (T 0)))