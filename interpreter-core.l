(load "./helpers.l") ;; helper functions
(load "./desugarer.l")

; evaluate the program on its own, with an empty environment
(defun eval-program (program)
    (eval-expr (desugar program) (list (cons 'T T) (cons 'F nil) (cons 'nil nil))))

; evaluate an expression in an environment
; TODO: make this a let and define fn name and args (might need to nest tho)
(defun eval-expr (expr env)
    (cond 
        ((numberp expr) expr)
        ; ((symbolp expr) 
        ;     (if (assoc expr env) (handle-symbol expr env) 
        ;     (error "function or constant is not defined")))
        ((symbolp expr) (handle-symbol expr env))
        ; somehow the expression is something that's not a list, but is not a number or symbol either
        ((atom expr) (error "something is wrong"))
        ; special check for conditionals in order to prevent infinite loops due to eager evaluation
        ((eq (car expr) 'if) 
            (if 
                (eval-expr (cadr expr) env) 
                (eval-expr (caddr expr) env) 
                (eval-expr (cadddr expr) env)))
        ; if we are evaluating a short circuiting operator, handle it specifically
        ; the desugarer guarentees that each or and and statement contains two arguments
        ((eq (car expr) 'or) 
            (if (eval-expr (cadr expr) env) T (eval-expr (caddr expr) env)))
        ((eq (car expr) 'and) 
            (if (eval-expr (cadr expr) env) (eval-expr (caddr expr) env) nil))
        ; check if primitive function; if so, defer to lisp
        ((primitive-fn? (car expr)) (apply-primitive (car expr) (eval-expr-list (cdr expr) env)))
        ; if we are using a lambda function, we don't need to evaluate it. We wait to apply it
        ((eq 'lambda (car expr)) expr)
        ; lambda application
        ((lambda-ap? expr) 
            (eval-expr 
                (caddar expr)
                (append (pairlis (cadar expr) (eval-expr-list (cdr expr) env)) env)))
        ; in this case, the first element should a function symbol that points to a function stored in the environment
        (T (let ((fn (eval-expr (car expr) env)))
            (eval-expr (cons fn (cdr expr)) env)))))

; (check-expect (eval-program '(+ 1 2 3 4 5 6)) 21)

; (print (if (eq (eval-program program1) 36) 'pass 'fail))
; (print (if (eq (eval-program program2) 14) 'pass 'fail))
; (print (if (eval-program shortCircuit2) 'pass 'fail))
; (print (eval-program shortCircuit2))

; (print (eval-program '(let ((a 1) (b 2)) (+ a b))))