(load "./tests/test-programs.l")
(load "./helpers.l") ;; helper functions

; evaluate the program on its own, with an empty environment
(defun eval-program (program)
    (eval-expr program '()))

; evaluate an expression in an environment
(defun eval-expr (expr env)
    (cond 
        ((numberp expr) expr)
        ((symbolp expr) 
            (if (assoc expr env) (handle-symbol expr env) 
            (error "function or constant is not defined")))
        ; somehow the expression is something that's not a list, but is not a number or symbol either
        ((atom expr) (error "something is wrong"))
        ; special check for conditionals in order to prevent infinite loops due to eager evaluation
        ((eq (car expr) 'if) 
            (if (eval (car expr) env) (eval (cadr expr) env) (eval (caddr expr) env)))
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

(print (if (eq (eval-program program1) 36) 'pass 'fail))
(print (if (eq (eval-program program2) 14) 'pass 'fail))

