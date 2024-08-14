; TODO: use a hash table for this!

(defun desugar (expr) 
    (if (atom expr) expr 
        (let ((fn-name (car expr)) (args (cdr expr))) 
            (cond 
                ; expand collapse into a list of binary operators
                ((eq fn-name '+) (desugar-associative-binop '+ 0 args))
                ((eq fn-name '*) (desugar-associative-binop '* 1 args))
                ; not using desugar-associative-binop because and and or specifically are implemented with short circuiting
                ; inequalities (> and = have already been implemented)
                ((eq fn-name '<) (desugar (list '> (cadr args) (car args))))
                ((eq fn-name '>=) (desugar (list 'or (cons '> args) (cons '= args))))
                ((eq fn-name '<=) (desugar (list 'or (cons '< args) (cons '= args))))
                ((eq fn-name 'or) (desugar-or args))
                ((eq fn-name 'and) (desugar-and args))
                ((eq fn-name 'let) (desugar-let args))
                ((eq fn-name 'let*) (desugar-let* args))
                ((eq fn-name 'cond) (desugar-cond args))
                ((eq fn-name 'list) (desugar-list args))
                (T expr)))))

; note that this is inefficient because we can prune or and and statements
; we know will pass/fail here, but choose to expand them out anyway (wasting space and time)
; we are strict about this being desugaring, not evaluation or optimization
(defun desugar-or (args) 
    (cond 
        ((null args) nil)
        ((atom args) args)
        (T (list 'or (desugar (car args)) (desugar-or (cdr args))))))

(defun desugar-and (args)
    (cond 
        ((null args) T)
        ((atom args) args)
        (T (list 'and (desugar (car args)) (desugar-and (cdr args))))))

; TODO: fix the fact that the desugaring is NOT recursive here!
(defun desugar-let (args)
    (let ((bindings (car args)) (body (cadr args))) 
        (cons 
            (list 'lambda (mapcar 'car bindings) (desugar body)) 
            (mapcar (lambda (x) (desugar (cadr x))) bindings)))) ; we desugar the values here

; bindings are sequential here
; so it will nest the lambda functions
; could just call let and bootstrap even more but we'll get there anyway, might as well do it efficiently
(defun desugar-let* (args) 
    (let ((bindings (car args)) (body (cadr args))) 
        (if (null bindings) (desugar body) 
            (desugar (list 'let (list (car bindings)) (list 'let* (cdr bindings) body))))))

(defun desugar-cond (args) 
    (cond 
        ((null args) nil)
        ((atom args) args)
        (T (list 'if (desugar (caar args)) (desugar (cadar args)) (desugar-cond (cdr args))))))

; is there a lisp function that does this for me?
; like this pattern I mean
; idiom time
(defun desugar-associative-binop (op id-element args)
    (cond 
        ((null args) id-element)
        ((null (cdr args)) (car args))
        ; ((= 2 (length args)) (list op (car args) (cadr args)))
        (T (list op (desugar (car args)) (desugar-associative-binop op id-element (cdr args))))))

; can this be implemented with desugar-associative-binop?
(defun desugar-list (args) 
    (cond 
        ((null args) nil)
        (T (list 'cons (desugar (car args)) (desugar-list (cdr args))))))

(print (desugar '(let ((a (list 1 2 3 4 5 6 7 8 9)) (b 2)) (cons b a))))