(defconstant program1 '((lambda (a b) (+ a (+ a b))) (+ 8 2) (+ 9 7)))
(defconstant program2 '((lambda (a b) (a b)) (lambda (c) (+ c c)) (+ 3 4)))

(defconstant shortCircuit1 '(or 1 (/ 1 0)))
(defconstant shortCircuit2 '(if T 2 (/ 1 0)))