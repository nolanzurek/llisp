(load "./testing/check-test.l")
(load "./interpreter-core.l")

; (defconstant program1 '((lambda (a b) (+ a (+ a b))) (+ 8 2) (+ 9 7)))
; (defconstant program2 '((lambda (a b) (a b)) (lambda (c) (+ c c)) (+ 3 4)))

; (defconstant shortCircuit1 '(or 1 (/ 1 0)))
; (defconstant shortCircuit2 '(if T 2 (/ 1 0)))


(defun run-tests ()
  (reset-test-counters)
  
  ; addition and multiplication, combined
    (check-expect (eval-program '(+ 1 2 3 4 5 6)) 21)
    (check-expect (eval-program '(* 1 2 3 4 5 6)) 720)
    (check-expect (eval-program '(+ (* 1 4 2) (+ 4 (* 9 1)))) 21)

    ; inequalities
    (check-expect (eval-program '(> 1 2)) nil)
    (check-expect (eval-program '(> 2 1)) T)
    (check-expect (eval-program '(= 1 2)) nil)
    (check-expect (eval-program '(= 2 2)) T)
    (check-expect (eval-program '(>= 2 1)) T)
    (check-expect (eval-program '(>= 1 2)) nil)
    (check-expect (eval-program '(<= 2 1)) nil)
    (check-expect (eval-program '(<= 1 2)) T)

    ; short circuiting
    (check-expect (eval-program '(or 1 (/ 1 0))) T)
    (check-expect (eval-program '(and nil (/ 1 0))) nil)

    ; and and or (more than 2 args)
    (check-expect (eval-program '(and 1 2)) T)
    (check-expect (eval-program '(or 1 2)) T)
    (check-expect (eval-program '(and 1 2 3)) T)
    (check-expect (eval-program '(or 1 2 3)) T)
    (check-expect (eval-program '(and nil 2 3 4)) nil)
    (check-expect (eval-program '(or 0 2 3 4)) T)
    (check-expect (eval-program '(or 0 0 0 0)) T)


  ; Define your tests here using check-expect
  (check-expect (eval-program '((lambda (a b) (+ a (+ a b))) (+ 8 2) (+ 9 7))) 36)
  (check-expect (eval-program  '((lambda (a b) (a b)) (lambda (c) (+ c c)) (+ 3 4))) 14)
  (check-expect (eval-program '(or 1 (/ 1 0))) T)
  
  ;; Report results
  (report-test-results))

(run-tests)