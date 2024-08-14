(defvar *test-total-count* 0 "Tests run")
(defvar *test-pass-count* 0 "Tests passed")

(defun reset-test-counters ()
  "Reset the test counters."
  (setf *test-total-count* 0)
  (setf *test-pass-count* 0))

(defun report-test-results ()
  "Report the number of tests run and passed."
  (format t "~&Total tests run: ~D~%" *test-total-count*)
  (format t "Tests passed: ~D~%" *test-pass-count*)
  (format t "Tests failed: ~D~%" (- *test-total-count* *test-pass-count*)))

(defmacro check-expect (exp res)
  "Evaluate an expression and check it against the expected result."
  `(let ((result ,exp))
     (incf *test-total-count*)
     (if (equal result ,res)
         (incf *test-pass-count*)
         (format t "~&Test failed: ~S expected ~S but got ~S~%" ',exp ,res result))))