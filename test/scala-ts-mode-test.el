;;; scala-ts-mode-test.el --- Tests for scala-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for scala-ts-mode.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'scala-ts-mode)

(defun scala-ts-test--rule-offset (matcher)
  "Return the offset for the indentation rule matching MATCHER."
  (nth 2
       (cl-find-if (lambda (rule)
                     (equal (car rule) matcher))
                   (cdr (assq 'scala scala-ts--indent-rules)))))

(defun scala-ts-test--contains-nonzero-integer-p (form)
  "Return non-nil when FORM contains a non-zero integer."
  (cond
   ((integerp form) (not (zerop form)))
   ((consp form)
    (or (scala-ts-test--contains-nonzero-integer-p (car form))
        (scala-ts-test--contains-nonzero-integer-p (cdr form))))))

(ert-deftest scala-ts-indent-rules-do-not-capture-custom-offset ()
  "Non-zero indentation rules should resolve their offset lazily."
  (dolist (rule (cdr (assq 'scala scala-ts--indent-rules)))
    (should-not
     (scala-ts-test--contains-nonzero-integer-p (nth 2 rule)))))

(ert-deftest scala-ts-indent-rules-respect-buffer-local-offset ()
  "Indentation offsets should use the current buffer-local setting."
  (let ((positive-offset
         (scala-ts-test--rule-offset '(parent-is "^template_body$")))
        (negative-offset
         (scala-ts-test--rule-offset
          '(n-p-gp "^ERROR$" "^enumerators$" nil))))
    (with-temp-buffer
      (setq-local scala-ts-indent-offset 4)
      (should (= (symbol-value positive-offset) 4))
      (should (= (funcall negative-offset nil nil nil) -4)))))

(provide 'scala-ts-mode-test)
;;; scala-ts-mode-test.el ends here
