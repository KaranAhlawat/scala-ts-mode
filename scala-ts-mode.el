;;; scala-ts-mode.el --- Scala Tree-Sitter Mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Treesitter based Scala major mode for Emacs (based on the new treesit lib)
;;; Code:
(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")

(defvar scala-ts-mode--keywords
  '("package" "val" "def")
  "Keywords for `scala-ts-mode'.")

(defvar scala-ts-mode--keywords-conditionals
  '("if" "then" "else" "case")
  "Conditionals for `scala-ts-mode'.")

(defvar scala-ts-mode--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'scala
   :feature 'keyword
   `([,@scala-ts-mode--keywords] @font-lock-keyword-face)

   :language 'scala
   :feature 'conditional
   `([,@scala-ts-mode--keywords-conditionals] @font-lock-keyword-face)

   :language 'scala
   :feature 'literal
   '((boolean_literal) @font-lock-builtin-face
     (integer_literal) @font-lock-number-face
     (floating_point_literal) @font-lock-number-face
     [
      (symbol_literal)
      (string)
      (character_literal)
      (interpolated_string_expression)
     ] @font-lock-string-face))
  "Treesitter font-lock settings for `scala-ts-mode'.")

;;;###autoload
(define-derived-mode scala-ts-mode prog-mode " Scala (TS)"
  "Major mode for Scala files using tree-sitter."
  :group 'scala-ts

  (when (treesit-ready-p 'scala)
    (treesit-parser-create 'scala)

    (setq-local treesit-font-lock-settings scala-ts-mode--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list '((keyword conditional literal)))

    (treesit-major-mode-setup)))

(provide 'scala-ts-mode)
;;; scala-ts-mode.el ends here
