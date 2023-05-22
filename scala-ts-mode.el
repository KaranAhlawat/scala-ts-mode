;;; scala-ts-mode.el --- Scala Tree-Sitter Mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karan Ahlawat

;; Author: Karan Ahlawat <ahlawatkaran12@gmail.com>
;; Keywords: scala, languages, tree-sitter

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(defvar scala-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?'  "/" table)
    (modify-syntax-entry ?/ "< 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?/ ". 14b")
    (modify-syntax-entry ?* ". 23b")
    table)
  "Syntax table for `scala-ts-mode'.")

(defvar scala-ts-mode--keywords
  '("case"
    "class"
    "enum"
    "extends"
    "derives"
    "finally"
    "object"
    "override"
    "package"
    "trait"
    "type"
    "val"
    "var"
    "with"
    "given"
    "using"
    "end"
    "implicit"
    "extension"
    "with"
    "def"
    "import"
    "export")
  "Keywords for `scala-ts-mode'.")

(defvar scala-ts-mode--keywords-type-qualifiers
  '("abstract"
    "final"
    "lazy"
    "sealed"
    "private"
    "protected")
  "Type qualifiers for `scala-ts-mode'.")

(defvar scala-ts-mode--keywords-control
  '("if"
    "then"
    "else"
    "match"
    "do"
    "for"
    "yield"
    "while"
    "try"
    "catch"
    "throw"
    "return")
  "Control flow for `scala-ts-mode'.")

(defvar scala-ts-mode--brackets
  '("(" ")" "[" "]" "{" "}")
  "Brackets for `scala-ts-mode'.")

(defvar scala-ts-mode--delimiters
  '("." ",")
  "Delimiters for `scala-ts-mode'.")

(defvar scala-ts-mode--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'scala
   :feature 'keyword
   `([,@scala-ts-mode--keywords] @font-lock-keyword-face
     [,@scala-ts-mode--keywords-type-qualifiers] @font-lock-keyword-face
     (opaque_modifier) @font-lock-keyword-face
     (infix_modifier) @font-lock-keyword-face
     (transparent_modifier) @font-lock-keyword-face
     (open_modifier) @font-lock-keyword-face
     (inline_modifier) @font-lock-keywordface
     (null_literal) @font-lock-constant-face
     (wildcard) @font-lock-builtin-face
     (annotation) @font-lock-preprocessor-face
     "new" @font-lock-keyword-face
     [,@scala-ts-mode--keywords-control] @font-lock-keyword-face
     ;; `case' is handled specially here, to limit it into a context
     (case_block
      (case_clause ("case") @font-lock-keyword-face)))

   :language 'scala
   :feature 'extra
   `([,@scala-ts-mode--brackets] @font-lock-bracket-face
     [,@scala-ts-mode--delimiters] @font-lock-delimiter-face)
   
   :language 'scala
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'scala
   :feature 'doc-comment
   :override t
   `((((comment) @font-lock-doc-face)
      (:match ,(rx-to-string '( : bol "/*"
                                (* (| (not "*")
                                      (: "*" (not "/"))))
                                (+ "*") "/")
                             t)
              @font-lock-doc-face)))

   :language 'scala
   :feature 'definition
   '((class_definition
      name: (identifier) @font-lock-type-face)
     (enum_definition
      name: (identifier) @font-lock-type-face)
     (object_definition
      name: (identifier) @font-lock-type-face)
     (trait_definition
      name: (identifier) @font-lock-type-face)
     (full_enum_case
      name: (identifier) @font-lock-type-face)
     (simple_enum_case
      name: (identifier) @font-lock-type-face))

   :language 'scala
   :feature 'variable
   '((class_parameter
      name: (identifier) @font-lock-variable-name-face)
     (val_definition
      pattern: (identifier) @font-lock-variable-name-face)
     (var_definition
      pattern: (identifier) @font-lock-variable-name-face)
     (val_declaration
      name: (identifier) @font-lock-variable-name-face)
     (var_declaration
      name: (identifier) @font-lock-variable-name-face)
     ;; expressions
     (field_expression field: (identifier) @font-lock-property-use-face)
     ;; this and super
     (((identifier) @font-lock-builtin-face)
      (:match "^this$" @font-lock-builtin-face))
     (((identifier) @font-lock-builtin-face)
      (:match "^super$" @font-lock-builtin-face)))

   :language 'scala
   :feature 'type
   '((type_identifier) @font-lock-type-face
     ;; expressions
     ((field_expression
       value: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     (((identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face)))

   :language 'scala
   :feature 'function
   '(;; method definitions
     (function_declaration
      name: (identifier) @font-lock-function-name-face)
     (function_definition
      name: (identifier) @font-lock-function-name-face)

     ;; method invocations
     (call_expression
      function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (operator_identifier) @font-lock-function-call-face)
     (call_expression
      function: (field_expression
                 field: (identifier) @font-lock-function-call-face))
     ((call_expression
       function: (identifier) @font-lock-function-call-face)
      (:match "^[A-Z]" @font-lock-function-call-face))
     (generic_function
      function: (identifier) @font-lock-function-call-face)
     (interpolated_string_expression
      interpolator: (identifier) @font-lock-function-call-face)

     ;; function definitions
     (function_definition
      name: (identifier) @font-lock-function-name-face)
     (parameter
      name: (identifier) @font-lock-variable-name-face)
     (binding
      name: (identifier) @font-lock-variable-name-face))

   :language 'scala
   :feature 'import
   '((import_declaration
      path: (identifier) @font-lock-type-face)
     ((stable_identifier (identifier) @default))
     ((import_declaration
       name: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((stable_identifier (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     (export_declaration
      path: (identifier) @default)
     ((stable_identifier (identifier) @default))
     ((export_declaration
       path: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((stable_identifier (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((namespace_selectors (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face)))

   :language 'scala
   :feature 'operator
   '((infix_expression operator: (identifier) @font-lock-operator-face)
     (infix_expression operator: (operator_identifier) @font-lock-operator-face)
     (infix_type operator: (operator_identifier) @font-lock-operator-face)
     (infix_type operator: (operator_identifier) @font-lock-operator-face)
     (operator_identifier) @font-lock-operator-face
     ["=>" "<-" "@"] @font-lock-operator-face)

   :language 'scala
   :feature 'literal
   '((boolean_literal) @font-lock-constant-face
     (integer_literal) @font-lock-number-face
     (floating_point_literal) @font-lock-number-face
     [
      (symbol_literal)
      (string)
      (character_literal)
      (interpolated_string)
      (interpolated_string_expression) ; TODO is this redundant?
      ] @font-lock-string-face
     (interpolation) @font-lock-string-face
     (interpolation "$" @font-lock-string-face)))
  "Treesitter font-lock settings for `scala-ts-mode'.")

(defun scala-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no nameor if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "class_definition"
         "trait_definition"
         "enum_definition"
         "object_definition"
         "full_enum_case"
         "simple_enum_case"
         "function_definition")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name"))
     t)))

;;;###autoload
(define-derived-mode scala-ts-mode prog-mode " Scala (TS)"
  "Major mode for Scala files using tree-sitter."
  :group 'scala-ts
  :syntax-table scala-ts-mode--syntax-table

  (when (treesit-ready-p 'scala)
    (treesit-parser-create 'scala)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    (setq-local treesit-font-lock-settings scala-ts-mode--treesit-font-lock-settings)
    ;; TODO Split this into levels to respect user choices
    (setq-local treesit-font-lock-feature-list '((comment
                                                  doc-comment
                                                  definition
                                                  keyword
                                                  type
                                                  variable
                                                  import
                                                  function
                                                  operator
                                                  literal
                                                  extra)))

    (setq-local treesit-defun-name-function #'scala-ts-mode--defun-name)
    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Class" "\\`class_definition\\'" nil nil)
                  ("Trait" "\\`trait_definition\\'" nil nil)
                  ("Enum" "\\`enum_definition\\'"' nil nil)
                  ("Object" "\\`object_definition\\'" nil nil)
                  ("Function" "\\`function_definition\\'" nil nil)))
    
    (treesit-major-mode-setup)

    (add-to-list 'auto-mode-alist '("\\.sc\\(ala\\)?\\'" . scala-ts-mode))))

(provide 'scala-ts-mode)
;;; scala-ts-mode.el ends here
