;;; scala-ts-mode.el --- Scala Tree-Sitter Mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Treesitter based Scala major mode for Emacs (based on the new treesit lib)
;;; Code:
(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")

(defvar scala-ts--keywords
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

(defvar scala-ts--keywords-type-qualifiers
  '("abstract"
    "final"
    "lazy"
    "sealed"
    "private"
    "protected")
  "Type qualifiers for `scala-ts-mode'.")

(defvar scala-ts--keywords-control
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

(defvar scala-ts--brackets
  '("(" ")" "[" "]" "{" "}")
  "Brackets for `scala-ts-mode'.")

(defvar scala-ts--delimiters
  '("." ",")
  "Delimiters for `scala-ts-mode'.")

(defvar scala-ts--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'scala
   :feature 'keyword
   `([,@scala-ts--keywords] @font-lock-keyword-face
     [,@scala-ts--keywords-type-qualifiers] @font-lock-keyword-face
     (opaque_modifier) @font-lock-keyword-face
     (infix_modifier) @font-lock-keyword-face
     (transparent_modifier) @font-lock-keyword-face
     (open_modifier) @font-lock-keyword-face
     (inline_modifier) @font-lock-keywordface
     (null_literal) @font-lock-constant-face
     (wildcard) @font-lock-builtin-face
     (annotation) @font-lock-keyword-face
     "new" @font-lock-keyword-face
     [,@scala-ts--keywords-control] @font-lock-keyword-face
     ;; `case' is handled specially here, to limit it into a context
     (case_block
      (case_clause ("case") @font-lock-keyword-face)))

   :language 'scala
   :feature 'extra
   `([,@scala-ts--brackets] @font-lock-bracket-face
     [,@scala-ts--delimiters] @font-lock-delimiter-face)
   
   :language 'scala
   :feature 'comment
   '((comment) @font-lock-comment-face
     ((comment) @font-lock-type-face
      (:match "^/[*][*][^*].*[*]/$" @font-lock-type-face)))

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

;;;###autoload
(define-derived-mode scala-ts-mode prog-mode " Scala (TS)"
  "Major mode for Scala files using tree-sitter."
  :group 'scala-ts

  (when (treesit-ready-p 'scala)
    (treesit-parser-create 'scala)

    (setq-local treesit-font-lock-settings scala-ts--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list '((comment
                                                  definition
                                                  keyword
                                                  type
                                                  variable
                                                  import
                                                  function
                                                  operator
                                                  literal
                                                  extra)))
    (treesit-major-mode-setup)))

(provide 'scala-ts-mode)
;;; scala-ts-mode.el ends here
