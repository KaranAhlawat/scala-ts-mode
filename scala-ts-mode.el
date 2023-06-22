;;; scala-ts-mode.el --- Scala Tree-Sitter Mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karan Ahlawat

;; Author: Karan Ahlawat <ahlawatkaran12@gmail.com>
;; Version: 0.0.1
;; Filename: scala-ts-mode.el
;; Package-Requires: ((emacs "29.0.91"))
;; Keywords: scala, languages, tree-sitter
;; URL: https://github.com/KaranAhlawat/scala-ts-mode

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

;; This package provides a tree-sitter based major mode for the Scala
;; programming language.  Currently, the supported features and their
;; statuses are
;; 1. font-locking (complete, looking for bugs and maintainance)
;; 2. imenu (basic support, needs work)
;; 3. indentation (initial support, incomplete, broken)

;;; Code:

(require 'rx)
(require 'treesit)
(require 'thingatpt)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-parent-while "treesit.c")
(declare-function treesit-parent-until "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")

(defcustom scala-ts-indent-offset 2
  "Number of spaces for each indentation in `scala-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'scala-ts)

(defvar scala-ts--syntax-table
  (let ((table (make-syntax-table)))
    ;; "." denotes punctuation
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?/ "." table)
    ;; Just reinforcing some defaults
    (modify-syntax-entry ?\\ "\\" table) ; Escape seq start
    (modify-syntax-entry ?\" "\"" table) ; String start
    (modify-syntax-entry ?'  "/" table)  ; Char start
    (modify-syntax-entry ?/ "< 12" table) ; Comment seq a, first two char are /
    (modify-syntax-entry ?\n ">" table) ; Comment seq a, ends with a newline
    (modify-syntax-entry ?/ ". 14b") ; Comment seq b, starts and ends with /
    (modify-syntax-entry ?* ". 23b") ; Comment seq b, second start and first end char is *
    table)
  "Syntax table for `scala-ts-mode'.")


;; utility functions -- begin

(defun scala-ts--node-type= (type node)
  "Compare TYPE and type of NODE for string equality."
  (string= type (treesit-node-type node)))

;; utility functions -- end

;; Keywords
(defvar scala-ts--keywords
  '("case" "class" "enum" "extends" "derives" "finally"
    "object" "override" "package" "trait" "type" "val"
    "var" "with" "given" "using" "end" "implicit"
    "extension" "with" "def" "import" "export" "new")
  "Keywords for `scala-ts-mode'.")

(defvar scala-ts--keywords-type-qualifiers
  '("abstract" "final" "lazy"
    "sealed" "private" "protected")
  "Type qualifiers for `scala-ts-mode'.")

(defvar scala-ts--keywords-control
  '("if" "then" "else" "match" "do" "for" "yield"
    "while" "try" "catch" "throw" "return")
  "Control flow for `scala-ts-mode'.")

(defvar scala-ts--brackets
  '("(" ")" "[" "]" "{" "}")
  "Brackets for `scala-ts-mode'.")

(defvar scala-ts--delimiters
  '("." "," ";")
  "Delimiters for `scala-ts-mode'.")

(defvar scala-ts--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'scala
   :feature 'keyword
   `([,@scala-ts--keywords ] @font-lock-keyword-face
     [,@scala-ts--keywords-type-qualifiers] @font-lock-keyword-face
     (opaque_modifier) @font-lock-keyword-face
     (infix_modifier) @font-lock-keyword-face
     (transparent_modifier) @font-lock-keyword-face
     (open_modifier) @font-lock-keyword-face
     (inline_modifier) @font-lock-keyword-face
     [,@scala-ts--keywords-control] @font-lock-keyword-face
     (null_literal) @font-lock-constant-face
     (wildcard) @font-lock-builtin-face
     (annotation) @font-lock-preprocessor-face
     ;; `case' is handled specially here, to limit it into a context
     (case_block
      (case_clause ("case") @font-lock-keyword-face)))

   :language 'scala
   :feature 'extra
   `([,@scala-ts--brackets] @font-lock-bracket-face
     [,@scala-ts--delimiters] @font-lock-delimiter-face)
   
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
   '(((import_declaration
       path: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     (stable_identifier (identifier) @default)
     ((stable_identifier (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     (export_declaration
      path: (identifier) @default)
     (stable_identifier (identifier) @default)
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
     ["=>" "<-" "@" (operator_identifier)] @font-lock-operator-face)

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
      (interpolated_string_expression)
      ] @font-lock-string-face
     (interpolation) @font-lock-string-face
     (interpolation "$" @font-lock-string-face)))
  "Treesitter font-lock settings for `scala-ts-mode'.")

(defun scala-ts--indent-end (node _parent _bol)
  "Return anchor position for end clause NODE."
  (let ((label (treesit-node-text (treesit-node-next-sibling node) t)))
    (cond
     ;; Check if end <identifier> is already correctly indented
     ((let ((next-sibling (treesit-node-next-sibling node))
            (prev-sibling (treesit-node-prev-sibling node)))
        (and (string=
              (treesit-node-type next-sibling)
              "_end_ident")
             (string-match-p
              "definition"
              (treesit-node-type prev-sibling))
             (string=
              label
              (treesit-node-text
               (treesit-node-child prev-sibling 0 t)))
             (treesit-node-start prev-sibling))))
     ;; indent end <identifier> correctly
     ((string= (treesit-node-type (treesit-node-next-sibling node))
               "_end_ident")
      (treesit-node-start (treesit-parent-until
                           node
                           (lambda (node)
                             (and (string-match-p
                                   "definition"
                                   (treesit-node-type node))
                                  (string= label
                                           (treesit-node-text
                                            (treesit-node-child node 0 t))))))))
     ;; indent end <everything eles> correctly
     (t
      (save-excursion
        (goto-char (treesit-node-start node))
        (re-search-backward label)
        (back-to-indentation)
        (point))))))


(defun scala-ts--indent-if (node parent _bol)
  "Return anchor position for NODE when PARENT is if_expression."
  (let ((offset scala-ts-indent-offset))
    (cond
     ((and (string= (treesit-node-type node)
                    "else")
           (string= (treesit-node-type (treesit-node-next-sibling node))
                    "if_expression"))
      (treesit-node-start parent))
     ;; else
     ((string= (treesit-node-type node)
               "else")
      (treesit-node-start (treesit-parent-while
                           parent
                           (lambda (cur-node)
                             (string= (treesit-node-type cur-node)
                                      "if_expression")))))
     ;; then
     ((string= (treesit-node-type node)
               "then")
      (treesit-node-start (treesit-parent-while
                           parent
                           (lambda (cur-node)
                             (string= (treesit-node-type cur-node)
                                      "if_expression")))))
     ;; indented and non
     ((string= (treesit-node-type node)
               "indented_block")
      (+ offset (treesit-node-start (treesit-parent-while
                                     parent
                                     (lambda (cur-node)
                                       (string= (treesit-node-type cur-node)
                                                "if_expression"))))))
     (t
      (+ offset (treesit-node-start (treesit-parent-while
                                     parent
                                     (lambda (cur-node)
                                       (string= (treesit-node-type cur-node)
                                                "if_expression")))))))))

(defun scala-ts--indent-no-node (_node _parent _bol)
  "Indent when the node at point is nil."
(defun scala-ts--indent-no-node (_node _parent bol)
  "Return anchor position when node is nil with BOL."
  (save-excursion
    (let* ((offset scala-ts-indent-offset)
           (pos (re-search-backward
                 (rx (not
                      (in control
                          space
                          blank)))))
           (last-node (treesit-node-at pos)))
      (pcase (treesit-node-text last-node t)
        ((rx (| "for"
                "yield"
                "if"
                "then"
                "else"
                "try"
                "catch"
                "finally")
             eol)
         (goto-char (treesit-node-start last-node))
         (backward-word)
         (if (looking-at-p "end")
             (point)
           (goto-char (treesit-node-start last-node))
           (+ offset (point))))
        
        ((rx (| ":"
                "="
                "{"
                "with")
             eol)
         (re-search-backward (rx (| ":"
                                    "="
                                    "{")
                                 eol)
                             (treesit-node-start last-node)
                             t)
         ;; (goto-char (treesit-node-end last-node))
         (back-to-indentation)
         (+ offset (point)))

        ("match"
         (goto-char (treesit-node-start last-node))
         (backward-word)
         (if (looking-at-p "end")
             (point)
           (+ offset (point))))
        
        ((rx "}" eol)
         (goto-char (treesit-node-start last-node))
         (back-to-indentation)
         (if (= (point)
                (treesit-node-start last-node))
             (treesit-node-start last-node)
           (- (point) offset))) ; essentially a dedent
        
        (_
         (goto-char (treesit-node-end last-node))
         (back-to-indentation)
         (point))))))

;; TODO fix and clean up
(defun scala-ts--indent-for (node parent _bol)
  "Indent NODE if it's PARENT is if_expression."
  (if (string= (treesit-node-type node)
               "ERROR")
      (treesit-node-start (treesit-node-parent parent))
    (let ((offset scala-ts-indent-offset)
          (prev-sibling (treesit-node-prev-sibling node)))
      (if prev-sibling
          (treesit-node-start prev-sibling)
        (+ offset (treesit-node-start (treesit-node-parent parent)))))))

(defvar scala-ts--indent-rules
  (let ((offset scala-ts-indent-offset))
    `((scala
       ((node-is "^comment$") no-indent 0)
       
       ((node-is "^}$") parent-bol 0)
       
       ((parent-is "^if_expression$") scala-ts--indent-if 0)
       
       ((n-p-gp "^enumerators$" "^for_expression$" nil) parent ,offset)
       ((n-p-gp "^ERROR$" "^enumerators$" nil) prev-line ,(- offset))
       ((n-p-gp "^yield$" "^for_expression$" nil) parent 0)
       
       ((n-p-gp "^catch$" "^try_expression$" nil) parent 0)
       
       ((node-is "^end$") scala-ts--indent-end 0)
       
       ;; Handle function annotations
       ((n-p-gp "^def$" "^function_definition$" nil) parent 0)

       
       ((parent-is "^ERROR$")
        (lambda (n p b)
          (message "Parent: %s" (treesit-node-text (treesit-node-child p 0)))
          b)
        0)
       
       ;; The beast, no-nodes (and ERROR nodes)
       (no-node scala-ts--indent-no-node 0)
       
       ((parent-is "^compilation_unit$") column-0 0)
       ((parent-is "^trait_definition$") parent-bol ,offset)
       ((parent-is "^function_definition$") parent-bol ,offset)
       ((parent-is "^object_definition$") parent-bol ,offset)
       ((parent-is "^class_definition$") parent-bol ,offset)
       ((parent-is "^enum_definition$") parent-bol ,offset)
       ((parent-is "^val_definition$") parent-bol ,offset)
       ((parent-is "^var_definition$") parent-bol ,offset)
       ((parent-is "^enum_body$") parent-bol ,offset)
       ((parent-is "^template_body$") parent-bol ,offset)
       
       ((node-is "^trait_definition$") prev-sibling 0)
       ((node-is "^function_definition$") prev-sibling 0)
       ((node-is "^object_definition$") prev-sibling 0)
       ((node-is "^class_definition$") prev-sibling 0)
       ((node-is "^enum_definition$") prev-sibling 0)
       ((node-is "^val_definition$") prev-sibling 0)
       ((node-is "^var_definition$") prev-sibling 0)
       ((node-is "^enum_body$") prev-sibling 0)
       ((node-is "^template_body$") prev-sibling 0)

       ((n-p-gp "^identifier$" "^indented_block$" nil)
        (lambda (n p _bol)
          (when (member (treesit-node-text n)
                        '("then" "catch" "else" "finally"))
            (- (treesit-node-start p) ,offset)))
        0)
       
       ((n-p-gp "^call_expression$" "^indented_block$" nil)
        (lambda (n p _bol)
          (when (member (treesit-node-text (treesit-node-child-by-field-name
                                            n
                                            "function"))
                        '("then" "catch" "else" "finally"))
            (- (treesit-node-start p) ,offset)))
        0)
       
       ((n-p-gp "^indented_block$" "^ERROR$" nil) no-indent 0)
       ((parent-is "^indented_block$") parent 0)
       ((node-is "^indented_block$") parent-bol ,offset)
       ((node-is "^block$") parent ,offset)
       
       ;; Uhhh I don't think this is very good :)
       )))
  "Tree-sitter indent rules for `scala-ts-mode'.")

(defun scala-ts--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "class_definition"
         "trait_definition"
         "enum_definition"
         "object_definition"
         "full_enum_case"
         "simple_enum_case"
         "function_definition")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name")
      t))
    ("function_declaration"
     (treesit-node-text
      (treesit-node-child node 0)
      t))))

;;;###autoload
(define-derived-mode scala-ts-mode prog-mode " Scala (TS)"
  "Major mode for Scala files using tree-sitter."
  :group 'scala-ts
  :syntax-table scala-ts--syntax-table

  (when (treesit-ready-p 'scala)
    (treesit-parser-create 'scala)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    (setq-local treesit-font-lock-settings scala-ts--treesit-font-lock-settings)
    ;; TODO Split this into levels to respect user choices
    (setq-local treesit-font-lock-feature-list '((comment doc-comment definition)
                                                 (keyword  type)
                                                 (import extra)
                                                 (variable function operator literal)))


    (setq-local
     treesit-simple-indent-rules scala-ts--indent-rules)

    (setq-local treesit-defun-name-function #'scala-ts--defun-name)
    ;; TODO (could possibly be more complex?)
    (setq-local treesit-simple-imenu-settings
                `(("Class" "\\`class_definition\\'" nil nil)
                  ("Trait" "\\`trait_definition\\'" nil nil)
                  ("Enum" "\\`enum_definition\\'"' nil nil)
                  ("Object" "\\`object_definition\\'" nil nil)
                  ("Function" "\\`function_definition\\'" nil nil)
                  ("Definition" "\\`function_declaration'" nil nil)))
    
    (treesit-major-mode-setup)))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-ts-mode)))

(provide 'scala-ts-mode)
;;; scala-ts-mode.el ends here
