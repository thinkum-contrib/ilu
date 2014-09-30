;; 
;; idl-mode.el
;; 
;;   Scott Hassan (hassan@cs.stanford.edu)
;;
;; Note: mostly stolen from c-mode and python-mode
;;
;; html://www-db.stanford.edu/~hassan/ILU/idl-mode.el
;;
;; place this few lines in your .emacs file.
;;
;;   (autoload 'idl-mode "idl-mode.el" "Hmmm idl..." t)
;;   (setq auto-mode-alist (cons '("\\.idl$" . idl-mode) auto-mode-alist))
;;

(provide 'idl-mode)

(defvar idl-mode-hook nil
  "*Hook called by `idl-mode'.")

(defvar idl-mode-abbrev-table nil
  "Abbrev table in use in idl-mode buffers.")

(define-abbrev-table 'idl-mode-abbrev-table nil)

(defun idl-populate-syntax-table (table)
  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?_  "_"     table)
  (modify-syntax-entry ?\\ "\\"    table)
  (modify-syntax-entry ?+  "."     table)
  (modify-syntax-entry ?-  "."     table)
  (modify-syntax-entry ?=  "."     table)
  (modify-syntax-entry ?%  "."     table)
  (modify-syntax-entry ?<  "."     table)
  (modify-syntax-entry ?>  "."     table)
  (modify-syntax-entry ?&  "."     table)
  (modify-syntax-entry ?|  "."     table)
  (modify-syntax-entry ?\' "\""    table))

(defvar idl-mode-syntax-table nil
  "Syntax table used in idl-mode buffers.")
(if idl-mode-syntax-table
    ()
  (setq idl-mode-syntax-table (make-syntax-table))
  (idl-populate-syntax-table c-mode-syntax-table)
  ;; add extra comment syntax
  (modify-syntax-entry ?/  ". 14"  idl-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"  idl-mode-syntax-table))

(defun idl-mode ()
  "Major mode for editing CORBA idl files.
Do `\\[idl-describe-mode]' for detailed documentation.
Knows about IDL indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only."
  (interactive)
  (kill-all-local-variables)

  (setq  major-mode 'idl-mode mode-name "IDL")
  (set-syntax-table idl-mode-syntax-table)
)


(defvar idl-font-lock-keywords
(let ((prefixes "unsigned\\|short\\|long\\|const")
      (types (concat "int\\|long\\|char\\|float\\|double\\|void\\|struct\\|"
		     "union\\|enum\\|typedef"))
      (ctoken "\\(\\sw\\|\\s_\\|[:~*&]\\)+")
      )
   (list

    (cons (concat
	   "\\b\\("
	   (mapconcat
	    'identity
	    '(
"any" "attribute" "boolean" "case"
"char" "const" "context" "default" "double"
"enum" "exception" "FALSE" "float" "in" "inout"
"interface" "long " "module" "Object" "octet"
"oneway" "out" "raises" "readonly" "sequence"
"short" "string" "struct" "switch" "TRUE"
"typedef" "unsigned" "union" "void"
	      )
	    "\\|")
	   "\\)[ \n\t(]")
          1)
    (list (concat
	   "^\\(" ctoken "[ \t]+\\)?"	; type specs; there can be no
	   "\\(" ctoken "[ \t]+\\)?"	; more than 3 tokens, right?
	   "\\(" ctoken "[ \t]+\\)?"
	   "\\([*&]+[ \t]*\\)?"		; pointer
	   "\\(" ctoken "\\)[ \t]*(")	; name
	  8 'font-lock-function-name-face)

    (list (concat "^\\(typedef[ \t]+struct\\|struct\\|static[ \t]+struct\\)"
		  "[ \t]+\\(" ctoken "\\)[ \t]*\\(\{\\|$\\)")
	  2 'font-lock-function-name-face)

    )
)
  "*Additional expressions to highlight in IDL mode.")





