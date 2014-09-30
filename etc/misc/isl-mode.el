;; 
;; isl-mode.el
;;
;;   Scott Hassan (hassan@cs.stanford.edu)
;;
;; Note: mostly stolen from c-mode and python-mode
;;
;; html://www-db.stanford.edu/~hassan/ILU/isl-mode.el
;;
;; place this few lines in your .emacs file.
;;
;;   (autoload 'isl-mode "isl-mode.el" "Hmmm isl..." t)
;;   (setq auto-mode-alist (cons '("\\.isl$" . isl-mode) auto-mode-alist)) ;;

(provide 'isl-mode)

(defvar isl-mode-hook nil
  "*Hook called by `isl-mode'.")

(defvar isl-mode-abbrev-table nil
  "Abbrev table in use in isl-mode buffers.")

(define-abbrev-table 'isl-mode-abbrev-table nil)

(defvar isl-mode-syntax-table nil
  "Syntax table in use in isl-mode buffers.")

;; stolen from pascal-mode.
(if isl-mode-syntax-table
    ()
  (setq isl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\"  isl-mode-syntax-table)
  (modify-syntax-entry ?( "()1"  isl-mode-syntax-table)  
  (modify-syntax-entry ?) ")(4"  isl-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" isl-mode-syntax-table)
  (modify-syntax-entry ?{ "<"    isl-mode-syntax-table)
  (modify-syntax-entry ?} ">"    isl-mode-syntax-table)
  (modify-syntax-entry ?+ "."    isl-mode-syntax-table)
  (modify-syntax-entry ?- "."    isl-mode-syntax-table)
  (modify-syntax-entry ?= "."    isl-mode-syntax-table)
  (modify-syntax-entry ?% "."    isl-mode-syntax-table)
  (modify-syntax-entry ?< "."    isl-mode-syntax-table)
  (modify-syntax-entry ?> "."    isl-mode-syntax-table)
  (modify-syntax-entry ?& "."    isl-mode-syntax-table)
  (modify-syntax-entry ?| "."    isl-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    isl-mode-syntax-table)
  (modify-syntax-entry ?\' "\""  isl-mode-syntax-table))


(defun isl-mode ()
  "Major mode for editing ILU isl files.
Do `\\[isl-describe-mode]' for detailed documentation.
Knows about ISL indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only."
  (interactive)
  (kill-all-local-variables)

  (setq  major-mode 'isl-mode  mode-name "ISL")

  (set-syntax-table isl-mode-syntax-table)

)


(defvar isl-font-lock-keywords
   (list
    (cons (concat
	   "[ \n\t(=:]\\("
	   (mapconcat
	    'identity
	    '(
"array" "asynchronous" "authentication" "boolean" "brand"
"cardinal" "character" "class " "collectible" "constant"
"default" "end" "enumeration" "exception" "false" "from"
"functional" "imports" "in" "inout" "integer" "interface"
"limit" "long" "methods" "object" "of" "optional" "others"
"out" "raises" "real" "record" "sequence" "short" "sibling"
"singleton" "sink" "source" "superclass" "superclasses"
"supertypes" "true" "union"
	      )
	    "\\|")
	   "\\)[ \n\t,=:(;]")
          1)
    ;; classes
   '("TYPE[ \t]+\\([a-zA-Z_]+[-a-zA-Z0-9_]*\\)"
     1 font-lock-type-face)
    )
  "*Additional expressions to highlight in ISL mode.")

(put 'isl-mode 'font-lock-keywords-case-fold-search t)




