;;; newspeak-mode.el --- Major mode for the Newspeak programming language

(require 'font-lock)

(defvar newspeak-mode-syntax-table
  (let ((table (copy-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?_  "w"    table)  ; _ is part of a "word"
    (modify-syntax-entry ?:  "w"    table)  ; : is part of a "word"
    (modify-syntax-entry ?#  "w"    table)  ; # is part of a "word"
    (modify-syntax-entry ?-  "w"    table)  ; - is part of a "word"
    (modify-syntax-entry ?\( "()1n" table)  ; nestable, Pascal-style comments
    (modify-syntax-entry ?\) ")(4n" table)  ; nestable, Pascal-style comments
    (modify-syntax-entry ?\* ". 23" table)  ; nestable, Pascal-style comments
    (modify-syntax-entry ?\[ "(\]"  table)  ; blocks
    (modify-syntax-entry ?\] ")\["  table)  ; blocks
    (modify-syntax-entry ?{  "(}"   table)  ; tuples
    (modify-syntax-entry ?}  "){"   table)  ; tuples
    (modify-syntax-entry ?\' "\""   table)  ; strings
    (modify-syntax-entry ?\. "."    table)  ; statement separator
    (modify-syntax-entry ?\; "."    table)  ; cascades
    (modify-syntax-entry ?^  "."    table)  ; return
    table))

(defconst newspeak-mode-font-lock-keywords
  (let* ((reserved-word "\\<\\(class\\|outer\\|private\\|protected\\|public\\|self\\|super\\|transient\\)\\>")
         (class-name "\\<class \\([_A-Za-z][_A-Za-z0-9]*\\)\\>")
         (type "\\(\\^?<[][_A-Za-z0-9|`: ]+>\\)")
         (oddball "\\<\\(nil\\|true\\|false\\)\\>")
         (panic "\\<\\(panic\\)\\>")
         (number "\\<\\(-?[0-9]+\\(.[0-9]+\\)?\\)\\>")
         (extended-number "\\<\\(-?\\([0-9]+r\\)[0-9A-Z]+\\(.[0-9A-Z]+\\)?\\)\\>")
         (symbol "\\<\\(\\#[_A-Za-z][_A-Za-z0-9:]*\\)\\>")
         keywords)
    (setq keywords
          `((,reserved-word 1 (symbol-value 'font-lock-keyword-face))
            (,class-name 1 (symbol-value 'font-lock-type-face))
            (,type 1 (symbol-value 'font-lock-comment-face))
            (,oddball 1 (symbol-value 'font-lock-constant-face))
            (,panic 1 (symbol-value 'font-lock-warning-face))
            (,number 1 (symbol-value 'font-lock-constant-face))
            (,extended-number 1 (symbol-value 'font-lock-constant-face))
            (,symbol 1 (symbol-value 'font-lock-constant-face))
            keywords))))

(defun insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(define-derived-mode newspeak-mode prog-mode "Newspeak"
  "A major mode for editing Newspeak (newspeaklanguage.org)."
  (show-paren-mode t)
  (setq show-trailing-whitespace t)
  (setq tab-width 5)
  (local-set-key (kbd "TAB") 'insert-tab-char)
  ;(standard-display-ascii ?\t "»\t")
  (setq font-lock-defaults '(newspeak-mode-font-lock-keywords))
  (set-syntax-table newspeak-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.ns\\'" . newspeak-mode))

(provide 'newspeak-mode)
