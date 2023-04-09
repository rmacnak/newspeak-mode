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
  `(("class \\([_A-Za-z][_A-Za-z0-9]*\\)" 1 (symbol-value 'font-lock-type-face))
    ("outer \\([_A-Za-z][_A-Za-z0-9]*\\)" 1 (symbol-value 'font-lock-type-face))
    ("\\^?<[][_A-Za-z0-9|`: ,]+>" 0 (symbol-value 'font-lock-comment-face))
    ("\\<-?\\([0-9]+r\\)[0-9A-Z]+\\(.[0-9A-Z]+\\)?\\>" 0 (symbol-value 'font-lock-constant-face))
    ("\\<-?[0-9]+\\(.[0-9]+\\)?\\>" 0 (symbol-value 'font-lock-constant-face))
    ("#\\([_A-Za-z][_A-Za-z0-9]*\\:\\)+" 0 (symbol-value 'font-lock-constant-face))
    ("#[_A-Za-z][_A-Za-z0-9]*" 0 (symbol-value 'font-lock-constant-face))
    ("#[!%&*+/,<=>?@\\|~-]+" 0 (symbol-value 'font-lock-constant-face))
    ("\\<\\(nil\\|false\\|true\\)\\>" 0 (symbol-value 'font-lock-constant-face))
    ("\\<\\(panic\\)\\>" 0 (symbol-value 'font-lock-warning-face))
    ("\\<\\(class\\|outer\\|private\\|protected\\|public\\|self\\|super\\|transient\\)\\>" 0 (symbol-value 'font-lock-keyword-face))
    ))

(define-derived-mode newspeak-mode prog-mode "Newspeak"
  "A major mode for editing Newspeak (newspeaklanguage.org)."
  (show-paren-mode t)
  (setq show-trailing-whitespace t)
  (setq tab-width 5)
  (local-set-key (kbd "TAB") 'self-insert-command)
  (electric-indent-local-mode -1)
  ;(standard-display-ascii ?\t "»\t")
  (setq font-lock-defaults '(newspeak-mode-font-lock-keywords))
  (set-syntax-table newspeak-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.ns\\'" . newspeak-mode))

(provide 'newspeak-mode)
