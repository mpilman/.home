(defconst irony-packages
  '(irony company-irony company-irony-c-headers))

(defun irony/init-irony ()
  (use-package irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(defun irony/init-company-irony ()
  (eval-after-load 'company
    '(progn
       (use-package company-irony)
       (add-to-list 'company-backends
                    '(company-irony-c-headers company-irony))
       )))
