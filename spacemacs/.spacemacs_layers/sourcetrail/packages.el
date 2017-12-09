(defconst sourcetrail-packages
  '(sourcetrail))

(defun sourcetrail/init-sourcetrail ()
  (use-package sourcetrail)
  (add-hook 'c-mode-common-hook 'sourcetrail-mode))
