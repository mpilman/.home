(defconst rtags-packages
  '(rtags ivy-rtags flycheck-rtags))

(defun rtags/init-rtags ()
  (use-package rtags))

(defun rtags/init-ivy-rtags ()
  (when (require 'ivy nil 'noerror)
    (use-package ivy-rtags)
    (setq rtags-display-result-backend 'ivy)))

(defun rtags/init-flycheck-rtags ()
  (when (require 'flycheck nil 'noerror)
    (require 'flycheck-rtags)
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil))
    (add-hook 'c-mode #'setup-flycheck-rtags)
    (add-hook 'c++-mode #'setup-flycheck-rtags)
    (add-hook 'objc-mode #'setup-flycheck-rtags)
    ))
