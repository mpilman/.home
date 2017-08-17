(defconst rtags-packages
  '(rtags ivy-rtags))

(defun rtags/init-rtags ()
  (use-package rtags))

(defun rtags/init-ivy-rtags ()
  (when (require 'ivy nil 'noerror)
    (use-package ivy-rtags)
    (setq rtags-display-result-backend 'ivy)))
