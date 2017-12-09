(when (fboundp 'sourcetrail-send-location)
  (defun sourcetrail-hook ()
    (spacemacs/set-leader-keys-for-minor-mode 'sourcetrail-mode "t" 'sourcetrail-send-location))
  (add-hook 'sourcetrail-mode-hook 'sourcetrail-hook))
