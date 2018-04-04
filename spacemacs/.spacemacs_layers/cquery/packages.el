(setq cquery-packages '(
                        (cquery :requires company-lsp)
                        ))


(defun cquery/init-cquery ()
  (when (require 'cquery nil 'noerror)
    (setq cquery-executable cquery-binary)
    (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
    (setq cquery-extra-init-params '(:index (:comments 2)
                                     :cacheFormat "msgpack"
                                     :completion (:detailedLabel t)))
    (spacemacs|add-company-backends
      :backends company-lsp
      :modes c-mode c++-mode
      )
    (add-hook 'c-mode-common-hook 'lsp-cquery-enable)
    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "s" 'lsp-ui-peek-find-workspace-symbol)
      (spacemacs/set-leader-keys-for-major-mode mode "r" 'lsp-ui-peek-find-references)
      )
    )
  )
