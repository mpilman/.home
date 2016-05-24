;;; load package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package)
	 package)))
   packages))

;;; Make sure archive descriptions are downloaded
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)

(ensure-package-installed
 'evil
 'evil-escape
 'labburn-theme
 'relative-line-numbers
 'evil-leader
 'helm
 )

;;; Autocompletion for lisp
(setq tab-always-indent 'complete)
;;; Syntax highlighting
(setq font-lock-maximum-decoration t)

(load-theme 'labburn t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'evil)
(evil-mode 1)
(evil-escape-mode)

(require 'ido)
(ido-mode t)

(require 'helm-config)

;;; key bindings
(setq-default evil-escape-key-sequence "jk")
(evil-leader/set-key
  "b" 'helm-buffers-list
  "k" 'kill-buffer)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(semantic-mode t)

(require 'relative-line-numbers)
(global-relative-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dadaca" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(linum ((t (:background "#3f3f3f" :foreground "#636363" :height 1.0)))))

(defun create-tags (dir-name)
     "Create tags file."
     (interactive "DDirectory: ")
     (eshell-command 
      (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

