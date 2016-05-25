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
 'evil-surround
 'evil-escape
 'labburn-theme
 'relative-line-numbers
 'evil-leader
 'helm
 'rtags
 'company
 'fiplr
 'powerline
 'neotree
 )

(require 'powerline)
(powerline-default-theme)

(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
(setq rtags-use-helm t)

;;; Autocompletion for lisp
(setq tab-always-indent 'complete)
;;; Syntax highlighting
(setq font-lock-maximum-decoration t)

(load-theme 'labburn t)

;;; Mouse support in terminal
(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'evil)
(evil-mode 1)
(evil-escape-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching)

(require 'helm-config)

(require 'fiplr)
(setq fiplr-root-markers '("Makefile"))

;;; key bindings
(setq-default evil-escape-key-sequence "jk")
(evil-leader/set-key
  "b" 'helm-buffers-list
  "d" 'kill-buffer
  "g" 'rtags-find-symbol-at-point
  "s" 'rtags-find-symbol)

(define-key evil-normal-state-map "L" "$")
(define-key evil-normal-state-map "H" "^")
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)
(define-key evil-normal-state-map "\\" 'next-buffer)
(define-key evil-normal-state-map "|" 'previous-buffer)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(semantic-mode t)

(require 'fiplr)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-map (kbd "SPC") 'neotree-enter)
	    (define-key evil-normal-state-map (kbd "RET") 'neotree-enter)
	    (define-key evil-normal-state-map (kbd "q") 'neotree-hide)))

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

