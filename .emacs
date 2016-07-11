;; load package manager
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

;;; Visible-bell also deactivites the normal bell which is annoying
(setq visible-bell t)

;;; Store backup files in tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make sure emacs does not create new splits all the time
(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

;;; Make sure archive descriptions are downloaded
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)

(ensure-package-installed
 'evil
 'evil-surround
 'evil-escape
 'zenburn-theme
 'relative-line-numbers
 'evil-leader
 'helm
 'rtags
 'company
 'fiplr
 'powerline
 'neotree
 'hydra
 'rainbow-identifiers
 'projectile
 'helm-projectile
 'rainbow-delimiters
 'company-jedi
 'back-button
 'paredit
 )

;;; Enable paredit whenever a lisp-file is opened
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; Add back button to emacs
(require 'back-button)
(back-button-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq company-global-modes '(not gud-mode))
(add-hook 'after-init-hook 'global-company-mode)

;;; Python
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(require 'projectile)
(projectile-global-mode)

(require 'powerline)
(powerline-default-theme)

(require 'rtags)
(require 'company-rtags)

;;; rtags configuration
(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
(setq rtags-use-helm t)

;;; Load .h-files in C++-mode by default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; Autocompletion for lisp
(setq tab-always-indent 'complete)
;;; Syntax highlighting
(setq font-lock-maximum-decoration t)
;;; more fancy syntax highlighting
;(add-hook 'after-init-hook 'global-color-identifiers-mode)
;(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(load-theme 'zenburn t)

;;; Mouse support in terminal
(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(defvar evil-enabled nil)

(require 'evil-leader)
(require 'evil)
(require 'evil-surround)

(defun toggle-evil ()
 (interactive)
 (if evil-enabled
     (progn
       (evil-escape-mode 0)
       (global-evil-leader-mode)
       (global-evil-surround-mode 0)
       (evil-mode 0)
       (setq evil-enabled nil))
   (progn
     (global-evil-leader-mode)
     (global-evil-surround-mode 1)
     (evil-mode 1)
     (evil-escape-mode)
     (setq evil-enabled t))))

(toggle-evil)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching)

(require 'helm-config)

(require 'fiplr)
(setq fiplr-root-markers '("Makefile"))

;;; key bindings
(setq-default evil-escape-key-sequence "jk")
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'helm-buffers-list
  "d" 'kill-buffer
  "g" 'rtags-find-symbol-at-point
  "s" 'rtags-find-symbol
  "m" 'back-button-push-mark-local-and-global
  "h" 'back-button-global-backward
  "l" 'back-button-global-forward
  "p" 'point-to-register
  "j" 'jump-to-register)

(define-key evil-normal-state-map "L" "$")
(define-key evil-normal-state-map "H" "^")
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)
(define-key evil-normal-state-map "\\" 'next-buffer)
(define-key evil-normal-state-map "|" 'previous-buffer)
(define-key evil-normal-state-map "gt" 'next-frame)

;;; Ctrl-P for non-evil mode
(global-set-key (kbd "C-p") 'fiplr-find-file)

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

;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(custom-safe-themes
;   (quote
;    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
; '(show-paren-mode t))
;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dadaca" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
; '(linum ((t (:background "#3f3f3f" :foreground "#636363" :height 1.0)))))

(defun create-tags (dir-name)
     "Create tags file."
     (interactive "DDirectory: ")
     (eshell-command 
      (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

(defhydra hydra-rtags-menu (:color pink
				    :hint nil)
    "
^Action^
^^^^^^^^
_rs_: find references for symbol
_rp_: find references at point
_d_: run diagnostics
_gs_: goto symbol...
_gf_: goto file...
_f_: fixit
_m_: rtags-menu
_i_: print symbol info
_p_: preprocess file
_t_: print type under cursor
"
    ("rp" rtags-find-references-at-point :exit t)
    ("rs" rtags-find-references :exit t)
    ("gs" rtags-find-symbol :exit t)
    ("d" rtags-diagnostics :exit t)
    ("gf" rtags-find-file :exit t)
    ("f" rtags-fixit :exit t)
    ("m" rtags-imenu :exit t)
    ("i" rtags-print-symbol-info :exit t)
    ("p" rtags-preprocess-file :exit t)
    ("t" rtags-symbol-type :exit t)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

(defhydra hydra-space-menu (:color pink
				    :hint nil)
    "
^Action^
^^^^^^^^
_r_: rtags...
_d_: Don't show dos-endings for dos-unix mixed files
_lc_: reload config
_ec_: edit .emacs file
_gi_: guess current indentation
"
    ("r" (hydra-rtags-menu/body) :exit t)
    ("lc" (load-file "~/.emacs") :exit t)
    ("ec" (find-file "~/.home/.emacs") :exit t)
    ("gi" (c-guess) :exit t)
    ("d" (remove-dos-eol) :exit t)
    ("c" nil "cancel")
    ("q" quit-window "quit" :color blue))

(define-key evil-normal-state-map (kbd "SPC") 'hydra-space-menu/body)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gdb-non-stop-setting nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Meslo LG L for Powerline" :foundry "bitstream" :slant normal :weight normal :height 113 :width normal)))))

;;; Set path to undodb
(setenv "PATH" (concat (getenv "PATH") ":~/undodb"))
(setq exec-path (append exec-path '("~/undodb")))

;;; This seems to fix a nasty GUD-bug
(defun gdb-setup-windows ()
  "Layout the window pattern for option `gdb-many-windows'."
  (interactive)
  (gdb-get-buffer-create 'gdb-locals-buffer)
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (set-window-dedicated-p (selected-window) t)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil ( / ( * (window-height) 3) 4)))
        (win2 (split-window nil ( / (window-height) 3)))
        (win3 (split-window-right)))
    (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
    (select-window win2)
    (set-window-buffer
     win2
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win4 (split-window-right)))
      (gdb-set-window-buffer
       (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
    (select-window win1)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (let ((win5 (split-window-right)))
      (gdb-set-window-buffer (if gdb-show-threads-by-default
                                 (gdb-threads-buffer-name)
                               (gdb-breakpoints-buffer-name))
                             nil win5))
    (select-window win0)))
