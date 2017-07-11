;;; Config --- My emacs config
;;; Commentary:
;;; Code:
;; load package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;;; Load path: we want to have ~/.home/emacs in there
(add-to-list 'load-path "~/.emacs.config")

(defun ensure-package-installed (&rest packages)
  "Assure every PACKAGES is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
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
 'zenburn-theme
 'nlinum-relative
 'evil-leader
 'helm
 'rtags
 'ycmd
 'company-ycmd
 'flycheck-ycmd
 'helm-rtags
 'flycheck-rtags
 'company
 'fiplr
 'neotree
 'hydra
 'rainbow-identifiers
 'projectile
 'helm-projectile
 'rainbow-delimiters
 'company-jedi
 'cargo
 'flycheck
 'flycheck-rust
 'racer
 'rust-mode
 'rustfmt
 'auctex
 'company-auctex
 'slime
 'slime-company
 'paredit
 'back-button
 'cmake-mode
 'cmake-font-lock
 'csharp-mode
 'clang-format
 'magit
 'magit-svn
 'evil-magit
 'tide
 'use-package
 'exec-path-from-shell
 'rpm-spec-mode
 'multiple-cursors
 'mc-extras
 )

(require 'org)
(org-babel-load-file "~/.emacs.config/settings.org")



;;; Mouse support in terminal
(xterm-mouse-mode)
(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line
;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; Ctrl-P for non-evil mode



;;; Rust



;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(custom-safe-themes
;   (quote
;    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
; '(show-paren-mode t))


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

;;; PATH
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary "en_US")
 '(package-selected-packages
   (quote
    (mc-extras multiple-cursors company-ycmd flycheck-ycmd ycmd nlinum-relative company-rtags flycheck-rtags helm-rtags rpm-spec-mode omnisharp meghanada exec-path-from-shell use-package spacemacs-theme yaml-mode dockerfile-mode docker sr-speedbar tide zenburn-theme smart-mode-line slime-company rustfmt rtags relative-line-numbers realgud rainbow-identifiers rainbow-delimiters racer powerline paredit org-jira neotree markdown-mode magit-svn labburn-theme hydra helm-projectile flycheck-rust fiplr evil-surround evil-magit evil-leader evil-escape csharp-mode company-jedi company-auctex cmake-font-lock clang-format cargo back-button auctex-latexmk)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dadaca" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Meslo LG L for Powerline")))))
