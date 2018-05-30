;;; config.el -*- lexical-binding: t; -*-

; local leader
(setq doom-localleader-key ",")

; font
(setq doom-font
      (font-spec
       :family "Source Code Pro"
       :size 15))

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-frameg)
      (add-hook 'kill-emacs-hook 'save-frameg)))

; enable menu-bar on OS X. There's no reason not to
(when (string-equal system-type "darwin")
  (progn
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (when (window-system)
      (menu-bar-mode))
    ; ls on OS X does not support dired
    (setq dired-use-ls-dired nil)
    ))

; general settings
(setq doom-line-numbers-style 'relative)

;;
;; Key bindings
;;
(map!
 ; -- General key bindings in normal mode
 :nv "L" #'evil-end-of-line
 :nv "H" #'evil-first-non-blank
 :i [A-backspace] #'backward-kill-word
 ; search
 (:leader
   (:desc "Search" :prefix "s"
     :desc "Clear"  :n "c" #'evil-ex-nohighlight
     :desc "Search" :n "s" #'swiper
     :desc "Grep"   :n "g" #'counsel-projectile-rg))
 ; lsp
 (:after lsp-ui :map lsp-ui-mode-map
   (:localleader
     :desc "Find Symbol"     :n "s" #'lsp-ui-peek-find-workspace-symbol
     :desc "Find References" :n "r" #'lsp-ui-peek-find-references
     :desc "Codeaction"      :n "a" #'lsp-ui-sideline-apply-code-actions
     :desc "Find definition" :n "g" #'xref-find-definitions))
 )

; company
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

; cquery
(setq cquery-project-roots '("~/Projects/fdb/foundation/")
      cquery-cache-dir "~/.cquery-index")

;;
;; Helper functions
;;

; restore frame size
(defun save-frameg ()
  "Gets the current frame's geometry and saves to ~/.emacs.frameg."
  (let ((frameg-left (frame-parameter (selected-frame) 'left))
        (frameg-top (frame-parameter (selected-frame) 'top))
        (frameg-width (frame-parameter (selected-frame) 'width))
        (frameg-height (frame-parameter (selected-frame) 'height))
        (frameg-file (expand-file-name "~/.emacs.frameg")))
    (with-temp-buffer
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
       ";;; This file stores the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       " '("
       (format " (top . %d)\n" (max frameg-top 0))
       (format " (left . %d)\n" (max frameg-left 0))
       (format " (width . %d)\n" (max frameg-width 0))
       (format " (height . %d)))\n" (max frameg-height 0)))
      (when (file-writable-p frameg-file)
        (write-file frameg-file)))))

(defun load-frameg ()
  "Loads ~/.emacs.frameg which should load the previous frame's geometry."
  (let ((frameg-file (expand-file-name "~/.emacs.frameg")))
    (when (file-readable-p frameg-file)
      (load-file frameg-file))))
