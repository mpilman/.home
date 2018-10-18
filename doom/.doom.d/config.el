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
(setq display-line-numbers-type 'relative)

;;
;; Groovy
;;

(setq groovy-lang-jar "/Users/mpilman/.m2/repository/com/palantir/ls/groovy-language-server/0.5.5/groovy-language-server-0.5.5-all.jar")

;;
;; Helper functions
;;

(defun copy-filename-to-clipboard ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (file-name-nondirectory (buffer-file-name)))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun generate-file-identifier ()
  (interactive)
  (let ((val (random (expt 2 24))))
    (end-of-line)
    (newline-and-indent)
    (insert (format "constexpr static flat_buffers::FileIdentifier file_identifier = %d;" val))
    ))

(defun generate-file-identifier-for-fake-root ()
  (interactive)
  (let ((val (random (expt 2 24))))
    (save-excursion
      (beginning-of-line)
      (search-forward "serializer(")
      (replace-match "serialize_fake_root(")
      (search-forward ",")
      (replace-match (format ", %d," val))
      )
    ))

(defun insert-random-file-id ()
  (interactive)
  (insert (format "%d" (random (expt 2 24)))))

;;
;; C++
;;
(c-add-style
 "fdb"
 '("ellemtel"
   (c-basic-offset . 4)
   (c-offsets-alist
    (innamespace . -)
    )))
(setq c-default-style "fdb")

;;
;; Java
;;
(setq lsp-java--workspace-folders (list "/Users/mpilman/Snowflake/trunk"
                                        "/Users/mpilman/Projects/testjava"))

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
     :desc "Grep"   :n "g" #'counsel-projectile-rg)
   (:desc "buffer" :prefix "b"
     :desc "Copy Name" :n "c" #'copy-filename-to-clipboard))
 ; c/c++
 ; lsp
 (:mode (c-mode c++-mode)
   (:localleader
     :desc "Gen Fake-Root"  :n "f" #'generate-file-identifier-for-fake-root
     :desc "Gen FileId"     :n "i" #'generate-file-identifier))
 (:after lsp-ui :map lsp-ui-mode-map
   (:localleader
     :desc "Find Symbol"     :n "s" #'lsp-ui-peek-find-workspace-symbol
     :desc "Find References" :n "r" #'lsp-ui-peek-find-references
     :desc "Rename Symbol"   :n "R" #'lsp-rename
     :desc "Codeaction"      :n "a" #'lsp-ui-sideline-apply-code-actions
     :desc "Find definition" :n "g" #'xref-find-definitions))
 (:mode logview-mode
   (:localleader
     (:desc "Narrow" :prefix "n"
       :desc "Up to" :n "u" #'logview-narrow-up-to-this-entry
       :desc "From"  :n "f" #'logview-narrow-from-this-entry)
     (:desc "Level filtering" :prefix "l"
       :desc "Error"    :n "e" #'logview-show-only-errors
       :desc "Warnings" :n "w" #'logview-show-errors-and-warnings
       :desc "Info"     :n "i" #'logview-show-errors-warnings-and-information
       :desc "All"      :n "a" #'logview-show-all-levels)
     (:desc "General Filters" :prefix "f"
       :desc "Edit"         :n "e" #'logview-edit-filters
       :desc "Include Name" :n "n" #'logview-add-include-name-filter
       :desc "Exclude Name" :n "N" #'logview-add-exclude-name-filter
       :desc "Include Msg"  :n "m" #'logview-add-include-message-filter
       :desc "Exclude Msg"  :n "M" #'logview-add-exclude-message-filter
       :desc "Include Thd"  :n "t" #'logview-add-include-thread-filter
       :desc "Exclude Thd"  :n "T" #'logview-add-exclude-thread-filter)
     (:desc "Views" :prefix "v"
       :desc "Switch to"   :n "s" #'logview-switch-to-view
       :desc "Delete view" :n "x" #'logview-delete-view
       :desc "Save view"   :n "w" #'logview-save-filters-as-view-for-submode
       :desc "Edit views"  :n "e" #'logview-edit-submode-views
       :desc "Highlight"   :n "h" #'logview-highlight-view-entries
       :desc "Unhighlight" :n "H" #'logview-unhighlight-view-entries)
     :desc "Refresh"   :n "R" #'logview-refresh-buffer-as-needed
     :desc "Clear All" :n "C" #'+logview/clear-everything))
 )

; company
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

; cquery
(setq cquery-project-roots '("~/Projects/fdb/foundation/")
      cquery-cache-dir "~/.cquery-index")

; python
(setq python-shell-interpreter "/usr/local/bin/python3")

;;
;; LogView
;;
(defun +logview/clear-everything ()
  (interactive)
  (logview-reset-all-filters)
  (widen))

(add-to-list 'auto-mode-alist '("trace\\..*\\.json\\'" . logview-mode))
(custom-set-variables
 '(logview-additional-level-mappings
   (quote
    (("FDBLevels"
      (error "40")
      (warning "30")
      (information "20" "10")
      (debug "5")
      (trace)
      (aliases)))))
 '(logview-additional-submodes
   (quote
    (("FDB"
      (format . "{ \"Severity\": \"LEVEL\", \"Time\": \"IGNORED\", \"ActualTime\": \"TIMESTAMP\", \"Machine\": \"THREAD\", \"Type\": \"NAME\",")
      (levels . "FDBLevels")
      (timestamp)
      (aliases))))))

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
