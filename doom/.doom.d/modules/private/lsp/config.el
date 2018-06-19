;;; config.el -*- lexical-binding: t; -*-
(defvar +cc-default-include-paths (list "include/")
  "A list of default paths, relative to a project root, to search for headers in
C/C++. Paths can be absolute. This is ignored if your project has a compilation
database.")

(defvar +cc-default-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++11" ; use C++11 by default
              (when IS-MAC
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                (list "-stdlib=libc++"))))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.")


;;
;; Plugins
;;

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode)
  :preface
  (defalias 'cpp-mode 'c++-mode)

  (defun +cc-c++-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (or (file-exists-p (expand-file-name
                             (concat (file-name-sans-extension buffer-file-name)
                                     ".cpp")))
             (when-let* ((file (car-safe (projectile-get-other-files
                                          buffer-file-name
                                          (projectile-current-project-files)))))
               (equal (file-name-extension file) "cpp")))))

  (defun +cc-objc-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))

  (push '(+cc-c++-header-file-p  . c++-mode)  magic-mode-alist)
  (push '(+cc-objc-header-file-p . objc-mode) magic-mode-alist)

  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "stroustrup")

  :config
  (set! :electric '(c-mode c++-mode objc-mode java-mode)
    :chars '(?\n ?\}))
  (set! :company-backend
    '(c-mode c++-mode objc-mode))

  ;;; Style/formatting
  ;; C/C++ style settings
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  ;;; Better fontification (also see `modern-cpp-font-lock')
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! (c-mode c++-mode) #'highlight-numbers-mode)
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)

  ;; Custom style, based off of linux
  (map-put c-style-alist "doom"
           `((c-basic-offset . ,tab-width)
             (c-comment-only-line-offset . 0)
             (c-hanging-braces-alist (brace-list-open)
                                     (brace-entry-open)
                                     (substatement-open after)
                                     (block-close . c-snug-do-while)
                                     (arglist-cont-nonempty))
             (c-cleanup-list brace-else-brace)
             (c-offsets-alist
              (statement-block-intro . +)
              (knr-argdecl-intro . 0)
              (substatement-open . 0)
              (substatement-label . 0)
              (statement-cont . +)
              (case-label . +)
              ;; align args with open brace OR don't indent at all (if open brace
              ;; is at eolp and close brace is after arg with no trailing comma)
              (arglist-intro . +)
              (arglist-close +cc-lineup-arglist-close 0)
              ;; don't over-indent lambda blocks
              (inline-open . 0)
              (inlambda . 0)
              ;; indent access keywords +1 level, and properties beneath them
              ;; another level
              (access-label . -)
              (inclass +cc-c++-lineup-inclass +)
              (label . 0))))

  ;;; Keybindings
  ;; Disable electric keys because it interferes with smartparens and custom
  ;; bindings. We'll do it ourselves (mostly).
  (setq c-tab-always-indent nil
        c-electric-flag nil)
  (dolist (key '("#" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
    (define-key c-mode-base-map key nil))
  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (map! :map c++-mode-map "<" nil ">" nil)

  ;; ...and leave it to smartparens
  (sp-with-modes '(c++-mode objc-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))


(def-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;;
;; Tools
;;

(def-package! disaster :commands disaster)

(def-package! clang-format
  :config
  (add-hook! 'before-save-hook '+cc-clang-format/format-on-save)
  )

(defun +cc-clang-format/format-on-save ()
  (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
    (clang-format-buffer)))

;;
;; Major modes
;;

(def-package! cmake-mode
  :defer t
  :config
  (set! :company-backend 'cmake-mode '(company-cmake company-yasnippet)))

(def-package! demangle-mode :hook llvm-mode)


;;
;; Company plugins
;;

(def-package! company-cmake
  :when (featurep! :completion company)
  :after cmake-mode)

(def-package! company-lsp
  :config
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends))

;;
;; LSP
;;

(def-package! lsp-mode
  :config
  (progn
    (setq lsp-enable-indentation nil)
    (when (featurep! +cc)
      (add-hook! (c-mode c++-mode) 'lsp-mode))
    (when (featurep! +bash)
      (lsp-define-stdio-client
       lsp-bash
       "bash"
       #'projectile-project-root
       '("bash-language-server" "start")
       )
      (add-hook! sh-mode 'lsp-bash-enable))
    (when (featurep! +python)
      (lsp-define-stdio-client
       lsp-python
       "python"
       #'projectile-project-root
       '("pyls"))
      (add-hook! python-mode 'lsp-python-enable))
    )
  )

(def-package! lsp-ui
  :config
  (progn
    (add-hook! (c-mode c++-mode) 'flycheck-mode)
    (add-hook! lsp-mode 'lsp-ui-mode)))

(def-package! cquery
  :config
  (progn
    (setq
     cquery-executable "/usr/local/bin/cquery"
     cquery-extra-args '("--log-file=/tmp/cq.log")
     cquery-extra-init-params '(:index (:comments 2)
                                       :cacheFormat "msgpack"
                                       :completion (:detailedLabel t)))
    (add-hook! (c-mode c++-mode) 'lsp-cquery-enable)))

(def-package! yasnippet
  :config
  (add-hook! (c-mode c++-mode) 'yas-minor-mode))
