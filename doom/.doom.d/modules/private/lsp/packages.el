;;; packages.el -*- lexical-binding: t; -*-

(depends-on! :completion company)
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! yasnippet)
(when (featurep! +cc)
  (package! cmake-mode)
  (package! cuda-mode)
  (package! demangle-mode)
  (package! disaster)
  (package! modern-cpp-font-lock)
  (package! clang-format)
  (package! cquery))

