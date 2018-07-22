;;; lang/lsp/config.el -*- lexical-binding: t; -*-

;; * General
(def-package! lsp-mode
  :commands (lsp-mode))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 5)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header nil
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-border (doom-color 'fg)))

(def-package! company-lsp
  :after lsp-mode)



;; * Python
(def-package! lsp-python
  :commands (lsp-python-enable)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (set-company-backend! 'python-mode '(company-lsp company-files company-yasnippet))
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

;; * Go
(def-package! lsp-go
  :init (add-hook 'go-mode-hook #'lsp-go-enable)
  :config
  (set-company-backend! 'go-mode '(company-lsp)))
