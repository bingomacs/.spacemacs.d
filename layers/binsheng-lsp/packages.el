;;; packages.el --- binsheng-lsp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: manjaro <manjaro@administrator>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `binsheng-lsp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `binsheng-lsp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `binsheng-lsp/pre-init-PACKAGE' and/or
;;   `binsheng-lsp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst binsheng-lsp-packages
  '(lsp-mode
    lsp-ui
    company-lsp
    lsp-go
    lsp-javascript-typescript
    lsp-css
    lsp-html)
  "The list of Lisp packages required by the binsheng-lsp layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; packages.el ends here


(defun binsheng-lsp/init-lsp-mode()
 (use-package lsp-mode
     :diminish lsp-mode
     :hook (lsp-after-open . lsp-enable-imenu)
     :config
     (setq lsp-inhibit-message t)
     (setq lsp-message-project-root-warning t)
     (setq create-lockfiles nil)

     ;; Restart server/workspace in case the lsp server exits unexpectedly.
     ;; https://emacs-china.org/t/topic/6392
     (defun lsp-restart-server ()
       "Restart LSP server."
       (interactive)
       (lsp-restart-workspace)
       (revert-buffer t t)
       (message "LSP server restarted."))

     ;; Support LSP in org babel
     ;; https://github.com/emacs-lsp/lsp-mode/issues/377
     (cl-defmacro lsp-org-babel-enbale (lang &optional enable-name)
       "Support LANG in org source code block. "
       (cl-check-type lang string)
       (cl-check-type enable-name (or null string))
       (let ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
             (intern-pre (intern (format "lsp--org-babel-edit-prep:%s" lang)))
             (client (intern (format "lsp-%s-enable" (or enable-name lang)))))
         `(progn
            (defun ,intern-pre (info)
              (let ((lsp-file (or (->> info caddr (alist-get :file))
                                  buffer-file-name)))
                (setq-local buffer-file-name lsp-file)
                (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
                (,client)))
            (if (fboundp ',edit-pre)
                (advice-add ',edit-pre :after ',intern-pre)
              (progn
                (defun ,edit-pre (info)
                  (,intern-pre info))
                (put ',edit-pre 'function-documentation
                     (format "Prepare local buffer environment for org source block (%s)."
                             (upcase ,lang))))))))))


(defun binsheng-lsp/init-lsp-ui()
  (use-package lsp-ui
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references)
                ("C-c u" . lsp-ui-imenu))
    :hook (lsp-mode . lsp-ui-mode)))


(defun binsheng-lsp/init-company-lsp()
  (use-package company-lsp
    :after company
    :defines company-backends))


(defun binsheng-lsp/post-init-lsp-go()
  ;; Go support for lsp-mode using Sourcegraph's Go Language Server
  ;; Install: go get -u github.com/sourcegraph/go-langserver
  (use-package lsp-go
    :commands lsp-go-enable
    :hook (go-mode . lsp-go-enable)
    :config (lsp-org-babel-enbale "go")))



(defun binsheng-lsp/post-init-lsp-javascript-typescript()

  ;; Javascript, Typescript and Flow support for lsp-mode
  ;; Install: npm i -g javascript-typescript-langserver
  (use-package lsp-javascript-typescript
    :commands lsp-javascript-typescript-enable
    :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable)
    :config (lsp-org-babel-enbale "js" "javascript-typescript")))


(defun binsheng-lsp/init-lsp-css()

  ;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
  ;; Install: npm i -g vscode-css-languageserver-bin
  (use-package lsp-css
    :commands (lsp-css-enable
               lsp-less-enable
               lsp-sass-enable
               lsp-scss-enable)
    :hook ((css-mode . lsp-css-enable)
           (less-mode . lsp-less-enable)
           (sass-mode . lsp-sass-enable)
           (scss-mode . lsp-scss-enable))
    :config
    (lsp-org-babel-enbale "css")
    (lsp-org-babel-enbale "sass"))

  )


(defun binsheng-lsp/init-lsp-html()

  ;; HTML support for lsp-mode using vscode-html-languageserver-bin
  ;; Install: npm i -g vscode-html-languageserver-bin
  (use-package lsp-html
    :commands lsp-html-enable
    :hook ((html-mode . lsp-html-enable)
           (web-mode . lsp-html-enable)))
  )


