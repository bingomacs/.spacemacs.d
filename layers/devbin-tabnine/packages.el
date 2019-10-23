;;; packages.el --- tabnine layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: super <bin@super>
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
;; added to `tabnine-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `tabnine/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `tabnine/pre-init-PACKAGE' and/or
;;   `tabnine/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst devbin-tabnine-packages
  '(company-tabnine)
  "The list of Lisp packages required by the tabnine layer.

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


(defun devbin-tabnine/init-company-tabnine()
  (use-package company-tabnine
    :ensure t
    :defer t
    :config
    (progn
      (setq company-tabnine-max-num-results 3)

      (add-to-list 'company-transformers 'tabnine//sort-by-tabnine t)
      ;; The free version of TabNine is good enough,
      ;; and below code is recommended that TabNine not always
      ;; prompt me to purchase a paid version in a large project.
      (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
        (let ((company-message-func (ad-get-arg 0)))
          (when (and company-message-func
                     (stringp (funcall company-message-func)))
            (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
              ad-do-it))))
      )
    ))

(defun devbin-tabnine/post-init-company-tabnine()
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-tabnine)))

;;; packages.el ends here
