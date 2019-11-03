;;; packages.el --- devbin-mail layer packages file for Spacemacs.
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
;; added to `devbin-mail-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `devbin-mail/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `devbin-mail/pre-init-PACKAGE' and/or
;;   `devbin-mail/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst devbin-mail-packages
  '(wanderlust
	w3m)
  "The list of Lisp packages required by the devbin-mail layer.

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
(defun devbin-mail/init-wanderlust()
  "Initialize WanderLust."
  (use-package wl
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "awl" 'wl
        "awm" 'compose-mail)
      (setq read-mail-command 'wl
            mail-user-agent 'wl-user-agent
            org-mime-library 'semi)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook)
      (spacemacs/declare-prefix-for-mode 'wl-draft-mode "mm" "mime" "mime-edit")
      (with-eval-after-load 'mime-edit
        (spacemacs/set-leader-keys-for-major-mode 'wl-draft-mode
          dotspacemacs-major-mode-leader-key 'wl-draft-send-and-exit
          "k" 'wl-draft-kill
          "s" 'wl-draft-save
          "z" 'wl-draft-save-and-exit
          "m" mime-edit-mode-entity-map
          "c" 'bbdb-:start-completion)))
    :config
    (progn
      (add-hook 'wl-folder-mode-hook 'evil-emacs-state);; Unknown Reason
      (dolist (mode '(wl-message-mode
                      wl-summary-mode
                      wl-folder-mode
                      wl-draft-mode
                      mime-view-mode))
        (add-to-list 'evil-emacs-state-modes mode)))))

(defun devbin-mail/init-w3m()
  (use-package w3m
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "aw3" 'w3m))
    :config
    (setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8
          w3m-default-display-inline-images t
          w3m-use-cookies t
          w3m-namazu-default-index nil)))


;;; packages.el ends here
