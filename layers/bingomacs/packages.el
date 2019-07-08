;;; packages.el --- bingomacs layer packages file for Spacemacs.
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
;; added to `bingomacs-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bingomacs/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bingomacs/pre-init-PACKAGE' and/or
;;   `bingomacs/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bingomacs-packages
  '(leanote
    magit-todos
    dired-icon
    beacon
    (aria2 :location (recipe :fetcher github :repo "LdBeth/aria2.el"))
    ;; (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
    (aweshell :location (recipe :fetcher github :repo "manateelazycat/aweshell"))
    (company-english-helper :location (recipe :fetcher github :repo "manateelazycat/company-english-helper"))
    (insert-translated-name :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
    carbon-now-sh
    go-tag
    figlet)
  "The list of Lisp packages required by the bingomacs layer.

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




;; (setq-default mode-line-misc-info
;;              (assq-delete-all 'which-function-mode mode-line-misc-info))

;; (which-func-mode)
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
;; '((which-func-mode ("" which-func-format " "))))

(defun bingomacs/init-magit-todos()
  (use-package magit-todos
    :defer t
    :hook (magit-mode . magit-todos-mode)
    :config
    (setq magit-todos-require-colon nil)
    (define-key magit-todos-section-map "j" nil)))

(defun bingomacs/init-dired-icon ()
  "Initialize dired-icon"
  (add-hook 'dired-mode-hook 'dired-icon-mode)
  (add-hook 'dired-mode-hook
            (lambda ()
              (highlight-lines-matching-regexp "\.org$" 'hi-yellow))))




(defun bingomacs/init-leanote()
  (use-package leanote
    :defer t
    :config
    (progn
      (add-hook 'markdown-mode-hook
                (lambda ()
                  (leanote)
                  (leanote-spaceline-status))))))


(defun bingomacs/init-aria2()
  (use-package aria2
    :defer t))

(defun bingomacs/init-awesome-tab()
  (use-package awesome-tab
    :config
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd ",tt") 'awesome-tab-switch-group)
      (define-key evil-normal-state-map (kbd ",ta") 'awesome-tab-select-beg-tab)
      (define-key evil-normal-state-map (kbd ",te") 'awesome-tab-select-end-tab)
      (define-key evil-normal-state-map (kbd ",t<") 'awesome-tab-move-current-tab-to-left)
      (define-key evil-normal-state-map (kbd ",t>") 'awesome-tab-move-current-tab-to-right)
      (define-key evil-normal-state-map (kbd ",th") 'awesome-tab-forward)
      (define-key evil-normal-state-map (kbd ",tl") 'awesome-tab-backward))
    (setq awesome-tab-cycle-scope 'tabs) ; Navigate through visible tabs only.
    (awesome-tab-mode t)))
(defun bingomacs/eshell-keymap ()
  (evil-define-key 'insert eshell-mode-map
    (kbd "C-u") 'eshell-kill-input
    (kbd "C-a") 'eshell-bol
    (kbd "C-d") 'kevin/quit-or-delete-char
    (kbd "C-r") 'kevin/ivy-eshell-history
    (kbd "TAB") 'pcomplete-std-complete))
(defun bingomacs/init-aweshell()
  (use-package aweshell
    :commands (aweshell-toggle)
    :hook (eshell-first-time-mode . bingomacs/eshell-keymap)
    :config
    (setq eshell-highlight-prompt t)
    (setq eshell-prompt-function 'epe-theme-lambda)
    (setq eshell-history-file-name (concat user-emacs-directory "eshell/history"))))

(defun bingomacs/init-insert-translated-name()
  (use-package insert-translated-name
    :ensure nil
    :bind ("C-c t t" . 'insert-translated-name-insert)
    :config
    (setq insert-translated-name-translate-engine 'youdao)
    (defvar insert-translated-name-camel-style-mode-list
      '(go-mode))))

(defun bingomacs/init-company-english-helper()
  (use-package company-english-helper
    :ensure nil
    :after company
    :bind ("C-c t e" . 'toggle-company-english-helper)) )

(defun bingomacs/init-beacon()
  (use-package beacon
    :defer t
    :init
    (beacon-mode 1)))


(defun bingomacs/post-init-go-tag()
  (use-package go-tag
    :config
    (setq go-tag-args (list "-transform" "camelcase"))))

(defun bingomacs/init-carbon-now-sh()
  (use-package carbon-now-sh
    :defer))

(defun bingomacs/init-figlet()
  (use-package figlet
    :defer))

;;; packages.el ends here
