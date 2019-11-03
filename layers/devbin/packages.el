;;; packages.el --- devbin layer packages file for Spacemacs.
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
;; added to `devbin-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `devbin/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `devbin/pre-init-PACKAGE' and/or
;;   `devbin/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst devbin-packages
  '(leanote
    magit-todos
    all-the-icons-dired
    dired-subtree
    beacon
    (aria2 :location (recipe :fetcher github :repo "LdBeth/aria2.el"))
    ;; (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
    (aweshell :location (recipe :fetcher github :repo "manateelazycat/aweshell"))
    (company-english-helper :location (recipe :fetcher github :repo "manateelazycat/company-english-helper"))
    (insert-translated-name :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
    carbon-now-sh
    edit-server
    speed-type
    go-tag
    figlet


    (prettify-utils :location (recipe :fetcher github :repo "Ilazki/prettify-utils.el"))
    (pretty-eshell   :location local)
    (pretty-fonts    :location local)
    (pretty-magit    :location local))
  "The list of Lisp packages required by the devbin layer.

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

(defun devbin/init-magit-todos()
  (use-package magit-todos
    :defer t
    :hook (magit-mode . magit-todos-mode)
    :config
    (setq magit-todos-require-colon nil)
    (define-key magit-todos-section-map "j" nil)))


(defun devbin/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(defun devbin/init-dired-subtree()
(use-package dired-subtree
  :defer t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-cycle))))


(defun devbin/init-leanote()
  (use-package leanote
    :defer t
    :config
    (progn
      (add-hook 'markdown-mode-hook
                (lambda ()
                  (leanote)
                  (leanote-spaceline-status))))))


(defun devbin/init-aria2()
  (use-package aria2
    :defer t))

(defun devbin/init-awesome-tab()
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
(defun devbin/eshell-keymap ()
  (evil-define-key 'insert eshell-mode-map
    (kbd "C-u") 'eshell-kill-input
    (kbd "C-a") 'eshell-bol
    (kbd "C-d") 'kevin/quit-or-delete-char
    (kbd "C-r") 'kevin/ivy-eshell-history
    (kbd "TAB") 'pcomplete-std-complete))
(defun devbin/init-aweshell()
  (use-package aweshell
    :commands (aweshell-toggle)
    :hook (eshell-first-time-mode . devbin/eshell-keymap)
    :config
    (setq eshell-highlight-prompt t)
    (setq eshell-prompt-function 'epe-theme-lambda)
    (setq eshell-history-file-name (concat user-emacs-directory "eshell/history"))))

(defun devbin/init-insert-translated-name()
  (use-package insert-translated-name
    :ensure nil
    :bind ("C-c t t" . 'insert-translated-name-insert)
    :config
    (setq insert-translated-name-translate-engine 'youdao)
    (defvar insert-translated-name-camel-style-mode-list
      '(go-mode))))

(defun devbin/init-company-english-helper()
  (use-package company-english-helper
    :ensure nil
    :after company
    :bind ("C-c t e" . 'toggle-company-english-helper)) )

(defun devbin/init-beacon()
  (use-package beacon
    :ensure nil
    :defer t
    :init
    (beacon-mode 1)
    (setq beacon-blink-delay 0.1)
    (setq beacon-blink-duration 0.1)
    (setq beacon-blink-when-buffer-changes nil)
    (setq beacon-blink-when-point-moves-horizontally nil)
    (setq beacon-blink-when-point-moves-vertically 0)
    (setq beacon-color 0.6)
    (setq beacon-dont-blink-commands (quote (forward-line)))
    (setq beacon-size 20)))

(defun devbin/init-edit-server()
  (use-package edit-server
    :init (edit-server-start)
    :config (setq edit-server-default-major-mode 'org-mode)))

(defun devbin/init-speed-type()
  (use-package speed-type
    :defer))

(defun devbin/post-init-go-tag()
  (use-package go-tag
    :config
    (setq go-tag-args (list "-transform" "camelcase"))))

(defun devbin/init-carbon-now-sh()
  (use-package carbon-now-sh
    :defer))

(defun devbin/init-figlet()
  (use-package figlet
    :defer
	:init
	;; broadway isometric1 starwars doom banner
	(setq figlet-default-font "banner")))


(defun devbin/init-prettify-utils ()
  (use-package prettify-utils))

(defun devbin/init-pretty-eshell ()
  (use-package pretty-eshell
    :init
    (progn
      ;; Change default banner message
      (setq eshell-banner-message (s-concat (s-repeat 20 "---") "\n\n"))

      ;; More prompt styling
      (setq pretty-eshell-header "\n︳")
      (setq pretty-eshell-prompt-string " "))

    :config
    (progn
      ;; Directory
      (pretty-eshell-section
       esh-dir
       "\xf07c"  ; 
       (abbreviate-file-name (eshell/pwd))
       '(:foreground "#268bd2" :bold bold :underline t))

      ;; Git Branch
      (pretty-eshell-section
       esh-git
       "\xe907"  ; 
       (magit-get-current-branch)
       '(:foreground "#8D6B94"))

      ;; Python Virtual Environment
      (pretty-eshell-section
       esh-python
       "\xe928"  ; 
       pyvenv-virtual-env-name)

      ;; Time
      (pretty-eshell-section
       esh-clock
       "\xf017"  ; 
       (format-time-string "%H:%M" (current-time))
       '(:foreground "forest green"))

      ;; Prompt Number
      (pretty-eshell-section
       esh-num
       "\xf0c9"  ; 
       (number-to-string pretty-eshell-prompt-num)
       '(:foreground "brown"))

      (setq pretty-eshell-funcs
            (list esh-dir esh-git esh-python esh-clock esh-num)))))

(defun devbin/init-pretty-fonts ()
  (use-package pretty-fonts
    :config
    ;; !! This is required to avoid segfault when using emacs as daemon !!
    (spacemacs|do-after-display-system-init
     ;; (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
     ;; (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

     ;; (pretty-fonts-set-fontsets-for-fira-code)
     (pretty-fonts-set-fontsets
      '(;; All-the-icons fontsets
        ("fontawesome"
         ;;                         
         #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

        ("all-the-icons"
         ;;    
         #xe907 #xe928)

        ("github-octicons"
         ;;                               
         #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

        ("material icons"
         ;;              
         #xe871 #xe918 #xe3e7  #xe5da
         ;;              
         #xe3d0 #xe3d1 #xe3d2 #xe3d4))))))

(defun devbin/init-pretty-magit ()
  (use-package pretty-magit
    :config
    (progn
      (pretty-magit-add-leaders
       '(("Feature" ? (:foreground "slate gray" :height 1.2))
         ("Add"     ? (:foreground "#375E97" :height 1.2))
         ("Fix"     ? (:foreground "#FB6542" :height 1.2))
         ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
         ("Docs"    ? (:foreground "#3F681C" :height 1.2))))

      (pretty-magit-setup))))


;;; packages.el ends here
