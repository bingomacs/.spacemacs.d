;;; packages.el --- binsheng layer packages file for Spacemacs.
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
;; added to `binsheng-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `binsheng/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `binsheng/pre-init-PACKAGE' and/or
;;   `binsheng/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst binsheng-packages
  '(org
    deft
    leanote
    org-pomodoro
    beacon
    wanderlust
    (aria2 :location (recipe :fetcher github :repo "LdBeth/aria2.el"))
    calfw
    calfw-org
    easy-hugo)
  "The list of Lisp packages required by the binsheng layer.

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





(defun binsheng/post-init-org()
  (with-eval-after-load 'org
    (setq org-src-fontify-natively t)
    (setq org-agenda-inhibit-startup t)
    (setq org-agenda-use-tag-inheritance nil)
    (setq org-agenda-span 'day)
    (setq org-agenda-window-setup 'current-window)
    (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
    ;; agenda files for tag search
    (setq org-agenda-dir "~/.agenda/")
    ;; (let ((notes-dir "~/.org"))
    ;;   (if (file-exists-p notes-dir)
    ;;       (progn
    ;;         (load-library "find-lisp")
    ;;         (setq org-agenda-files (find-lisp-find-files "~/.org" "\.org$"))
    ;;         )))
    ;; (setq org-agenda-files (list "~/.org/"))
    (setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")))
    (setq org-todo-keyword-faces '(("TODO" . "red")
                                   ("DOING" . "yellow")
                                   ("DONE" . "green")
                                   ("ABORT" . "gray")))
    (setq org-log-done 'note)
    (setq org-agenda-include-diary nil)
    ;; 折叠时不再显示「...」
    (setq org-ellipsis "▼")
    ;; Change task state to DOING when clock in
    (setq org-clock-in-switch-to-state "DOING")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    (setq org-plantuml-jar-path
          (expand-file-name "~/.spacemacs.d/layers/binsheng/plantuml.jar")))
    (org-babel-do-load-languages
     'org-babel-load-languages
      '((js . t)
        (latex .t)
        (python . t)
        (shell . t)
        (java . t)
        (go . t)
        (emacs-lisp . t)
        (plantuml . t)
        (C . t)
        (ditaa . t)))

    ;; 设置org-babel缩进
    (setq org-edit-src-content-indentation 0)
    (setq org-src-tab-acts-natively t)

    ;; #+CAPTION: 設定圖片寬度為 100
    ;; #+ATTR_HTML: :width 100
    ;; file:data/2013/pict/test.png
    (setq org-image-actual-width '(300))

    (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-dir))

    (setq org-agenda-sorting-strategy
          '((agenda priority-down time-up)
            (todo priority-down category-keep)
            (tags priority-down category-keep)))
    (setq org-todo-keyword-faces
          '(("WAITING" . (:foreground "gray" :weight bold))))

    ;; define the refile targets
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
    (setq org-capture-templates
          '(("i" "inbox" entry (file+headline org-agenda-file-inbox "inbox")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
             "* TODO [#B] %?\n  %i\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("s" "Code Snippet" entry
             (file org-agenda-file-code-snippet)
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
            ("w" "work" entry (file+headline org-agenda-file-gtd "work")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1[[zsh:1: command not found: osascript]])
            ;; org-mac-chrome-get-frontmost-url org-mac-chrome-insert-frontmost-url
            ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n %(org-mac-chrome-get-frontmost-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 1)))
    )



;; brew install terminal-notifier
;; brew linkapps
(defun binsheng/notify-osx (title msg)
  (message title "call binsheng notify")
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" msg
                "-active" "org.gnu.Emacs"))

(defun binsheng/notify-linux (title msg)
  (message title "call binsheng notify")
  (call-process "notify-send"
                nil 0 nil
                "-i" "face-monkey"
                title
                msg))


(defun binsheng/notify (title msg)
  (if (eq system-type 'darwin)
      (binsheng/notify-osx title msg)
    (binsheng/notify-linux title msg)))

(defun binsheng/post-init-org-pomodoro ()
  (add-hook 'org-pomodoro-finished-hook
            (lambda () (binsheng/notify "Pomodoro Completed!" "Time for a break.")))
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda () (binsheng/notify "Pomodoro Short Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda () (binsheng/notify "Pomodoro Long Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-killed-hook
            (lambda () (binsheng/notify "Pomodoro Killed" "One does not simply kill a pomodoro!"))))

  ;; (setq-default mode-line-misc-info
  ;;              (assq-delete-all 'which-function-mode mode-line-misc-info))

  ;; (which-func-mode)
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
                ;; '((which-func-mode ("" which-func-format " "))))
(defun binsheng/init-easy-hugo()
  (use-package easy-hugo
    :defer t
    :init
    (setq easy-hugo-basedir "~/git/blog/")
    (setq easy-hugo-postdir "content/post")
    (setq easy-hugo-root "/")))

;; 用来快速浏览、过滤、编辑文本笔记
(defun binsheng/post-init-deft()
  (use-package deft
    :defer t
    :config (setq deft-directory "~/.org/"
                  deft-extensions '("md" "org" "txt")
                  deft-recursive t
                  deft-use-filename-as-title t)))

(defun binsheng/init-leanote()
  (use-package leanote
    :defer t
    :config
    (progn
      (add-hook 'markdown-mode-hook
                (lambda ()
                  (leanote)
                  (leanote-spaceline-status))))))

(defun binsheng/init-wanderlust ()
  "Initialize WanderLust."
  (use-package wanderlust
    :defer t
    :init
    (progn
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


(defun binsheng/init-aria2()
  (use-package aria2
    :defer t))


(defun binsheng/init-beacon()
  (use-package beacon
    :defer t
    :init
    (beacon-mode 1)))

(defun binsheng/init-calfw()
  (use-package calfw
    :defer t))

(defun binsheng/init-calfw-org()
  (use-package calfw-org
    :defer t))


;;; packages.el ends here
