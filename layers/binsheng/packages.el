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
    (setq org-agenda-files (list "~/org/agenda.org"))
    (setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")))
    (setq org-todo-keyword-faces '(("TODO" . "red")
                                   ("DOING" . "yellow")
                                   ("DONE" . "green")
                                   ("ABORT" . "gray")))
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
        (js . t)
        (emacs-lisp . t)
        (plantuml . t)
        (C . t)
        (ditaa . t)))


    (setq org-agenda-dir "~/org/agenda/")

    ;; define the refile targets
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    ;; (setq org-agenda-files (list org-agenda-dir))

    (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
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
                title
                msg))


(defun binsheng/notify (title msg)
  (if (eq system-type 'darwin)
      (binsheng/notify-osx title mgs)
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
    :defer
    :init
    (setq easy-hugo-basedir "~/hugo/daily/")
    (setq easy-hugo-postdir "content/post")
    (setq easy-hugo-root "/")))

;; 用来快速浏览、过滤、编辑文本笔记
(defun binsheng/post-init-deft()
  (use-package deft
    :config (setq deft-directory "~/.org/"
                  deft-extensions '("md" "org" "txt")
                  deft-recursive t
                  deft-use-filename-as-title t)))

(defun binsheng/init-leanote()
  (use-package leanote
    :config
    (progn
      (add-hook 'markdown-mode-hook
                (lambda ()
                  (leanote)
                  (leanote-spaceline-status))))))


;;; packages.el ends here
