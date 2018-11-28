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
    magit-todos
    beacon
    (aria2 :location (recipe :fetcher github :repo "LdBeth/aria2.el"))
    calfw
    mu4e
    cal-china-x
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

    ;; #+CAPTION: 设定图片宽度为100
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

    (with-eval-after-load 'org-agenda
      (require 'org-projectile)
      (mapcar '(lambda (file)
                 (when (file-exists-p file)
                   (push file org-agenda-files)))
              (org-projectile-todo-files)))


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

(defun binsheng/init-magit-todos()
  (use-package magit-todos
    :defer t
    :hook (magit-mode . magit-todos-mode)
    :config
    (setq magit-todos-require-colon nil)
    (define-key magit-todos-section-map "j" nil)))


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


(defun binsheng/post-init-mu4e()
  (with-eval-after-load 'mu4e
    (with-eval-after-load 'mu4e-alert
      ;; Enable Desktop notifications notifier for mac, notifications for linux
      (mu4e-alert-set-default-style 'notifier))

    ;;; Set up some common mu4e variables
    (setq mu4e-maildir "~/mails"
          mu4e-trash-folder "/已删除"
          mu4e-refile-folder "/Archive"
          mu4e-sent-folder "/已发送"
          mu4e-drafts-folder "/草稿箱"
          ;; sync email from imap server
          mu4e-get-mail-command "offlineimap"
          mu4e-update-interval 300
          mu4e-compose-signature-auto-include nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t)

    ;;; Mail directory shortcuts
    (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?g)
          ("/已发送" . ?s)
          ("/Archive" . ?a)
          ("/已删除" . ?d)))

  ;;; Bookmarks
    (setq mu4e-bookmarks
          `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
            ("date:today..now" "Today's messages" ?t)
            ("date:7d..now" "Last 7 days" ?w)
            ("mime:image/*" "Messages with images" ?p)
            (,(mapconcat 'identity
                         (mapcar
                          (lambda (maildir)
                            (concat "maildir:" (car maildir)))
                          mu4e-maildir-shortcuts) " OR ")
             "All inboxes" ?i)))))


(defun binsheng/init-cal-china-x()
  (use-package cal-china-x
    :commands cal-china-x-setup
    :hook (calendar-load . cal-china-x-setup)
    :config
    ;; `S' can show the time of sunrise and sunset on Calendar
    (setq calendar-location-name "Chengdu"
          calendar-latitude 30.67
          calendar-longitude 104.06)

    ;; Holidays
    (setq calendar-mark-holidays-flag t)

    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays
          '((holiday-lunar 1 15 "元宵节")
            (holiday-lunar 7 7 "七夕节")
            (holiday-fixed 3 8 "妇女节")
            (holiday-fixed 3 12 "植树节")
            (holiday-fixed 5 4 "青年节")
            (holiday-fixed 6 1 "儿童节")
            (holiday-fixed 9 10 "教师节")))
    (setq holiday-other-holidays
          '((holiday-fixed 2 14 "情人节")
            (holiday-fixed 4 1 "愚人节")
            (holiday-fixed 12 25 "圣诞节")
            (holiday-float 5 0 2 "母亲节")
            (holiday-float 6 0 3 "父亲节")
            (holiday-float 11 4 4 "感恩节")))
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  holiday-other-holidays))))

;;; packages.el ends here
