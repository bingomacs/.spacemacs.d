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
  '(org
    deft
    leanote
    org-pomodoro
    ob-go
    dashboard
    magit-todos
    dired-icon
    beacon
    (aria2 :location (recipe :fetcher github :repo "LdBeth/aria2.el"))
    ;; (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
    (aweshell :location (recipe :fetcher github :repo "manateelazycat/aweshell"))
    (company-english-helper :location (recipe :fetcher github :repo "manateelazycat/company-english-helper"))
    (insert-translated-name :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
    calfw
    pinentry
    cal-china-x
    calfw-org
    carbon-now-sh
    go-tag
    easy-hugo)
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





(defun bingomacs/post-init-org()
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
          (expand-file-name "~/.spacemacs.d/layers/bingomacs/plantuml.jar")))
    (org-babel-do-load-languages
     'org-babel-load-languages
      '((js . t)
        (latex .t)
        (python . t)
        (shell . t)
        (java . t)
        (go . t)
        (sql . t)
        (emacs-lisp . t)
        (http . t)
        (restclient . t)
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
      (mapcar #'(lambda (file)
                 (when (file-exists-p file)
                   (push file org-agenda-files)))
              (org-projectile-todo-files)))

    ;; 加密文章
    ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
    ;; org-mode 設定
    (require 'org-crypt)

    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "crypt")

    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))

    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key nil)


    (require 'org-habit)
    (setq org-habit-show-done-always-green t) 
    ;; 减少显示天数，使其可以放在任务条的左边
    (setq org-habit-graph-column 1)
    (setq org-habit-preceding-days 10)
    (setq org-habit-following-days 2)
    ;; 恢复默认日历行为
    (setq org-habit-show-habits-only-for-today nil)
    (let ((agenda-sorting-strategy
           (assoc 'agenda org-agenda-sorting-strategy)))
      (setcdr agenda-sorting-strategy
              (remove 'habit-down (cdr agenda-sorting-strategy))))

    (require 'org-tempo)

    (setq-default
     ;; inhibit-startup-screen t;隐藏启动显示画面
     calendar-date-style 'iso
     calendar-day-abbrev-array ["七" "一" "二" "三" "四" "五" "六"]
     calendar-day-name-array ["七" "一" "二" "三" "四" "五" "六"]
     calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
     calendar-week-start-day 1
     ;; (setq-default org-agenda-format-date (quote my-org-agenda-format-date-aligned))
     org-agenda-inhibit-startup t
     org-agenda-window-setup (quote current-window)
     org-deadline-warning-days 5;;最后期限到达前5天即给出警告
     org-agenda-show-all-dates t
     org-agenda-skip-deadline-if-done t
     org-agenda-skip-scheduled-if-done t
     org-reverse-note-order t ;;org.el
     org-link-file-path-type  'relative
     org-log-done 'time
     ;; code执行免应答（Eval code without confirm）
     org-confirm-babel-evaluate nil)
    (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))

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

(defun bingomacs/init-ob-go()
  (use-package ob-go))


(defun bingomacs/init-dashboard()
  (use-package dashboard
    :ensure t
    :config
    (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
    (setq dashboard-startup-banner '"~/.spacemacs.d/img/jump.png")
    (setq dashboard-center-content t)
    (setq dashboard-items '((recents  . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)
                            (registers . 5)))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-init-info t)
    (dashboard-setup-startup-hook)))

;; brew install terminal-notifier
;; brew linkapps
(defun bingomacs/notify-osx (title msg)
  (message title "call bingomacs notify")
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" msg
                "-active" "org.gnu.Emacs"))

(defun bingomacs/notify-linux (title msg)
  (message title "call bingomacs notify")
  (call-process "notify-send"
                nil 0 nil
                "-i" "face-monkey"
                title
                msg))


(defun bingomacs/notify (title msg)
  (if (eq system-type 'darwin)
      (bingomacs/notify-osx title msg)
    (bingomacs/notify-linux title msg)))

(defun bingomacs/post-init-org-pomodoro ()
  (add-hook 'org-pomodoro-finished-hook
            (lambda () (bingomacs/notify "Pomodoro Completed!" "Time for a break.")))
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda () (bingomacs/notify "Pomodoro Short Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda () (bingomacs/notify "Pomodoro Long Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-killed-hook
            (lambda () (bingomacs/notify "Pomodoro Killed" "One does not simply kill a pomodoro!"))))

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

(defun bingomacs/init-easy-hugo()
  (use-package easy-hugo
    :defer t
    :init
    (setq easy-hugo-basedir "~/git/blog/")
    (setq easy-hugo-postdir "content/post")
    (setq easy-hugo-root "/")))

;; 用来快速浏览、过滤、编辑文本笔记
(defun bingomacs/post-init-deft()
  (use-package deft
    :defer t
    :config (setq deft-directory "~/Nextcloud/"
                  deft-extensions '("md" "org" "txt")
                  deft-recursive t
                  deft-use-filename-as-title t)))

(defun bingomacs/init-leanote()
  (use-package leanote
    :defer t
    :config
    (progn
      (add-hook 'markdown-mode-hook
                (lambda ()
                  (leanote)
                  (leanote-spaceline-status))))))


;; pinentry-start 要使用的时候 Mac 下需要
;; 把allow-emacs-pinentry 加入 .gnupg/gpg-agent.conf
;; http://elpa.gnu.org/packages/pinentry.html
;; (setenv "GPG_AGENT_INFO" nil) 可能有用
(defun bingomacs/init-pinentry()
  (use-package pinentry
    :defer t))


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


(defun bingomacs/init-calfw()
  (use-package calfw
    :defer t))

(defun bingomacs/init-calfw-org()
  (use-package calfw-org
    :defer t))

(defun bingomacs/init-cal-china-x()
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
