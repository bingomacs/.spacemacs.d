;;; packages.el --- devbin-org layer packages file for Spacemacs.
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
;; added to `devbin-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `devbin-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `devbin-org/pre-init-PACKAGE' and/or
;;   `devbin-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst devbin-org-packages
  '(org
    ob-go
    ob-kotlin
    org-pomodoro
    pinentry
    calfw
    calfw-org
    cal-china-x
    easy-hugo)
  "The list of Lisp packages required by the devbin-org layer.

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

(defun devbin-org/post-init-org()
  (with-eval-after-load 'org
    (setq org-src-fontify-natively t)
    (setq org-agenda-inhibit-startup t)
    (setq org-agenda-use-tag-inheritance nil)
    (setq org-agenda-span 'day)
    (setq org-agenda-window-setup 'current-window)
    (setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")))
    (setq org-todo-keyword-faces '(("TODO")
                                   ("DOING" . "yellow")
                                   ("DONE" . "green")
                                   ("ABORT" . "gray")))
    ;; :COLUMNS:  %75ITEM(Task) %8PRIORITY(Priority) %9TODO(Status) %8EFFORT(Effort){:} %16SCHEDULED %16DEADLINE
    (setq org-columns-default-format "%50ITEM(Task) %8PRIORITY(Priority) %6TODO(Status) %6Effort(Effort){:} %8CLOCKSUM %16SCHEDULED %16DEADLINE")
    ;; global Effort estimate values
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))
    (setq org-agenda-log-mode-items (quote (closed state)))
    (setq org-log-done 'notevery)
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    (setq org-log-into-drawer t)
    (setq org-clock-out-when-done t)
    (setq org-agenda-include-diary nil)
    ;; 折叠时不再显示「...」
    (setq org-ellipsis "▼")
    ;; Change task state to DOING when clock in
    (setq org-clock-in-switch-to-state "DOING")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)

    ;; Tags with fast selection keys
    (setq org-tag-alist (quote ((:startgroup)
                                ("@errand" . ?e)
                                ("@office" . ?o)
                                ("@home" . ?H)
                                ("@farm" . ?f)
                                (:endgroup)
                                ("WAITING" . ?w)
                                ("HOLD" . ?h)
                                ("READING" . ?r)
                                ("PERSONAL" . ?P)
                                ("WORK" . ?W)
                                ("FARM" . ?F)
                                ("ORG" . ?O)
                                ("BLOG" . ?B)
                                ("NORANG" . ?N)
                                ("crypt" . ?E)
                                ("NOTE" . ?n)
                                ("CANCELLED" . ?c)
                                ("FLAGGED" . ??))))

    ;; Allow setting single tags without the menu
    (setq org-fast-tag-selection-single-key (quote expert))

	(add-hook 'org-mode-hook (lambda ()
                       "Beautify org symbols."
                       (push '("[ ]" . ?☐) prettify-symbols-alist)
                       (push '("[X]" . ?☑) prettify-symbols-alist)
                       (push '("[-]" . ?⛞) prettify-symbols-alist)
                       (push '("#+TITLE" . ?🕮) prettify-symbols-alist)
                       (push '("#+DATE" . ?📆) prettify-symbols-alist)
                       (push '("#+AUTHOR" . ?👤) prettify-symbols-alist)
                       (push '("#+EMAIL" . ?🖂) prettify-symbols-alist)
                       (push '("#+OPTIONS" . ?⚙) prettify-symbols-alist)
                       (push '("#+TAGS" . ?🏷) prettify-symbols-alist)
                       (push '("#+DESCRIPTION" . ?🗎) prettify-symbols-alist)
                       (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
                       (push '("#+END_SRC" . ?□) prettify-symbols-alist)
                       (push '("#+BEGIN_QUOTE" . ?») prettify-symbols-alist)
                       (push '("#+END_QUOTE" . ?«) prettify-symbols-alist)
                       (push '("#+HEADERS" . ?☰) prettify-symbols-alist)
                       (push '("#+RESULTS:" . ?💻) prettify-symbols-alist)
                       (prettify-symbols-mode 1)))

    ;; For tag searches ignore tasks with scheduled and deadline dates
    (setq org-agenda-tags-todo-honor-ignore-options t)

    ;; 设置org-babel缩进
    (setq org-edit-src-content-indentation 0)
    (setq org-src-tab-acts-natively t)

    ;; #+CAPTION: 设定图片宽度为100
    ;; #+ATTR_HTML: :width 100
    ;; file:data/2013/pict/test.png
    (setq org-image-actual-width '(300))

    (setq org-agenda-dir "~/.agenda/")
    (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-agenda-file-punch (expand-file-name "punch.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-dir))

    (setq org-agenda-sorting-strategy
          '((agenda priority-down time-up)
            (todo priority-down category-keep)
            (tags priority-down category-keep)))

    (setq org-priority-faces
          '((65 :inherit org-priority :foreground "red")
            (66 :inherit org-priority :foreground "brown")
            (67 :inherit org-priority :foreground "blue")))

    (with-eval-after-load 'org-agenda
      (add-hook 'org-agenda-finalize-hook #'devbin-org/org-agenda-time-grid-spacing)
      (require 'org-projectile)
      (mapcar #'(lambda (file)
                  (when (file-exists-p file)
                    (push file org-agenda-files)))
              (org-projectile-todo-files)))

    (evil-define-key '(normal visual motion) org-mode-map
      "gh" 'outline-up-heading
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gl" 'outline-next-visible-heading
      "gu" 'outline-previous-visible-heading)

    (crypt)

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
     calendar-week-start-day 7
     ;; (setq-default org-agenda-format-date (quote my-org-agenda-format-date-aligned))
     org-deadline-warning-days 5;;最后期限到达前5天即给出警告
     org-agenda-show-all-dates t
     org-agenda-skip-deadline-if-done t
     org-agenda-skip-scheduled-if-done t
     org-reverse-note-order t ;;org.el
     org-link-file-path-type  'relative
     org-log-done 'time
     ;; code执行免应答（Eval code without confirm）
     org-confirm-babel-evaluate nil)
    (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "☯" "☭" "♥" "✜" "♠" "☢" "❀" "★"))

    ;; define the refile targets
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)

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
            ("p" "punch" entry (file+datetree org-agenda-file-punch)
             "* %^{想法}%? %U")
            ("j" "Journal Entry" entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 1)))

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
       (C . t)
       (dot . t)
       (plantuml . t)
       (ditaa . t)))))


(defun devbin-org/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))


;;[[https://emacs-china.org/t/topic/3971/2][求教org中todo已完成纪录怎么自动归档到外部文件 - Org-mode - Emacs China]]
(defun archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun enable-auto-archive ()
  (add-hook 'after-save-hook 'archive-done-tasks))

(defun crypt()
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
  (if (eq system-type 'darwin)
      (pinentry-start)))


;; pinentry-start 要使用的时候 Mac 下需要
;; 把allow-emacs-pinentry 加入 .gnupg/gpg-agent.conf
;; http://elpa.gnu.org/packages/pinentry.html
;; This will force Emacs to use its own internal password prompt instead of an external pin entry program.
(setenv "GPG_AGENT_INFO" nil)
(defun devbin-org/init-pinentry()
  (use-package pinentry
    :defer t))

(defun devbin-org/init-ob-go()
  (use-package ob-go
    :defer t))

(defun devbin-org/init-ob-kotlin()
  (use-package ob-kotlin
    :defer t))

(defun devbin-org/post-init-org-pomodoro ()
  (add-hook 'org-pomodoro-started-hook
            (lambda ()(do-applescript "tell application \"JustFocus\"\n    launch\n    start pomodoro\nend tell")))
  (add-hook 'org-pomodoro-finished-hook
            (lambda () (devbin-org/notify "Pomodoro Completed!" "Time for a break.")))
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda () (devbin-org/notify "Pomodoro Short Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda () (devbin-org/notify "Pomodoro Long Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-killed-hook
            (lambda () (progn (do-applescript "tell application \"JustFocus\"\n    stop\nend tell")
                               (devbin-org/notify "Pomodoro Killed" "One does not simply kill a pomodoro!")))))


;; brew install terminal-notifier
;; brew linkapps
(defun devbin-org/notify-osx (title msg)
  (message title "call devbin notify")
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" msg
                "-active" "org.gnu.Emacs"))

(defun devbin-org/notify-linux (title msg)
  (message title "call devbin notify")
  (call-process "notify-send"
                nil 0 nil
                "-i" "face-monkey"
                title
                msg))


(defun devbin-org/notify (title msg)
  (if (eq system-type 'darwin)
      (devbin-org/notify-osx title msg)
    (devbin-org/notify-linux title msg)))



(defun devbin-org/init-calfw-org()
  (use-package calfw-org
    :defer t))

(defun devbin-org/init-calfw()
  (use-package calfw
    :defer t))


(defun devbin-org/init-cal-china-x()
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
            (holiday-fixed 3 12 "植树节")
            (holiday-lunar 1 1 "春节" 0)
            (holiday-lunar 1 2 "春节" 0)
            (holiday-lunar 1 3 "春节" 0)
            (holiday-solar-term "清明" "清明节")
            (holiday-solar-term "小寒" "小寒")
            (holiday-solar-term "大寒" "大寒")
            (holiday-solar-term "立春" "立春")
            (holiday-solar-term "雨水" "雨水")
            (holiday-solar-term "惊蛰" "惊蛰")
            (holiday-solar-term "春分" "春分")
            (holiday-solar-term "谷雨" "谷雨")
            (holiday-solar-term "立夏" "立夏")
            (holiday-solar-term "小满" "小满")
            (holiday-solar-term "芒种" "芒种")
            (holiday-solar-term "夏至" "夏至")
            (holiday-solar-term "小暑" "小暑")
            (holiday-solar-term "大暑" "大暑")
            (holiday-solar-term "立秋" "立秋")
            (holiday-solar-term "处暑" "处暑")
            (holiday-solar-term "白露" "白露")
            (holiday-solar-term "秋分" "秋分")
            (holiday-solar-term "寒露" "寒露")
            (holiday-solar-term "霜降" "霜降")
            (holiday-solar-term "立冬" "立冬")
            (holiday-solar-term "小雪" "小雪")
            (holiday-solar-term "大雪" "大雪")
            (holiday-solar-term "冬至" "冬至")
            (holiday-lunar 5 5 "端午节" 0)
            (holiday-lunar 8 15 "中秋节" 0)
            (holiday-lunar 7 7 "七夕情人节" 0)
            (holiday-lunar 12 8 "腊八节" 0)
            (holiday-lunar 9 9 "重阳节" 0)))
    (setq holiday-other-holidays
          '((holiday-fixed 1 1 "元旦")
            (holiday-fixed 2 14 "情人节")
            (holiday-fixed 3 8 "妇女节")
            (holiday-fixed 3 14 "白色情人节")
            (holiday-fixed 4 1 "愚人节")
            (holiday-fixed 5 1 "劳动节")
            (holiday-fixed 5 4 "青年节")
            (holiday-float 5 0 2 "母亲节")
            (holiday-fixed 6 1 "儿童节")
            (holiday-float 6 0 3 "父亲节")
            (holiday-fixed 9 10 "教师节")
            (holiday-fixed 10 1 "国庆节")
            (holiday-fixed 10 24 "程序员节")
            (holiday-float 11 4 4 "感恩节")
            (holiday-fixed 12 25 "圣诞节")))
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  holiday-other-holidays))))

;; 用来快速浏览、过滤、编辑文本笔记
;; (defun devbin-org/post-init-deft()
;;   (use-package deft
;;     :defer t
;;     :config (setq deft-directory "~/Nextcloud/"
;;                   deft-extensions '("md" "org" "txt")
;;                   deft-recursive t
;;                   deft-use-filename-as-title t)))


(defun devbin/easy-hugo ()
  (interactive)
  (evil-define-key
    (list 'normal 'insert 'visual 'motion)
    easy-hugo-mode-map
    "n" 'easy-hugo-newpost
    "D" 'easy-hugo-article
    "p" 'easy-hugo-preview
    "P" 'easy-hugo-publish
    "o" 'easy-hugo-open
    "d" 'easy-hugo-delete
    "e" 'easy-hugo-open
    "c" 'easy-hugo-open-config
    "f" 'easy-hugo-open
    "N" 'easy-hugo-no-help
    "v" 'easy-hugo-view
    "r" 'easy-hugo-refresh
    "g" 'easy-hugo-refresh
    "s" 'easy-hugo-sort-time
    "S" 'easy-hugo-sort-char
    "G" 'easy-hugo-github-deploy
    "A" 'easy-hugo-amazon-s3-deploy
    "C" 'easy-hugo-google-cloud-storage-deploy
    "q" 'evil-delete-buffer
    (kbd "TAB") 'easy-hugo-open
    (kbd "RET") 'easy-hugo-preview)
  (define-key global-map (kbd "C-c C-e") 'easy/hugo))


(defun devbin-org/init-easy-hugo()
  (use-package easy-hugo
    :defer t
    :init
    (setq easy-hugo-basedir "~/git/blog/")
    (setq easy-hugo-url "https://devbin.github.io/")
    (setq easy-hugo-preview-url "http://127.0.0.1:1313/")
    (setq easy-hugo-postdir "content/post")
    (setq easy-hugo-default-ext ".org")
    (setq easy-hugo-bloglist
          '(((easy-hugo-basedir . "~/git/dabai/")
            (easy-hugo-url . "http://devbin.tech")
            (setq easy-hugo-preview-url "http://127.0.0.1:1313/")
            (setq easy-hugo-postdir "content/post")
            (setq easy-hugo-default-ext ".org"))))
    (add-hook 'easy-hugo-mode-hook 'devbin/easy-hugo)))

;;; packages.el ends here
