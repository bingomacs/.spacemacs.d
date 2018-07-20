;;; private/binsheng/init.el -*- lexical-binding: t; -*-

;; (setq doom-font (font-spec :family "Source Code Pro" :size 15)
;;      doom-variable-pitch-font (font-spec :family "Source Code Pro")
;;      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
;;      doom-big-font (font-spec :family "Source Code Pro" :size 16))

(setq doom-cjk-font (font-spec :family "STKaiti" :size 18))
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")))
(setq org-todo-keyword-faces '(("TODO" . "red")
                               ("DOING" . "yellow")
                               ("DONE" . "green")
                               ("ABORT" . "gray")))
(setq doom-localleader-key ","
      +default-repeat-forward-key ";"
      +default-repeat-backward-key "'"
      evil-want-C-u-scroll t
      evil-want-integration nil
      evil-shift-width 2
      evil-snipe-override-evil-repeat-keys nil
      evil-collection-company-use-tng nil
      evil-respect-visual-line-mode t
      +evil-collection-disabled-list '(elfeed notmuch kotlin-mode simple dired helm ivy anaconda-mode))

;; * UI
(setq frame-title-format
      '("emacs%@"
        (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")))
      doom-font (font-spec :family "Source Code Pro" :size 15)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 14)
      doom-unicode-font (font-spec :family "Source Code Pro" :size 13)
      doom-big-font (font-spec :family "Source Code Pro" :size 16)
      ovp-font "Iosevka"
      doom-theme 'doom-one
      doom-line-numbers-style 'relative
      +doom-modeline-buffer-file-name-style 'truncate-upto-project
      doom-neotree-enable-variable-pitch t
      doom-neotree-project-size 1.2
      doom-neotree-line-spacing 0
      doom-neotree-folder-size 1.0
      doom-neotree-chevron-size 0.6
      ;; scroll-conservatively 0
      doom-line-numbers-visual-style t
      ;;browse-url-browser-function 'xwidget-webkit-browse-url
      indicate-buffer-boundaries nil
      ;;frame-alpha-lower-limit 0
      indicate-empty-lines nil
      which-key-idle-delay 0.3)
