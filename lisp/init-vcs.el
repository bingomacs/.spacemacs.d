;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Version control systems.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Git
(use-package magit
  :commands (magit-define-popup-switch magit-refresh-buffer)
  :functions (all-the-icons-faicon all-the-icons-alltheicon)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-c M-g" . magit-file-popup))
  :preface
  (when centaur-pretty-magit
    (defvar pretty-magit-alist nil)
    (defvar pretty-magit-prompt nil)

    ;; Pretty magit http://www.modernemacs.com/post/pretty-magit
    (defmacro pretty-magit (word icon props &optional no-prompt?)
      "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
      `(prog1
           (add-to-list 'pretty-magit-alist
                        (list (rx bow (group ,word (eval (if ,no-prompt? "" ":"))))
                              ,icon ,props))
         (unless ,no-prompt?
           (add-to-list 'pretty-magit-prompt (concat ,word ": ")))))

    (defun add-magit-faces ()
      "Add face properties and compose symbols for buffer from pretty-magit."
      (interactive)
      (with-silent-modifications
        (--each pretty-magit-alist
          (-let (((rgx icon props) it))
            (save-excursion
              (goto-char (point-min))
              (while (search-forward-regexp rgx nil t)
                (compose-region
                 (match-beginning 1) (match-end 1) icon)
                (when props
                  (add-face-text-property
                   (match-beginning 1) (match-end 1) props)))))))))
  ;; :init (use-package ghub+ :commands ghubp-get-notifications)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))
  (magit-define-popup-switch 'magit-fetch-popup
    ?t "Fetch all tags" "--tags")

  (when centaur-ivy-icon
    (with-eval-after-load 'all-the-icons
      (setq pretty-magit-alist nil)
      (setq pretty-magit-prompt nil)

      (pretty-magit "Feature"
                    (all-the-icons-faicon "map-signs")
                    '(:foreground "#46D9FF" :height 1.2))
      (pretty-magit "Enhance"
                    (all-the-icons-faicon "cog")
                    '(:foreground "#ECBE7B" :height 1.2)
                    t)
      (pretty-magit "Improve"
                    (all-the-icons-faicon "cog")
                    '(:foreground "#ECBE7B" :height 1.2))
      (pretty-magit "Optimize"
                    (all-the-icons-faicon "cogs")
                    '(:foreground "#51afef" :height 1.2))
      (pretty-magit "Add"
                    (all-the-icons-faicon "plus-square")
                    '(:foreground "#c678dd" :height 1.2))
      (pretty-magit "Remove"
                    (all-the-icons-faicon "minus-square")
                    '(:foreground "#da8548" :height 1.2))
      (pretty-magit "Clean"
                    (all-the-icons-faicon "scissors")
                    '(:foreground "#da8548" :height 1.2))
      (pretty-magit "Fix"
                    (all-the-icons-faicon "bug")
                    '(:foreground "#ff6c6b" :height 1.2))
      (pretty-magit "Refactor"
                    (all-the-icons-faicon "wrench")
                    '(:foreground "#51afef" :height 1.2))
      (pretty-magit "Bump"
                    (all-the-icons-faicon "anchor")
                    '(:foreground "#98be65" :height 1.2))
      (pretty-magit "Docs"
                    (all-the-icons-faicon "file-text")
                    '(:foreground "#98be65" :height 1.2))
      (pretty-magit "master"
                    (all-the-icons-alltheicon "git")
                    '(:box t :height 1.2)
                    t)
      (pretty-magit "origin"
                    (all-the-icons-faicon "github")
                    '(:box t :height 1.2)
                    t)

      (advice-add #'magit-status :after #'add-magit-faces)
      (advice-add #'magit-refresh-buffer :after #'add-magit-faces)

      (with-eval-after-load 'ivy
        (defun magit-commit-prompt ()
          "Magit prompt and insert commit header with faces."
          (interactive)
          (insert (ivy-read "Commit Type " pretty-magit-prompt
                            :require-match t :sort t :preselect "Add: "))
          (add-magit-faces))

        (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
        (add-hook 'git-commit-setup-hook 'magit-commit-prompt)))))

;; Magit interfaces for GitHub
;; (use-package magithub
;;   :after magit
;;   :init
;;   (setq magithub-api-timeout 5)
;;   (magithub-feature-autoinject t))

;; ;; Gitflow externsion for Magit
(use-package magit-gitflow
  :diminish magit-gitflow-mode
  :functions magit-define-popup-action
  :bind (:map magit-status-mode-map
              ("G" . magit-gitflow-popup))
  :hook (magit-mode . turn-on-magit-gitflow)
  :config
  (magit-define-popup-action 'magit-dispatch-popup
    ?G "GitFlow" #'magit-gitflow-popup ?!))

;; Git-Svn extension for Magit
(use-package magit-svn
  :diminish magit-svn-mode
  :hook (magit-mode . magit-svn-mode))

;; Show source file TODOs in Magit
(use-package magit-todos
  :hook (magit-status-mode . magit-todos-mode))

;;; Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Walk through git revisions of a file
(use-package git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;; Highlighting regions by last updated time
(use-package smeargle
  :bind (:map vc-prefix-map
              ("S" . smeargle)
              ("C" . smeargle-commits)
              ("R" . smeargle-clear)))

;; Git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote)

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
