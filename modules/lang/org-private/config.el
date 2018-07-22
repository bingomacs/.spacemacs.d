;;; lang/org-private/config.el -*- lexical-binding: t; -*-


(def-package! deft
  :config (setq deft-directory "~/.org/"
                deft-extensions '("md" "org" "txt")
                deft-recursive t
                deft-use-filename-as-title t))


(def-package! easy-hugo
  :defer
  :init
  (setq easy-hugo-basedir "~/git/blog/"
        easy-hugo-hugo-postdir "content/post"
        easy-hugo-root "/"))


(after! org
  (setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")))
  (setq org-todo-keyword-faces '(("TODO" . "red")
                                 ("DOING" . "yellow")
                                 ("DONE" . "green")
                                 ("ABORT" . "gray")))
  )
