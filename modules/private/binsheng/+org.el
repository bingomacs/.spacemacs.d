;;; private/binsheng/+org.el -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (setq org-agenda-dir "~/.agenda/")
  (setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")))
  (setq org-todo-keyword-faces '(("TODO" . "pink")
                                 ("DOING" . "yellow")
                                 ("DONE" . "green")
                                 ("ABORT" . "gray")))


  ;; 设置org-babel缩进
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)


  ;; define the refile targets
  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-dir))

  )
