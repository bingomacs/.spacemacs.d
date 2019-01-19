(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package evil
  ;; :hook (after-init . evil-mode)
  :init
  (progn
    (evil-mode 1)
    (setq evil-want-C-u-scroll t))
  :config
  (progn
                                        ; Color cursor based on mode
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))



                                        ; Wipe out most existing C- bindings
                                        ;(dolist (key '("\C-a" "\C-b" "\C-c" "\C-d" "\C-e" "\C-f" "\C-g"
                                        ;               "\C-h" "\C-j" "\C-k" "\C-l" "\C-n" "\C-o" "\C-p"
                                        ;               "\C-q" "\C-t" "\C-u" "\C-v" "\C-x" "\C-z" "\e"))
                                        ;  (global-unset-key key))

                                        ; C-w to delete words
    (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
                                        ;(define-key minibuffer-local-map (kbd "C-6") 'evil-buffer)

                                        ; j and k move the visual line in long wrapped lines
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)))
                                        ; Setup , as a leader
(use-package evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key "wd" 'delete-window)
    (evil-leader/set-key "wo" 'delete-other-windows)
    (evil-leader/set-key "ws" 'split-window-below)
    (evil-leader/set-key "wh" 'split-window-horizontally)
    (evil-leader/set-key "wv" 'split-window-vertically)
    (evil-leader/set-key "ww" 'other-window)
    (evil-leader/set-key "wk" 'kill-buffer-and-window)
    (evil-leader/set-key "gs" 'magit-status)
    (evil-leader/set-key "ff" 'counsel-find-file)
    (evil-leader/set-key "fr" 'counsel-recentf)
    (evil-leader/set-key "fs" 'save-buffer)

    (setq evil-leader/in-all-states 1)
    ;; (evil-leader/set-leader ","))
                                        ; ,x to be M-x
    (evil-leader/set-key "x" 'execute-extended-command)))

                                        ; Allow escaping of lots of things
(use-package evil-escape
  :config
  (progn
    (evil-escape-mode)
    (global-set-key (kbd "<escape>") 'evil-escape)))



                                        ; Various tag matching
(use-package evil-matchit)

(use-package evil-magit
  :after magit
  ;; :init (setq evil-magit-state 'motion)
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  )
;; :config
;; (add-hook 'magit-mode-hook 'evil-local-mode))
                                        ; C-+ C-- to increase/decrease number like Vim's C-a C-x
(use-package evil-numbers
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)))

                                        ; Surround
(use-package evil-surround
  :load-path "packages/evil-surround"
  :config
  (progn
    (global-evil-surround-mode 1)))

                                        ; Tabs
(use-package evil-tabs
  :config
  (progn
    (global-evil-tabs-mode t)
    (evil-leader/set-key "t" 'elscreen-create)
    (global-unset-key (kbd "C-l"))
    (global-unset-key (kbd "C-h"))
    (define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
    (define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)))


(provide 'init-evil)

;;; init-evil.el ends here
