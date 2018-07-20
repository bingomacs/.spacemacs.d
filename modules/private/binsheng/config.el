;;; private/binsheng/config.el -*- lexical-binding: t; -*-

(load! "+org")

(def-package! beacon
  :config
  (beacon-mode 1))

(def-package! org-pomodoro
  :config
  (progn
    (add-hook 'org-pomodoro-finished-hook
              (lambda () (binsheng/notify "Pomodoro Completed!" "Time for a break.")))
    (add-hook 'org-pomodoro-break-finished-hook
              (lambda () (binsheng/notify "Pomodoro Short Break Finished" "Ready for Another?")))
    (add-hook 'org-pomodoro-long-break-finished-hook
              (lambda () (binsheng/notify "Pomodoro Long Break Finished" "Ready for Another?")))
    (add-hook 'org-pomodoro-killed-hook
              (lambda () (binsheng/notify "Pomodoro Killed" "One does not simply kill a pomodoro!")))
    )
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



(def-package! color-identifiers-mode
  :init (add-hook! 'after-init-hook 'global-color-identifiers-mode))

(def-package! rainbow-identifiers
  :init (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))


(setq magit-repository-directories '(("~/git" . 2)))
