(defun bingomacs/pyuic()
  "use pyuic5 convert .ui file to .py"
  (interactive)
  (let ((name (file-relative-name (buffer-file-name))))
    (shell-command
     (format "pyuic5 %s -o %s.py" name (file-name-sans-extension name)))))

(defun jd-shell ()
  (interactive)
  (let ((default-directory "/sshx:ali:~"))
    (eshell (generate-new-buffer-name "*jd*"))))

(defun bingomacs/enable-socks()
  "Enable socks proxy"
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))


(defun bingomacs/disable-socks()
  "Disable socks proxy"
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))


(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "imgs/" (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ;; take screenshot
  (if (eq system-type 'darwin)
      (progn
        (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat "\"" filename "\"" ))
        (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ;; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-display-inline-images))
