(defun bingomacs/pyuic()
  "use pyuic5 convert .ui file to .py"
  (interactive)
  (let ((name (file-relative-name (buffer-file-name))))
    (shell-command
     (format "pyuic5 %s -o %s.py" name (file-name-sans-extension name)))))


(defun bingomacs/enable-socks()
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))


(defun bingomacs/disable-socks()
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))
