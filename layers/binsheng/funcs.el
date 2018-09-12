
(defun binsheng/convert2py()
  (interactive)
  (let ((name (file-relative-name (buffer-file-name))))
    (message name)
    (shell-command
     (format "pyuic5 %s -o %s.py" name (file-name-sans-extension name)))))
