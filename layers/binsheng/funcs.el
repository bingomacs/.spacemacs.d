
(defun binsheng/convert2py()
  (interactive)
  (shell-command
   (format "pyuic5 %s -o %s.py" (buffer-file-name) (file-name-sans-extension (buffer-file-name)))))
