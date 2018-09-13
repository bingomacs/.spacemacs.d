(defun binsheng/convert2py()
  "use pyuic5 convert .ui file to .py"
  (interactive)
  (let ((name (file-relative-name (buffer-file-name))))
    (shell-command
     (format "pyuic5 %s -o %s.py" name (file-name-sans-extension name)))))
