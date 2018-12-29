(defun lines-to-cslist (start end &optional arg)
  (interactive "r\nP")
  (let ((insertion
         (mapconcat 
          (lambda (x) (format "\"%s\"" x))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)
    (when arg (forward-char (length insertion)))))
