(require 'maze-binary)
(require 'maze-ascii)

(defun maze/insert (maze)
  (insert (maze/to-ASCII maze))
  (message "A maze-ing!"))

(defun maze/insert-binary (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/insert (maze/binary columns rows)))

(provide 'maze)
