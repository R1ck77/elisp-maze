(require 'maze-binary)
(require 'maze-wilson)
(require 'maze-ascii)

(defun maze/insert (maze)
  (insert (maze/to-ASCII maze))
  (message "A maze-ing!"))

(defun maze/insert-binary (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/insert (maze/binary columns rows)))

(defun maze/insert-wilson (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/insert (maze/wilson columns rows)))

(provide 'maze)
