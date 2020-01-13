(require 'maze-binary)
(require 'maze-wilson)
(require 'maze-ascii)
(require 'maze-walk)

(defun maze/insert (maze)
  (insert (maze/to-ASCII maze))
  (message "A maze-ing!"))

(defun maze/insert-binary (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/insert (maze/binary columns rows)))

(defun maze/insert-wilson (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/insert (maze/wilson columns rows)))

(defun maze/silly-walk (steps-per-second)
  (interactive "P")
  (maze/walk-infinite-silly-random-walk (/ 1.0 (or steps-per-second 10))))

(defun maze/dumb-walk (steps-per-second)
  (interactive "P")
  (maze/walk-infinite-dumb-random-walk (/ 1.0 (or steps-per-second 10))))

(provide 'maze)
