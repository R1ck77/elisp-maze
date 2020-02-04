(require 'maze-binary)
(require 'maze-wilson)
(require 'maze-hunt-and-kill)
(require 'maze-recursive-backtracker)
(require 'maze-ascii)
(require 'maze-random-walk)
(require 'maze-border-walk)
(require 'maze-dijkstra)
(require 'maze-dijkstra-backtrack)

(defun maze/insert (maze)
  (insert (maze/to-ASCII maze))
  (message "A maze-ing!"))

(defun maze/debug--print-state ()
  (if maze/debug
      (message (format "Random state: %s" maze/random-state))))

(defun maze/insert-binary (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/debug--print-state)
  (maze/insert (maze/binary columns rows)))

(defun maze/insert-wilson (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/debug--print-state)
  (maze/insert (maze/wilson columns rows)))

(defun maze/insert-hunt-and-kill (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/debug--print-state)
  (maze/insert (maze/hunt-and-kill columns rows)))

(defun maze/insert-recursive-backtracker (rows columns)
  (interactive "nRows? \nnColumns? ")
  (maze/debug--print-state)
  (maze/insert (maze/recursive-backtracker columns rows)))

(defun maze/interpret--delay-value (steps-per-second)
  (/ 1.0 (or steps-per-second 10)))

(defun maze/silly-walk (steps-per-second)
  (interactive "P")
  (maze/debug--print-state)
  (maze/walk-infinite-silly-random-walk (maze/interpret--delay-value steps-per-second)))

(defun maze/dumb-walk (steps-per-second)
  (interactive "P")
  (maze/debug--print-state)
  (maze/walk-infinite-dumb-random-walk (maze/interpret--delay-value steps-per-second)))

(defun maze/border-walk (steps-per-second)
  (interactive "P")
  (maze/walk-border-walk (/ 1.0 (or steps-per-second 10))))

(defun maze/find-long-path ()
  (interactive)
  (maze/dijb-find-long-path))

(defun maze/dijkstra ()
  (interactive)
  (maze/dij-compute-dijkstra (point)))

(provide 'maze)
