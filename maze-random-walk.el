(require 'maze-walk)

(defun maze/walk-infinite-silly-random-walk (delay)
  "Probably one of the most silly maze traversal algorithm ever written"
  (while t
    (goto-char (maze/random-choice (maze/walk-available-positions)))
    (maze/walk--show-position delay)))

(defun maze/walk--backtracking-direction (direction)
  (case direction
    ((:up) :down)
    ((:left) :right)
    ((:right) :left)
    ((:down) :up)
    (t (error (format "Unknown direction %s" direction)))))

(defun maze/walk--pick-non-backtracking-move (available-moves previous-move)
  (let ((direction-to-avoid (maze/walk--backtracking-direction (car previous-move))))
    (maze/random-choice (--filter (not (eq direction-to-avoid (car it))) available-moves))))

(defun maze/walk--random-non-backtracking-move (available-moves previous-move)
  (cond
   ((not previous-move) (maze/random-choice available-moves))
   ((= (length available-moves) 1) (car available-moves))
   (t (maze/walk--pick-non-backtracking-move available-moves previous-move))))

(defun maze/walk-infinite-dumb-random-walk (delay)
  "Improvement over the silly random walk that doesn't needlessy backtracks"
  (let ((previous-move))
    (while t
      (let ((current-move (maze/walk--random-non-backtracking-move (maze/walk-available-moves) previous-move)))
        (goto-char (cdr current-move))
        (setq previous-move current-move))
      (maze/walk--show-position delay))))

(provide 'maze-random-walk)
