(require 'maze-dijkstra)

(defconst maze-dijb-color "red")

(cl-defstruct maze-dijb-state current score)

(defun maze/dijb-new-state (current score)
  (make-maze-dijb-state :current current
                        :score score))

(defun maze/dijb--get-scored-moves (index)
  (-filter #'cdr
           (--map (cons it (maze/dij-score-at-point it))
                  (maze/walk-available-positions index))))

(defun maze/dijb--compare-moves (comparator a-move another-move)
  (and a-move
       another-move
       (funcall comparator (cdr a-move) (cdr another-move))))

(defun maze/dijb-next-move (scored-moves current-score)
  (--min-by (maze/dijb--compare-moves #'< it other)
            (--filter (< (cdr it) current-score) scored-moves)))

(defun maze/dijb-next-move-from-state (state)
  (maze/dijb-next-move (maze/dijb--get-scored-moves (maze-dijb-state-current state))
                       (maze-dijb-state-score state)))

(defun maze/dijb-next-state (state)
  (let ((next-move (maze/dijb-next-move-from-state state)))
    (when next-move
      (make-maze-dijb-state :current (car next-move )
                            :score (cdr next-move)))))

;;; TODO/FIXME check for nil score at start!
(defun maze/dijb-traceback (start)
  "Colorize the maze back to the starting point for the previously computed dijkstra score"
  (let ((state (maze/dijb-new-state start (maze/dij-score-at-point start))))
    (maze/until (not state)
      (setq state (maze/dijb-next-state state))
      (when state
        (maze/dij-debug-mark-with-color (maze-dijb-state-current state) maze-dijb-color)
        (sleep-for 0.01)
        (redisplay)))))

(provide 'maze-dijkstra-backtrack)
