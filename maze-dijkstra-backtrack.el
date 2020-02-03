(require 'maze-dijkstra)

(defconst maze-dijb-color "red")

(cl-defstruct maze-dijb-state id current score)

(defun maze/dijb-new-state (id current score)
  (make-maze-dijb-state :id id
                        :current current
                        :score score))

;;; TODO/FIXME hic sunt leones
(defun maze/dijb--get-scored-moves (id index)
  (-filter #'cdr
           (--map (cons it (maze/dij-score-at-point id it))
                  (maze/walk-available-positions index))))

(defun maze/dijb--compare-moves (comparator a-move another-move)
  (and a-move
       another-move
       (funcall comparator (cdr a-move) (cdr another-move))))

(defun maze/dijb-next-move (scored-moves current-score)
  (--min-by (maze/dijb--compare-moves #'< it other)
            (--filter (< (cdr it) current-score) scored-moves)))

(defun maze/dijb-next-move-from-state (state)
  (maze/dijb-next-move (maze/dijb--get-scored-moves (maze-dijb-state-id state)
                                                    (maze-dijb-state-current state))
                       (maze-dijb-state-score state)))

(defun maze/dijb-next-state (state)
  (let ((next-move (maze/dijb-next-move-from-state state)))
    (when next-move
      (make-maze-dijb-state :id (maze-dijb-state-id state)
                            :current (car next-move )
                            :score (cdr next-move)))))

;;; TODO/FIXME check for nil score at start!
(defun maze/dijb-traceback (start)
  "Colorize the maze back to the starting point for the previously computed dijkstra score"
  (let* ((id (maze/map-get-current-id start))
         (state (maze/dijb-new-state id start (maze/dij-score-at-point id start))))
    (maze/until (not state)
      (setq state (maze/dijb-next-state state))
      (when state
        (maze/dij-debug-mark-with-color (maze-dijb-state-current state) maze-dijb-color)
        (redisplay)))))

(provide 'maze-dijkstra-backtrack)
