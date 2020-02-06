(require 'dash)
(require 'maze-utils)
(require 'maze-data)

(cl-defstruct maze-rb-state
  used-cells
  stack)

(defun maze/rb--get-current (state)
  (car (maze-rb-state-stack state)))

(defun maze/rb--compute-valid-neighbors (maze index)
  (--map (maze/position-to-index maze it)
         (maze/valid-neighbors maze (maze/index-to-position maze index))))

(defun maze/rb--compute-valid-moves (valid-neighbors used-cells-indices)
    (--filter (not (maze/map-get it used-cells-indices)) valid-neighbors))

(defun maze/rb--compute-valid-moves-from-maze (maze used-cells index)
  (maze/rb--compute-valid-moves (maze/rb--compute-valid-neighbors maze index)
                                used-cells))

(defun maze/rb--valid-moves (maze state)
  (maze/rb--compute-valid-moves-from-maze maze (maze-rb-state-used-cells state) (maze/rb--get-current state)))

(defun maze/rb--compute-new-stack (maze used-cells stack)
  (--drop-while (not (maze/rb--compute-valid-moves-from-maze maze used-cells it)) stack))

(defun maze/rb--backtrack (maze state)
  (let ((stack (maze-rb-state-stack state))
        (used-cells (maze-rb-state-used-cells state)))
    (make-maze-rb-state :used-cells used-cells
                        :stack (maze/rb--compute-new-stack maze used-cells stack))))

(defun maze/rb--push (state index)
  (setf (maze-rb-state-stack state) (cons index (maze-rb-state-stack state))))

(defun maze/rb--previous-leg (maze state)
  (--map (maze/index-to-position maze it) (-take 2 (maze-rb-state-stack state))))

(defun maze/rb--carve-passage (maze positions)
  (maze/carve-passage maze (car positions) (cadr positions)))

(defun maze/rb--next-move (maze state)
  (let ((next-index (maze/random-choice (maze/rb--valid-moves maze state))))
    (when next-index
      (maze/map-put next-index t (maze-rb-state-used-cells state))
      (maze/rb--push state next-index)
      (maze/rb--carve-passage maze (maze/rb--previous-leg maze state)))))

(defun maze/rb--next-path (maze state)
  (let ((new-maze))
    (while (setq new-maze (maze/rb--next-move maze state))
      (setq maze new-maze))
    maze))

(defun maze/rb--empty-stack? (state)
  (not (maze-rb-state-stack state)))

(defun maze/rb--create-path (maze state)
  (while (not (maze/rb--empty-stack? state))
    (setq maze (maze/rb--next-path maze state))
    (setq state (maze/rb--backtrack maze state)))
  maze)

(defun maze/rb--create-state (start)
  (let ((used-cells (maze/map-create)))
    (maze/map-put start t used-cells)
    (make-maze-rb-state :used-cells used-cells
                        :stack (list start))))

(defun maze/recursive-backtracker (columns rows)
  (let ((empty-maze (maze/create-empty columns rows)))
    (maze/rb--create-path empty-maze
                          (maze/rb--create-state (maze/random-cell empty-maze)))))

(provide 'maze-recursive-backtracker)
