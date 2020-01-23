(require 'maze-walk)

(defconst maze/border-moves (list :up '(:right :up :left :down)
                                  :left '(:up :left :down :right)
                                  :right '(:down :right :up :left)
                                  :down '(:left :down :right :up))
  "List of move priorities for the border walk maze (counter clockwise)")

(defun maze/border--move (available-moves previous-move)
  (let ((priorities (or (if previous-move
                            (plist-get maze/border-moves (car previous-move))
                          (list :right :up :left :down))))
        (next-move))
    (while (and (not next-move) priorities)
      (let ((candidate (assq (car priorities) available-moves)))
        (setq priorities (rest priorities))
        (setq next-move candidate)))
    next-move))

(cl-defstruct maze--border-state
  direction-move
  previous-move
  n-moves stop)

(defun maze/border--next-move (previous-move)
  (maze/border--move (maze/walk-available-moves) previous-move))

(defun maze/border--make-first-state (first-direction-move)
  "Create the first state with just the first direction"
  (make-maze--border-state :direction-move first-direction-move
                           :previous-move nil
                           :n-moves 0
                           :stop nil))

(defun maze/border--make-stop-state (n-moves)
  "Create the state that signals that there are no more moves"
  (make-maze--border-state :direction-move nil
                           :previous-move nil
                           :n-moves n-moves
                           :stop t))

(defun maze/border--next-move-state (state)
  "Return a new state with a new (possibly nil) move"
  (make-maze--border-state :direction-move (maze/border--next-move (maze--border-state-direction-move state))
                           :previous-move (maze--border-state-direction-move state)
                           :n-moves (1+ (maze--border-state-n-moves state))
                           :stop nil))

(defun maze/border--evolve-state (state)
  "Return the new state (which may be a stop)"
  (if (not (maze--border-state-direction-move state))
      (maze/border--make-stop-state (maze--border-state-n-moves state))
    (progn
      (goto-char (cdr (maze--border-state-direction-move state)))
      (maze/border--next-move-state state))))

(defun maze/border--same-move (border-walk-state-a border-walk-state-b)
  (equal (maze--border-state-direction-move border-walk-state-a)
         (maze--border-state-direction-move border-walk-state-b)))

(defun maze/border-walk (delay)
  "Walk the maze always keeping a hand on the wall"
  (let* ((first-state (maze/border--make-first-state (maze/border--next-move nil))))
    (setq state first-state)
    (when (maze--border-state-direction-move first-state)
      (maze/until (or (maze--border-state-stop state)
                      (maze/border--same-move state first-state))
        (setq state (maze/border--evolve-state state))
        (maze/walk--show-position delay)))
    (message "Maze traversed in %d moves" (maze--border-state-n-moves state))))

(provide 'maze-border-walk)
