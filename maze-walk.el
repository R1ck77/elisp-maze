(require 'maze-utils)

(defconst maze-player-char (string-to-char "@"))

(defconst maze/traversed-color "green")

(defmacro maze/save-point (&rest forms)
  (let ((pos (make-symbol "pos"))
        (result (make-symbol "result")))
    `(let ((,pos (point))
           (,result (progn ,@forms)))
       (goto-char ,pos)
       ,result)))

(defun maze/walk-move-line (N)
  (let ((column (current-column)))
    (forward-line N)
    (forward-char column)))

(defun maze/walk--row-move-available (n)
  (maze/save-point
   (let ((current-char (char-after)))
     (maze/walk-move-line n)
     (if (eq (char-after) current-char)
         (point)))))

(defun maze/walk--top-move-available ()
  (maze/walk--row-move-available -1))

(defun maze/walk--bottom-move-available ()
  (maze/walk--row-move-available 1))

(defun maze/walk--column-move-available (n)
  (maze/save-point
   (let ((current-char (char-after)))
     (forward-char n)
     (if (eq (char-after) current-char)
         (point)))))

(defun maze/walk--left-move-available ()
  (maze/walk--column-move-available -1))

(defun maze/walk--right-move-available ()
  (maze/walk--column-move-available 1))

(defun maze/walk-available-positions ()
  "Return a list of available positions as point coordinates

Works on the assumption that the maze has boundaries (otherwise
the function would fail when invoked on a line start)"
  (--filter it (--map (funcall it) (list #'maze/walk--top-move-available
                                         #'maze/walk--right-move-available
                                         #'maze/walk--bottom-move-available
                                         #'maze/walk--left-move-available))))

(defun maze/walk-available-moves ()
  "Return a list of available moves as an association list of coordinates

Works on the assumption that the maze has boundaries (otherwise
the function would fail when invoked on a line start)"
  (--filter (cdr it) (--map (cons (car it) (funcall (cdr it))) (list (cons :up #'maze/walk--top-move-available)
                                                                     (cons :right #'maze/walk--right-move-available)
                                                                     (cons :down #'maze/walk--bottom-move-available)
                                                                     (cons :left #'maze/walk--left-move-available)))))

(defun maze/walk--show-position (delay)
  (let ((current-char (char-after)))
    (delete-char 1 t)
    (insert-char maze-player-char)
    (forward-char -1)
    (sleep-for delay)
    (redisplay)
    (delete-char 1)
    (insert-char current-char)
    (forward-char -1)
    (put-text-property (point) (1+ (point)) 'font-lock-face (list :background maze/traversed-color))))

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

(defconst maze/walk-border-moves (list :up '(:right :up :left :down)
                                       :left '(:up :left :down :right)
                                       :right '(:down :right :up :left)
                                       :down '(:left :down :right :up))
  "List of move priorities for the border walk maze (counter clockwise)")

(defun maze/walk--border-move (available-moves previous-move)
  (let ((priorities (or (if previous-move
                            (plist-get maze/walk-border-moves (car previous-move))
                          (list :right :up :left :down))))
        (next-move))
    (while (and (not next-move) priorities)
      (let ((candidate (assq (car priorities) available-moves)))
        (setq priorities (rest priorities))
        (setq next-move candidate)))
    next-move))

(cl-defstruct maze--walk-state
  direction-move
  previous-move
  n-moves stop)

(defun maze/walk--next-move (previous-move)
  (maze/walk--border-move (maze/walk-available-moves) previous-move))

(defun maze/walk-border--make-first-state (first-direction-move)
  "Create the first state with just the first direction"
  (make-maze--walk-state :direction-move first-direction-move
                         :previous-move nil
                         :n-moves 0
                         :stop nil))

(defun maze/walk-border--make-stop-state (n-moves)
  "Create the state that signals that there are no more moves"
  (make-maze--walk-state :direction-move nil
                         :previous-move nil
                         :n-moves n-moves
                         :stop t))

(defun maze/walk-border--next-move-state (state)
  "Return a new state with a new (possibly nil) move"
  (make-maze--walk-state :direction-move (maze/walk--next-move (maze--walk-state-direction-move state))
                         :previous-move (maze--walk-state-direction-move state)
                         :n-moves (1+ (maze--walk-state-n-moves state))
                         :stop nil))

(defun maze/walk-border--evolve-state (state)
  "Return the new state (which may be a stop)"
  (if (not (maze--walk-state-direction-move state))
      (maze/walk-border--make-stop-state (maze--walk-state-n-moves state))
    (progn
      (goto-char (cdr (maze--walk-state-direction-move state)))
      (maze/walk-border--next-move-state state))))

(defun maze/walk-border--same-move (border-walk-state-a border-walk-state-b)
  (equal (maze--walk-state-direction-move border-walk-state-a)
         (maze--walk-state-direction-move border-walk-state-b)))

(defun maze/walk-border-walk (delay)
  "Walk the maze always keeping a hand on the wall"
  (let* ((first-state (maze/walk-border--make-first-state (maze/walk--next-move nil))))
    (setq state first-state)
    (when (maze--walk-state-direction-move first-state)
      (maze/until (or (maze--walk-state-stop state)
                      (maze/walk-border--same-move state first-state))
        (setq state (maze/walk-border--evolve-state state))
        (maze/walk--show-position delay)))
    (message "Maze traversed in %d moves" (maze--walk-state-n-moves state))))

(provide 'maze-walk)
