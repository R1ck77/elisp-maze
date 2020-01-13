(require 'maze-utils)

(defconst maze-player-char (string-to-char "@"))

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

;;; TODO/FIXME doesn't restore the text properties (which is not actually so bad, no matter how unintended)
(defun maze/walk--show-position (delay)
  (let ((current-char (char-after)))
    (delete-char 1 t)
    (insert-char maze-player-char)
    (forward-char -1)
    (sit-for delay)
    (delete-char 1)
    (insert-char current-char)
    (forward-char -1)))

(defun maze/walk-infinite-silly-random-walk (&optional delay)
  "Probably one of the most silly maze traversal algorithm ever written"
  (while t
    (goto-char (maze/random-choice (maze/walk-available-positions)))
    (maze/walk--show-position (or delay 0.1))))

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

(defun maze/walk-infinite-dumb-random-walk (&optional delay)
  "Improvement over the silly random walk that doesn't needlessy backtracks"
  (let ((previous-move))
    (while t
      (let ((current-move (maze/walk--random-non-backtracking-move (maze/walk-available-moves) previous-move)))
        (goto-char (cdr current-move))
        (setq previous-move current-move))
      (maze/walk--show-position (or delay 0.1)))))

(provide 'maze-walk)
