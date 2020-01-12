(require 'maze-utils)

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
  (--filter (cdr it) (--map (cons (car it) (funcall (cdr it))) (list (cons :top #'maze/walk--top-move-available)
                                                                 (cons :right #'maze/walk--right-move-available)
                                                                 (cons :bottom #'maze/walk--bottom-move-available)
                                                                 (cons :left #'maze/walk--left-move-available)))))

(defun maze/walk-infinite-dumb-random-walk (&optional delay)
  "Probably the most dumb maze traversal algorithm ever written"
  (while t
    (goto-char (maze/random-choice (maze/walk-available-positions)))
    (let ((current-char (char-after)))
      (delete-char 1 t)
      (insert-char (string-to-char "@"))
      (forward-char -1)
      (sit-for (or delay 0.1))
      (delete-char 1)
      (insert-char current-char)
      (forward-char -1))))

(provide 'maze-walk)
