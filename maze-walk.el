(require 'maze-data)
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

(defun maze/walk--row-move-available (index n)
  (maze/save-point
   (goto-char index)
   (let ((current-char (char-after index)))
     (maze/walk-move-line n)
     (if (eq (char-after) current-char)
         (point)))))

(defun maze/walk--top-move-available (index)
  (maze/walk--row-move-available index -1))

(defun maze/walk--bottom-move-available (index)
  (maze/walk--row-move-available index 1))

(defun maze/walk--column-move-available (index n)
  (maze/save-point
   (goto-char index)
   (let ((current-char (char-after index)))
     (forward-char n)
     (if (eq (char-after) current-char)
         (point)))))

(defun maze/walk--left-move-available (index)
  (maze/walk--column-move-available index -1))

(defun maze/walk--right-move-available (index)
  (maze/walk--column-move-available index 1))

(defun maze/walk-available-positions (&optional index)
  "Return a list of available positions as point coordinates

Works on the assumption that the maze has boundaries (otherwise
the function would fail when invoked on a line start)"
  (let ((index (or index (point))))
    (--filter it (--map (funcall it index) (list #'maze/walk--top-move-available
                                                 #'maze/walk--right-move-available
                                                 #'maze/walk--bottom-move-available
                                                 #'maze/walk--left-move-available)))))

(defun maze/walk-available-moves ()
  "Return a list of available moves as an association list of coordinates

Works on the assumption that the maze has boundaries (otherwise
the function would fail when invoked on a line start)"
  (--filter (cdr it)
            (--map (cons (car it) (funcall (cdr it) (point)))
                   (list (cons :up #'maze/walk--top-move-available)
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

(provide 'maze-walk)
