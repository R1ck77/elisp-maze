(require 'dash)
(require 'maze-data)
(require 'maze-utils)

(defun maze/wilson-erase-last-loop (path index)
  (--drop-while (/= it index) path))

(defun maze/wilson-erase-loops (path)
  "Return a path with loops erased"
  (let ((tmp))
    (--each path (if (seq-contains tmp it)
                     (setq tmp (maze/wilson-erase-last-loop tmp it))
                   (progn
                     (setq tmp (cons it tmp)))))
    (nreverse tmp)))

(defun maze/--random-walk (maze current-index)
  "Return a random position from the current one"
  (let ((legal-moves (maze/valid-neighbors maze (maze/index-to-position maze current-index))))
    (maze/position-to-index maze (maze/random-choice legal-moves))))

(defun maze/--unfold-keep-going (next-value next-seed)
  "Return value for unfold after a generic non visited cell has been added"
  (cons next-value (cons next-seed nil)))

(defun maze/--unfold-reached-visited-cell (next-value next-seed)
  "Return value for unfold after a visited cell has been reached

The next result will be the last"
  (cons next-value (cons next-seed t)))

(defun maze/--unfold-stop-after-next-insertion (next-value-to-insert)
  "Add the current (visited) cell and then stop the next iteration"
  (cons next-value-to-insert '(nil . nil)))

(defun maze/--looped-path-unfold (maze used-cells current-index is-end-cell)
  "Unfold a new looped path that connects a starting cell with one of (possibly many) visited cells"
  (if current-index
      (if is-end-cell
          (maze/--unfold-stop-after-next-insertion current-index)
        (let ((next-position (maze/--random-walk maze current-index)))
          (if (gethash next-position used-cells)
              (maze/--unfold-reached-visited-cell current-index next-position)
            (maze/--unfold-keep-going current-index next-position))))))

(defun maze/--looped-path (maze used-cells start)
  (append (-unfold  (lambda (seed)
                      (maze/--looped-path-unfold maze
                                                 used-cells
                                                 (car seed) (cdr seed)))
                    (cons start nil))))

(defun maze/--wilson-path (maze used-cells start)
  "Return a path starting from path and ending in a used-cell

start must not be in the used-cells list"
  (if (gethash start used-cells)
      (error "Starting from an used cell!"))
  (maze/wilson-erase-loops (maze/--looped-path maze used-cells start)))

(defun maze/--create-initial-exclusion-table (maze)
    (let ((maze-size (maze/get-cells-number maze))
        (used-cells (make-hash-table)))
    (puthash (/ maze-size 2) t used-cells)
    used-cells))

(defun maze/--carve-wilson (maze)
  (let ((new-maze maze)
        (exclusion-table (maze/--create-initial-exclusion-table maze))
        (maze-size (maze/get-cells-number maze)))
    (while (< (hash-table-count exclusion-table) maze-size)
      (let ((new-path (maze/--wilson-path new-maze exclusion-table (maze/random-cell new-maze exclusion-table))))
        (--each (-partition-in-steps 2 1 new-path)
          (setq new-maze (maze/carve-passage-index new-maze (car it) (cadr it))))
        (--each new-path (puthash it t exclusion-table))))
    new-maze))

(defun maze/wilson (columns rows)
  (maze/--carve-wilson (maze/create-empty columns rows)))

(provide 'maze-wilson)
