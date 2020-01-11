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

;;; TODO/FIXME a lot of memory churning…
;;; TODO/FIXME long
(defun maze/--random-walk (maze current-index)
  "Return a random position from the current one"
  (apply #'maze/position-to-index
         (cons maze
               (cdr (maze/random-choice (apply #'maze/valid-neighbors
                                               (cons maze (maze/index-to-position maze current-index))))))))

;;; TODO/FIXME long
(defun maze/--wilson-path (maze used-cells start)
  "Return a path starting from path and ending in a used-cell

start must not be in the used-cells list"
  (if (gethash start used-cells) (error "Starting from an used cell!"))
  (maze/wilson-erase-loops
   (-unfold  (lambda (current-index)
               (if current-index
                   (let ((next-position (maze/--random-walk maze current-index)))
                     (if (gethash next-position used-cells)
                         (cons current-index next-position)
                       (cons current-index nil)))))
             start)))

(defun maze/--create-initial-exclusion-table (maze)
    (let ((maze-size (maze/get-cells-number maze))
        (used-cells (make-hash-table)))
    (puthash (/ maze-size 2) t used-cells)
    used-cells))

(defun all-keys (table)
  (let ((x))
    (maphash (lambda (k v) (setq x (cons k x))) table)
    x))

;;; TODO/FIXME the exclusion table makes the whole module non functional (it's not copied!)
(defun maze/--carve-wilson (maze)
  (let ((new-maze maze)
        (exclusion-table (maze/--create-initial-exclusion-table maze))
        (maze-size (maze/get-cells-number maze)))
    (while (< (hash-table-count exclusion-table) maze-size)
      (let ((new-path (maze/--wilson-path new-maze exclusion-table (maze/random-cell new-maze exclusion-table))))
        ;; TODO/FIXME add the path to both the exclusion table and carve the passages!
        (print (format "%s (%s)" new-path (all-keys exclusion-table)))
        (redisplay)
        (--each (-partition-in-steps 2 1 new-path)
          (setq new-maze (maze/carve-passage-index new-maze (car it) (cadr it))))
        (--each new-path (puthash it t exclusion-table))))
    new-maze))

(defun maze/wilson (columns rows)
  (maze/--carve-wilson (maze/create-empty columns rows)))

(provide 'maze-wilson)
