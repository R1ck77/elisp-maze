(require 'maze-data)
(require 'maze-utils)
(require 'maze-walk)
(require 'dash)

(defconst maze/dij-property-name :maze-dijkstra)

(cl-defstruct maze-dij-state
  visited
  scored
  current)

(defun maze/dij-new-state (current)
  (let ((scored (make-hash-table)))
    (puthash current 0 scored)
    (make-maze-dij-state :visited (make-hash-table)
                         :scored scored
                         :current current)))

(defun maze/dij-format-state (state)
  (format "Current: %d\nVisited: %s\nScored: %s"
          (maze-dij-state-current state)
          (maze/hash-table-to-list (maze-dij-state-visited state))
          (maze/hash-table-to-list (maze-dij-state-scored state))))

(defun maze/dij-log-state (state &optional format)
  (message (or format "%s") (maze/dij-format-state state)))

(defun maze/dij-frontier-present? (state)
  (/= (hash-table-count (maze-dij-state-scored state)) 0))

(defun maze/dij-get-frontier (state)
  (maze/hash-table-to-list (maze-dij-state-scored state)))

(defun maze/dij-update-current (state new-current)
  (make-maze-dij-state :visited (copy-hash-table (maze-dij-state-visited state))
                       :scored (copy-hash-table (maze-dij-state-scored state))
                       :current new-current))

(defun maze/dij-copy-state (state)
  (make-maze-dij-state :visited (copy-hash-table (maze-dij-state-visited state))
                       :scored (copy-hash-table (maze-dij-state-scored state))
                       :current (maze-dij-state-current state)))

(defun maze/dij-state-visit-current (state)
  "End of the current neighbors evaluation: mark the current index

Update the data structures"
  (let ((new-state (maze/dij-copy-state state))
        (current (maze-dij-state-current state)))
    (let ((scored-neighbors (maze-dij-state-scored new-state)))
      (let ((score (gethash current scored-neighbors)))
        (puthash current score (maze-dij-state-visited new-state))
        (remhash current scored-neighbors)))
    new-state))

(defun  maze/dij-state-get-current-score (state)
  (let ((current (maze-dij-state-current state)))
    (gethash current (maze-dij-state-visited state))))

(cl-defstruct maze-dij-property
  visited
  score)

(defun maze/dij-set-score (score)
  (make-maze-dij-property :visited nil
                          :score score))

(defun maze/dij-update-maze-score (current-property score-candidate)
  (if current-property
      (let ((old-score (maze-dij-property-score current-property)))
        (if (< score-candidate old-score)
            (maze/dij-set-score score-candidate)
          current-property))
    (maze/dij-set-score score-candidate)))

(defun maze/dij--get-property (point)
  (get-text-property point maze/dij-property-name))

(defun maze/dij-set-property (index property)
  (put-text-property index (1+ index)
                     maze/dij-property-name property))

(defun maze/dij--score-from-property (property)
  "nil means infinite"
  (if property
      (maze-dij-property-score property)))

(defun maze/dij-score-at-point (point)
  "nil means infinite"
  (maze/dij--score-from-property (maze/dij--get-property point)))

(defun maze/dij-debug-mark-with-color (index color)
  (put-text-property index (1+ index)
                     'font-lock-face (list :background color)))

(defun maze/dij-debug-mark-all-with-color (indices color)
  (maphash (lambda (it unused) (maze/dij-debug-mark-with-color it color)) indices)
  (redisplay))

(defun maze/dij--update-score-at-point (index other-score)
  (let ((new-property (maze/dij-update-maze-score (maze/dij--get-property index)
                                                  (1+ other-score))))
    (maze/dij-set-property index new-property)
    new-property))

(defun maze/dij--unvisited-neighbors (point visited-positions)
  (--filter (not (gethash it visited-positions)) (maze/walk-available-positions point)))

(defun maze/dij--set-new-score-on-neighbor! (mutable-state current-score neighbor)
  (let ((new-property (maze/dij--update-score-at-point neighbor current-score)))    
    (puthash neighbor (maze/dij--score-from-property new-property) (maze-dij-state-scored mutable-state))))

(defun maze/dij--score-neighbors (state)
  (let ((unvisited-neighbors (maze/dij--unvisited-neighbors (maze-dij-state-current state) (maze-dij-state-visited state)))
        (this-score (or (maze/dij-score-at-point (maze-dij-state-current state)) 0))
        (new-state (maze/dij-copy-state state)))
    (--each unvisited-neighbors (maze/dij--set-new-score-on-neighbor! new-state this-score it))
    new-state))

(defun maze/dij-create-visited (score)
  (make-maze-dij-property :visited t
                          :score score))

(defun maze/dij--mark-current-visited (state)
  (let ((new-state (maze/dij-state-visit-current state)))
    (maze/dij-set-property (maze-dij-state-current new-state)
                           (maze/dij-create-visited (maze/dij-state-get-current-score new-state)))
    new-state))

(defun maze/dij--compare-score (a b)
  "Returns positive if a > b, negative if b < a and 0 otherwise

Throws if both values are :inf, as this situation should never happen"
  (cond
   ((and (eq a :inf) (eq b :inf)) (error "comparison between infinites shouldn't happen"))
   ((eq a :inf) t)
   ((eq b :inf) nil)
   ((= a b) nil)
   ((> a b) t)
   (t nil)))

(defun maze/dij--select-next-node-from-list (scored-list)
  (car (--min-by (maze/dij--compare-score (cdr it) (cdr other))
                 scored-list)))

(defun maze/dij--select-next-node (state)
  (maze/dij-update-current state
                           (maze/dij--select-next-node-from-list (maze/dij-get-frontier state))))

(defun current-score-hook ()
  (message "Score: %s" (or (maze/dij-score-at-point (point)) :inf)))

(defun maze/dij-compute-dijkstra (start)
  (buffer-disable-undo)
  (let ((state (maze/dij-new-state start)))
    (while (maze/dij-frontier-present? state)
      (setq state (maze/dij--select-next-node state))
      (maze/dij-debug-mark-all-with-color (maze-dij-state-visited state) "blue")
      (maze/dij-debug-mark-all-with-color (maze-dij-state-scored state) "dark grey")
      (maze/dij-debug-mark-with-color (maze-dij-state-current state) "red")
      (setq state (maze/dij--score-neighbors state))
      (setq state (maze/dij--mark-current-visited state)))
    (message "Dijkstra algorithm terminated"))
  (buffer-enable-undo))

(provide 'maze-dijkstra)
