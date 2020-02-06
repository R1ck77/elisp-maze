(require 'maze-data)
(require 'maze-utils)
(require 'maze-walk)
(require 'dash)

(defconst maze/dij-property-name :maze-dijkstra)

;;; Use a functional (but slow approach, due to GC) in handling the state
(defvar maze/dij-immutable-state nil)

;;; Show the algorithm as it recurses the maze. VERY slow
(defconst maze/dij-debug-progression nil)

(defun maze/dij-copy-state (state)
  (make-maze-dij-state :visited (maze/map-copy (maze-dij-state-visited state))
                       :scored (maze/map-copy (maze-dij-state-scored state))
                       :current (maze-dij-state-current state)))

(defun maze/dij--optional-copy-state (state)
  "Copy or return the same state, depending on the approach defined in maze/dij-immutable-state"
  (if maze/dij-immutable-state
      (maze/dij-copy-state state)
    state))

(cl-defstruct maze-dij-state
  visited
  scored
  current)

(defun maze/dij-new-state (current)
  (let ((scored (maze/map-create)))
    (maze/map-put current 0 scored)
    (make-maze-dij-state :visited (maze/map-create)
                         :scored scored
                         :current current)))

(defun maze/dij-format-state (state)
  (format "Current: %d\nVisited: %s\nScored: %s"
          (maze-dij-state-current state)
          (maze/map-to-list (maze-dij-state-visited state))
          (maze/map-to-list (maze-dij-state-scored state))))

(defun maze/dij-log-state (state &optional format)
  (message (or format "%s") (maze/dij-format-state state)))

(defun maze/dij-frontier-present? (state)
  (/= (maze/map-count (maze-dij-state-scored state)) 0))

(defun maze/dij-get-frontier (state)
  (maze/map-to-list (maze-dij-state-scored state)))

(defun maze/dij-update-current (state new-current)
  (let ((new-state (maze/dij--optional-copy-state state)))
    (setf (maze-dij-state-current new-state) new-current)
    new-state))

(defun maze/dij-state-visit-current (state)
  "End of the current neighbors evaluation: mark the current index

Update the data structures"
  (let ((new-state (maze/dij--optional-copy-state state))
        (current (maze-dij-state-current state)))
    (let ((scored-neighbors (maze-dij-state-scored new-state)))
      (let ((score (maze/map-get current scored-neighbors)))
        (maze/map-put current score (maze-dij-state-visited new-state))
        (maze/map-del current scored-neighbors)))
    new-state))

(defun  maze/dij-state-get-current-score (state)
  (let ((current (maze-dij-state-current state)))
    (maze/map-get current (maze-dij-state-visited state))))

(cl-defstruct maze-dij-property
  visited
  score
  id)

(defun maze/dij-set-score (id score)
  (make-maze-dij-property :visited nil
                          :score score
                          :id id))

(defun maze/dij-update-maze-score (id current-property score-candidate)
  (if current-property
      (let ((old-score (maze-dij-property-score current-property)))
        (if (< score-candidate old-score)
            (maze/dij-set-score id score-candidate)
          current-property))
    (maze/dij-set-score id score-candidate)))

(defun maze/map--get-property (point)
  (get-text-property point maze/dij-property-name))

(defun maze/map-get-current-id (point)
  (maze-dij-property-id (maze/map--get-property point)))

(defun maze/map-safe-get-property (id point)
  (let ((property (maze/map--get-property point)))
    (if (and property (eq id (maze-dij-property-id property)))
        property)))

(defun maze/dij-set-property (index property)
  (put-text-property index (1+ index)
                     maze/dij-property-name property))

(defun maze/dij--score-from-property (id property)
  "nil means infinite"
  (if property 
      (maze-dij-property-score property)))

(defun maze/dij-score-at-point (id point)
  "nil means infinite"
  (maze/dij--score-from-property id (maze/map-safe-get-property id point)))

(defun maze/dij-debug-mark-with-color (index color)
  (put-text-property index (1+ index)
                     'face (list :background color)))

(defun maze/dij-debug-mark-all-with-color (indices color)
  (maze/map-iterate (lambda (it unused)
                      (maze/dij-debug-mark-with-color it color)) indices)
  (redisplay))

(defun maze/dij--update-score-at-point (id index other-score)
  (let ((new-property (maze/dij-update-maze-score id
                                                  (maze/map-safe-get-property id index)
                                                  (1+ other-score))))
    (maze/dij-set-property index new-property)
    new-property))

(defun maze/dij--unvisited-neighbors (point visited-positions)
  (--filter (not (maze/map-get it visited-positions)) (maze/walk-available-positions point)))

(defun maze/dij--set-new-score-on-neighbor! (id mutable-state current-score neighbor)
  (let ((new-property (maze/dij--update-score-at-point id neighbor current-score)))    
    (maze/map-put neighbor (maze/dij--score-from-property id new-property) (maze-dij-state-scored mutable-state))))

(defun maze/dij--score-neighbors (id state)
  (let ((unvisited-neighbors (maze/dij--unvisited-neighbors (maze-dij-state-current state) (maze-dij-state-visited state)))
        (this-score (or (maze/dij-score-at-point id (maze-dij-state-current state)) 0))
        (new-state (maze/dij--optional-copy-state state)))
    (--each unvisited-neighbors (maze/dij--set-new-score-on-neighbor! id new-state this-score it))
    new-state))

(defun maze/dij-create-visited (id score)
  (make-maze-dij-property :visited t
                          :score score
                          :id id))

(defun maze/dij--mark-current-visited (id state)
  (let ((new-state (maze/dij-state-visit-current state)))
    (maze/dij-set-property (maze-dij-state-current new-state)
                           (maze/dij-create-visited id (maze/dij-state-get-current-score new-state)))
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
  (message "Score: %s" (or (maze/dij-score-at-point (maze/map-get-current-id (point))
                                                    (point)) :inf)))

(defun maze/dij--debug-draw (state)
  (when maze/dij-debug-progression
    (maze/dij-debug-mark-all-with-color (maze-dij-state-visited state) "blue")
    (maze/dij-debug-mark-all-with-color (maze-dij-state-scored state) "dark grey")))

(defun maze/dij-compute-dijkstra-for-result (start)
  (let ((id start)
        (state (maze/dij-new-state start)))
    (maze/with-undo-disabled
      (while (maze/dij-frontier-present? state)
        (setq state (maze/dij--select-next-node state))
        (maze/dij--debug-draw state)
        (setq state (maze/dij--score-neighbors id state))
        (setq state (maze/dij--mark-current-visited id state))))
    state))

(defun maze/dij-compute-dijkstra (start)
  (let* ((maze/dij-debug-progression t)
         (last-state (maze/dij-compute-dijkstra-for-result start)))
    (maze/dij-debug-mark-with-color (maze-dij-state-current last-state) "red"))
  (message "Dijkstra algorithm terminated"))

(provide 'maze-dijkstra)
