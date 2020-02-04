(defun maze/map-create ()
  (make-hash-table))

(defun maze/map-copy (table)
  (copy-hash-table table))

(defun maze/map-put (key value table)
  (puthash key value table))

(defun maze/map-count (table)
  (hash-table-count table))

(defun maze/map-get (key table)
  (gethash key table))

(defun maze/map-del (key table)
  (remhash key table))

(defun maze/map-iterate (f table)
  (maphash f table))

(provide 'maze-map)
