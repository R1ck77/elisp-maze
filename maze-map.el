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

(defun maze/map-to-list (table)
  (maze/hash--table-to-list table))

(defun maze/hash--table-to-list (table)
  "Return a list of (k . v) cons cells from a table"
  (if (hash-table-p table)
      (let ((result))
        (maphash (lambda (k v)
                   (setq result (cons (cons k v) result)))
                 table)
        result)))

(provide 'maze-map)
