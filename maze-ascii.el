(require 'maze-map)
(require 'maze-data)
(require 'maze-utils)

;; (defconst maze/cross-char "┼")      ; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
;; (defconst maze/horizontal-char "─") ; BOX DRAWINGS LIGHT HORIZONTAL
;; (defconst maze/vertical-char "│")   ; BOX DRAWINGS LIGHT VERTICAL
;; (defconst maze/empty-char " ")      ; SPACE (duh)
;; (defconst maze/newline "\n")

(defconst maze/background-color "white smoke")
(defconst maze/foreground-color "red")
(defconst maze/full-block "▒")
(put-text-property 0 1 'font-lock-face (list :foreground maze/foreground-color
                                             :background maze/background-color)
                   maze/full-block)
(defconst maze/cross-char maze/full-block)
(defconst maze/horizontal-char maze/full-block)
(defconst maze/vertical-char maze/full-block)
(defconst maze/empty-char " ")
(put-text-property 0 1 'font-lock-face (list :foreground maze/foreground-color
                                             :background maze/background-color)
                   maze/empty-char)
(defconst maze/newline "\n")

(defun maze/has-top-passage? (maze index)
  (let ((connection (cons (- index (maze-columns maze)) index)))
    (maze/map-get connection (maze-connections maze))))

(defun maze/has-left-passage? (maze index)
  (let ((connection (cons (1- index) index)))
    (maze/map-get connection (maze-connections maze))))

(defun maze/cell-to-ASCII (maze column row)
  (let ((this-index (maze/position-to-index maze (list column row))))
    (list (list maze/cross-char (if (maze/has-top-passage? maze this-index)
                                    maze/empty-char
                                  maze/horizontal-char))
          (list (if (maze/has-left-passage? maze this-index)
                    maze/empty-char
                  maze/vertical-char)
                maze/empty-char))))

(defun maze/row--to-ASCII-list (maze row)
  (--reduce-from (cons it acc) (list (list (list maze/cross-char maze/newline) (list maze/vertical-char maze/newline)))
                 (--map (maze/cell-to-ASCII maze it row)
                        (number-sequence (1- (maze-columns maze)) 0 -1))))

(defun maze/ASCII--list-to-lines (ascii-row)
  "shuffle the data returned by row-to-ASCII in two proper text rows"
  (let ((first-line "")
        (second-line ""))
    (--each ascii-row (progn
                        (setq first-line (apply #'concat (cons first-line (car it))))
                        (setq second-line (apply #'concat (cons second-line (cadr it))))))
    (list first-line second-line)))

(defun maze/row-to-ASCII-lines (maze row)
  (maze/ASCII--list-to-lines (maze/row--to-ASCII-list maze row)))

(defun maze/bottom-ASCII (maze)
  (--reduce-from  (concat maze/cross-char maze/horizontal-char acc)
                  (concat maze/cross-char maze/newline)
                  (number-sequence 0 (1- (maze-columns maze)))))

(defun maze/to-ASCII-lines (maze)
  "Return a string representing the maze in crude ASCII art."
  (append (--mapcat (maze/row-to-ASCII-lines maze it)
                    (number-sequence 0 (1- (maze-rows maze))))
          (list (maze/bottom-ASCII maze))))

(defun maze/to-ASCII (maze)
  (--reduce-from (concat acc it) "" (maze/to-ASCII-lines maze)))

(provide 'maze-ascii)
