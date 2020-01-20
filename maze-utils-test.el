(require 'maze-utils)
(require 'buttercup)

(defun create-set-with (integers)
  (let ((table (make-hash-table)))
    (--each integers
      (puthash it t table))
    table))

(describe "maze-utils"
  (describe "maze/random-cell"
    (it "returns the last non occupied cell"
      (let ((exclusion-table (create-set-with (number-sequence 0 399))))
        (remhash 42 exclusion-table)
        (expect (maze/random-cell (maze/create-empty 20 20)
                                  exclusion-table)
                :to-be 42)))
    (it "returns nil if the maze is already occupied"
      (expect (maze/random-cell (maze/create-empty 20 20)
                                (create-set-with (number-sequence 0 399)))
              :to-be nil)))
  (describe "maze/valid-moves"
    (it "returns the correct value in a generic cell"
      (expect (maze/valid-moves (maze/create-empty 10 10) 6 4)
              :to-have-same-items-as (list (list :top 6 3)
                                           (list :right 7 4)
                                           (list :bottom 6 5)
                                           (list :left 5 4))))
    (it "returns the correct cells in the corners"
      (expect (maze/valid-moves (maze/create-empty 10 10) 0 0)
              :to-have-same-items-as (list (list :bottom 0 1)
                                           (list :right 1 0)))
      (expect (maze/valid-moves (maze/create-empty 10 10) 0 9)
              :to-have-same-items-as (list (list :top 0 8)
                                           (list :right 1 9)))
      (expect (maze/valid-moves (maze/create-empty 10 10) 9 9)
              :to-have-same-items-as (list (list :top 9 8)
                                           (list :left 8 9)))
      (expect (maze/valid-moves (maze/create-empty 10 10) 9 0)
              :to-have-same-items-as (list (list :bottom 9 1)
                                           (list :left 8 0)))))
  (describe "maze/valid-neighbors"
    (it "returns the correct value in a generic cell"
      (expect (maze/valid-neighbors (maze/create-empty 10 10) '(6 4))
              :to-have-same-items-as (list (list 6 3)
                                           (list 7 4)
                                           (list 6 5)
                                           (list 5 4))))
    (it "returns the correct cells in the corners"
      (expect (maze/valid-neighbors (maze/create-empty 10 10) '(0 0))
              :to-have-same-items-as (list (list 0 1)
                                           (list 1 0)))
      (expect (maze/valid-neighbors (maze/create-empty 10 10) '(0 9))
              :to-have-same-items-as (list (list 0 8)
                                           (list 1 9)))
      (expect (maze/valid-neighbors (maze/create-empty 10 10) '(9 9))
              :to-have-same-items-as (list (list 9 8)
                                           (list 8 9)))
      (expect (maze/valid-neighbors (maze/create-empty 10 10) '(9 0))
              :to-have-same-items-as (list (list 9 1)
                                           (list 8 0)))))
  (describe "maze/until"
    (it "executes at least once"
      (let ((counter 0))
        (maze/until (= counter 1)
          (setq counter (1+ counter)))
        (expect counter :to-be 1)))
    (it "evaluates the predicate  at the end of each cycle"
      (let ((counter 0)
            (predicate-evaluations 0))
        (maze/until (progn
                      (setq predicate-evaluations (1+ predicate-evaluations))
                      (= counter 5))
          (setq counter (1+ counter)))
        (expect counter :to-be 5)
        (expect predicate-evaluations :to-be 5)))
    (it "returns the value of the last evaluation"
      (let* ((counter 0)
             (result (maze/until (= counter 5)
                       (setq counter (1+ counter))
                       (+ counter 12))))
        (expect result :to-be 17))))
  (describe "maze/comment"
    (it "can be used without arguments"
      (expect (maze/comment) :not :to-throw 'error))
    (it "just ignores whatever put into it, as long as it's valid syntax"
      (spy-on 'print)
      (maze/comment (print "a") (print "b"))
      (expect 'print :not :to-have-been-called))))
