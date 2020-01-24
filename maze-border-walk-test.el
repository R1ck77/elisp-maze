(require 'maze-border-walk)
(require 'maze-binary)
(require 'maze-ascii)
(require 'buttercup)

(describe "maze-border-walk"
  (describe "maze/border-walk"
    (it "finishes, eventually"
      (with-temp-buffer
        (spy-on 'message)
        (insert (maze/to-ASCII (maze/binary 8 4)))
        (goto-char (point-min))
        (forward-line 1)
        (forward-char 1)
        (maze/walk-border-walk 0)
        (expect 'message :to-have-been-called-times 1)
        (expect (spy-calls-args-for 'message 0)
                :to-equal '("Maze traversed in %d moves" 124))))
    (it "executes from the center of the maze too"
      (with-temp-buffer
        (spy-on 'message)
        (insert (maze/to-ASCII (maze/binary 8 4)))
        (goto-char (point-min))
        (forward-line (+ 1 2))
        (forward-char (+ 1 8))
        (maze/walk-border-walk 0)
        (expect 'message :to-have-been-called-times 1)
        (expect (spy-calls-args-for 'message 0)
                :to-equal '("Maze traversed in %d moves" 124))))))
