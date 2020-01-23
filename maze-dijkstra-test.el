(require 'maze-dijkstra)
(require 'maze-binary)
(require 'maze-ascii)
(require 'buttercup)

(describe "maze-dijkstra"
  (describe "maze/dijkstra-measure"
    (it "finishes, eventually"
      (with-temp-buffer
        (spy-on 'message)
        (insert (maze/to-ASCII (maze/binary 8 4)))
        (goto-char (point-min))
        (forward-line 1)
        (forward-char 1)
        (maze/dij-compute-dijkstra (point))
        (expect 'message :to-have-been-called-times 1)))))
