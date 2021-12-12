(ns day12)

(def test-sample
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(let [debug? true
      input (->> (if debug? test-sample (slurp "input12.txt"))
                 )]
  #_eof)
