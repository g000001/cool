(in-package :cool.pcl)

(iterate ((e in '(a b c d))) 
  (collect e))
;=>  (A B C D)

(iterate ((e on '(a b c d))) 
  (collect e)) 
;=>  ((A B C D) (B C D) (C D) (D))

(defun permutations (elements length)
  (if (= length 1)
      (iterate ((x in elements)) (collect (list x)))
      (let ((sub-permutations (permutations elements (- length 1))))
        (iterate ((x in elements))
          (join (iterate ((y in sub-permutations))
                  (collect (cons x y))))))))


(permutations '(1 2 3) 3)
