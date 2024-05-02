(in-package #:elo)

(defun main ()
  nil)

(defun probability-a-wins (a-rating b-rating)
  (/ 1.0 (+ 1 (expt 10 (/ (- a-rating b-rating) 400)))))

(defun elo (&key (rating-1 2400) (rating-2 2400) (k 32) (did-1-win t))
  (let* ((probability-1-wins (probability-a-wins rating-1 rating-2))
         (probability-2-wins (- 1.0 probability-1-wins)))

    (list
     :rating-1 (+ rating-1 (* k (if did-1-win
                                       (- 1 probability-2-wins)
                                       (- 1 probability-1-wins))))
     :rating-2 (+ rating-2 (* k (if did-1-win
                                       (- probability-1-wins)
                                       (- probability-1-wins)))))))
