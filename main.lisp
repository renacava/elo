(in-package #:elo)



(defun main ()
  nil)

(defun probability (rating-1 rating-2)
  (/ 1.0 (+ 1 (* 1.0 (expt 10 (/ (* 1.0 (- rating-1 rating-2)) 400))))))

(defun elo (&key (rating-1 2400) (rating-2 2400) (k 32) (did-1-win t))
  (let* ((probability-1-wins (probability rating-1 rating-2))
         (probability-2-wins (- 1.0 probability-1-wins)))
    (if did-1-win
        (progn
          (incf rating-1 (* k (- 1 probability-2-wins)))
          (incf rating-2 (* k (- 0 probability-1-wins))))
        (progn
          (incf rating-1 (* k (- 1 probability-1-wins)))
          (incf rating-2 (* k (- 0 probability-2-wins)))))

    (format t "Updated ratings:~%rating-1 = ~a~%rating-2 = ~a" rating-1 rating-2)))

;; (1.0 * 1.0) / (1 + 1.0 * Math.pow(10, (1.0 * (rating1 - rating2)) / 400))
