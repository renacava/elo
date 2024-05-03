>(in-package #:elo)

(defparameter rankings-table (make-hash-table :test #'equal))

(defun main ()
  nil)

(defun probability-a-wins (a-rating b-rating)
  (/ 1.0 (+ 1 (expt 10 (/ (- a-rating b-rating) 400)))))

(defun elo (&key (rating-1 400) (rating-2 400) (k 32) (did-1-win t))
  (let* ((probability-1-wins (probability-a-wins rating-1 rating-2))
         (probability-2-wins (- 1.0 probability-1-wins)))

    (list
     :rating-1 (+ rating-1 (* k (if did-1-win
                                    (- 1 probability-2-wins)
                                    (- probability-2-wins))))
     :rating-2 (+ rating-2 (* k (if did-1-win
                                    (- probability-1-wins)
                                    (- 1 probability-1-wins)))))))

(defun elo-match (winner-rating loser-rating &key (k 32))
  (let* ((probability-1-wins (probability-a-wins winner-rating loser-rating))
         (probability-2-wins (- 1.0 probability-1-wins)))

    (list
     :winner-rating (+ winner-rating (* k (- 1 probability-2-wins)))
     :loser-rating (+ loser-rating (* k (- probability-1-wins))))))

(defun match-elo-entrants! (entrant-1-id entrant-2-id did-entrant-1-win)
  (let* ((entrant-1-rating (get-entrant-rating entrant-1-id))
         (entrant-2-rating (get-entrant-rating entrant-2-id))
         (result (if did-entrant-1-win
                     (elo-match entrant-1-rating entrant-2-rating)
                     (elo-match entrant-2-rating entrant-1-rating))))
    (if did-entrant-1-win
        (progn
          (print 'dog)
          (set-entrant-rating entrant-1-id (getf result :winner-rating))
          (set-entrant-rating entrant-2-id (getf result :loser-rating)))
        (progn
          (print 'cat)
          (set-entrant-rating entrant-2-id (getf result :winner-rating))
          (set-entrant-rating entrant-1-id (getf result :loser-rating))))
    result))

(defun ensure-entrant (entrant-id &optional (starting-rating 400))
  (unless (gethash entrant-id rankings-table)
    (setf (gethash entrant-id rankings-table) starting-rating))
  entrant-id)

(defun register-entrant (entrant-id &optional (starting-rating 400))
  (or (gethash entrant-id rankings-table)
      (setf (gethash entrant-id rankings-table) starting-rating)))

(defun set-entrant-rating (entrant-id new-rating)
  (if (get-entrant-rating entrant-id)
      (setf (gethash entrant-id rankings-table) new-rating)
      (register-entrant entrant-id new-rating)))

(defun get-entrant-rating (entrant-id)
  (gethash (ensure-entrant entrant-id) rankings-table))

(defun print-entrant-rankings ()
  (hash-table-print rankings-table))
