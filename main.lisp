(in-package #:elo)

(defparameter rankings-table (make-hash-table :test #'equal))

(defun main ()
  (let ((entrants (list 'dumbledore 'voldemort 'harry 'snape 'mcgonagall 'flitwick)))
    (dotimes (i 1000) ;; 1000 match-ups
      (let ((fighters (elt-random-n entrants 2))) ;; between 2 randomly chosen entrants each
        (dotimes (i (1+ (random 100))) ;; where one entrant wins and the other loses, between 1 and 100 times in a row
          (match-elo-entrants! (first fighters) (second fighters) t))))
    (print-entrant-rankings)))

(defun probability-a-wins (a-rating b-rating)
  (/ 1.0 (+ 1 (expt 10 (/ (- a-rating b-rating) 400)))))

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
        (set-entrant-ratings entrant-1-id (getf result :winner-rating)
                             entrant-2-id (getf result :loser-rating))
        (set-entrant-ratings entrant-2-id (getf result :winner-rating)
                             entrant-1-id (getf result :loser-rating)))
    result))

(defun ensure-entrant (entrant-id &optional (starting-rating 400))
  (unless (gethash entrant-id rankings-table)
    (register-entrant entrant-id starting-rating))
  entrant-id)

(defun register-entrant (entrant-id &optional (starting-rating 400))
  (or (gethash entrant-id rankings-table)
      (setf (gethash entrant-id rankings-table) starting-rating)))

(defun set-entrant-rating (entrant-id new-rating)
  (if (get-entrant-rating entrant-id)
      (setf (gethash entrant-id rankings-table) new-rating)
      (register-entrant entrant-id new-rating)))

(defun set-entrant-ratings (&rest entrants-and-ratings)
  (let ((pairs (group entrants-and-ratings 2)))
    (loop for pair in pairs
          when (cdr pair)
          do (set-entrant-rating (car pair) (cadr pair)))
    pairs))

(defun get-entrant-rating (entrant-id)
  (gethash (ensure-entrant entrant-id) rankings-table))

(defun get-entrant-ratings (entrant-ids)
  (mapcar #'get-entrant-rating entrant-ids))

(defun print-entrant-rankings ()
  (hash-table-print rankings-table))
 
