;;; package --- An implementation of bubble sort.

;;; Commentary:

;;; Code:

(defun bubble-sort (list)
  "Sort LIST by bubble sort algorithm."
  (let ((rest list)
        result)
    (while rest
      (let (tmp i1 i2)
        (while (or rest i1 i2)
          (or i1 (setq i1 (pop rest)))
          (or i2 (setq i2 (pop rest)))
          (cond ((or (and i1 (null i2))
                     (and i1 i2 (< i1 i2)))
                 (push i1 tmp)
                 (setq i1 nil))
                ((or (and i2 (null i1))
                     (and i1 i2 (<= i2 i1)))
                 (push i2 tmp)
                 (setq i2 nil))))
        (push (pop tmp) result)
        (setq rest tmp)))
    result))

(provide 'bubble-sort)
;;; bubble-sort.el ends here
