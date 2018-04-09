;;; package --- An implementation of merge sort.

;;; Commentary:

;;; Code:

(defun merge-sort (list)
  "Sort LIST by merge sort algorithm."
  (let ((len (length list)))
    (cond ((< len 2) list)
          ((= len 2) (let ((i1 (nth 0 list))
                           (i2 (nth 1 list)))
                       (if (< i1 i2) list
                         (list i2 i1))))
          (t (let ((half (/ len 2))
                   left)
               (while (< half len)
                 (push (pop list) left)
                 (setq len (1- len)))
               (let ((l1 (merge-sort left))
                     (l2 (merge-sort list))
                     i1 i2 result)
                 (while (or l1 l2 i1 i2)
                   (or i1 (setq i1 (pop l1)))
                   (or i2 (setq i2 (pop l2)))
                   (cond ((or (and i1 (null i2))
                              (and i1 i2 (< i1 i2)))
                          (push i1 result)
                          (setq i1 nil))
                         ((or (and i2 (null i1))
                              (and i1 i2 (<= i2 i1)))
                          (push i2 result)
                          (setq i2 nil))))
                 (nreverse result)))))))

(provide 'merge-sort)
;;; merge-sort.el ends here
