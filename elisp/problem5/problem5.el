;;; package --- problem5

;;; Commentary:

;; Write a program that outputs all possibilities to put + or - or
;; nothing between the numbers 1, 2, ..., 9 (in this order) such that
;; the result is always 100.

;; For example: 1 + 2 + 34 - 5 + 67 - 8 + 9 = 100.

;;; Code:

(defun calc (formula)
  "Calculate FORMULA.

FORMULA must be a list of (1 op1 2 op2 ... op8 9)."
  (letrec ((inner
            (lambda (list tmp sum)
              (if (null list)
                  (+ tmp sum)
                (pcase-let ((`(,op ,val . ,rest) list))
                  (apply inner rest
                         (pcase op
                           ((or `+ `-)
                            (list (funcall op val)
                                  (+ tmp sum)))
                           (_
                            (list (+ (* tmp 10)
                                     (* val (if (< 0 tmp) 1 -1)))
                                  sum)))))))))
    (funcall inner (cdr formula) (car formula) 0)))

(defun dump (formula)
  "Dump FORMULA.

FORMULA must be a list of (1 op1 2 op2 ... op8 9)."
  (mapconcat (lambda (term)
               (pcase term
                 ((or `+ `-) (format " %s " term))
                 (`nil "")
                 (_ (number-to-string term))))
             formula ""))

(defun generate (numbers ops)
  "Return a list of formulas composed of NUMBERS and OPS."
  (let (result)
    (letrec ((inner
              (lambda (list &optional acc)
                (if (null (cdr list))
                    (push (reverse (cons (car list) acc)) result)
                  (dolist (op ops)
                    (funcall inner (cdr list)
                             (cons op (cons (car list) acc))))))))
      (funcall inner numbers)
      (nreverse result))))

(defun problem5 ()
  "Solve problem5."
  (dolist (formula (generate (number-sequence 1 9)
                             '(+ - nil)))
    (if (= (calc formula) 100)
        (insert (format "100 = %s\n" (dump formula))))))

(provide 'problem5)
;;; problem5.el ends here
