;;; package --- problem5+

;;; Commentary:

;; Write a program that outputs all possibilities to put + or - or *
;; or / or nothing between the numbers 1, 2, ..., 9 (in this order)
;; such that the result is always 100.

;; For example: 1 + 2 * 3 - 4 + 56 / 7 + 89 = 100.

;;; Code:

(defun calc-factor (factor)
  "Calculate FACTOR.

FACTOR must be a reverse list of (n1 op1 n2 op2 ... nN) or (n1)."
  (let (result op)
    (dolist (term (nreverse factor) result)
      (cond ((null result) (setq result term))
            ((null op) (setq op term))
            (t
             (and (eq op '/)
                  (or (zerop term)
                      (/= 0 (% result term)))
                  (error "Detected a remainder"))
             (setq result
                   (funcall op result term))
             (setq op nil))))))

(defun calc (formula)
  "Calculate FORMULA.

FORMULA must be a list of (1 op1 2 op2 ... op8 9)."
  (letrec ((inner
            (lambda (list tmp sum &optional work)
              (if (null list)
                  (+ sum (calc-factor (cons tmp work)))
                (pcase-let ((`(,op ,val . ,rest) list))
                  (apply inner rest
                         (pcase op
                           ((or `* `/)
                            (list val sum (cons op (cons tmp work))))
                           ((or `+ `-)
                            (list (funcall op val)
                                  (+ sum (calc-factor (cons tmp work)))))
                           (_           ; need to concatenate
                            (list (+ (* tmp 10)
                                     (* val (if (< 0 tmp) 1 -1)))
                                  sum work)))))))))
    (condition-case err
        (funcall inner (cdr formula) (car formula) 0)
      (error 0))))

(defun dump (formula)
  "Dump FORMULA.

FORMULA must be a list of (1 op1 2 op2 ... op8 9)."
  (mapconcat (lambda (term)
               (pcase term
                 ((pred numberp) (number-to-string term))
                 (`nil "")
                 (_ (format " %s " term))))
             formula ""))

(defun generate (numbers ops)
  "Return a list of formulas which contain combination of NUMBERS and OPS."
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

(defun problem5+ ()
  "Solve problem5+."
  (dolist (formula (generate (number-sequence 1 9)
                             '(+ - * / nil)))
    (if (= (calc formula) 100)
        (insert (format "100 = %s\n" (dump formula))))))

(provide 'problem5+)
;;; problem5+.el ends here
