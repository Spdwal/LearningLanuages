(listp 27)

(defun our-third (x)
  (car (cdr (cdr x))))

(our-third '(a b c d))

(defun our-number (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-number obj (cdr lst)))))

(format t "~A plus ~A equals ~A. ~%" 2 3 (+ 2 4))

(let ((x 1) (y 2))
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))


(defparameter
    *glob* 99)
(defconstant limit (+ *glob* 1))

;;(defconstant limit 3) throw an error


(boundp '*glob*)

(setf x (list 'a 'b 'c))

(setf (car x) 'n)

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A ~%" i (* i i))))

;;利用obj遍历x内的每一个元素
(defun show-element (x)
  (dolist (obj x)
    (format t "~A ~%" obj)))
