(setq empty-list ())

(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

;;运行时必须使用C-u C-x C-e
;;loops一般来说都会返回一个nil
(print-elements-of-list animals)

(defun triangle (number-of-rows)
  "Add up the number of pebbles in a triangle.
The first row has one bebble, the second row two pebbles,
The third row three pebbles, and so on.
Thr argument is NUMBRE-OF-ROWS."
  (interactive "n")
  (let ((total 0)
        (row-number 1))
    (while (<= row-number number-of-rows)
      (setq total  (+ total row-number))
      (setq row-number (1+ row-number)))
    total))


(triangle 5)
(triangle 4)
(triangle 3)


(defun triangle (number-of-rows)
  "Add up the number of pebbles in a triangle."
  (let ((total 0)
        (number-of-pebbles-in-row number-of-rows))
    (while (> number 0)
      (setq total (+ total number))
      (setq number (1- number)))
    total))


(defun reverse-list-with-while (list)
  "Using while, reverse the order of LIST."
  (let (value)
    (while list
      (setq value (cons (car list) value))
      (setq list (cdr list)))
    value))

(reverse-list-with-while animals)

(defun reverse-list-with-dolist (list)
  "Using dolist, reverse the order of LIST."
  (let (value)
    ;;dolist的第三个参数是它的返回值，这里返回value的值
    ;;第一个参数表示list中每一个元素的名字，拿来遍历list。
    (dolist (element list value)
      (setq value (cons element value)))))

(reverse-list-with-dolist animals)

(defun triangle-using-dotimes (number-of-rows)
  "Using dotimes, add up the number of pebbles in a triangle."
  (let ((total 0))
    ;;dotimes一共运行 0 - second-argument 这么多次
    (dotimes (number number-of-rows total)
      (setq total (+ total (1+ number))))))

(triangle-using-dotimes 4)

(defun print-elements-recursively (list)
  "Print each element of LIST on a line of its own.
Uses recursion."
  (if list
      (progn
        (print (car list))
        (print-elements-recursively
         (cdr list)))))

(print-elements-recursively animals)

(defun triangle-recursively (number)
  "Return the sum of the numbers 1 through NUMBER inclusive. Uses recursion"
  (if (= number 1)
      1
    (+ number
       (triangle-recursively
        (1- number)))))

(triangle-recursively 5)

(defun triangle-using-cond (number)
  (cond ((<= number 0) 0)
        ((= number 1) 1)
        ((> number 1)
         (+ number (triangle-using-cond (1- number))))))

(triangle-using-cond 5)

(defun square-each (numbers-list)
  "Square each of a NUMBERS LIST, recursively."
  (if (not numbers-list)
      nil
    (cons
     (* (car numbers-list) (car numbers-list))
     (square-each (cdr numbers-list)))))

(square-each '(1 2 3))

(defun add-elements (number-list)
  "Add the elements of NUMBERS-LIST together."
  (if (not number-list)
      0
    (+ (car number-list) (add-elements (cdr number-list)))))

(add-elements '(1 2 3))

(defun keep-three-letter-words (word-list)
  "Keep three letter words in WORD-LIST."
  (cond
   ;;First do-again-test: stop-condition
   ((not word-list) nil)
   ((eq 3 (length (symbol-name (car word-list))))
    (cons (car word-list) (keep-three-letter-words (cdr word-list))))
   (t (keep-three-letter-words (cdr word-list)))))

(keep-three-letter-words '(one two three four five six))

(defun triangle-initialization (number)
  "Return the sum of the numbers 1 through NUMBER inclusive.
This is the'initialization' component of a two function duo that uses recursion."
  (triangle-recursive-helper 0 0 number))

(defun triangle-recursive-helper (sum counter number)
  "Return SUM. using COUNTER, through NUMBER, inclusive.
This is the 'helper' component of a two functon duo that uses recursion."
  (if (> counter number)
      sum
    (triangle-recursive-helper (+ sum counter)
                               (1+ counter)
                               number)))

(triangle-initialization 4)

;;exercises

(defun triangle (number)
  (let ((total 0)
        (number-of-pebble-in-row-number number))
    (while (> number-of-pebble-in-row-number 0)
      (setq total (+ total (* number-of-pebble-in-row-number number-of-pebble-in-row-number)))
      (setq number-of-pebble-in-row-number (1- number-of-pebble-in-row-number)))
    total))

(triangle 4)

(defun triangle (number)
  (let ((answer 1))
    (while (> number 0)
      (setq answer (* answer number))
      (setq number (1- number)))
    answer))

(triangle 5)

;;recursive version

(defun triangle (number)
  (defun triangle-helper (sum counter number)
    (if (> counter number)
        sum
      (triangle-helper (+ sum (* counter counter))
                       (1+ counter)
                       number)))
  (triangle-helper 0 0 number))


(triangle 5)


(defun triangle (number)
  (defun triangle-helper (answer counter number)
    (if (> counter number)
        answer
      (triangle-helper (* answer counter)
                       (1+ counter)
                       number)))
  (triangle-helper 1 1 number))

(triangle 5)
