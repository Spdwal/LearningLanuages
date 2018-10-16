;;定义一个函数，形式为：
;;(defun functionName (argumentList)
;;  "optional-documention..."             注释
;;  (interactive argument-passing-info)   optional
;;  body.....)
;; interactive 利用C-u为其传入参数。
;; (interactive) or (interactive nil) 无参
;; (interactive "n") 一个数字参数
;; (interactive "s") 一个字符参数
;; (interactive "r") 传入两个数字参数，作为一个marked region的起点和终点
;; 以这个函数为例，如果直接M-x multiply-by-seven，则直接返回7，没有机会为其传入参数
;; 加入了(interactive "p")后可以为其传入一个前置参数，例如我C-u 5 M-x multiplu-by-seven则会返回35
;; 此时参数 5 将会绑定至 number 参数上，
(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (interactive "p") ;;小写的p表示如传入一个数字，不出发io操作
  (message "The result is %d" (* 7 number)))


;;let创建出来一个局部变量。
(let ((zebra 'stripes)
      (tiger 'fierce))
  (message "One kind of animal has %s and another is %s."
           zebra tiger))

;;如果let没有赋值的话，那么symbol会被默认赋值为nil
(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d variables with %s, %s, and %s value."
   birch pine fir oak))

(if (> 5 4 )
    (message "5 is greater than 4"))

(> 5 4)
;;t

(if t
    (message "t is True"))
;;"t is True"

(defun type-of-animal (characteristic)
  "Print message in echo depending on CHARACTEfRISTIC.
If the CHARACTERISTIC is the symbol 'fierce', then warn of a tiger;
else say it's not fierce."
  (if (equal characteristic 'fierce)
      (message "It's a tiger!")
    (message "It's not fierce!")))

(type-of-animal 'fierce)
;;"It’s a tiger!"

(type-of-animal 'zebra)
;;"It’s not fierce!"

(if "Hello"
    (message "String is true"))
;;"String is true"

(if nil
    (message "Nil is false"))
;;nil

(message "We are %d characters into this buffer."
         ;;这里保存当前point的数值
         (- (point)
            (save-excursion
              (goto-char (point-min))
              ;;返回的是光标在point-min处的(point)数值，即是1
              ;;最后两者相减
              (point))))
;;"We are 1841 characters into this buffer."

(let ((foo (buffer-name))
      (bar (buffer-size)))
  (message
   "This buffer is %s and has %d characters."
   foo bar))
;;"This buffer is test.el • Chapter_3 and has 2056 characters."


;;equal and eq test whether two objects are the same.
;;注意，equal只要两者是有相同的结构和内容即可，而eq需要完全指向同一个object
(if (string-equal
     (number-to-string 21)
     (substring (emacs-version) 10 12))
    (message "This is version 21 Emacs")
  (message "This is not version 21 Emacs"))
;;"This is not version 21 Emacs"

;;string-lessp的两个参数必须是string或者symbol，比较规则与c语言中的strcmp类似，利用ASCII进行比较。
(string-lessp "a" "b")
;;t
(string-lessp "b" "a")
;;nil

;;exercises
(defun double-the-number (number)
  (* 2 number))
(message (number-to-string (double-the-number 12)))
;;"24"

(defun double-the-number-with-interactive (number)
  ;;如果参数是n，则是在M-x后输入number
  ;;如果参数是p，则是在C-u后，M-x前输入number
  (interactive "p")
  (message (number-to-string (* 2 number))))

(defun greater-than-number-of-fill-columnp (number)
  "Test whether the argument is greater than the value of fill-column"
  (if (> number fill-column)
      (message "The %d is greater than the number of fill-column." number)))

(> 99 fill-column)

(greater-than-number-of-fill-columnp 99)


(multiply-by-seven 15)
(point-min)
;;105
