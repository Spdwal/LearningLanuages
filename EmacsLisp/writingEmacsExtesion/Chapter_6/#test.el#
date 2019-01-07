(cons 'a 'b)
;;(a .


(list 'a 'b 'v)

(cdr '(a . b))
;;b

(cdr '(a b))
;;(b)
;;区别，(a . b)是 (a b)
;;而 (a b)是 (a (b . nil))

(consp '(a b v))
;;t 当参数为空表之外的任何列表时，返回true，其他时候返回false。
(listp nil)
;;t 当x为nil或者list的时候返回true，其他时候返回false


(defun flatten (lst)
  (if (null lst)
      nil
    (if (listp (car lst))
        (append (flatten (car lst))
                (flatten (cdr lst)))
      ;;如果 car lst不是一个list而是一个atom的时候。
      ;;将它与后面的cdr cons在一起。
      (cons (car lst)
            (flatten (cdr lst))))))

(flatten '(a (b c)))
;;(a b c)


(mapcar '(lambda (x)
           (capitalize x))
        '("lisp" "is" "cool"))
;;("Lisp" "Is" "Cool")


;;eq判断两个对象是不是一个对象
;;eq判断两个对象是否具有相同的结构和内容

(assoc 'green
       '((red. "ff0000")
         (green . "00ff00")
         (blue . "0000ff")))
;;(green . "00ff00")

(setq x '(a b c))
(nthcdr 2 x)
;;(c)
(setcdr (nthcdr 2 x) x)
;;(a b c a b . #2)它变成了一个环状的list
