(defun limited-save-excursion (&rest exprs)
  ;;rest意思是收集剩下的所有的参数，作为一个list传入
  "Like save-excursion, but only restores point."
  (let ((save-point (point)))
    ;;循环运行exprs这几个表达式。
    (while exprs
      (eval (car exprs))
      (setq exprs (cdr exprs)))
    (goto-char saved-point)))

;;宏函数的参数都是被默认引用的
;;在它获得控制权之前都不会被执行。
;;例如(incr x)
;;若不是宏函数的话，那么x本身并不会有变化。

(defmacro incr (var)
  "Add one to the named variable."
  (list 'setq var (list '+ var 1)))
;;宏函数的函数日是对于参数的一个展开式，然后这个展开式会被求值。

(defmacro limited-save-excursion (&rest subexprs)
  "Like save-excursion, but only restores point."
  '(let* ((orig-point (point))
          (result (progn ,@subexprs)))
     ;;,为去引用操作符，可以为子表达式独立的去掉引用。
     ;;,@为拼接去引用操作符，会讲subexprs中的值提取到一个列表中，并且移除外面括号的方法。
     (goto-char orig-point)
     result))
