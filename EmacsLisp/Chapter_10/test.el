;;kill-ring指向的是整个 kill ring
;;kill-ring-yank-point指向的是当前yank的string，也就是会顺着list pointer一个一个往下走
;;逻辑类似于M-y
;;函数rotate-yank-pointer该博啊kill-ring-yank-pointer指针的指向。



;;exercise
;;kill-ring-max is 60


(defun nth-element (list pos)
  (car (nthcdr (1- pos) list)))

(setq mylist '(1 2 3 4 5 6 7 8 9))

(nth-element mylist 4)
