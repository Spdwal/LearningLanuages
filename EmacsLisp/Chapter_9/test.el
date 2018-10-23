;;some codes                                        ;

(setq bouquet '(rose violet buttercup))



(setq flowers (cdr bouquet))
;;(violet buttercup)


(equal (cdr bouquet) flowers)

;;cdr list 其实就是获得了一个指向list中第二个元素地址的指针，所以此时返回t
(eq (cdr bouquet) flowers)
;;t

(setq flowers '(violet buttercup))
;;此时flowers 和cdr bouquet就指向的不是同一个对象了。
(eq (cdr bouquet) flowers)

;;exercise
(setq more-flowers (cons 'white flowers))

(setcar more-flowers 'fish)

more-flowers


(provide 'test)
