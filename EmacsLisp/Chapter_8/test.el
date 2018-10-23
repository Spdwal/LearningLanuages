(cons "another piece" '("a piece of text" "last piece"))

(car (nthcdr 1 '("another piece"
                 "a piece of text"`

                 "last piece")))


(defun zap-to-char (arg char)
  "Kill up to and including ARG'th occurrence of CHAR.
goes backward if ARG is negative; error is CHAR not found."
  ;;*表示只读时报错 p表示需要一个前置参数，如果没有，默认传1
  ;;c表示产生一个提示并且后续的参量时一个字符。提示信息时c之后的"Zap to char:"˘
  (interactive "*p\ncZap to char:")
  (kill-region (point)
               ;;每一个参量被逐一求值，并且返回周后一个参量的值
               (progn
                 ;;search-forward的第一个参数是一个string而不是char，所以需要转换
                 ;;第二个参数半丁查询范围，此例子中表示可以查询到buffer末尾，所以为nil
                 ;;第三个参数搞死这个函数如果查询失败怎么办，可以发出一个出错信号，也返回nil，这里返回nil
                 ;;第四个参数是重复计数值。如果是负数，就向后进行。
                 (search-forward
                  (char-to-string char) nil nil arg)
                 ;;此时point达到搜索到的位置
                 ;;然后使用(point)返回此时point的位置
                 )))


(defun kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved int the kill ring."
  (interactive "r")
  (condition-case nil
      ;;第一个参数告诉我们error signal不会存储下来
      ;;如果没有发生错误，就运行condition的第二个参数
      ;;delete-and-extract-region 将begin和end之间的string删除，并且返回这个string
      (let ((string (delete-and-extract-region beg end)))
        ;;when类似于if，但是它不存在else语句
        (when string
          (if (eq last-command 'kill-region)
              ;;如果(< end begin)为true，那么将string返还回去。
              (kill-append string (< end begin))
            ;;将string插入kill ring中
            (kill-new string)))
        (setq this-command 'kill-region))
    ;;第三个参数，当发生一个error时候怎么处理
    ;;这个handler body里面存在有一个condition语句，
    ;;在这个函数中即是下一行这句，确定这个buffer是不是真的是read-only
    ;;buffer-read-only是一个值，text-read-only是一个symbol,怎么组成的一个condition expression
    ;;查找手册得知，这里应该是保存list of errors signal，当list中的error发生的时候，那么就执行之后body内的expression
    ((buffer-read-only text-read-only)
     (copy-region beg end)
     (if kill-read-only-ok ;;正常情况下这个值为nil
         (message "Read only text copied to kill ring")
       ;;产生一个buff-read-only错误如果此buffer为read-only
       (barf-if-buffer-read-only)
       (signal 'text-read-only (list (current-buffer)))))))

;;defvar只对无值的变量赋值，如果变量已经有一个值，那么不会覆盖已经存在的值。并且可以给它保存一个文档。
;;绝大多数变量是emacs的内部变量，但是有一些是可以被edit-option来设置的.但是这些设置只在一个编辑过程中有效，如果要永久生效。需要在.emacs中设置它。
;; example:
(defvar line-number-mode nil
  "*Non nil means display line number in mode line.")
;;这里的星号表示我能够使用edit-option来改变line-number-mode变量的值，




(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If 'interprogram-cut-functin' is non nil, also save the text for a window system cut and paste."
  ;;用r来传入begin和end
  (interactive "r")
  (if (eq last-command 'kill-region)
      ;;将它cons在kill-ring的第一个元素里面，
      (kill-append (buffer-substring beg end) (< end begin))
    ;;new一个新的kill-ring
    (kill-new (buffer-substring beg end)))
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
If ;interprogram-cut-function is set, pass the resulting kill to it."
  ;;即是上面的那个(< end begin)的参数，t or false，放在前还是放在后。
  (kill-new (if before-p
                (concat string (car kill-ring))
              (concat (car kill-ring) string))
            t))


(concat "abv" "asfd")

(defun kill-new (string &optional replace)
  "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
If 'interprogram-cut-function' is non-nil apply it to string.
Optional second argument REPLACE non-nil means that STRNG will replace
the front of the kill ring, rather than being added to the list."
  ;;menu-bar-update-yank-menu是不是作为一个function存在 function-bound-predicate
  (and (fboundp 'menu-bar-update-yank-menu)
       ;;如果menu-bar-update-yank-menu这个函数存在，那么就返回true
       ;;可以使用Select and Paste菜单。
       (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  ;;将killring的第一个string换成传进去的string，也就是replace替换掉。
  (if (and replace kill-ring)
      (setcar kill-ring string)
    ;;将string放在kill ring list的第一个
    (setq kill-ring (cons string kill-ring))
    (if (> (length kill-ring) kill-ring-max)
        ;;如果长度超过了killring的最大长度，那么就将最后一个删除掉。
        (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      ;;使这个string可以被其他的代码所使用。是一个通信函数。
      (funcall interprogram-cut-function string (not replace))))


;;exercises

(defun search-string (string)
  "search for a string"
  (interactive "sSearch for a string:")
  (search-forward string)
  (message "Found!"))

(defun the-third-kill-string ()
  "Find the third symbol int the kill ring."
  (interactive)
  (let ((thirdth-kill-ring (car (nthcdr kill-ring 2)))
        (if thirdth-kill-ring
            (message thirdth-kill-ring)
          (message "The kill-ring is too short to have the third string.")))))
