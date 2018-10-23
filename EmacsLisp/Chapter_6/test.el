(defun what-line ()
  "Print the current line number (int the buffer) of the point."
  (interactive)
  ;;如果本身如果是在窄化状态下的，那么无论在body体内做了什么，最后都会返回body之前的状态
  (save-restriction
    ;;例如，此处进行了widen，但是在最后body结束的时候还是恢复了原本的操作。
    (widen)
    (save-excursion
      ;;来到行首
      (beginning-of-line)
      (message "Line %d"
               ;;count-lines计算point到行数为1的行的距离，
               ;;因为第二行到第一行只有1，所以最后还要+1
               (+ 1 (count-lines 1 (point)))))))


;;exercies

(defun first-60-char-echo ()
  (interactive)
  (save-restriction
    (widen)
    (message
     (buffer-substring 1 60))))
