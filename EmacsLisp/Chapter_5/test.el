(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  ;;*表示如果输入的是一个只读buffer的话，会报错然后重新输入
  (interactive "*bInsert buffer: ")
  ;;如果bufferp buffer不是一个buffobject而是它的名字的时候,那么就利用get-buffer来得到这个bufferobject
  ;;确保buffer绑定在一个object上，而不是一个string上
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  ;;创建了三个local变量，且全部默认绑定到nil上
  (let (start end newmark)
    (save-excursion
      (save-excursion
        ;;切换到输入的buff上
        (set-buffer buffer)
        ;;保存buffer中的两个point
        (setq start (point-min) end (point-max)))
      ;;从buffer中将所有内容copy进现在的buffer的point上
      (insert-buffer-substring buffer start end)
      ;;将喂入文本末尾的point保存在newmark中
      (setq newmark (point)))
    ;;返回最初的point上，且将newmarkpush进mark ring
    (push-mark new-mark)))

(defun my-beginning-of-buffer (&optional arg)
  "documents...."
  (interactive "P")
  (push-mark)
  (goto-char
   (if arg
       (if (> (buff-size) 10000)
           (* (prefix-numeric-value arg)
              (/ (buffer-size) 10))
         (/ (+ 10 (* (buffer-size)
                     (prefix-numeric-value arg)))
            10))
     (point-min)))
  (if arg (forward-line)))


;;exercises

(defun optional-greater-than-fill-columnp (&optional arg)
  "document..."
  (interactive "n")
  (or arg
      (setq arg 56))
  (if (> arg fill-column)
      (message "The argument is greater than fill-column.")
    (message "The argument is lesser than fill-column.")))
(provide 'test)
