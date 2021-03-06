(defun simplified-beginning-of-buffer ()
  "Move point to the beginning of the buffer;
leave mark at previous position."
  (interactive)
  ;;push一个mark到mark ring里面，可以利用C-x C-x进行跳转。
  (push-mark)
  (goto-char (point-min)))

(defun my-mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min)))

;;这个函数将point和mark之间被marked的region给加入到交互中输入的buff中去
(defun my-append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.
When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  ;;这个B表示将在Append to buffer: 输入要求的buffer名字，r表示mark和point会被作为两个参数传入start和end中去。
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      ;;get-buffer-create 跳转到buffer，如果buffer不存在，则创建它
      ;;所以set-buffer总会获得一个缓冲区
      (set-buffer (get-buffer-create buffer))
      ;;传入三个参数，oldbuf名字，来制定buffername，然后用start和end来指定substring的范围。
      (insert-buffer-substring oldbuf start end))))


;;exercises
(defun simplified-end-of-buffer ()
  "Move point to the end of the buffer"
  (interactive)
  (push-mark)
  (goto-char (point-max)))

(get-buffer (current-buffer))

(defun the-bufer-existp (input-name)
  "find where the buffer is exist"
  (interactive "BInput the buffer name:\n")
  (let ((buffer-name (get-buffer input-name)))
    (if buffer-name
        (message "The buffer %s is exist" input-name)
      (message "The buffer %s is not exist" input-name))))

(provide 'test)
