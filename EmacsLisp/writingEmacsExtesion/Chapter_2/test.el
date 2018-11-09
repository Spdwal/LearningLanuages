(defun other-window-backward (n)
  "Select Nth previous windos."
  (interactive "p")
  (other-window (- n)))

(defun other-window-backward (&optinoal n)
  "Select Nth previous window."
  (interactive "p")
  (if n
      (other-window (- n))
    (other-window -1)))

(defun other-window-backward (&optinoal n)
  "Select Nth previous window."
  (interactive "p")
  (other-window (if n
                    (- n)
                  -1)))

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  ;;如果P传进来一个参数，prefix-numeric-value将这个value也就是n，返回一个数字，本来它它一个raw form形式，
  ;;因为interactive的参数为P而不是p，然后
  (other-window (- (prefix-numeric-value n))))

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))

(global-set-key (kbd "C-,") 'point-to-top)

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn
        (setq buffer-read-only t)
        (message "File is a symlink"))))

(add-hook 'find-file-hooks 'read-only-if-symlink)

(add-hook 'find-file-hooks
          '(lambda ()
             (if (file-symlink-p buffer-file-name)
                 (progn
                   (setq buffer-read-only t)
                   (message "File is a symlink")))))

(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      ;;file-symlink-p返回一个string，指向这个symbol linkfile的源文件
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            ;;会直接找到target所指向的文件，并且关闭当前的buffer
            (find-alternate-file target)
          (error "Not vistiting a symbol")))
    (error "Not visiting a file")))

(defun clobber-symlink ()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            ;;y-or-no-p打印出一串信息，如果输入y返回t，如果输入n返回nil
            ;;format格式化一个string，返回一个string，有点像字符串流
            (if (y-or-n-p (format "Replace %s with %s"
                                  buffer-file-name
                                  target))
                (progn
                  ;;将buffer file name删除掉后，再将这个buffer作为一个普通文件而保存下来
                  (delete-file buffer-file-name)
                  (write-file buffer-file-name)))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))
;;这个函数使只有使用前置参数的时候，才可以切换到不存在的buffer。
;;existing-buffer是给这个advice的命名，在之后想对这个advice进行操作可以使用这个名字。
;;整个defadvice相当于在这个函数，switch-to-buffer接受参数之前再运行一个函数。
;;函数的定义在这个defadvice里面。
(defadvice switch-to-buffer (before existing-buffer
                                    activate compile)
  "When inteactive, switch to existing buffers only,
unless given a prefix argument."
  ;;当interactive的参数不是字符串而是表达式的时候，这些表达式会运算得到一个参数列表传递给函数。
  (interactive
   (list (read-buffer "Switch to buffer:"
                      ;;other-buffer返回最近使用的一个buffer
                      (other-buffer)
                      ;;如果没有前置参数，则返回t，否则返回nil
                      ;;如果为t，那么表示只接受已经存在的buffer
                      ;;如果为nil，那么表示对buffer名称不做限制
                      (null current-prefix-arg)))))
