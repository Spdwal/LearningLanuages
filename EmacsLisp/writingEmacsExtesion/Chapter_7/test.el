(defvar refill-mode nil
  "Mode variable for refill minor mode.")

(make-variable-buffer-local 'refill-mode)
;;使其变成buffer本地变量

(defun refill (start end len)
  "After a tesxt change, refill the current paragraph."
  (let ((left (if (zerop len)
                  start
                (max (save-excursion
                       (goto-char start)
                       ;;前往行首
                       (beginning-of-line 0)
                       (point))
                     ;;此处是防止现在已经处在一个段落尾部，造成没有办法进行fill的情况
                     (save-excursion
                       (goto-char start)
                       ;;前往段落首
                       (backward-paragraph 1)
                       (point))))))
    (save-excursion
      (fill-region left end nil nil t))))


(defun refill-mode (&optional arg)
  "Refill minor mode."
  (interactive "P")
  (setq refill-mode
        (if (null arg)
            ;;如果没有参数，那么就打开或者关闭
            (not refill-mode)
          (> (prefix-numeric-value arg) 0)))
  (if refill-mode
      ;;参数保证我们修改的只是局部的after-change-functions·
      (add-hook 'after-change-functions 'refill nil t)
    (remove-hook 'after-change-functions 'refill t)))
