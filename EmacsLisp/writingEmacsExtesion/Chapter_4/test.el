(defun insert-current-time ()
  "Insert the current time"
  (interactive "*")
  (insert (current-time-string)))

;;格式化时间字符串
(format-time-string "%l.%M %p" (current-time))
;;" 8.31 下午"

;;defvar以星号开始，有特殊的意义，它表示这个变量是一个用户选项。
;;用户选项可以以set-variable命令以交互的方式进行设置，emacs会向用户请求一个一个变量名和一个值。
;;文档又一个特殊的结构\[command]当文本字符串显示给用户时，例如使用C-h 或者apropos时，command会被替换成command在此例中也就是insert-time的键绑定
;;如果insert-time没有键绑定，那么就会显示M-x insert-time
(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f 'format-time-string').")

(defvar insert-date-format "%x"
  "*Format for \\[insert-time] (c.f 'format-data-string').")

(defun insert-time ()
  "Insert the current time according to insert-time-format."
  (interactive "*")
  (insert (format-time-string insert-time-format
                              (current-time))))

(defun insert-date ()
  "Insert the current date according to insert-date-format."
  ;;此处星号表示如果buffer为只读时。终止这个函数。
  (interactive "*")
  (insert (format-time-string insert-date-format
                              (current-time))))

(add-hook 'local-write-file-hooks 'update-writestamps)

(defun update-writestamps ()
  "Find writestamps and replace them with the current time."
  (save-excursion
    ;;记录了narrowing的结果，如果没有narrowing就没有效果。
    (save-restriction
      ;;保存上一次搜索的结果
      (save-match-data
        (widen)
        (goto-char (point-min))
        (while (search-forward writestamp-prefix nil t)
          (let ((start (point)))
            ;;达到括号尾。
            ;;其实此处就是将WRITESTAMPS(())括号内的内容删除
            (search-forward writestamp-suffix)
            (delete-region start (- (point) 2))
            (goto-char start)
            ;;然后插入新的日期
            (insert (format-time-string writestamp-format
                                        (current-time))))))))
  ;;hooks函数中的返回值非常重要，如果返回非空，则表示这个hook函数接管了将buffer写入文件的工作，则hook中的其他函数不会被执行。
  ;;所以在此让他返回nil
  nil)


(defvar writestamp-format "%C"
  "*Format for writestamps (c.f. 'format-time-string').")

(defvar writestamp-prefix "WRITESTAMP(("
  "*Unique string identifying start of writestamp.")

(defvar writestamp-suffix "))"
  "*String that terminates a writestamp.")

(defun update-writestamps ()
  "Find writestamps and replace them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^"
                        (regexp-quote writestamp-prefix))
                nil t)
          (let ((start (point)))
            (if (re-search-forward (concat (regexp-quote
                                            writestamp-suffix)
                                           "$")
                                   (save-excursion
                                     (end-of-line)
                                     (point))
                                   t)
                (progn
                  (delete-region start (match-beginning 0))
                  (goto-char start)
                  (insert (format-time-string writestamp-format
                                              (current-time))))))))))
  nil)

(defun updte-writestamps ()
  "Find writestamps and replace them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((regexp (concat "^"
                              (regexp-quote writestamp-prefix)
                              "\\(.*\\) "
                              (regexp-quote writestamp-suffix)
                              "$")))
          (while (re-search-forward regexp nil t)
            (replace-match (format-time-string writestamp-format
                                               (current-time))
                           t t nil 1))))))
  nil)
