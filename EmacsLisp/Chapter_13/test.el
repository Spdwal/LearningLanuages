(defun count-words-region (beginning end)
  "Print number of words in the region.
Words are defined as at least one word-constituent
character followed by at least one character that
is not a word-constituent. The buffer's syntax
table determines which characters these are."
  (interactive "r")
  (message "Counting words in region...")
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      ;;此处有一个bug，当我们的region里没有单词的时候，我向后搜索到第一个单词，然后point>end，然后跳出循环，此时count为1
      ;;但是实际上这个region里并没有单词，所以这个代码其实是错误的。
      (while (< (point) end)
        ;;\\w表示构成词的字符，\\W表示不构成词的字符，
        ;;+表示至少出现一次，*表示出现任意次。
        (re-search-forward "\\w+\\W*")
        (setq count (1+ count)))
      ;;根据count打印不同的信息
      (cond ((zerop count)
             (mesage
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))


;;修改过bug的代码
(defun count-words-region (beginning end)
  "Print number of words in the region.
Words are defined as at least one word-constituent
character followed by at least one character that
is not a word-constituent. The buffer's syntax
table determines which characters these are."
  (interactive "r")
  (message "Counting words in region...")
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      ;;此处有一个bug，当我们的region里没有单词的时候，我向后搜索到第一个单词，然后point>end，然后跳出循环，此时count为1
      ;;但是实际上这个region里并没有单词，所以这个代码其实是错误的。
      ;;修改后先测试end是不是在point之后。再research-forward。
      (while (and (< (point) end)
                  ;;第二个参数end表示搜索不会出end的范围，如果超出会跑出一个错误。
                  ;;第三个参数t表示查询失败时它返回nil而不会抛出错误。
                  (re-search-forward "\\w+\\W*" end t))
        ;;\\w表示构成词的字符，\\W表示不构成词的字符，
        ;;+表示至少出现一次，*表示出现任意次
        (setq count (1+ count)))
      ;;根据count打印不同的信息
      (cond ((zerop count)
             (mesage
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

(defun recursive-count-words (region-end)
  (if (and (< (point) region-end)
           (re-search-forward "\\w+\\W*" region-end t))
      (1+ (recursive-count-words region-end))))

(defun count-words-region (beginning end)
  "Pring number of words in the region.
Words are defined as at least one word-constituent
character followed by at least one character that is
not a word-constituent. The buffer's syntax table
determines which characters these are."
  (interactive "r")
  (messsage "Counting words in region...")
  (save-excursion
    (goto-char beginning)
    (let ((count (recursive-count-words end)))
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))
