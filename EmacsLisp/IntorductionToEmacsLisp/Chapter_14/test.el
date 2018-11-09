(defun count-words-in-defun ()
  ;;搜索一个defun内的词数
  "Return the number of words and symbols in a defun"
  (beginning-of-defun)
  (let ((count 0)
        (end (save-excursion (end-of-defun) (point))))
    (while
        (and (< (point) end)
             (re-search-forward
              "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*"
              end t))
      (setq count (1+ count)))
    count))


(defun count-words-defun ()
  "Number of words and symbols in a function definition."
  ;;对上一个函数进行包装，使其能进行交互
  (interactive)
  (message
   "Count words and symbols in function definition ... ")
  ;;获得count
  (let ((count (count-words-in-defun)))
    (cond ((zerop count)
           (message "The definition does NOT have any words or symbols."))
          ((= 1 count)
           (message
            "The definition has 1 word or symbol."))
          (t
           (message
            "The definition has %d words or symbols." count)))))

(global-set-key "\C-c=" 'count-words-defun)


(defun lengths-list-file (filename)
  "Return list of definitions' lenths within FILE.
The returned list is a list of numbers.
Each number is the number of words or symbols in one function definition."
  (message "Working on '%s' ... " filename)
  ;;保存之前的鼠标点
  (save-excursion
    (let ((buffer (find-file-noselect filename))
          (lengths-list))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^(defun" nil t)
        (setq lengths-list
              (cons (count-words-in-defun) lengths-list)))
      (kill-buffer buffer)
      lengths-list)))
