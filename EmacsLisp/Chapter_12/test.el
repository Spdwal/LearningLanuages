;;
(sentence-end)
;;"\\([.?!…‽][]\"'”’)}]*\\($\\|[  ]$\\|	\\|[  ][  ]\\)\\|[。．？！]+\\)[  	\n]*"
;; \|在正则语法中表示or 所以用\\|来转义|

(re-search-forward "asdf")
;;并不返回值，只是会在minibuffer打印一个信息

(defun forward-sentence (&optional arg)
  "Move forward to next sentence-end. With argument, repeat.
With negative argument, move backward repeatly to sentence-beginning.
Sentence ends are identified by the value of sentence-endstreated as a regular expression.
Also every paragraph boundary terminates sentences as well."
  (interactive "p")
  ;;arg是否存在，如果不存在，赋值为1
  (or arg (setq arg 1))
  ;;循环，若arg<0时一直进行循环，也就是向前搜索。
  (while (< arg 0)
    (let ((par-beg
           ;;先将point移动到段落末尾，返回该点的值，再利用save-excursion返回本来的点的位置。
           (save-excursion (start-of-paragraph-text) (point))))
      ;;research-backward
      (if (re-search-backward
           (concat sentence-end "[^ \t\n]") par-beg t)
          ;;match-end返回之前最后一次查找所查找到的position。
          (goto-char (1- (match-end 0)))
        ;;如果没有找到，就返回段落的前部。
        (goto-char par-beg)))
    (setq arg (1+ arg)))
  ;;向后搜索
  (while (> arg 0)
    (let ((par-end
           (save-excursion (end-of-paragraph-text) (point))))
      (if (re-search-forward sentence-end par-end t)
          ;;这个表达式朝后移动，并且忽略所有空格，制表符和回车符，直到找到一个印刷字符为止。
          (skip-chars-backward " \t\n")
        ;;如果没有找到，那么就返回par-end，也就是段落的尾部。
        (goto-char par-end)))
    (setq arg (1- arg))))


(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer.
Returns the count of paragraphs left to move."
  (interactive "^p")
  (or arg (setq arg 1))
  ;;保存point
  (let* ((opoint (point))
         (fill-prefix-regexp
          ;;fill-prefix是填充前缀，它存在，且不为""
          (and fill-prefix (not (equal fill-prefix ""))
               ;;且paragraph-ignore-fill-prefix为nil
               (not paragraph-ignore-fill-prefix)
               ;;此时此表达式被求值，且作为此and表达式的值返回，称为fill-prefix-regexp的值。
               ;;读入一个字符串，并且返回一个精确匹配它的正则表达式
               (regexp-quote fill-prefix)))
         ;; Remove ^ from paragraph-start and paragraph-sep if they are there.
         ;; These regexps shouldn't be anchored, because we look for them
         ;; starting at the left-margin.  This allows paragraph commands to
         ;; work normally with indented text.
         ;; This hack will not find problem cases like "whatever\\|^something"DO.
         (parstart (if (and (not (equal "" paragraph-start))
                            ;;aref 返回某个容器的第几个元素。多数时候是vector,string
                            ;;这里的?^是一个正则，表示除了^的所有的元素都可以。
                            (equal ?^ (aref paragraph-start 0)))
                       (substring paragraph-start 1)
                     paragraph-start))
         (parsep (if (and (not (equal "" paragraph-separate))
                          (equal ?^ (aref paragraph-separate 0)))
                     (substring paragraph-separate 1)
                   paragraph-separate))
         (parsep
          (if fill-prefix-regexp
              (concat parsep "\\|"
                      fill-prefix-regexp "[ \t]*$")
            parsep))
         ;; This is used for searching.
         (sp-parstart (concat "^[ \t]*\\(?:" parstart "\\|" parsep "\\)"))
         start found-start)
    (while (and (< arg 0) (not (bobp)))
      (if (and (not (looking-at parsep))
               (re-search-backward "^\n" (max (1- (point)) (point-min)) t)
               (looking-at parsep))
          (setq arg (1+ arg))
        (setq start (point))
        ;; Move back over paragraph-separating lines.
        (forward-char -1) (beginning-of-line)
        (while (and (not (bobp))
                    (progn (move-to-left-margin)
                           (looking-at parsep)))
          (forward-line -1))
        (if (bobp)
            nil
          (setq arg (1+ arg))
          ;; Go to end of the previous (non-separating) line.
          (end-of-line)
          ;; Search back for line that starts or separates paragraphs.
          (if (if fill-prefix-regexp
                  ;; There is a fill prefix; it overrides parstart.
                  (let (multiple-lines)
                    (while (and (progn (beginning-of-line) (not (bobp)))
                                (progn (move-to-left-margin)
                                       (not (looking-at parsep)))
                                (looking-at fill-prefix-regexp))
                      (unless (= (point) start)
                        (setq multiple-lines t))
                      (forward-line -1))
                    (move-to-left-margin)
                    ;; This deleted code caused a long hanging-indent line
                    ;; not to be filled together with the following lines.
                    ;; ;; Don't move back over a line before the paragraph
                    ;; ;; which doesn't start with fill-prefix
                    ;; ;; unless that is the only line we've moved over.
                    ;; (and (not (looking-at fill-prefix-regexp))
                    ;;      multiple-lines
                    ;;      (forward-line 1))
                    (not (bobp)))
                (while (and (re-search-backward sp-parstart nil 1)
                            (setq found-start t)
                            ;; Found a candidate, but need to check if it is a
                            ;; REAL parstart.
                            (progn (setq start (point))
                                   (move-to-left-margin)
                                   (not (looking-at parsep)))
                            (not (and (looking-at parstart)
                                      (or (not use-hard-newlines)
                                          (bobp)
                                          (get-text-property
                                           (1- start) 'hard)))))
                  (setq found-start nil)
                  (goto-char start))
                found-start)
              ;; Found one.
              (progn
                ;; Move forward over paragraph separators.
                ;; We know this cannot reach the place we started
                ;; because we know we moved back over a non-separator.
                (while (and (not (eobp))
                            (progn (move-to-left-margin)
                                   (looking-at parsep)))
                  (forward-line 1))
                ;; If line before paragraph is just margin, back up to there.
                (end-of-line 0)
                (if (> (current-column) (current-left-margin))
                    (forward-char 1)
                  (skip-chars-backward " \t")
                  (if (not (bolp))
                      (forward-line 1))))
            ;; No starter or separator line => use buffer beg.
            (goto-char (point-min))))))

    (while (and (> arg 0) (not (eobp)))
      ;; Move forward over separator lines...
      (while (and (not (eobp))
                  (progn (move-to-left-margin) (not (eobp)))
                  (looking-at parsep))
        (forward-line 1))
      (unless (eobp) (setq arg (1- arg)))
      ;; ... and one more line.
      (forward-line 1)
      (if fill-prefix-regexp
          ;; There is a fill prefix; it overrides parstart.
          (while (and (not (eobp))
                      (progn (move-to-left-margin) (not (eobp)))
                      (not (looking-at parsep))
                      (looking-at fill-prefix-regexp))
            (forward-line 1))
        (while (and (re-search-forward sp-parstart nil 1)
                    (progn (setq start (match-beginning 0))
                           (goto-char start)
                           (not (eobp)))
                    (progn (move-to-left-margin)
                           (not (looking-at parsep)))
                    (or (not (looking-at parstart))
                        (and use-hard-newlines
                             (not (get-text-property (1- start) 'hard)))))
          (forward-char 1))
        (if (< (point) (point-max))
            (goto-char start))))
    (constrain-to-field nil opoint t)
    ;; Return the number of steps that could not be done.
    arg))
