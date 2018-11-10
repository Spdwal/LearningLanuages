(defvar unscroll-to nil
  "Text position for next call to 'unroll")

;;第一个stroll-up的时候，保存unscroll-to这个位置
;;如果之后是连续的sctroll-up命令，那么就不改变unscroll-to这个位置
(defadvice scroll-up (before remember-for-unroll
                             activate compile)
  "Remember where we started from, for 'unscroll'."
  (if (not (eq last-command 'scroll-up))
      (setq unscroll-to (point))))


;;此处仅仅恢复光标的位置，而不恢复整个屏幕的情况
(defun unscroll ()
  "Jump to location spectified bt 'scroll-to'."
  (interactive)
  (goto-char unscroll-to))


(defvar unscroll-point nil
  "Cursor position for next call to 'unscroll'.")

(defvar unscroll-window-start nil
  "Window start for next call to 'unscroll'.")

;;窗口横向滚动的列数。
(defvar unscroll-hscroll nil
  "Hscroll for next call to 'unscroll'.")

;;同名修饰会改变之前的修饰。
(defadvice scroll-up (before remember-for-unscroll
                             activate compile)
  "Remember where we started from, for 'unscroll'."
  (if (not (eq last-command 'scroll-up))
      (progn
        (setq unscroll-point (point))
        (setq unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defun unscroll ()
  "revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  ;;第一个参数设置是操作的是哪个窗口，如果是nil那么就表示默认使用当前选中的窗口
  (set-window-start nil unscroll-window-start)
  ;;和set-window-start类似
  (set-window-hscroll nil unscroll-hscroll))


(defun unscroll-maybe-remember ()
  (setq this-command 'unscrollable)
  (if (not (eq last-command 'unscrollable))
      (setq unscroll-point (point)
            unscroll-window-start (point)
            unscroll-hscroll (window-hscroll))))

(defadvice scroll-up (before remember-for-unscroll
                             activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
                               activate compile)
  "remember where we start from, for 'unscroll."
  (unscroll-maybe-remember))

;;etc...
