(counsel-find-file)

;;只显示上一层的目录名字和本文件的名字 buffer-name返回一个string
(message (buffer-name))


;;显示绝对路径 buffer-file-name，这里显示的是一个filename， 返回一个string
(message (buffer-file-name))

;;一个buffer包含了从文件中拷贝过来的信息，我们实际上修改的时候就是在buffer上修改，最后再写入进file中。
;;buffer is visting the file.改变了buffer并没有改变file，只有最后save之后，才会彻底改变file里的内容。
;;但是并不是所有的buffer都关联到一个file，例如*Scrath*或者*Help*。


;;buffer-name只是获得buffer的名字，而current-buffer获得的是buffer本身。
(current-buffer)
;;#<buffer test.el • Chapter_2>

;;返回最近使用的buffer，但是并不是你现在使用的这个
(other-buffer)
;;#<buffer test.el>

;;转换到上一个使用的buffer，传入的参数必须是函数对象。
;;快捷键 C-x b
(switch-to-buffer (other-buffer))

;;返回光标所在的第几个字母所在地
(point)
;;585

;;返回此buffer允许的最小point数字
(point-min)
;;1

;;同理返回最大数字
(point-max)
;;725

(move-point-visually)

;;exercises for chapter2 跳转到当前buffer的中间部位。
(goto-char (/ (point-max) 2))


(provide 'test)
