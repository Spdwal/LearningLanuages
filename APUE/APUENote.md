# APUE

## 搭建环境

首先从[官网](http://www.apuebook.com/code3e.html)上下载下来需要的代码,然后进行解压：
```shell
tar -xvf src.3e.tar                                        //解压文件
cd apue.3e                                                 //进入文件夹
```
此时可以运行文件夹里的systype.sh可以获得自身系统的类型,接下莱利用文件夹里的Makefile进行编译：
```shell
 make
```
稍等片刻即可，此时查看make之后的文件夹，我们只关注其中的两个，一个是include，一个是lib，其他的都是书里的源代码，可以根据章节自己查找。接下来：
```shell
sudo cp ./include/apue.h /usr/include/
sudo cp ./lib/libapue.a /usr/local/lib/
```
libapue.h是apue.h头文件中包含的函数和宏的具体实行，是一个静态链接库，所以需要将它放入/usr/local/lib/中。

在Linux下，查看/etc/ld.so.conf.d文件中添加库的搜索路径:
```shell
echo “yourldpath” > /etc/ld.so.conf.d/myldpath.conf
```


