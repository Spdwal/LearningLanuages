# Chapter 1 Introduction

## 什么是并发？

并发就是多个独立的活动同时发生。

在单处理器计算机中，由于机器某一时刻只能执行一个任务，不过它可以在每秒进行多次任务切换，给人一种并行的假象。现代的多处理器计算机可以做到真正的并行多个任务，我们称其为硬件并发。

### 任务切换

系统每次从一个任务切换到另一个任务时，需要切换一次上下文，在进行上下文的切换的时候，操作系统必须为当前的任务保存CPU状态和指令指针，并计算出要切换到哪个任务，并为即将切换到的任务重新加载处理器状态，然后CPU将新任务的指令和数据的内存载入到缓存中。但是切换任务并不是没有代价的，做这一切都需要时间。硬件并发在多处理器或者多核系统上效果更加显著，但是正常情况下，计算机里总是存在着超出硬件可并行最大任务数的任务在执行，所以任务切换仍然适用。但是具体的调动和时间片的分配，有特殊的算法进行决定。

#### 多进程并发

将应用程序分为多个独立的进程，他们在同一时刻运行，独立的进程可以通过进程间常规的通信渠道传递信息，不过有以下几个劣势

+ 进程间通信较为复杂，且慢。
+ 由于每个进程都要维护自己的资源，所以占用空间较多。
+ 进程上下文切换的时间开销较大。

但是同时，它却可以更容易的编写安全的并发代码。

#### 多线程并发

线程像是很轻量级的进程，每个线程互相独立运行，且线程可以在不同的指令序列里运行，但是进程中所有的线程都共享地址空间，并且所有线程都访问到大部分的数据。这种共享虽然效率更高，但是更加难以编写安全的代码。

## 为什么使用并发？

+ 关注点分离，即将无关代码和相关代码分离。
+ 获取更好的性能。

当收益比不上成本的时候，不使用并发。

此外，线程是有限的资源，如果让太多的线程同时运行，则会消耗很多操作系统资源。而且因为每个线程需要一个独立的堆栈空间，所以运行太多的线程也会耗尽进程的可用内存和地址空间。

线程池可以用来限制线程的数量，常用于服务器等需要高并发的场景中。

## C++中的并发

C++98标准并不承认线程的存在，但是可以使用编译器的扩展 ，例如POSIX或者win32。

C++11添加了对线程的支持。
