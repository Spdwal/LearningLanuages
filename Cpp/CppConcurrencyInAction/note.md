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

## 线程管理基础

每个程序至少有一个线程：执行main()函数的线程，其余线程有各自的入口函数，线程与原始线程同时运行，如同main函数执行完会腿粗一样，当线程执行完入口函数后，线程也会退出。

### 启动线程

使用C++线程库启动线程，可以归结于构造std::thread对象。

std::thread构造时传入参数没有过多的限制，只要该类型含有operator()即可，所以可以传入函数指针，仿函数，lambda，fuction对象，都可以。

在命名函数对象时，尽量使用统一初始化的语法，防止解析错误。

启动了线程后，是要等待线程正确加入，或者让其自主运行，如果放其自主运行，会出现某些特殊的bug.

```c++
struct func
{
    int& i;
    func(int& i__):i(i__){}
    void operator()()
    {
        for(unsigned j = 0; j < 1000000; ++j)
        {
            do_something(i);
        }
    }
}

void oops()
{
    int some_local_state = 0;
    func my_func(some_local_state);
    std::thread my_thread(my_func);
    my_thread.detach();
}
```

此处代码的主要问题是my_func内的变量“i”其实不过是oops函数内的一个局部变量的引用，由于oops线程运行结束，然后some_local_state作为局部变量被销毁，my_func内的i就变成引用了一个被销毁的变量，触发UB。处理方法为将数据复制到线程中，而非放在共享内存中，或者可以使用join确认线程在函数结束前结束。

#### 等待线程完成

如果需要等待线程，那么相关的thread实例需要调用thread::join()。

同时调用join的行为还清理了线程相关的储存部分，这样thread不会再与 __已经完成__的线程有任何关联。

只能对一个线程使用一次join，可以用thread::join()来判断该线程是否能再次join。

#### 使用RAII来更加方便的管理线程

```c++
class thread_guard
{
    std::thread& t;
public:
    explicit thread_guard(std::thread& __t): t(__t){}
  	~thread_guard()
    {
        if(t.joinable())
        {
            t.join();
        }
    }
    thread_guard(thread_guard const&) = delete;
	thread_guard& operator=(thread_guard const &) = delete;
};

struct func;

void f()
{
    int some_local_state = 0;
    func my_func(some_local_state);
    std::tread t(my_func);
    thread_guard g(t);
    do_something_in_current_thread();
}
```

当主线程执行到f()结尾处时，局部对象thread_guard()将要第一个被销毁，这是调用 thread_guard的析构函数，从而调用t.join()。

#### 后台运行线程

使用thread::detach()会让线程在后天运行，这就意味值主线程不能与之产生直接交互，如果线程分离，就不可能有thread对象能引用它。

如果一个线程可以被分离，那么也可以被join，所以也可以使用joinable()来判断是否可以分离线程。

## 向线程函数传递参数

```c++
void f(int i, std::string const & s);
std::thread t (f, 3, "hello");
```

thread的构造函数接受一个可变参量作为参数，线程函数中的参数也就可以作为可变参数传入。

在这里隐式调用了string的构造函数，但是如果在其调用期间出了问题，将来在debug时非常难以处理，所以最好在构造thread对象之前就把 string对象构造好，然后再传进去。

```c++
void f(int i, std::string const & s);
string str{"Hello"};
std::thread t(f, 3, str);
```

但是这一段代码还是存在问题，因为函数f中第二个参数期待传入一个引用，但是实际上由于thread并不知道这一点，所以此时传入的其实是str在t中的复制对象的引用，所以可以使用 std::ref()将参数转换成引用的形式。

```c++
std::thread t(f, 3, ref(str));
```

另外还有一种情况是参数只能移动而不能拷贝的时候，例如unquie_ptr，它的拷贝语义是被禁用了的，但是它却可以使用移动语义在函数之间传递，所以在传递它时需要使用std::move()进行显式的移动。

```c++
void process_big_object(std::unique_ptr<big_object>);

std::unique_ptr<bit_object> p(new big_object);
p->prepare_data(42);
std::thread t(process_big_object, std::move(p));
```

同理之下thread和unique_ptr也有相似的地方，但是thread和unique_ptr不同在它并不是一个包含某些动态空间的对象，而是一个资源的集合，每一个thread的实例管理一条线程的运行，它可以在实例之间传递，但是和unique_ptr一样，它也是只能移动不能拷贝的。

## 线程之间传递所有权

* 线程之间传递所有权必须使用移动语义。
* 一个thread对象只能管理一个线程，如果想让一个thread管理两条线程。
* 如果向已经有关联线程的thread对象传递一个线程，那么系统会调用std::terminate，来终止整个进程。
* 线程可以作为函数参数进行传递，但是一定记得，必须利用rvalue作为参数。

代码：thread_group

```c++
class scoped_thread{
    std::thread t;
public:
    explicit scoped_thread(std::thread t__): t(std::move(t__)){
        if(!t.joinable())
            throw std::logic_error("No thread");
    }
    
    ~scoped_thread(){
        t.join();
    }
    scoped_thread(scoped_thread const&) = delete;
    scoped_thread& opertator(scoped_thread const&)=delete;
};

struct func;

void f(){
    int some_local_state;
    scoped_thread t(std::thread(func(some_local_state)));
    do_some_thing_in_current_thread();
}
```

这个代码和之前的thread_guard的区别主要是在对thread::joinable的检查上，scoped_thread是在构造时检查，而thread_guard是在析构时进行检查。

代码：量产线程。

```c++
void do_work(unisgned id);
void f()
{
    std::vector<std::thread> threads;
    for(unsigned i = 0; i < 20; ++i){
        threads.push_back(std::thread(do_work, i));
	}
    std::for_each(threads.begin(),threads.end(), std::mem_fn(&std::thread::join));
}
```

这个代码简单点来说，就是对每一个线程调用join。这里的mem_fn是一个挺有意思的函数，作用是将类的成员函数进行包装，然后进行调用，或者访问，在调用或者访问的时候需要传入进去类的实例。

### 运行时决定线程的数量

C++标准库中的std::thread::hardware_concurrency()函数的返回值即是本机硬件可以真正并行的最大线程数量。如果在系统中找不到可用的数据，那么这个函数会返回0。

因为上下文的频繁切换会降低性能，所以最好不要同时启动过多的线程。

常见的处理方式是使用两个vector，一个vector\<T>来管理线程计算的结果(在计算时使用)，一个vector<thread\>来管理线程。当然最后要调用join，来等待所有线程的终结。

因为函数无法return一个值给主线程，所以我们需要给线程传递一个引用来保存结果。

### 识别线程

线程标识符类型是std::thread::id，可以使用以下两种方式进行检索。

+ 可以调用std::thread::get_id来直接获取，如果thread对象没有和任何线程相关，那么get_id返回std::thread::id默认构造值，这个值表示没有线程。
+ 在当前线程中调用std::this_thread::get_id()也可以获得线程标识符。

std::thread::id可以自由拷贝和对比，因为标识符可以复用。

例如当线程需要分离一项工作的时候，主线程可能要做一些与其他线程不同的工作，这种情况下，启动其他的线程前，它可以使用std::this_thread::id来获取主线程的id，然后将它存储，然后每个线程都要检查一下，其拥有的id是否与初始线程相同。

std::hash提供了std::thread::id的特化版本，所以它也可以作为无序容器的键值。(无序容器的底层都是由hash表实现)。

thread::id本身是一个class，但是它重载了operator<<，所以可以直接用iostream进行输出。

```c++
std::cout << std::this_thread::get_id() << std::endl
```

# 线程间共享数据

## 共享数据带来的问题

在A进程依据一个数据进行工作的时候，由于线程的切换，工作中断，控制权交给其他的线程，但是在B线程工作的过程中，对此共享数据进行了更改，再切换回A进程时，A此时继续之前的工作，此时存在以下几种可能：

+ A和B线程都是对此数据进行写入，那么此时B线程所做的工作被完全破坏。
+ A的工作是根据共享数据的值进行操作，但是它在读取数据之前被暂停，而后B线程对数据进行了更改，再次切换回A线程时，数据已经是B线程进行更改过的数据，引发错误。
+ A对数据进行读取，但是在读取过程中，线程被暂停，而后B线程改变了数据，之后A再次读取数据，此时数据已经被B线程更改，发生错误。

这些错误都有一个共同的名字：条件竞争

#### 条件竞争(trace condition)

并发中竞争条件的形成，取决于一个以上线程的相对执行顺序，每个线程都抢着完成自己任务。

C++标准中定义了数据竞争这个术语：一种特殊的条件竞争，并发的去修改一个独立对象，数据竞争是为定义行为的起因。

恶性条件竞争通常发生于完成对多于一个的数据块的修改时。条件竞争很难查找，也很难复现。

条件竞争通常是时间敏感的，所以在一调试模式运行时，它常会完全消失，因为调试模式回影响程序执行的时间。

#### 避免恶性条件竞争

+ 对数据结构采取某种保护机制，确保只有进行修改的线程才能看到不变量被破坏的中间状态。
+ 对数据结构和不变量的设计进行修改，修改完的结构必须能完成一系列不可分割的变化，也就是保证每个不变量保持稳定的状态。
+ 使用事务的方法去处理数据结构的更新，所需的一些数据和读取都存储在事务日志中，然后将之前的操作和为一步，再进行提交。当数据结构被另一个线程修改之后，或处理已经重启的情况下，提交就会无法进行。(不是太懂， TODO)

保护共享数据结构的最基本的方式，就是使用C++标准库的互斥量。

## 使用互斥量保护对象

当访问共享数据前，使用互斥量将相关数据锁住，在当访问结束后，再将数据解锁。

互斥量是C++中一种最通用的数据保护机制，但是它不是__银弹__，互斥量自身也可能造成死锁或者对性能造成过大影响。

#### C++中使用互斥量

C++中通过实例化mutex创建互斥量，通过调用成员函数mutex::lock进行上锁，mutex::unlock进行解锁。

C++为互斥量提供了一个RAII的模版累std::lock_guard，其会在构造的时候提供已上锁的互斥量，并且在析构的时候进行解锁，从而保证了一个已锁的互斥量总是会被正确的解锁。

```c++
#include<list>
#include<mutex>

std::list<int> some_list
std::mutex some_mutex

void add_to_list(int new_value){
	std::lock_guard<std::mutex> guard(some_mutex);
	some_list.push_back(new_value);
}

bool list_contains(int value_to_find){
	std::lock_guard<std::mutex> guard(some_mutex);
	return std::find(some_list.begin(), some_list.end(), value_to_find != some_list.end());
}
```

大多数情况下，互斥量通常与保护的数据放在同一个类中，而不是定义成全局变量。将其放在一个类中，就可以让他们联系在一起，也可以对类的功能进行封装，并进行数据保护。

互斥量保护的数据需要对接口的设计相当谨慎，要确保互斥量能锁住任何对保护数据的访问，并且不留后门。

使用互斥量来保护数据，要注意wapper类的成员函数不能返回被保护的数据的指针或者引用，这样会导致数据在没有互斥量的保护下被访问。

不要将受保护数据的指针或者引用传递到互斥锁作用域之外，无论是函数的返回值，还是存储在外部的可见内存，或者以参数的形式传递到用户提供的函数中去。



