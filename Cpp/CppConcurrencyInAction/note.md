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

虽然接口内部可能实现了线程安全，但是在接口和接口之间却可能存在着条件竞争。如下例：

```c++
template<typename T, typename Container=std::deque<T>>
class stack{
public:
	explicit stack(const Container&);
	explicit stack(Container && = Container());
	template<class Alloc> explicit stack(const Alloc&);
	template<class Alloc> stack(const Container&, const Alloc&);
	template<class Alloc> stack(Container&&, const Alloc&);
	template<class Alloc> stack(stack&&, const Alloc&);
	
	bool empty() const;
	size_t size() const;
	T& top();
	T const& top() const;
	void push(T const&);
	void push(T&&);
	void pop();
	void swap(stack&&);
};

stack<int> s;
if(!s.empty()){
    int const value = s.top();
    s.pop();
    do_something(value);
}
```

在上述代码中，如果是顺序单线程执行的话，并没有问题，但是如果是在多线程中执行的话，可能在s.empty()和s.pop()之间，被其他线程给修改了此stack，使其变为了空stack，对空stack使用pop会触发未定义行为，这种就是结构固有的问题。

解决方式之一就是使用互斥量来保护top和pop两个成员函数，但是，同样没有银弹。

在std::stack::pop中，设计人员先获取顶部元素，再从栈中移除数据，正常的情况下这是没有问题的，避免数据移除后却没有拷贝出去，但是存在一个问题，如果在return时，由于别的线程使用完了内存，导致返回值的拷贝构造函数抛出异常bad_alloc，那么依然会出现数据没有正确拷贝出去的情况。依然会出现条件竞争，幸运的是我们还有别的选项可以使用。

+ 传入一个引用。

  ```c++
  std::vector<int> result;
  some_stack.pop(result);
  ```

  但是这样子的话需要构造一个栈中类型的实例，对性能有很大的影响。

+ 无异常抛出拷贝构造函数或者移动构造函数。

  局限性太强，用户自定义的类型，并不一定满足这个要求。

+ 返回指向弹出值的指针

  如果采用原生指针，那么就带来了内存管理方面的问题，如果使用智能指针，那么伴随而来的就是内存的开销问题。

+ 选项1+2或者选项1+3

  ```c++
  #include<exception>
  #include<memory>
  
  struct empty_stack: std::exception
  {
  	const char* what() const throw();
  };
  
  template<typename T>
  class threadsafe_stack{
  public:
      threadsafe_stack();
      threadsafe_stack(const threadsafe_stack&);
      threadsafe_stack& operator=(const threadsafe_stack&) = delete;
      
      void push(T new_value);
      std::shared_ptr<T> pop();
      void pop(T& value);
      bool empty() const;
  };
  ```

  不过总觉得这样的实现有点画蛇添足，只是单纯的重载了两个成员函数，根据情况进行使用而已。

  锁的粒度大小非常重要，粒度太小，没有办法锁住所有的想要保护的数据，锁的粒度太大的话，会造成性能的巨大浪费。

  #### 死锁

  一对线程需要对它们所有的互斥量做一些操作，其中每个线程都有一个互斥量，且等待另一个解锁，这样就没有线程能工作，因为它们都在等待对方释放互斥量，这种情况就是死锁。它最大的问题就是由两个或者两个以上的互斥量来锁定一个操作。

  避免死锁的一般建议就是让两个互斥量以相同的顺序上锁：总是在互斥量B之前锁住互斥量A，就永远不会死锁。

  std::lock可以一次性锁住多个的互斥量，且没有副作用(死锁)。

  ```c++
  class some_big_object;
  void swap(some_big_object& lhs, some_big_object& rhs);
  class X{
  private:
      some_big_object some_detail;
      std::mutex m;
  public:
      x(some_big_object const& d): some_detail(sd){}
      
      friend void swap(X& rhs, X& rhs){
          if(&rhs == & lhs){
              return;
          }
          
          std::lock(lhs.m, rhs.m);
          
          std::lock_guard<std::mutex> lock_a(lhs.m, std::adopt_lock);
          std::lock_guard<STD::mutex> lock_b(rhs.m, std::adopt_lock);
          
          swap(lhs.some_detail, rhs.some_detail);
      }
  }
  ```

  当std::lock成功获取了第N个互斥量上的锁的时候，当尝试从第N+1个互斥量中再获取锁的时候，就会有异常抛出，然后依次释放之前的所有锁，所以std::lock要不就都锁住，要不就都锁不住。

  然后通过将锁和std::adopt_lock同时传入lock_guard中，将被lock锁住的锁交给lock_guard管理，而不是让lock_guard再创建一个锁。

  但是lock没有办法通过api对其中的单个锁进行操作，只能通过开发者来确保。

  但是有锁并不是死锁的必要条件，如果两个线程A和B，A中调用了B.join()，B中调用了A.join()，那么这两个线程就会产生死锁，因为它们正在互相等待。

  下面为一下避免死锁的建议：

  ##### 避免嵌套锁

  一个线程获取一个锁时，不要再去获取第二个锁。如果确实需要获取多个锁的时候，使用一个std::lock来做这个事情，避免产生死锁

  ##### 避免在持有锁时调用给用户提供的代码

  因为给用户代码我们并不知道他会做什么，可能在代码中就使用了锁，这样就有可能产生死锁。

  ##### 使用固定书序获取锁

  当必须使用多个锁，且不能使用std::lock来获取它们的时候，用固定的顺序获取它们。

  ##### 使用锁的层次结构

  对应用进行分层，并且识别在给定层上所有的可上锁的互斥量，当代码企图对一个互斥量上锁，在该层锁已经被底层持有时，上锁是不允许的。可以在运行时对其进行检查，痛殴分配层数到每个互斥量上，以及记录每个线程上锁的互斥量。

  ```
  hierarchical_mutex high_level_mutex(1000);
  hierarchical_mutex low_level_mutex(5000);
  
  int do_low_level_stuff();
  
  int low_level_func(){
      std::lock_guard<hierarchical_mutex> lk(low_level_mutex);
      return do_low_level_stuff();
  }
  
  void high_level_stuff(int some_param);
  
  void high_level_func(){
      std::lock_guard<hierarchical_mutex>lk(high_level_mutex);
      high_level_stuff(low_level_func());
  }
  
  void thread_a(){
      high_level_func();
  }
  
  hierarchical_mutex(100);
  void do_other_stuff();
  
  void other_stuff(){
      high_level_func();
      do_other_stuff();
  }
  
  void thread_b(){
      std::lock_guard<gierarchical_mutex> lk(other_mutex);
      other_stuff();
  }
  ```

  大致的意思就是层级锁内保存一个int作为他的层级，当层级较低的锁被该线程持有的情况下，他不能获得层级较高的锁。当层级错误时候可能会抛出一个异常，或者直接终止程序。

  但是当多个同层级的互斥锁不能同时持有，所以在有些设计上没有办法用到。

  死锁并不一定只会发生在锁之间，它也会发生在任何同步构造中——可能会产生一个等待循环。

  ##### std::unique_lock——灵活的锁

  unique_lock和lock_guard最大的不同在于，它的实例并不会一直占有一个锁。如果你将std::adopt_lock作为第二个参数传入，这个参数会告诉构造函数获得一个已经被锁住的mutex。如果第二个参数是std::defer_lock，它表示在构造这个unique_lock时，这个mutex暂时不会被加锁，只有调用了std::unique::lock或者std::unique_lock被传入std::lock()的时候，它才会被加锁。std::unique_lock虽然更加灵活，但是它却需要更多的空间，并且性能上也比std::lock_guard要差一些。这一切都是存储了是否要加锁的信息的代价。

  ```c++
  class some_big_object;
  bool operator<(come_big_object& lhs, some_big_object& rhs);
  
  class X{
  private:
  	some_big_object some_detail;
      mutable std::mutex m;
  public:
      x(some_big_object const& sd):some_detail(sd){}
      
      friend bool operator<(X const& lhs, X const& rhs){
          if(&lhs==&rhs){
              return false;
          }
          std::unique_lock<std::mutex> lock_a(lhs.m, std::defer_lock);
          std::unique_lock<std::mutex> lock_b(rhs.m, std::defer_lock);
          std::lock(lock_a, lock_b);
          return lhs.some_detail<rhs.some_detail;
      }
  };
  ```

##### 不同域中互斥量所有权的传递

std::unique_lock是一个可以移动，不可以赋值的类型。

一种使用方式是允许一个函数去锁住一个互斥量，然后把这个互斥量作为返回值返回给调用它的函数，然后调用者可以在这个锁保护的范围内执行其他的动作。

```c++
std::unique_lock<std::mutex> get_lock(){
    extern std::mutex some_mutex;
    std::unique_lock<std::mutex> lk(some_mutex);
    prepare_data();
    return lk;
}

void process_data(){
    std::unique_lock<std::mutex> lk(get_lock());
    do_something();
}
```

但是正常情况下，并不会直接使用返回unique_lock这么粗暴的做法。而是使用RAII，来将锁和数据封装起来，因为wapper类是会在函数之间作为参数传递的，所以同时锁也应该要能移动。

unique_lock同时也可以提前通过调用unlock()来提前释放锁，这表示我们可以在程序类已经不需要锁的地方提前释放锁，避免性能损失。

##### 锁的粒度

粒度用来描述一个锁保护的数据量大小，一个细粒度锁能够保护较小的数据，一个粗粒度锁可以保护较多的数据。

如果很多线程正在等待同一个资源，当有线程持有锁的时间过长，就会增加等待的时间。在可能的情况下，锁住互斥量的同时只能对共享数据进行访问。在维持锁的粒度上，unique_lock非常好用。

```c++
void get_and_process_data(){
    std::unique_lock<std::mutex> my_lock(the_mutex);
    some_class data_to_process=get_next_data_chunk();
    my_lock.unlock();
    result_type result = process(data_to_process);
    my_lock.lock();
    wirte_result(data_to_process, result);
}
```

锁不仅是能锁住合适粒度的数据，还要控制锁的持有时间，以及什么操作在执行的同时可以控制持有锁的时间，以及什么操作在执行的同时可以持有锁。__一般情况下将锁的持有时间所见到最小。__

## 保护共享数据结构的替代设施

互斥量并不是保护共享数据的唯一方式。有一种比较极端的情况就是：共享数据在并发访问和初始化时候需要保护，但是之后需要进行隐式同步。

###### 保护共享数据的初始化过程

使用一个互斥量的延迟初始化过程

```c++
std::shared_ptr<some_resource> resource_ptr;
std::mutex resource_mutex;
void foo(){
    std::unique_lock<std::mutex> lk(reource_mutex);
    if(!resource_ptr){
        resource_ptr.reset(new some_resource);
    }
    lk.unlock();
    resource_ptr->do_something();
}
```

双重检查锁模式

```c++
void undefined_behaviour_with_double_checked_locking(){
    if(!resource_ptr){
        std::lock_guard<std::mutex> lk(resource_mutex);
        if(!resource_ptr){
            resource_ptr.reset(new some_resource);
        }
    }
    
    resource_ptr->do_something();
}
```

以上模式存在有潜在的条件竞争，在第一次检查resource_ptr之后，若另一个线程调用resource_ptr.reset(),虽然此时该共享指针已经不是指向NULL，但是他所构建的实例却可能还没有完全构建出来，但是第一个if语句却直接跳到了do_something()函数处，得到了UB。

这个例子就是一个典型的条件竞争--------数据竞争。C++标准库提供了std::once_flag和std::call_once来处理这种情况。每个线程只要调用std::call_once，在std::call_once结束的时候，就能安全的知道指针已经被其他线程初始化了。使用std::call_once比显式使用互斥量消耗的资源更少，特别是当初始化完成之后。

```
std::shared_ptr<some_resource> resource_ptr;
std::once_flag resource_flag;

void init_resource(){
    resource_ptr.reset(new some_resource);
}

void foo(){
    std::call_once(resource_flag, init_resource);
    
    resource_ptr->do_something();
}
```

当一个局部变量被声明成static类型。这种变量在声明后已经完成了初始化，对于多线程调用的函数，这意味着这里有条件竞争，每个线程都抢着去定义这个变量，但是在c++11标准中，初始化和定义完全在一个线程中进行，没有其他线程可以对其进行处理。

###### 保护很少更新的数据结构

针对读写者问题，需要一种不同的互斥量：允许两种不同的使用方式。

std::mutex已经在c++17中支持，环境clang++ 10.0。

当一个线程获得一个已经上锁的mutex，并对其再次上锁，这个操作是错误的。但是标准库提供了std::recursive_mutex，它可以在同一个线程内多次上锁，但是允许其他线程访问的时候，必须释放所有的锁。

# 同步并发操作

## 等待一个时间或者其他条件

当一个线程等待另一个线程完成任务时，它会有很多选择。

1. 持续的检查共享数据标识位，知道另一线程完成工作时对这个标识位进行重设，不过这会浪费线程执行时间。

2. 选择在检查间隙，调用std::this_thread::sleep_for()进行周期性的睡眠。

   ```
   bool flag;
   std::mutex m;
   
   void wait_for_flag(){
       std::unique_lock<std::mutex> lk(m);
       while(!flag){
           lk.unlock();
           std::this_thread_sleep_for(std::chrono::milliseconds(100));
           lk.lock();
       }
   }
   ```

   这个实现比第一种要好很多，但是它很难确定正确的休眠时间，太长的休眠时间可能会让任务等待线程醒来。

3. 使用C++标准库提供的工具去等待事件的发生。

### 等待任务达成

C++标准库对条件变量有两套实现：std::condition_variable和std::condition_variable_any。这两个实现都包含在condition_variable头文件声明中，两者都需要与一个互斥量一起才能工作，前者只能和std::mutex一起工作，后者可以和任何满足最低标准的互斥量一起工作。但是std::condition_variable_any会在体积，性能，以及系统资源的使用方面产生额外的开销，所以一般将std::condition_variable作为首选的类型。

```c++
std::mutex mut;
std::queue<data_chunk> data_queue;
std::condition_variable data_cond;

void data_preparation_thread()
{
    while(more_data_to_prepare()){
        data_chunk const data = prepare_data();
        std::lock_guard<std::mutex> lk(mut);
        data_queue.push(data);
        //给持有data_cond的线程发送一个唤醒信号
        data_cond.notify_one();
    }
}

void data_processing_thread(){
    while(true){
        //必须使用unique_lock，因为涉及到提前解锁的操作。
        std::unique_lock<std::mutex> lk(mut);
        //如果接受到了一个notify_one信号，且传入的conditon为true，则此线程唤醒，获得锁，
        //先接到信号，然后获得锁，然后检查cond，如果为真就唤醒，如果为false，就释放锁。
        //所以需要使用unique_lock来加锁
        data_cond.wait(lk, []{return !data_queue.empty();});
        data_chunk data = data_queue.front();
        data_queue.pop();
        lk.unlock();
        process(data);
        if(is_last_chunk(data))
            break;
    }
}
```

###### 虚假唤醒

```c++
std::condition_variable con_var;
...
if(cond == false){
	con_var.wait(lock);
    do_something();
}
```

当cond为false时候，此线程需要等待另一个notify信号，然后一个线程使cond == true，发送一个信号，然后此线程开始与其他线程进行互斥量的争夺，如果竞争失败，那么其他线程对cond进行了处理，再然后此线程获得锁，调用do_something(),此时便产生了UB。

```c++
std::condition_variable con_var;
...
while(cond == false){
    con_var.wait(lock);
}
do_something();
```

上述代码可以很好的防止虚假唤醒，因为在获取锁之后，如果cond被修改，它又进入while循环，然后进入等待状态，释放自己的锁。

## 使用future等待一次性事件

C++标准库将一次性事件称为期望，当一个线程需要等待一个特性的一次性事件的时候，在某种程度上，它就需要知道这个事件在未来表现的形式。之后，这个线程会周期性的等待或者检查，事件是否触发，在执行期间它也会执行其他任务，知道对应的任务出发，而后等待期望的状态就会变成就绪。

在C++标准库中，有两种期望：future和shared_future。std::future的实例只能和一个指定事件相关联，而shared_ptr可以关联多个事件。在后者的实现中，所有实例会在同时变为就绪状态，并且它们可以访问与事件相关的任何数据。期望对象本身并不提供同步访问。当多个线程需要访问一个独立期望对象时候，它们必须使用互斥量或者类似的同步机制对访问进行保护。

```c++
#include<future>
#include<iostream>
int find_the_answer_to_ltuae();
void do_other_stuff();

int main(){
    std::future<int>the_answer = std::async(find_the_answer_to_ltuae);
    do_other_stuff();
    std::cout << "The answer is " << the_answer.get() << std::endl;
}
```

与thread做的一样，std::async允许通过添加额外的参数。

期望是否进行等待取决于std::async是否启动一个线程，或者是否有任务正在进行同步，但是也可以向std::async传递一个参数std::launch，还可以是std::launch::defered，用来表明函数调用被要吃到wait()和get()调用才执行。

sstd::launch::async表明函数必须在其所在的独立线程上执行，std::launch::deferred | std::launch::async表明可以选择这两种方式的一种。

```c++
auto f6=std::async(std::launch::asny, Y(), 1.2);            //在新线程中执行
auto f7=std::async(std::launch::defered, baz,std::ref(x));  //在wait()或者get()调用时候执行。
auto f8=std::async(std::launch::defered | std::launch::async,
                  baz, std::ref(x));                        //到底何时运行依赖函数的实现
auto f9=std::async(baz, std::ref(x));
f7.wait();                                                  //调用defered类型的事件。
```

std::packaged_task把一个期望绑定在一个函数或者一个仿函数上，当packaged__task对象被调用，他调用自己绑定的函数或者仿函数，将期望状态设置为就绪，然后返回数据。这个技术可以用在线程池上。

当一个粒度较大的操作可以被分解成独立的子任务时候，每个子任务可以包含在一个std::packaged_task中实例中，之后这个实例将会传递到任务调度器或者线程池中。

Packaged_task<>的模版参数是一个函数签名，比如std::packaged_task<double(double)>即是接受一个返回值为double，参数为一个double的函数。且里面的类型可以进行隐式转换。

当packaged_task作为一个函数被调用的时候，可谓函数调用操作符提供所需的参数，并且将返回值作为异步结果存储在std::future中，可通过get_future()来获得它。当需要异步任务的返回值时候，可以将期望的状态改为就绪。

###### 使用std::promise

Std::promise提供了提供给future一个值或者是一个异常的途径，std::promise只能被使用一次。

promise和future组成了一个机制，在期望上可以阻塞等待线程，同时，提供数据的线程可以使用组合中的承诺来对相关值进行设置，以及将期望的值设置为就绪。当promise的值以及设置完毕，那么对应期望的值的状态变为就绪，并且可以用于检索已存储的值。

```c++
#include<future>

void process_connections(connection_set& connections)
{
    //连接全部完成则退出。
    while(!done(connections)){
        //依次检验每一个链接，
        for(connection_iterator connection = connections.begin(), end = connections.end();
           connection != end;
            ++connection){
            //是否有数据传入
            if(connection->has_incoming_data()){
                //获得数据
                data_packet data = connection->incoming();
                //此处假设一个数据有一个ID，有一个负载。
                //一个id映射到一个promise。
                //设置result为data.payload
                std::promise<payload_type>& p = 
                    connection->get_promise(data.id);
                p.set_value(data.payload);
            }
            //发送已经出队的传出数据
            if(connection0>has_outgoing_data()){
                outgoing_packet data=
                    connection->top_of_outgoing_queue();
                connection->send(data.payload);
                //将承诺设置为true，来表示传出成功。
                data.promise.set_value(true);
			}
        }
    }
}
```

###### 将期望存储为异常

```
std::future<double> f = std::async(square_root, -1);
double y = f.get();
```

在任何情况下，y获得函数调用的结果，当线程调用f.get()的时候，就能再看到异常了，即使在一个单线程例子中。函数作为std::async的一部分时，当在调用时抛出一个异常，那么这个异常就会存储到“期望”的结果中，之后期望的状态被设置为就绪，之后调用get()会抛出这个存储的异常。当函数打包进入std::packaged_task中时，当这个任务被调用时，同样的事情也会发生。

通过对函数的显示调用，promise也提供同样的功能，但是当希望存入的是一个异常的时候，就需要时set_excetion()成员函数，而不是set_value()。

```c++
extern std::promise<double> some_promise;
try{
    some_promise.set_value(calculate_value());
}catch(...){
    some_promise.set_exception(std::current_exception());
}
```

这里使用了std::current_exception()来检索抛出的异常。可以用std::copy_exception()来存储一个异常，而不抛出它。

```c++
some_promise.set_excetion(std::copy_exception(std::logic_error("foo )));
```

到现在为止我们都在使用std::future，但是在很多线程在等待的时候，只有一个线程能获取等待结果，当多个线程需要等待相同的时间的结果，就需要使用std::shared_future来代替std::future了。

###### 多个线程的等待

Shared_future是可拷贝的，所以多个对象可以引用同一个关联期望的结果。

在每一个std::shared_future的独立对象上，成员函数调用返回的结果还是不同步的，所以为了在多个线程访问一个独立对象的时候，为了避免数据竞争，不许使用锁来对访问进行保护。为了替代只有一个拷贝的情况，可以让每个线程都拥有自己对应的拷贝对象。这样，当每个线程都通过自己拥有的std::shartd_future对象获取结果，那么多个线程访问共享同步的结果就是安全的。

转移所有权是对右值的隐式操作，所以可以通过对promise对象的成员函数get_future()的返回值，直接构造一个std::shared_future对象。

```c++
std::promise<std::string> p;
std::shared_future<std::string> sf(p.get_future());
```

同时std::future还存在有std::future::share方法，可以将期望转换成shared_future，直接转移期望的所有权。

## 限定等待时间

超时方式有两种：一种是指定时间延迟，一种是指定一个时间点。

### 时钟

时钟的当前时间可以用过调用静态成员函数now()从时钟中获取：std::chrono::system_clock::now()；

但是这个时钟依赖于系统时间，所以是不稳定的，标准库还提供了一个稳定时钟：std::chrono::steady_clock；

std::chrono::high_resolution_clock可能是标准库中提供的具有最小节拍周期的时钟，同时也是具有最高分辨率的时钟。

### 时延

时延是时间部分最简单的，std::chrono::duration<>模版函数能够对时延进行处理。第一个参数是一个类型表示：int, double等，第二个参数是指定部分，表示每一个单元用的秒数。例如几分钟的时间可以写成：

```c++
std::chrono::duration<short, std::ratio<60, 1>>     //60秒为1分钟
```

标准库在命名空间内为延时变量提供一系列预定义类型：nanoseconds, microseconds, milliseconds, seconds, minutes, hours等等。

在不要求截断的情况下，时延的转换是隐式的。显示转化可以由std::chrono::duration_cast<>来完成。

```c++
std::chrono::milliseconds ms(54802);
std::chrono::seconds s = std::chrono::duration_cast<std::chrono::seconds>(ms);
```

这样结果就是截断的，而不是进行了舍入。

延时支持计算，所以可以对两个延时量进行加减，或是对一个延时变量乘除一个常数，来获得一个新延迟变量。在时延中可以用过count成员函数获得单位时间的数量。

```c++
std::chrono::milliseconds(1234).count();         //1234
```

基于时延的等待可以由std::chrono::duration<>来完成。

```c++
std::future<int> f = std::async(some_task);
if(f.wait_for(std::chrono::milliseconds(35)) == std::future_status::ready){
    do_something_with(f.get());
}
```

等待函数会返回一个状态值，来表示等待是超时，还是继续等待。在这种情况下，可以等待一个期望，所以当函数等待超时时，会返回std::future_status::timeout，当期望改变，函数会返回std::future_status::readty，当期望任务延迟了，函数会返回std::future_status::deferred。

### 时间点

时钟的时间点可以用std::chrono::time_point<>的类型模版实例来表示，实例的第一个参数用来制定所要使用的时钟，第二个参数表示时间的计量单位(std::chrono::duration<>)。一个时间点的值就是时间的长度。

时间戳是时钟的一个基本属性，但是不可以直接查询，或者在c++标准中已经指定。

```c++
auto start = std::chrono::high_resolution_clock::now();
do_something();
auto stop = std::chrono::high_resolution_clock::now();
std::cout << "do_something() took " << std::chrono::duration<double, std::chrono::seconds>(stop-start).count() << " seconds" << std::endl;
```

Std::chrono::time_point()实例的时钟参数可不仅是能够指定UNIX时间戳的。

```c++
#include<condition_variable>
#include<mutex>
#include<chrono>

std::condition_variable cv;
bool done;
std::mutex m;

bool wait_loop(){
    //获取一个时间点。
    auto const timeout=std::chrono::steady_clock::now() + std::chrono::milliseconds(500);
    std::unique_lock<std::mutex> lk(m);
    while(!done){
       	//等待到这个时间点
        if(cv.wait_until(lk, timeout) == std::cv_status::timeout)
            break;
    }
    return done;
}
```

### 具有超时功能的函数

对一个特定线程添加一个延迟处理。当这个线程无所事事的时候，就不会占用其他线程的处理时间。这两个处理函数分别是std::this_thread::sleep_for和std::this_thread::sleep_until();

当线程因为指定时延进入睡眠时，可以通过sleep_for()进行唤醒，或者因指定时间点睡眠的，可以使用sleep_until唤醒。超时可以配合条件变脸和期望一起使用。超时甚至可以在尝试获取一个互斥锁时使用。std::mutex和std::recursive_mutex都不支持超时锁，但是std::timed_mutex和std::recurseive_timed_mutex支持。这两种类型也有try_lock_for()和try_lock_until()成员函数存在，可以在一段时间内尝试，或者在指定时间点前获取互斥锁。

## 使用同步操作简化代码

### 使用期望的函数式编程

#### 快速排序FP模式版本

```c++
template<typename T>
std::list<T> sequential_quick_sort(std::list<T> input){
    if(input.empty()){
        return input;
    }
    std::list<T> result;
    result.splice(result.begin(), input, input.begin());
    //将第一个值作为锚点
    T const& pivot = *result.begin();
    auto devide_point = std::partition(input.begin(),input.end(),[&](T const& t)[return t < pivot;]);
    
    std::list<T> lower_part;
    lower_part.splice(lower_part.end(), input, input.begin(), devide_point);
    auto new_lower(sequential_quick_sort(std::move(lower_part)));
    auto new_higher(sequential_quick_sort(std::move(input)));
    
    result.splice(result.end(), new_higher);
    result.splice(result.begin(), new_lower);
    return result;
}
```

。。。。。裤子都脱了就给我看了个这个。这个快速排序唯一不同的一点就是把lower和higher这两个链表给存储起来，然后通过splice进行拼接。

#### 快速排序线程强化版

```c++
template<typename T>
std::list<T> parallal_quick_sort(std::list<T> input){
    if(input.empty()){
        return input;
    }
    
    std::list<T> result;
    result.splice(result.begin(), input, input.begin());
    T const& pivot = *result.begin();
    auto divide_point = std::partition(input.begin() input, input.end(),
                                      [&](T const& t){return t < pivot;});
    
    std::list<T> lower_part;
    lower_part.splice(lower_part.end(), input, input.begin(),
                     divide_point);
    //此处代码的parallel_quick_sort之前可加&也可不加&，不知道为何
    std::future<std::list<T>> new_lower(std::async(parallel_quick_sort<T>, std::move(lower_part)));
    auto new_higher(parallel_quick_sort(std::move(input)));
    
    reslut.splice(result.end(), new_higher);
    reslut.splice(result.begin(), new_lower.get());
    return result;
}
```

asny会启动一个新线程，这样当递归三次时候，就有8个线程在运行，当递归十次的时候，会有1024个线程在运行。

比起使用asd::async()，你可以写一个spawn_task函数对std::packaged_task和std::thread进行简单的包装：

```c++
template<typename F, typename A>
std::future<std::result_of<F(A&&)>::type>
spawn_task(F&& f, A&& a){
	using std::result_of<F(A&&)>::type = result_type;
	std::packaged_task<result)type(A&&)> task(std::move(task(f)));
	std::future<result_type> res(task.get_future());
	std::thread t(std::move(task), std::move(a));
	t.detack();
	return res;
}
```

因为避开了共享易变数据，函数化编程可算是并发编程的典范，并且也是通讯顺序进程的范型。这里的线程理论上是完全分开的，也就是没有共享数据，但是有通讯通道允许信息再不同的线程内进行传递。

### 使用消息传递的同步操作

CSP的概念十分简单：当没有共享数据，每个线程就可以独立进行思考，其行为纯粹基于其所收到的信息。每个线程都有一个状态机：当线程收到一条信息，它将会以某种方式更新其状态，并且可能向其他线程发出一条或者多条信息，对于消息的处理依赖于线程的初始化状态。

无论你选择用什么方式去实现每个线程，任务都会分成独立的处理部分，这样会消除潜在的混乱，这样编程就会变得更加简单，且有更高的容错率。

真正的通讯顺序处理是没有共享数据的，所有的消息都是通过消息队列传递，但是因为c++线程共享一块地址 空间，所以达不到真正通讯处理的要求。所以我们有责任确保在我们的实现中，线程不存在共享数据。当然为了线程间的通信，消息队列是必须要共享的。

一个ATM逻辑类的简单实现

```c++
struct card_inserted{
    std::string account;
}

class atm{
    messaging::receiver incoming;
    messaging::sender bank;
    messaging::sender interface_hardware;
    
    void (atm::*state)();
    
    std::string account;
    std::string pin;
    
    void waiting_for_card(){
        interface_hardware.send(display_enter_card());
        incoming.wait().handle<card_inserted>(
      	  [&](card_inserted const& msg){
       	     account=msg.account;
       	     pin="";
  			 interface_hardware.send(display_enter_pin());
             state=&atm::getting_pin;
          });
    }
    
    void getting_pin(){
        //wait等待三种信号，并对它们进行处理
        incoming.wait()
            .handle<digit_pressed>([&](digit_pressed const& msg){
                unsigned const pin_length=4;
                pin+=msg.digit;
                if(pin.length()==pin_length){
                    bank.send(verify_pin(account, pin, incoming));
                    state = &atm::verifying_pin;
                }
            })
            .handle<chear_last_pressed>([&](clear_last_pressed const& msg){
                if(!pin.empty()){
                    pin.resize(pin.length() - 1);
                }
            })
            .handle<cancel_pressed>([&](cancel_pressed const& msg){
                state=&atm::done_processing;
            });
    }
public:
    void run(){
        state = &atm::waiting_for_card;
        try{
            for(;;){
                (this->*state)();
            }
        }catch(messaging::close_queue const&){
            
        }
    }
};
```

