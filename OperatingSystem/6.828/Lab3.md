# Part A

Int/env.h这个文件里包含了关于用户环境的大部分定义。内核使用Env结构体来对用户环境进行追踪，在这个lab中，我们会从头建立一个用户坏境，我们需要设计JOS来提供多用户环境，在lab4中我们会关注如何让一个用户环境fork出另一个用户环境。

在kern/env.c中，内核维护了3个和环境有关的全局变量。

```c
struct Env *envs = NULL;                    // 所有的环境
struct Env *curenv = NULL;                  // 当前的环境
static struct Env *env_free_list;           // 空闲的环境列表
```

当JOS开始运行之后，envs指针指向一个Env结构体数组，它提供了系统中所有的环境。在我们的设计中，JOS内核会提供一个最大的数字NENV来限定同时运行的环境，尽管在大部分时间中，同时运行的环境远远达不到这个数字。(NENV定义在inc/env.h)。envs数组会每一个活跃的用户维护一个env环境。

JOS内核将所有的没有活跃的Env结构体保存在env_free_list中，这种设计方式方便用户环境进行分配和回收。

内核也会将curenv指针指向在任意时刻正在执行的用户环境的Env结构题，当内核启动的时候，并且还没有任何用户环境运行的时候，curenv的值为NULL。

Env结构体定义在inc/env.h文件中

```c
　　struct Env {

　　　　struct Trapframe env_tf;      //saved registers

　　　　struct Env * env_link;         //next free Env

　　　　envid_t env_id;　　            //Unique environment identifier

　　　　envid_t env_parent_id;        //envid of this env's parent

　　　　enum EnvType env_type;　    　//Indicates special system environment

　　　　unsigned env_status;　　      //Status of the environment

　　　　uint32_t env_runs;           //Number of the times environment has run

 

　　　　pde_t *env_pgdir;　　　     　//Kernel virtual address of page dir.

　　};　　
```

env_tf：这个结构体定义在inc/trap.h中，当环境没有在运行的时候，hold环境中被保存的寄存器的值，内核也会在系统从用户态切换到内核态时候保存这些值，这样的话用户环境可以在之后被回复并且继续执行。

env_link：指向在env_free_list中， 该结构体后一个free的env结构体，前提是这个结构体没有被分配给任意一个用户环境时，该字段才有用。

env_id：这个值可以确定唯一的使用这个结构体的用户环境是什么。当这个用户环境终止，内核会吧这个结构体分配给另外一个不同的环境，这个环境会有不同的env_id值。

env_parent_id：内核保存住产生出这个环境的环境的值，也就是一个进程的父进程的id，通过这个，可以构成一个环境树，通过这个可以指定一个环境可以被允许做什么，来确保安全。

env_type：用于分辨特殊的环境，在大部分的环境中，它是ENV_TYPE_USER，在之后的lab中我们会介绍其他的类型。

env_status；保存了一下几种值的一种：

​	ENV_FREES：此环境没有在运行，然后此环境在env_free_list中。

​	ENV_RUNBLE：此环境正在等待运行在处理器上。

​	ENV_RUNNING：这个环境正在运行。

​	ENV_NOT_RUNNABLE：表示这个结构体代表的是一个活跃的用户 环境，但是它不能被调度运行，因为他在等待其他环境传递给它的消息。

​	ENV_DYING：表示这个结构体对应的是一个僵尸环境。一个僵尸环境在下一次陷入内核时会被回收。

env_pgdir：这个变量存放着这个环境的页目录的虚拟地址。

就和 UNIX操作系统一样，一个JOS的环境拥有线程和地址空间的概念。线程被保存的寄存器的值来定义，而地址空间时由env_pgdir所指向的页目录还有页表来定义的。为了运行一个运行环境，内核不惜设置合适的寄存器的值以及合适的地址空间。

Env结构体和proc结构体很像，两个结构体都保存的环境，用户模式寄存器状态在Trapfram中，在JOS中，单独的环境并没有像process一样拥有他自己的内核栈，在当前，只会有一个JOS环境真正的运行在黑河中，所以JOS只需要一个内核栈。

## Allocating The Environments Array

## Exercise 1

在kern/pmap.c的mem_init()函数中分配并且映射env数组，这个数组是一个yongyou NENV个Env结构体的数组，分配的时候和分配pages非常相似。和pages数组一样，内存所在的这部分空间页应该是用户模式只读的，被映射到虚拟地址UENVS处。

代码基本上照抄Lab2中关于pages的alloc代码即可：

```c
envs = (struct Env*)boot_alloc(NENV*sizeof(struct Env));
memset(envs, 0, NENV * sizeof(struct Env));
boot_map_region(kern_pgdir, UENVS, PTSIZE, PADDR(envs), PTE_U);
```

