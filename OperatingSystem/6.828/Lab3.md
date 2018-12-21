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

## Creating and Running Environments

现在我们要做的事在kern/env.c中运行一个用户环境，因为我们现在还没有一个文件系统，所以我们将会设置内核去加载一个在内核中的二进制程序景象文件。

Lab3中的GUNMakefile产生了一大堆二进制镜像在obj/user/文件夹中，我们观察kern/Makefrag中的代码，我们将会发现一些魔法来链接这些以.o结尾的二进制文件进入内核中执行。这个-b链接命令使这些文件被作为一个作为二机制执行文件链接到内核之后。

### Exercise 2

在文件env.c中完成如下函数：

env_init()：初始化所有的在envs数组中的Env结构体，并且把他们加入到env_free_list中，在中间调用env_init_percpu()，来设置硬件段并且将每一个段设置为优先级0或者优先级3。

```c
// Mark all environments in 'envs' as free, set their env_ids to 0,
// and insert them into the env_free_list.
// Make sure the environments are in the free list in the same order
// they are in the envs array (i.e., so that the first call to
// env_alloc() returns envs[0]).
//
void
env_init(void)
{
	// Set up envs array
	// LAB 3: Your code here.
	int i = 0;

	env_free_list = NULL;
    // 和pageinit类似，但是因为需要有相同的顺序，所以需要用头插法。
	for(i = NENV-1; i >=0 ;--i){
		envs[i].env_id = 0;
		envs[i].env_status = ENV_FREE;
		envs[i].env_link = env_free_list;
		env_free_list = &&envs[i];
	}
	
	// Per-CPU part of the initialization
	env_init_percpu();
}
```

env_setup_vm():分配一个pgdir为一个新环境并且在内核中为新的环境初始化一个空间地址。

```c

//
// Initialize the kernel virtual memory layout for environment e.
// Allocate a page directory, set e->env_pgdir accordingly,
// and initialize the kernel portion of the new environment's address space.
// Do NOT (yet) map anything into the user portion
// of the environment's virtual address space.
//
// Returns 0 on success, < 0 on error.  Errors include:
//	-E_NO_MEM if page directory or table could not be allocated.
//
static int
env_setup_vm(struct Env *e)
{
	int i;
	struct PageInfo *p = NULL;

	// Allocate a page for the page directory
	if (!(p = page_alloc(ALLOC_ZERO)))
		return -E_NO_MEM;

	// Now, set e->env_pgdir and initialize the page directory.
	//
	// Hint:
	//    - The VA space of all envs is identical above UTOP
	//	(except at UVPT, which we've set below).
	//	See inc/memlayout.h for permissions and layout.
	//	Can you use kern_pgdir as a template?  Hint: Yes.
	//	(Make sure you got the permissions right in Lab 2.)
	//    - The initial VA below UTOP is empty.
	//    - You do not need to make any more calls to page_alloc.
	//    - Note: In general, pp_ref is not maintained for
	//	physical pages mapped only above UTOP, but env_pgdir
	//	is an exception -- you need to increment env_pgdir's
	//	pp_ref for env_free to work correctly.
	//    - The functions in kern/pmap.h are handy.
	
	// LAB 3: Your code here.
    e->env_pgdir = (pde_t*)page2kva(p);
    p->pp_ref++;
    
    for(int i =0; i < PDX(UTOP); ++i){
        e-env_pgdir[i] = NULL;
    }
    
    for(int i = PDX(UTOP); i < NPENTIRES; ++i){
        e->env_pgdir[i] = kern_pgdir[i];
    }
	// UVPT maps the env's own page table read-only.
	// Permissions: kernel R, user R
	e->env_pgdir[PDX(UVPT)] = PADDR(e->env_pgdir) | PTE_P | PTE_U;
	
	return 0;
}

```

region_alloc():为他分配和map物理地址。

```c
//
// Allocate len bytes of physical memory for environment env,
// and map it at virtual address va in the environment's address space.
// Does not zero or otherwise initialize the mapped pages in any way.
// Pages should be writable by user and kernel.
// Panic if any allocation attempt fails.
//
static void
region_alloc(struct Env *e, void *va, size_t len)
{
	// LAB 3: Your code here.
	// (But only if you need it for load_icode.)
	//
	// Hint: It is easier to use region_alloc if the caller can pass
	//   'va' and 'len' values that are not page-aligned.
	//   You should round va down, and round (va + len) up.
	//   (Watch out for corner-cases!)
    // RoundDown and RoundUp
	void *strat = (void *) ROUNDDOWN(va, PGSIZE);
	void *end   = (void *) ROUNDUP(va+len, PGSIZE);
	
    // 保存page信息，供之后使用。
	struct PageInfo *p = NULL;
	void *i;
	int result;
	for(i = start; i < end; i += PGSIZE){
		p = page_alloc(0);
		if(p == NULL){
			panic("Region alloc failed.\n");
		}
		// 在env_pgdir中插入page，并且map进地址。
		reslut = page_insert(env->env_pgdir, p, i, PTE_W |PTE_U);
		if(result != 0){
			panic("Region alloc failed in page insert.\n");
		}
	}
}

```

Load_icode()：分析一个ELF二进制文件的景象，就和boot loader所做的非常相似，将他的contents加载到一个新的环境的用户空间。

```c
//
// Set up the initial program binary, stack, and processor flags
// for a user process.
// This function is ONLY called during kernel initialization,
// before running the first user-mode environment.
//
// This function loads all loadable segments from the ELF binary image
// into the environment's user memory, starting at the appropriate
// virtual addresses indicated in the ELF program header.
// At the same time it clears to zero any portions of these segments
// that are marked in the program header as being mapped
// but not actually present in the ELF file - i.e., the program's bss section.
//
// All this is very similar to what our boot loader does, except the boot
// loader also needs to read the code from disk.  Take a look at
// boot/main.c to get ideas.
//
// Finally, this function maps one page for the program's initial stack.
//
// load_icode panics if it encounters problems.
//  - How might load_icode fail?  What might be wrong with the given input?
//
static void
load_icode(struct Env *e, uint8_t *binary)
{
	// Hints:
	//  Load each program segment into virtual memory
	//  at the address specified in the ELF segment header.
	//  You should only load segments with ph->p_type == ELF_PROG_LOAD.
	//  Each segment's virtual address can be found in ph->p_va
	//  and its size in memory can be found in ph->p_memsz.
	//  The ph->p_filesz bytes from the ELF binary, starting at
	//  'binary + ph->p_offset', should be copied to virtual address
	//  ph->p_va.  Any remaining memory bytes should be cleared to zero.
	//  (The ELF header should have ph->p_filesz <= ph->p_memsz.)
	//  Use functions from the previous lab to allocate and map pages.
	//
	//  All page protection bits should be user read/write for now.
	//  ELF segments are not necessarily page-aligned, but you can
	//  assume for this function that no two segments will touch
	//  the same virtual page.
	//
	//  You may find a function like region_alloc useful.
	//
	//  Loading the segments is much simpler if you can move data
	//  directly into the virtual addresses stored in the ELF binary.
	//  So which page directory should be in force during
	//  this function?
	//
	//  You must also do something with the program's entry point,
	//  to make sure that the environment starts executing there.
	//  What?  (See env_run() and env_pop_tf() below.)

	// LAB 3: Your code here.

	// Now map one page for the program's initial stack
	// at virtual address USTACKTOP - PGSIZE.

	// LAB 3: Your code here.
	struct Elf* header = (struct Elf*)binary;
	struct Proghdr *ph, *eph;
	if(header->e_magic != ELF_MAGIC){
		panic("Load icode failed: The binary we load is not elf.\n");
	}

	if(header->e_entry == NULL){
		panic("Load icode failed:")
	// 设置程序的入口点，进行切换后就可以直接运行此程序
	e->env_tf.tf_eip = header->e_entry;
	lcr3(PADDR(e->env_pgdir));
        
    // 取program header 和 end of program header。
	ph = (struct Proghdr *)((uint8_t)header + header->e_phoff);
	eph = ph + header->e_phnum;
	for(; ph < eph; ph++){
		if(ph->p_type == ELF_PROG_LOAD){
			if(ph->p_memsz - ph->p_filesz < 0){
				panic("Load icode failed: p_memsz < p_filesz.\n");
			}
			
            // 将物理地址从va保存入evrionment
			region_alloc(e, (void *)p->p_va, p->mem_sz);
            // 将程序整个移动到ph->p_va，
			memmove((void *)ph->p_va, binary+ph->p_offset, ph->p_filesz);
            // 将.bss置0。
			memset((void *)(ph->p_va + ph->p_filesz), 0, p->p_memsz - ph->p_filesz);
		}
	}
	
    // 设置用户栈
	region_alloc(e, (void *) (USTACKTOP - PGSIZE), PGSIZE);
}

```

env_create：利用env_alloc和load_icode函数加载一个ELF文件到用户环境中去。

```c

//
// Allocates a new env with env_alloc, loads the named elf
// binary into it with load_icode, and sets its env_type.
// This function is ONLY called during kernel initialization,
// before running the first user-mode environment.
// The new env's parent ID is set to 0.
//
void
env_create(uint8_t *binary, enum EnvType type)
{
	// LAB 3: Your code here.
	struct Env *e;
	int result;
	if((result = env_alloc(&e, 0)) != 0){
		panic("Env_create failed: env_alloc failed.\n");
	}
	// 加载
	load_icode(e, binary);
	e->env_type = type;
}
```

env_run：真正开始一个用户环境

```c

//
// Context switch from curenv to env e.
// Note: if this is the first call to env_run, curenv is NULL.
//
// This function does not return.
//
void
env_run(struct Env *e)
{
	// Step 1: If this is a context switch (a new environment is running):
	//	   1. Set the current environment (if any) back to
	//	      ENV_RUNNABLE if it is ENV_RUNNING (think about
	//	      what other states it can be in),
	//	   2. Set 'curenv' to the new environment,
	//	   3. Set its status to ENV_RUNNING,
	//	   4. Update its 'env_runs' counter,
	//	   5. Use lcr3() to switch to its address space.
	// Step 2: Use env_pop_tf() to restore the environment's
	//	   registers and drop into user mode in the
	//	   environment.

	// Hint: This function loads the new environment's state from
	//	e->env_tf.  Go back through the code you wrote above
	//	and make sure you have set the relevant parts of
	//	e->env_tf to sensible values.

	// LAB 3: Your code here.
    // 太简单了。。直接按照提示1 2 3 4 5做下来即可
	if(curenv != NULL && curenv->env_status == ENV_RUNNING){
		curenv->env_status = ENV_RUNNABLE;
	}

	curenv = e;
	curenv->env_status = ENV_RUNNING;
	curenv->env_run++;
	lcr3(PADDR(curenv->env_pgdir));

	env_pop_tf(&curenv->env_tf);

	panic("env_run not yet implemented");
}
```



