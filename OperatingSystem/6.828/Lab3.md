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
		env_free_list = &envs[i];
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

通过调用链，确保你理解下面的每一步。

- start (kern/entry.S)
- i386_init(kern/init.c)
  - cons_init
  - mem_init
  - env_init
  - trap_init(still incomplete at this point)
  - env_create
  - env_run
    - env_pop_tf

当我们在QEMU下运行它的时候，如果一切正常的话，我们的系统会进入用户空间并且通过系统调用，运行hello文件，但是这个时候，它并不会成功运行，因为JOS还没有设置相关硬件来实现从用户态到内核态的转换功能，当CPU发现它没有被设置能成正确处理这种系统调用中断的时候，他会触发一个保护一场，然后又发现这个保护异常也没办法处理，从而又产生一个错误异常，然后又发现仍然无法解决问题，所以最后会放弃，我们把这个称之为"triple fault"。接下来CPU会抚慰，系统会重启。

接下来我们来解决这个问题，不过解决之前我们可以使用调试器来检查一下程序要进入用户模式时做了什么。使用make qemu-gdb 并且在 env_pop_tf 处设置断点，这条指令应该是即将进入用户模式之前的最后一条指令。然后进行单步调试，处理会在执行完 iret 指令后进入用户模式。然后依旧可以看到进入用户态后执行的第一条指令了，该指令是一个cmp指令，开始于文件 lib/entry.S 中。 现在使用 b *0x... 设置一个断点在hello文件（obj/user/hello.asm）中的sys_cputs函数中的 int $0x30 指令处。这个int指令是一个系统调用，用来展示一个字符到控制台。如果你的程序运行不到这个int指令，说明有错误。

经过调试，可以运行到这个指令，但是si之后无限循环。qemu显示：

Triple fault.  Halting for inspection via QEMU monitor.

### Handling Interrupts and Exceptions

到目前为止，当程序运行到第一个系统调用int $0x30的时候，就会进入错误的状态，因为现在系统无法从用户态切换到内核态，所以需要一个基本的异常/系统调用处理机制，使得内核可以从用户态转换为内核态

### Exercise 3

阅读80386文档中的第九章和IA-32开发者手册的第五章。

完成，顺便还看了一下操作系统真相还原，推荐这本书。

在这个Lab中，我们会查看intel的中断，错误，等等，虽然异常，陷阱，中断，falut等等，在架构或者操作系统看来没有标准的意义，但是在x86上它们还是有席位的分别的。当在Lab外看到这些东西的时候，应该要明白它们之间的区别。

## Basics of protected control transfer

异常和中断都是被保护的转移方法，它们会使处理器从用户模式转移到内核模式(CPL = 0)，在这期间，用户态的代码没有权利去干扰内核的运转或者其他环境，在Intel的术语中，一个中断是一个处理器外部异步发生的事件所出发的保护转移方法。比如外部IO硬件的通知，一个异常正好相反，是当前正字啊运行的指令所带来的同步的处理器控制权的转移，比如是除0异常。

为了确保受保护的转移是真真正正的受保护，处理器的中断/异常机制被设计成正在运行的代码无法选择在中断或者异常出现的时候，无法选择内核进入的地点和方法。作为替代，处理器确定内核可以在小心的控制条件下进入，在x86中，由以下两种机制提供保护：

1. 中断描述符表：处理器确定中断和异常只能使内核进入一些特殊的被内核定义好的程序入口点，而不会被用户态代码确定。x86架构允许内核中有256种中断或者异常入口点，每一个都有一个中断向量，一个向量是一个0～255之间的数字，一个中断向量值是根据中断源来决定的：不同设备，错误条件，以及对内核的请求都会产生出不同的中断和中断向量的组合，CPU将使用这个向量作为这个中断在中断向量表中的索引。这个表是由内核设置的，放在内核空间中，这个和GDT非常像，通过表中的任何一个表项，处理器可以知道：
   + 需要加载到eip寄存器中的值，这个zhi指向了处理这个中断的中断处理程序的位置。
   + 需要加载到cs寄存器的值，里面还包含了这个中断处理程序的运行特权值。
2. 任务状态段：处理器在中断或者异常发生，调用异常处理程序之前需要一个空间来保存旧处理器的状态，比如eip和cs的值，这样一来，当异常处理程序结束之后可以重新载入旧状态，并且回复中断代码。但是这个保存的地址也要从无特权的用户态代码中保护起来，否则的话，错误或者不明确的代码会破坏内核的运行。当x86获得一个中断或者一个陷阱，并且使用特权从用户态转换到内核态时，他也会把它的堆栈切换到内核空间。一个叫任务状态段(TSS)的数据结构会详细记录这个堆栈所在的段的段描述符的地址，处理器会把SS,ESP,EFLAGS,CS,EIP和一个可选的错误码等等值压入到这个堆栈上，然后加载处理中断程序的CS，EIP值，并且设置ESP，SS寄存器指向新的堆栈。尽管TSS，并且支持其他很多功能，但是JOS仅仅用它来定义处理器从用户态到内核态所采用的内核对战，由于JOS中的内核特权级就是0，所以处理器用TSS字段的ESP0,SS0字段等来指明这个内核堆栈的位置和大小。

## Types of exceptions and interrupts

x86处理器产生的中断向量是0~31，映射到IDT的0~31。例如一个页错误，会产生一个14号向量的错误，内核可以产生大于31的中断向量，它们可以用过int指令产生，或者是由外部设备产生的外部中断。

在这一节中，我们会扩展JOS的功能，使它能够处理x86产生的0-31的内部中断，在下一节中，我们会使JOS处理中断向量48(0x30)，它主要被用来做系统调用，在Lab4中会继续扩展JOS使它能处理外部硬件终端，例如时钟中断。

## An Example

加下来我们看一个例子，现在处理器正在用户态运行一个代码并且遭到一个除0的错误。

1. 处理器转换到TSS中的SS0和ESP0所指向的堆栈，在JOS中分别存放着GD_KD和KSTACKTOP的值。
2. 处理器吧异常参数压入内核地址中，起始于地址KSTACKTOP。
3. 因为我们要处理的是除0异常，它的中断向量是0，处理器会读取IDT表中的0号表项，并且吧cs:eip的值设置为0号中断处理函数的地址值。
4. 中断处理函数开始执行，并且处理中断。

对于某些特定的异常，除了上面图中要保存的5个值以外，还要再压入一个字，叫做错误码，比如页表错误，就是其中的一个实例。

以上几步都是硬件自动完成的。

## Nested exceptions and interrupts

处理器可以从内核和用户模式中读取中断和异常。只有当处理器从用户态切换到内核态时候，才会自动的切换堆栈，并且把一些寄存器中原来的值压入到堆栈上，并且触发相应的中断处理函数。但是如果处理器已经犹豫正在处理中断而在内核态下，此时CPU只会向栈中压入更多的值，通过这种方式，内核旧可以处理嵌套中断。

如果处理器已经在内核态下并且遇到了嵌套中断，因为他不需要切换堆栈，所以它不需要存储SS,ESP寄存器的值。

在处理器处理嵌套中断的时候，有一个非常重要的警告。如果处理器在内核态下获得一个异常，但是由于栈空间不够而不能将它的旧状态push进栈中，那么处理器旧无法恢复到原来的状态了，他会自己动重启。

## Setting up the IDT

现在应该有了所有的基本信息去设置IDT表，并且在JOS处理异常，现在我们只需要处理内部异常，(中断向量号0~31)。

在头文件inc/trap.h和kern/trap.h中包含了和中断异常相关的非常重要的定义，我们应该好好熟悉一下其中包含的定义。kern/trap.h中包含了仅内核可见的一些定义，inc/trap.h中包含了用户态可见的一些定义。

每一个中断或者异常都有自己的中断处理函数，分别定义在trapentry.S中，trap_init()将初始化IDT表，每一个处理函数都应该构建一个结构体Trapframe在堆栈上，并且调用trap函数指向这个结构体，trap()然后处理异常和中断，给他分配一个中断处理函数。

所以整个操作系统的中断控制流程为：

1. Trap_init()现将所有的中断处理函数的起始地址放在中断向量表IDT中。
2. 当中断发生的时候，不管是内部中断还是外部中断，处理器捕捉到此中断，并且进入内核态，根据中断向量去查询中断向量表，找到对应的表项。
3. 保存被中断的程序的上下文到内核堆栈中，调用这个表项中指明的中断处理函数。
4. 执行中断处理函数。
5. 执行完成后，回复被中断的进程的上下文，返回用户态，继续运行这个进程。

## Exercise 4

编辑trapentry.S和trap.c，并且完成以下特性。在trapentry.S中的宏TRAPHANDLER和TRAPHANDER_NOEC会对你有帮助，就好像inc/trap.h中的T_*宏一样。你需要在trapentry.S中为每一个陷阱添加一个入口点，并且提供\_alltraps的值。

你需要修改trap_init()函数来初始化idt表，使表中每一项指向定义在trapentry.S中的入口指针，SETGATE宏定义都在这里能用的上。

你所实现的\_alltraps应该：

1. 把值压入堆栈让堆栈看起来像一个结构体Trapframe

2. 加载GD_KD的值到%ds，%es寄存器中

3. 把%esp的值压入，并且传递一个指向此Trapframe的指针到trap()函数中。

4. 调用trap

   考虑使用pushal指令，他会很好的和结构体Trapframe的布局配合好。

```nasm

/*
 * Lab 3: Your code here for generating entry points for the different traps.
 */
/* 参考80386文档的对于trap的描述，来确定每一个trap是否存在errocode */
	TRAPHANDLER_NOEC(DIVIDE, T_DIVIDE)
	TRAPHANDLER_NOEC(DEBUG, T_DEBUG)
	TRAPHANDLER_NOEC(NMI, T_NMI)
	TRAPHANDLER_NOEC(BRKPT, T_BRKPT)
	TRAPHANDLER_NOEC(OFLOW, T_OFLOW)
	TRAPHANDLER_NOEC(BOUND, T_BOUND)
	TRAPHANDLER_NOEC(ILLOP, T_ILLOP)
	TRAPHANDLER_NOEC(DEVICE, T_DEVICE)
	TRAPHANDLER_NOEC(DBLFLT, T_DBLFLT)
	TRAPHANDLER(TSS, T_TSS)
	TRAPHANDLER(SEGNP, T_SEGNP)
	TRAPHANDLER(STACK, T_STACK)
	TRAPHANDLER(GPFLT, T_GPFLT)
	TRAPHANDLER(PGFLT, T_PGFLT)
	TRAPHADDLER_NOEC(FPERR, T_FPERR)
	TRAPHANDLER(ALIGN, T_ALIGN)
	TRAPHANDLER_NOEC(MCHK, T_MCHK)
	TRAPHANDLER_NOEC(SIMDERR, T_SIMDERR)
	TRAPHANDLER_NOEC(SYSCALL, T_SYSCALL)
	TRAPHADDLER_NOEC(DEFAULT, T_DEFAULT)
/*
 * Lab 3: Your code here for _alltraps
 */

struct PushRegs {
	/* registers as pushed by pusha */
	/* 通过pushax 可以一次性全部push进入 */
	uint32_t reg_edi;
	uint32_t reg_esi;
	uint32_t reg_ebp;
	uint32_t reg_oesp;		/* Useless */
	uint32_t reg_ebx;
	uint32_t reg_edx;
	uint32_t reg_ecx;
	uint32_t reg_eax;
} __attribute__((packed));

struct Trapframe {
	struct PushRegs tf_regs;
	uint16_t tf_es;
	uint16_t tf_padding1;
	uint16_t tf_ds;
	uint16_t tf_padding2;
	uint32_t tf_trapno;
	/* 在这之上的数据需要手工来进行push， trapno就是errornumber*/
	/* below here defined by x86 hardware */
	uint32_t tf_err;
	uintptr_t tf_eip;
	uint16_t tf_cs;
	uint16_t tf_padding3;
	uint32_t tf_eflags;
	/* below here only when crossing rings, such as from user to kernel */
	uintptr_t tf_esp;
	uint16_t tf_ss;
	uint16_t tf_padding4;
} __attribute__((packed));



.globl _alltraps
_alltraps:
	push %ds
	push %es

	pushal

	movl $GD_KD, %eax
	movw %ax, %ds
	movw %ax, %es

	push %esp

	call trap

```

```c
// 声明各个函数。
void DIVIDE();
void DEBUG();
void NMI();
void BRKPT();
void OFLOW();
void BOUND();
void ILLOP();
void DEVICE();
void DBLFLT();
void TSS();
void SEGNP();
void STACK();
void GPFLT();
void PGFLT();
void FPERR();
void ALIGN();
void MCHK();
void SIMDERR();
void SYSCALL();

void
trap_init(void)
{
	extern struct Segdesc gdt[];

    
	// LAB 3: Your code here.
	// 利用SETGATE将处理函数与idt中的描述符所指向的函数地址相关联
	SETGATE(idt[T_DIVIDE], 0, GD_KT, DIVIDE, 0);
	SETGATE(idt[T_DEBUG], 0, GD_KT, DEBUG, 0);
	SETGATE(idt[T_NMI], 0, GD_KT, NMI, 0);
	SETGATE(idt[T_BRKPT], 0, GD_KT, BRKPT, 3);
	SETGATE(idt[T_OFLOW], 0, GD_KT, OFLOW, 0);
	SETGATE(idt[T_BOUND], 0, GD_KT, BOUND, 0);
	SETGATE(idt[T_ILLOP], 0, GD_KT, ILLOP, 0);
	SETGATE(idt[T_DEVICE], 0, GD_KT, DEVICE, 0);
	SETGATE(idt[T_DBLFLT], 0, GD_KT, DBLFLT, 0);
	SETGATE(idt[T_TSS], 0, GD_KT, TSS, 0);
	SETGATE(idt[T_SEGNP], 0, GD_KT, SEGNP, 0);
	SETGATE(idt[T_STACK], 0, GD_KT, STACK, 0);
	SETGATE(idt[T_GPFLT], 0, GD_KT, GPFLT, 0);
	SETGATE(idt[T_PGFLT], 0, GD_KT, PGFLT, 0);
	SETGATE(idt[T_FPERR], 0, GD_KT, FPERR, 0);
	SETGATE(idt[T_ALIGN], 0, GD_KT, ALIGN, 0);
	SETGATE(idt[T_MCHK], 0, GD_KT, MCHK, 0);
	SETGATE(idt[T_SIMDERR], 0, GD_KT, SIMDERR, 0);
	SETGATE(idt[T_SYSCALL], 0, GD_KT, SYSCALL, 3);
	// Per-CPU setup 
	trap_init_percpu();
}
```

### Question

1. 为什么不同的异常需要不同的处理函数。

​       这不废话么，不同的异常需要进行不同的处理，所以需要不同的函数进行处理。否则压根不需要这么多不同的中断。

2. grade脚本产生一个protection fault(trap 13),但是softint代码又运行了int $14,为什么它会产生一个vector 13的中断，在这个时候，内核做了什么？

   因为此时在用户态代码之下，用户态特权级为3，此时它企图产生一个特权级为0的中断，所以它产生了 一个protection fault的中断。

## Part B: Page Faults, Breakpoints Exceptions, and System Calls

### Handling Page Faults

Page fault异常，中断向量号14（T_PGFLT)，他是一个非常重要的异常。当一个处理器发现了一个page fault，它储存产生这个异常的地址在一个特殊的控制寄存器cr2中，在trap.c中，我们给了一个特殊的函数page_fault_hander()来处理page fault exception。

### Exercese 5

修改一下trap_dispatch函数，使系统能把缺页异常引导到page_fault_handler()，在修改完成后，运行magegrade，出现的结果是你修改后的JOS可以成功运行faultread，faultreadkernel，faultwarite，faultwritekernel测试程序。

```c

static void
trap_dispatch(struct Trapframe *tf)
{
	// Handle processor exceptions.
	// LAB 3: Your code here.
    // 太简单了。。就比较一下trapno即可
	if(tf->tf_trapno == T_PGFLT){
		page_fault_handler(tf);
	}else{
		print_trapframe(tf);
		if (tf->tf_cs == GD_KT)
			panic("unhandled trap in kernel");
		else {
			env_destroy(curenv);
			return;
		}
	}
	// Unexpected trap: The user process or the kernel has a bug.

}

```

## The Breakpoint Exception

断点异常，中断向量3，是用来允许调试者用来在代码中打断点，将当前代码替换成int3命令。在JOS中，我们会将这个异常转换成一个伪系统调用，这样子的话，任何用户环境都可以使用这个微系统调用来触发JOS kernel monitor。

### Exercise 6

修改trap_dispatch函数，使断点异常发生时，能够触发kernel monitor，修改完成后运行make grade，运行结果应该是你修改后能够正确运行breakpoint测试程序

```c

static void
trap_dispatch(struct Trapframe *tf)
{
	// Handle processor exceptions.
	// LAB 3: Your code here.
	if(tf->tf_trapno == T_PGFLT){
		page_fault_handler(tf);
	}else if(tf->tf_trapno == T_BRKPT){
		monitor(tf);
	}else{
		print_trapframe(tf);
		if (tf->tf_cs == GD_KT)
			panic("unhandled trap in kernel");
		else {
			env_destroy(curenv);
			return;
		}
	}
	// Unexpected trap: The user process or the kernel has a bug.

}
```

