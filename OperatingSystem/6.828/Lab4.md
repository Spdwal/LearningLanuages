# nPart A： multiprocessor support and cooperative multitasking

在这个lab的第一部分，我们会扩展JOS让他运行在一个多处理器的系统上，然后完成一下JOS的系统调用，来允许用户态环境可以产生一个新的环境。我们可以完成合作的循环调度，从而允许内核通过使环境自愿放弃对cpu的占用来切换环境，在第三部分，我们会完成抢先调度，他会允许内核从用户环境中获得CPU的资源，然后将它分配给另一个环境。

## Multiprocessor support

我们将会使JOS提供"symmetric multiprocessing"(smp)，一个多处理器模式在所有的CPU中都拥有相同的机会来获得系统的资源，例如内存和IO总线。当所有的CPU在SMP模式下都相同的时候，在boot过程中，它们可以分成两种种类：The bootstrap processor(BSP)是为了初始化系统并且引导整个操作系统，The applications processors(APS)是被BSP在完成了操作系统的启动之后启动的，哪一个处理器被当作BSP是由硬件和BIOS决定的，在这个时间点上，我们目前所有写过的代码，全部都运行在BSP上。

在一个SMP系里，每一个CPU都有一个local APIC(LAPIC)单元，LAPIC单元是为了给系统传送中断使用，LAPIC给它锁链接的CPU一个独有的标示码。在这个实验中，我们会利用LAPIC单元的基本操作。(kern/lapic.c)

+ 通过读取LAPIC标示符来找到我们的代码运行在哪个CPU上(cpunum())
+ 将STARTUP这个处理器内部中断从BSP中发送到APS中，从而启动其他的CPU。
+ 在C部分，我们为LAPIC的内部时钟编程，从而触发时钟中断，以提供premptive multitasking(apic_init())。

一个处理器通过memory-mapped I/O(MMIO)来访问他的LAPIC，在MMIO中，一个物理内存端口连接了一些io硬件的寄存器，所以同样的加载/存储操作指令可以被用来访问设备寄存器，我们已经在0xA0000处见到了一个IO孔(用来作为VGA的缓冲区)，LPAIC在物理地址0xFE000000处有一个IO孔，所以它对我们这种使用普通的直接映射在KERNBASE处的方法来说太高了。JOS的虚拟内存映射在MMIOBASE留下了一个4MB的间隙，所以我们有一个地方来映射这样的设备，后来的实验引入了更多的MMIO区域，我们将会编写一个简单的函数从该区域分配空间并且将设备内存映射到它上来。

### Exercise 1

在kern/pmap.c中实现mmio_map_region，要了解如何使用它，请查看kern/lapic.c中lapis_init函数的开头，在运行测试之前，我们需要完成下一个练习。

我们先看lapis.c中的lapic_init()函数如何使用mmio_map_region

```c
	// lapicaddr is the physical address of the LAPIC's 4K MMIO
	// region.  Map it in to virtual memory so we can access it.
	lapic = mmio_map_region(lapicaddr, 4096);
```

```c
//
// Reserve size bytes in the MMIO region and map [pa,pa+size) at this
// location.  Return the base of the reserved region.  size does *not*
// have to be multiple of PGSIZE.
//
void *
mmio_map_region(physaddr_t pa, size_t size)
{
	// Where to start the next region.  Initially, this is the
	// beginning of the MMIO region.  Because this is static, its
	// value will be preserved between calls to mmio_map_region
	// (just like nextfree in boot_alloc).
	static uintptr_t base = MMIOBASE;
	
	// Reserve size bytes of virtual memory starting at base and
	// map physical pages [pa,pa+size) to virtual addresses
	// [base,base+size).  Since this is device memory and not
	// regular DRAM, you'll have to tell the CPU that it isn't
	// safe to cache access to this memory.  Luckily, the page
	// tables provide bits for this purpose; simply create the
	// mapping with PTE_PCD|PTE_PWT (cache-disable and
	// write-through) in addition to PTE_W.  (If you're interested
	// in more details on this, see section 10.5 of IA32 volume
	// 3A.)
	//
	// Be sure to round size up to a multiple of PGSIZE and to
	// handle if this reservation would overflow MMIOLIM (it's
	// okay to simply panic if this happens).
	//
	// Hint: The staff solution uses boot_map_region.
	//
	// Your code here:
    // 使用boot_map_region来完成映射，权限位的话PTE_P是必须加的，然后加入
    // 注释中所要求的PTE_PCD和PTE_PWT。
    
	uintptr_t result = base;
	boot_map_region(kern_pgdir, base, ROUNDUP(size, PGSIZE), PTE_PCD | PTE_PWT | PTE_P);
    // 因为需要多次的
	base += ROUNDUP(size, PGSIZE);
	if(base > MMIOLIM){
		panic("mmio_map_region failed:  TOO HIGH ADRESS.\n");
	}
	return result;
	// panic("mmio_map_region not implemented");
}
```

## Application Processor bootstrap

在引导APS之前，BSP需要先手机多处理器系统的信息，例如CPU的总数等等，LAPIC单元的APIC ID和MMIO地址。kern/mpconfig.c中的mp_init函数依靠读取BIOS中的MP configuration table来读取这些信息。

Boot_aps函数(kern/init.c)驱动ap bootstrap程序，Aps在实模式中启动，就喝bootloader在boot/boot.S中启动一样，并且boot_asp将AP entry赋值到一个可以在实模式下可以寻址的内存位置。但是和bootloader不同的是，我们可以控制AP在哪里启动代码，我们吧entry赋值到0x70000(MPENTRY_PADDR)，但是在640kb一下任何未使用，页面对齐的物理地址都可以工作。在此之后，boot_aps()通过将STARTUP IPIs发送到LAPIC单元，AP应该在初始的CS:IP地址(MPENTRY_PADDR)开始运行其entry code，然后一个一个激活APs。

Kern/mpentry.S中的入口代码非常相似，经过一些简单的设置后，他将AP设置为启用分页的保护模式，然后调用C设置mp_main()(kern/init.c)中。boot_aps等待AP在其CpuInfo结构体的cpu_status字段中发出CPU_STARTED，然后唤醒下一个。

### Exercise 2

读取在kern/init.c中的boot_aps和mp_main函数，和kern/mpentry.S中的汇编代码，确保了解了这些代码的在Aps引导期间的控制流，然后在kern/pmap.c中修改page_init()的实现，避免将MPENTRY_PADDR的页面添加到空闲列表中，这样我们就可以安全的复制，并在物理地址运行AP引导代码，我们的代码应该通过更新的check_page_free_list测试。但是check_kern_pgdir测试可能会失败。

```c

// Start the non-boot (AP) processors.
static void
boot_aps(void)
{
	extern unsigned char mpentry_start[], mpentry_end[];
	void *code;
	struct CpuInfo *c;

	// Write entry code to unused memory at MPENTRY_PADDR
	code = KADDR(MPENTRY_PADDR);
	memmove(code, mpentry_start, mpentry_end - mpentry_start);

	// Boot each AP one at a time
	for (c = cpus; c < cpus + ncpu; c++) {
		if (c == cpus + cpunum())  // We've started already.
			continue;

		// Tell mpentry.S what stack to use 
		mpentry_kstack = percpu_kstacks[c - cpus] + KSTKSIZE;
		// Start the CPU at mpentry_start
		lapic_startap(c->cpu_id, PADDR(code));
		// Wait for the CPU to finish some basic setup in mp_main()
        // 等待cpu-status的值编程CPU_STARTED，然后可以对下一个cpu的APs进行操作。
		while(c->cpu_status != CPU_STARTED)
			;
	}
}

// Setup code for APs
void
mp_main(void)
{
	// We are in high EIP now, safe to switch to kern_pgdir 
	lcr3(PADDR(kern_pgdir));
	cprintf("SMP: CPU %d starting\n", cpunum());

	lapic_init();
	env_init_percpu();
	trap_init_percpu();
	xchg(&thiscpu->cpu_status, CPU_STARTED); // tell boot_aps() we're up

	// Now that we have finished some basic setup, call sched_yield()
	// to start running processes on this CPU.  But make sure that
	// only one CPU can enter the scheduler at a time!
	//
	// Your code here:

	// Remove this after you finish Exercise 6
	for (;;);
}

```

```nasm

#include <inc/mmu.h>
#include <inc/memlayout.h>

###################################################################
# entry point for APs
###################################################################

# Each non-boot CPU ("AP") is started up in response to a STARTUP
# IPI from the boot CPU.  Section B.4.2 of the Multi-Processor
# Specification says that the AP will start in real mode with CS:IP
# set to XY00:0000, where XY is an 8-bit value sent with the
# STARTUP. Thus this code must start at a 4096-byte boundary.
#
# Because this code sets DS to zero, it must run from an address in
# the low 2^16 bytes of physical memory.
#
# boot_aps() (in init.c) copies this code to MPENTRY_PADDR (which
# satisfies the above restrictions).  Then, for each AP, it stores the
# address of the pre-allocated per-core stack in mpentry_kstack, sends
# the STARTUP IPI, and waits for this code to acknowledge that it has
# started (which happens in mp_main in init.c).
#
# This code is similar to boot/boot.S except that
#    - it does not need to enable A20
#    - it uses MPBOOTPHYS to calculate absolute addresses of its
#      symbols, rather than relying on the linker to fill them

#define RELOC(x) ((x) - KERNBASE)
#define MPBOOTPHYS(s) ((s) - mpentry_start + MPENTRY_PADDR)

.set PROT_MODE_CSEG, 0x8	# kernel code segment selector
.set PROT_MODE_DSEG, 0x10	# kernel data segment selector

.code16           
.globl mpentry_start
mpentry_start:
	cli            
# 打开A20总线的接口。
	xorw    %ax, %ax
	movw    %ax, %ds
	movw    %ax, %es
	movw    %ax, %ss

	lgdt    MPBOOTPHYS(gdtdesc)
	movl    %cr0, %eax
	orl     $CR0_PE, %eax
	movl    %eax, %cr0
# 调用一个longjmp
	ljmpl   $(PROT_MODE_CSEG), $(MPBOOTPHYS(start32))

.code32
start32:
	movw    $(PROT_MODE_DSEG), %ax
	movw    %ax, %ds
	movw    %ax, %es
	movw    %ax, %ss
	movw    $0, %ax
	movw    %ax, %fs
	movw    %ax, %gs

	# Set up initial page table. We cannot use kern_pgdir yet because
	# we are still running at a low EIP.
	movl    $(RELOC(entry_pgdir)), %eax
	movl    %eax, %cr3
	# Turn on paging.
	movl    %cr0, %eax
	orl     $(CR0_PE|CR0_PG|CR0_WP), %eax
	movl    %eax, %cr0

	# Switch to the per-cpu stack allocated in boot_aps()
	movl    mpentry_kstack, %esp
	movl    $0x0, %ebp       # nuke frame pointer

	# Call mp_main().  (Exercise for the reader: why the indirect call?)
	movl    $mp_main, %eax
	call    *%eax

	# If mp_main returns (it shouldn't), loop.
spin:
	jmp     spin

# Bootstrap GDT
.p2align 2					# force 4 byte alignment
gdt:
	SEG_NULL				# null seg
	SEG(STA_X|STA_R, 0x0, 0xffffffff)	# code seg
	SEG(STA_W, 0x0, 0xffffffff)		# data seg

gdtdesc:
	.word   0x17				# sizeof(gdt) - 1
	.long   MPBOOTPHYS(gdt)			# address gdt

.globl mpentry_end
mpentry_end:
	nop

```

大致的调用顺序是boot_aps->entry_start->mp_main，在mp_main结束执行之后，将cpu->started的值转化为STARTED，从而一行boot_aps往下走，引导下一个pas。

```c

//
// Initialize page structure and memory free list.
// After this is done, NEVER use boot_alloc again.  ONLY use the page
// allocator functions below to allocate and deallocate physical
// memory via the page_free_list.
//
void
page_init(void)
{
	// LAB 4:
	// Change your code to mark the physical page at MPENTRY_PADDR
	// as in use

	// The example code here marks all physical pages as free.
	// However this is not truly the case.  What memory is free?
	//  1) Mark physical page 0 as in use.
	//     This way we preserve the real-mode IDT and BIOS structures
	//     in case we ever need them.  (Currently we don't, but...)
	//  2) The rest of base memory, [PGSIZE, npages_basemem * PGSIZE)
	//     is free.
	//  3) Then comes the IO hole [IOPHYSMEM, EXTPHYSMEM), which must
	//     never be allocated.
	//  4) Then extended memory [EXTPHYSMEM, ...).
	//     Some of it is in use, some is free. Where is the kernel
	//     in physical memory?  Which pages are already in use for
	//     page tables and other data structures?
	//
	// Change the code to reflect this.
	// NB: DO NOT actually touch the physical memory corresponding to
	// free pages!
	size_t i;
	page_free_list = NULL;
	int num_alloc = PADDR(boot_alloc(0)) / PGSIZE;
	int num_iohole = 96;
	struct PageInfo* mp_entry = pa2page(MPENTRY_PADDR);
	for(i = 0; i < npages; i++){
		if(i == 0){
			pages[i].pp_ref = 1;
        // Lab4 加一个判断条件。
		}else if(&pages[i] == mp_entry){
			pages[i].pp_ref = 1;
		}else if(i >= npages_basemem && i < npages_basemem + num_iohole + num_alloc){
			pages[i].pp_ref = 1;
		}else{
			pages[i].pp_ref = 0;
			pages[i].pp_link = page_free_list;
			page_free_list = &pages[i];
		}
	}
}
```

### Question

1. 比较kern/mentry.S和boot/boot.s的代码，要记得kern/mpentry.S中的代码被编译和连接到KERNBASE上方，就和别的内核代码一样，MPBOOTPHS这个宏的重要性在哪里，为什么他在kern/mpentry.S中出现，却不在boot/boot.S中出现，或者换一种说法，如果他被删掉之后会出现什么问题。

这个宏主要做的事是虚拟地址到物理地址的转换，

```nasm
#boot/boot.S
lgdt gdtdesc
```

```nasm
#kern/mpentry.S
#define MPBOOTPHYS(s) ((s) - mpentry_start + MPENTRY_PADDR)

lgdt MPBOOTPHYS(gdtdesc)
```

通过查看obj/boot/boot.asm，发现boot中的代码被连接在0x7c00，而mpentry作为内核代码，被连接在KERNBASE之上，所以需要进行地址转换。

## Per-CPU state and Initialization

在写一个多处理器系统的时候，有一个非常重要的方面就是区别每一个处理器的状态对另一个处理器都是私有的，然后还有一些全局状态是所有处理器共有的。kern/cpu.h中定义了大部分的处理器状体啊，包括了结构体CpuInfo,他保存了每一个cpu的值，cpunum(),返回正在处理的他CPU的ID，这个值可以应用来cpus这个数组里面，而thiscpu就是一个指向当前的CPU结构体CpuInfo宏的值。

以下是你需要关注的Cpu状态：

+ 每一个CPU的内核栈。

  因为每一个CPU都可以同步陷入内核，所以我们需要为每一个CPU准备一个内核栈，来阻止他们影响彼此的运行，数组percpu_kstacks\[NCPU]\[KSTKSIZE]为他们提供了内核栈的空间。

+ 每一个CPU的TSS和TSS描述符

  一个单独CPU的TSS是必要的，他用来指定每一个CPU的栈的位置，CPUi的TSS被保存在cpus[i].cpu_ts中，并且TSS描述符在GDT条目gdt[(GD_TSS0 >> 3) + i]中，kern/trap.c中的全局数据ts将会失效。

+ 每一个CPU的当前环境指针

  每一个CPU可以运行不同的用户进程，所以我们将symbol curenv改成了cpus[cpunum()].cpu_env。或者是thiscpu->cpu_env。他们指向当前CPU运行的环境。

+ 每一个cpu的系统寄存器

  所有的寄存器，包括系统寄存器，都是对于CPU来说是私有的，所以用来初始化寄存器的函数，例如lcr3，ltr，lgdt等等，必须在每一个CPU上运行一次，函数env_init_percpu和trap_init_percpu正是用来做这个的。

如果你写了一些CPU初始化的函数，一定记得要将所有的CPU初始化全部更改掉。

### Exercise 3

编辑mem_init_mp(kern/pmap.c)来映射每一个cpu栈从KSTACKTOP开始，就好像在inc/memlayout.h中一样，每一个栈的大小是KSTASIZE+未映射保护页KSTKGAP。我们的代码应该传递新的检查在check_kern_pgdir。

```c

// Modify mappings in kern_pgdir to support SMP
//   - Map the per-CPU stacks in the region [KSTACKTOP-PTSIZE, KSTACKTOP)
//
static void
mem_init_mp(void)
{
	// Map per-CPU stacks starting at KSTACKTOP, for up to 'NCPU' CPUs.
	//
	// For CPU i, use the physical memory that 'percpu_kstacks[i]' refers
	// to as its kernel stack. CPU i's kernel stack grows down from virtual
	// address kstacktop_i = KSTACKTOP - i * (KSTKSIZE + KSTKGAP), and is
	// divided into two pieces, just like the single stack you set up in
	// mem_init:
	//     * [kstacktop_i - KSTKSIZE, kstacktop_i)
	//          -- backed by physical memory
	//     * [kstacktop_i - (KSTKSIZE + KSTKGAP), kstacktop_i - KSTKSIZE)
	//          -- not backed; so if the kernel overflows its stack,
	//             it will fault rather than overwrite another CPU's stack.
	//             Known as a "guard page".
	//     Permissions: kernel RW, user NONE
	//
	// LAB 4: Your code here:
	uintptr_t kstacktop_i;
	uint32_t kstack_size = KSTKSIZE + KSTKGAP;
	for(int i =0; i < NCPU; i++){
		kstacktop_i = KSTACKTOP - i * (kstack_size);
		boot_map_region(kern_pgdir, kstacktop_i - KSTKSIZE, KSTKSIZE,  PADDR(percpu_kstacks[i]), PTE_W);
	}
	

}
```

按照注释来，很简单。

### Exercise 4

trap_init_percpu函数(kern/trap.c)，初始化TSS和TSS描述符，他在lab3中是正确的，但是在多处理器环境下是错误的，修改他的代码使他能够运行。

```c

// Initialize and load the per-CPU TSS and IDT
void
trap_init_percpu(void)
{
	// The example code here sets up the Task State Segment (TSS) and
	// the TSS descriptor for CPU 0. But it is incorrect if we are
	// running on other CPUs because each CPU has its own kernel stack.
	// Fix the code so that it works for all CPUs.
	//
	// Hints:
	//   - The macro "thiscpu" always refers to the current CPU's
	//     struct CpuInfo;
	//   - The ID of the current CPU is given by cpunum() or
	//     thiscpu->cpu_id;
	//   - Use "thiscpu->cpu_ts" as the TSS for the current CPU,
	//     rather than the global "ts" variable;
	//   - Use gdt[(GD_TSS0 >> 3) + i] for CPU i's TSS descriptor;
	//   - You mapped the per-CPU kernel stacks in mem_init_mp()
	//   - Initialize cpu_ts.ts_iomb to prevent unauthorized environments
	//     from doing IO (0 is not the correct value!)
	//
	// ltr sets a 'busy' flag in the TSS selector, so if you
	// accidentally load the same TSS on more than one CPU, you'll
	// get a triple fault.  If you set up an individual CPU's TSS
	// wrong, you may not get a fault until you try to return from
	// user space on that CPU.
	//
	// LAB 4: Your code here:
	thiscpu->cpu_ts.ts_esp0 = (uintptr_t)percpu_kstacks[cpunum()];
	thiscpu->cpu_ts.ts_ss0 = GD_KD;
	thiscpu->cpu_ts.ts_iomb = sizeof(struct Taskstate);

	gdt[GD_TSS0 >> 3 + cpunum()] = SEG16(STS_T32A, (uint32_t)(&(thiscpu->cpu_ts)), sizeof(struct Taskstate) - 1, 0);
	gdt[GD_TSS0 >> 3 + cpunum()].sd_s = 0;
    // 取编号，既然GD_TSS0没有右移三位，那么我们的cpu编号就要左移3位。
	ltr(GD_TSS0 + cpunum() << 3);
	lidt(&idt_pd);
	// Setup a TSS so that we get the right stack
	// when we trap to the kernel.
	/* ts.ts_esp0 = KSTACKTOP; */
	/* ts.ts_ss0 = GD_KD; */
	/* ts.ts_iomb = sizeof(struct Taskstate); */

	/* // Initialize the TSS slot of the gdt. */
	/* gdt[GD_TSS0 >> 3] = SEG16(STS_T32A, (uint32_t) (&ts), */
	/* 				sizeof(struct Taskstate) - 1, 0); */
	/* gdt[GD_TSS0 >> 3].sd_s = 0; */

	/* // Load the TSS selector (like other segment selectors, the */
	/* // bottom three bits are special; we leave them 0) */
	/* ltr(GD_TSS0); */

	/* // Load the IDT */
	/* lidt(&idt_pd); */
}
```

如果成功的话，使用make qemu CPUS=4或者make qemu-nox CPUS=4就会出现如下输出：

>```
>...
>Physical memory: 66556K available, base = 640K, extended = 65532K
>check_page_alloc() succeeded!
>check_page() succeeded!
>check_kern_pgdir() succeeded!
>check_page_installed_pgdir() succeeded!
>SMP: CPU 0 found 4 CPU(s)
>enabled interrupts: 1 2
>SMP: CPU 1 starting
>SMP: CPU 2 starting
>SMP: CPU 3 starting
>```

## Locking

我们的代码现在循环在mp_main的初始化AP中，在让AP走的更远之前，我们需要第一个地址调经竞争，当CPU们同时运行内核代码的时候，最简单的方法是使用一个大的内核锁，大内核锁是一个简单的全局锁，当人意一个环境进入内核模式的时候，他都被加锁，并且在进入用户模式的时候被解锁。在这种模式下，用户态程序可以并行的运行在CPU上，但是只有一个程序可以运行在内核态下，其他需要进入内核态的程序都需要等待。

kern/spinlock.h声明了大内核锁，他是kernel_lock，他也提供了lock_kernel函数和unlock_kernel函数来加锁解锁，我们需要在4个地方使用大内核锁。

+ 在i386_init()，中，在BSP唤醒其他CPU之前加锁。
+ 在mp_main中，在初始化AP之后加锁，然后调用sched_yield函数来运行AP上的程序。
+ 在trap函数中，当从用户态陷入到内核态的时候加锁，为了检查一个trap到底是用户态下发生的还是内核态下发生的，检查tr_cs的低位即可。
+ 在env_run函数中，在转化成为用户态前加锁，不要太早或者太晚做这件事情，这可能会引起条件竞争或者是死锁。

### Exercise 5

使用lock_kernel和unlock_kernel完成上面所说的锁。

```c

void
i386_init(void)
{
	// Initialize the console.
	// Can't call cprintf until after we do this!
	cons_init();

	cprintf("6828 decimal is %o octal!\n", 6828);
	cprintf("x=%d, y=%d\n", 3);

	// Lab 2 memory management initialization functions
	mem_init();

	// Lab 3 user environment initialization functions
	env_init();
	trap_init();

	// Lab 4 multiprocessor initialization functions
	mp_init();
	lapic_init();

	// Lab 4 multitasking initialization functions
	pic_init();

	// Acquire the big kernel lock before waking up APs
	// Your code here:
	lock_kernel();
	// Starting non-boot CPUs
	boot_aps();

#if defined(TEST)
	// Don't touch -- used by grading script!
	ENV_CREATE(TEST, ENV_TYPE_USER);
#else
	// Touch all you want.
	ENV_CREATE(user_primes, ENV_TYPE_USER);
#endif // TEST*

	// Schedule and run the first user environment!
	sched_yield();
}

```

```c

// Setup code for APs
void
mp_main(void)
{
	// We are in high EIP now, safe to switch to kern_pgdir 
	lcr3(PADDR(kern_pgdir));
	cprintf("SMP: CPU %d starting\n", cpunum());

	lapic_init();
	env_init_percpu();
	trap_init_percpu();
	xchg(&thiscpu->cpu_status, CPU_STARTED); // tell boot_aps() we're up

	// Now that we have finished some basic setup, call sched_yield()
	// to start running processes on this CPU.  But make sure that
	// only one CPU can enter the scheduler at a time!
	//
	// Your code here:
	lock_kernel();
	sched_yield();

	// Remove this after you finish Exercise 6
	for (;;);
}
```

```C

void
trap(struct Trapframe *tf)
{
	// The environment may have set DF and some versions
	// of GCC rely on DF being clear
	asm volatile("cld" ::: "cc");

	// Halt the CPU if some other CPU has called panic()
	extern char *panicstr;
	if (panicstr)
		asm volatile("hlt");

	// Re-acqurie the big kernel lock if we were halted in
	// sched_yield()
	if (xchg(&thiscpu->cpu_status, CPU_STARTED) == CPU_HALTED)
		lock_kernel();
	// Check that interrupts are disabled.  If this assertion
	// fails, DO NOT be tempted to fix it by inserting a "cli" in
	// the interrupt path.
	assert(!(read_eflags() & FL_IF));

	if ((tf->tf_cs & 3) == 3) {
		// Trapped from user mode.
		// Acquire the big kernel lock before doing any
		// serious kernel work.
		// LAB 4: Your code here.
		lock_kernel();
		assert(curenv);

		// Garbage collect if current enviroment is a zombie
		if (curenv->env_status == ENV_DYING) {
			env_free(curenv);
			curenv = NULL;
			sched_yield();
		}

		// Copy trap frame (which is currently on the stack)
		// into 'curenv->env_tf', so that running the environment
		// will restart at the trap point.
		curenv->env_tf = *tf;
		// The trapframe on the stack should be ignored from here on.
		tf = &curenv->env_tf;
	}

	// Record that tf is the last real trapframe so
	// print_trapframe can print some additional information.
	last_tf = tf;

	// Dispatch based on what type of trap occurred
	trap_dispatch(tf);

	// If we made it to this point, then no other environment was
	// scheduled, so we should return to the current environment
	// if doing so makes sense.
	if (curenv && curenv->env_status == ENV_RUNNING)
		env_run(curenv);
	else
		sched_yield();
}
```

```c

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
	if(curenv != NULL && curenv->env_status == ENV_RUNNING){
		curenv->env_status = ENV_RUNNABLE;
	}

	curenv = e;
	curenv->env_status = ENV_RUNNING;
	curenv->env_runs++;
	lcr3(PADDR(curenv->env_pgdir));
    // lab4
	unlock_kernel();
	env_pop_tf(&curenv->env_tf);
}

```

### Question

2.已经使用了一个大内核锁来保证同一个时间一个CPU可以跑这个内核代码，那为什么我们还需要为每一个CPU区分一个内核栈，描述一个场景，使用一个共享的内核栈会导致错误，即使是有大内核锁的存在。

当一个线程正在处理用户态的中断，内核栈中存储了栈帧，但是此时另一个线程的用户态程序也发生了中断，此时中断上下文也会被压栈，然后调用trap启动大内核锁，这是栈帧就被破坏掉了。

