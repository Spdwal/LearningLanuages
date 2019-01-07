

# Part A： multiprocessor support and cooperative multitasking

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
    // 如果没有PTE_W，之后的lab会出现错误。
    
	uintptr_t result = base;
	boot_map_region(kern_pgdir, base, ROUNDUP(size, PGSIZE), PTE_PCD | PTE_PWT | PTE_W);
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

## Round-Robin Scheduling

我们的下一个任务是使JOS内核可以使用轮询调度算法在数个环境之中跳转。轮询调度算法在JOS工作如下：

+ kern/sched.c中的sched_yield函数，是为了选择一个新的运行环境而定义的。他使用线性搜索的方式循环搜索envs数组，运行在前一个运行环境之后的环境，或者直接运行数组头的环境，找到他第一个找到的状态为ENV_RUNNABLE的环境，然后调用env_run函数来运行环境。
+ sched_yield函数一定不会同时在两个CPU上运行同一个环境，它会告诉我们一个环境正在运行在一个cpu上，因为它的状态是ENV__RUNNING。
+ lab提供了一个新函数叫sys_yeild，它被用来调用内核的sched_yield函数，然后自愿的放弃CPU资源给其他环境

### Exercise 6

在sched_yield中完成轮询调度算法，别忘了改动sys_call来调用sys_yield()。另外记得在mp_main中调用sched_yield。改动kern/init.c来穿件3个或者更多的环境来运行user/yield.c这个程序。

运行make qemu，在结束之前，我们因该看到环境之间来回切换了5此。可以使用多个CPU进行测试，使qemu cpu = 2。

```
...
Hello, I am environment 00001000.
Hello, I am environment 00001001.
Hello, I am environment 00001002.
Back in environment 00001000, iteration 0.
Back in environment 00001001, iteration 0.
Back in environment 00001002, iteration 0.
Back in environment 00001000, iteration 1.
Back in environment 00001001, iteration 1.
Back in environment 00001002, iteration 1.
...
```

当yield程序结束后，系统上应该没有可以运行的程序，调度应该运行JOS的monitor，如果这些没有发生，那么修正你的程序。

```c

// Choose a user environment to run and run it.
void
sched_yield(void)
{
	struct Env *idle;

	// Implement simple round-robin scheduling.
	//
	// Search through 'envs' for an ENV_RUNNABLE environment in
	// circular fashion starting just after the env this CPU was
	// last running.  Switch to the first such environment found.
	//
	// If no envs are runnable, but the environment previously
	// running on this CPU is still ENV_RUNNING, it's okay to
	// choose that environment.
	//
	// Never choose an environment that's currently running on
	// another CPU (env_status == ENV_RUNNING). If there are
	// no runnable environments, simply drop through to the code
	// below to halt the cpu.

	// LAB 4: Your code here.
	bool flag = false;
	if(curenv == NULL){
		idle = envs;
	}else{
		idle = envs + 1;
	}
	for(struct Env*e = idle; e != envs + NENV; e ++){
		if(e->env_status == ENV_RUNNABLE){
			flag = true;
			env_run(e);
			break;
		}
	}

	if(flag == false){
		for(struct Env *e = envs; e != idle; e++){
			if(e->env_status == ENV_RUNNABLE){
				flag = true;
				env_run(e);
				break;
			}
		}
	}

	if(flag == false && curenv != NULL && curenv->env_status == ENV_RUNNING){
		flag = true;
		env_run(curenv);
	}
	if(flag == false){
		sched_halt();
	}
	
	// sched_halt never returns
	sched_halt();
}

```

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
	ENV_CREATE(user_yield, ENV_TYPE_USER);
	ENV_CREATE(user_yield, ENV_TYPE_USER);
	ENV_CREATE(user_yield, ENV_TYPE_USER);
	// Schedule and run the first user environment!
	sched_yield();
}
```

```c
// Dispatches to the correct kernel function, passing the arguments.
int32_t
syscall(uint32_t syscallno, uint32_t a1, uint32_t a2, uint32_t a3, uint32_t a4, uint32_t a5)
{
	// Call the function corresponding to the 'syscallno' parameter.
	// Return any appropriate return value.
	// LAB 3: Your code here.

	// panic("syscall not implemented");

	switch (syscallno) {
	case SYS_cputs:
		return sys_cputs((const char*)a1, (size_t) a2);
	case SYS_cgetc:
		return sys_cgetc();
	case SYS_env_destroy:
		return sys_env_destroy((envid_t) a1);
	case SYS_getenvid:
		return sys_getenvid();
	case SYS_yield:
		return sys_yield();
	case NSYSCALLS:
		return 0;
	default:
		return -E_INVAL;
	}
}
```

最后运行make qemu CPUS=2

```c
[00000000] new env 00001000
[00000000] new env 00001001
[00000000] new env 00001002
[00000000] new env 00001003
Hello, I am environment 00001002.
Hello, I am environment 00001002.
Hello, I am environment 00001002.
Back in environment 00001002, iteration 0.
Back in environment 00001002, iteration 0.
Back in environment 00001002, iteration 0.
Back in environment 00001002, iteration 1.
Back in environment 00001002, iteration 1.
Back in environment 00001002, iteration 1.
Back in environment 00001002, iteration 2.
Back in environment 00001002, iteration 2.
Back in environment 00001002, iteration 2.
Back in environment 00001002, iteration 3.
Back in environment 00001002, iteration 3.
Back in environment 00001002, iteration 3.
Back in environment 00001002, iteration 4.
Back in environment 00001002, iteration 4.
Back in environment 00001002, iteration 4.
All done in environment 00001002.
All done in environment 00001002.
All done in environment 00001002.
```

### Question

3. 在我们的env_run的实现里面，我们需要调用lcr3函数，在调用这个函数之前和之后，我们都是用了一个env_run的参数e的引用，在加载cr3寄存器之前，MMU的地址上下文已经改变了，但是一个被称为e的虚拟地址还是和我们所给的地址上下文相关联，为什么这个指针e，在地址变化之前和之后都可以被解引用。

   因为这个e在内核栈上，所以它被一起从kern_pgdir复制到环境的页表了。

4. 无论什么时候，内核从一个环境转换到另一个环境，它必须确定旧环境的寄存器被保存并且可以在之后被重新加载，为什么？在哪里发生了这个事情。

   不保存旧环境，就没有办法在之后重新读取它的trapframe来获取它的全部寄存器。

   保存在_alltraps，回复发生在env.c中的env_pop_tf函数。

## System Calls for Environment Creation

尽管你的内核还没有能力在多个不同的用户态环境中切换，它始终只能执行内核最初设定好的程序，现在我们即将实现一个新的系统调用，它允许进程创建，并且开始新的进程。

UNIX提供了fork这个系统调用来创建进程，fork将会拷贝父进程的整个地址空间来创建子进程。父子进程在用户前仅有的区别是他们的进程号和父进程号不同，(分别为getpid和getppid的返回值)，在父进程中，fork返回子进程的id，在子进程中fork返回0。默认来说，每一个进程，都有它的私有地址空间，而且每一个进程的内存修改对其他的都是可以见的。

我们将会完成一个有一点不同，更加原始一点的JOS系统调用来创建新的用户环境，设计的系统调用如下：

+ sys_exofork：这个系统调用会创建一个空白进程，在其用户空间中没有映射任何物理内存，并且它是不可运行的。这个新的环境会和父进程拥有相同的寄存器状态。在父进程中，sys_exofork会返回envid_t类型的子进程id，或者是一个负的错误码，表示创建环境失败。在子即成中，它会返回0。因为子进程的states会被标记为不可运行，所以sys_exofork会在它的父进程允许子进程运行之后在子进程中返回。
+ Sys_env_set_status：将特定线程的status设置为ENV_RUNNABLE或者是ENV_NOT_RUNNABLE。这个系统调用将会允许一个新的线程可以运行，当它的地址空间和寄存器状态被完全初始化后。
+ Sys_page_alloc：用来分配物理地址并且将它的虚拟地址映射在纸上。
+ Sys_page_ map：从进程中复制一个页映射到另一个进程，即是内存共享。
+ sys_page_umap：删除到指定进程的指定虚拟地址的映射。

在以上所有接受环境id的系统调用，JOS内核支持一个约定，即是值为0表示当前环境，这个约定由kern/env.c中的envid2env函数来实施。

我们的测试程序user/dumbfork.c中提供了一个类似fork的原始实现，这个测试程序使用上面的系统调用来创建和运行带有自己空间副本的子环境，然后使用前面联系中的sys_yield来回切换这两个环境，父节点在在10次迭代后推出。子节点在20次迭代后退出。

### Exercise 7

完成上面所提到的所有的在kern/syscall.c中的系统调用，并且确定syscall会调用他们，你会需要使用到kern/pmap.c和kern/env中的许多函数，特别是envid2env()，现在来说的话，你每次调用envid2env函数，你可以在权限位上传入参数1，确保你检查了所有的不合法的系统调用参数，如果遇到了不合法的参数，那么返回-E_INVAL，使用user/dumbfork来测试素有的代码，然后确保它确实可以运行。

```c
// Allocate a new environment.
// Returns envid of new environment, or < 0 on error.  Errors are:
//	-E_NO_FREE_ENV if no free environment is available.
//	-E_NO_MEM on memory exhaustion.
static envid_t
sys_exofork(void)
{
	// Create the new environment with env_alloc(), from kern/env.c.
	// It should be left as env_alloc created it, except that
	// status is set to ENV_NOT_RUNNABLE, and the register set is copied
	// from the current environment -- but tweaked so sys_exofork
	// will appear to return 0.
	struct Env *e;
	envid_t result = env_alloc(&e, curenv->env_id);
	if(result < 0){
		return result;
	}
	
	e->env_status = ENV_NOT_RUNNABLE;
	e->env_tf = curenv->env_tf;
	e->env_tf.tf_regs.reg_eax = 0;
	return e->env_id;
	// LAB 4: Your code here.
	// panic("sys_exofork not implemented");
}
```

经过这个代码，长久以来我对于fork的疑问解除了，fork其实和传统意义上的返回不一样，他创建了一个新的进程，他的所有的状态都和父进程一样，除了status和eax里的值，然后fork在父进程中正常返回子进程的id。然后调度程序在进程表里面，将这个新创建的env调出来运行，此时他运行在fork调用的这个点上，但是他的eax里保存的是0,所以它的返回值也就是0。

```c

// Set envid's env_status to status, which must be ENV_RUNNABLE
// or ENV_NOT_RUNNABLE.
//
// Returns 0 on success, < 0 on error.  Errors are:
//	-E_BAD_ENV if environment envid doesn't currently exist,
//		or the caller doesn't have permission to change envid.
//	-E_INVAL if status is not a valid status for an environment.
static int
sys_env_set_status(envid_t envid, int status)
{
	// Hint: Use the 'envid2env' function from kern/env.c to translate an
	// envid to a struct Env.
	// You should set envid2env's third argument to 1, which will
	// check whether the current environment has permission to set
	// envid's status.

	// LAB 4: Your code here.
	struct Env *e;
	if(status != ENV_RUNNABLE && status != ENV_NOT_RUNNABLE){
		return -E_INVAL;
	}
	int result = envid2env(envid, &e, 1);
	if(result < 0){
		return -E_BAD_ENV;
	}
	e->env_status = status;
	return 0;
	// panic("sys_env_set_status not implemented");
}
```

跟着注释写，很简单。

```c

// Allocate a page of memory and map it at 'va' with permission
// 'perm' in the address space of 'envid'.
// The page's contents are set to 0.
// If a page is already mapped at 'va', that page is unmapped as a
// side effect.
//
// perm -- PTE_U | PTE_P must be set, PTE_AVAIL | PTE_W may or may not be set,
//         but no other bits may be set.  See PTE_SYSCALL in inc/mmu.h.
//
// Return 0 on success, < 0 on error.  Errors are:
//	-E_BAD_ENV if environment envid doesn't currently exist,
//		or the caller doesn't have permission to change envid.
//	-E_INVAL if va >= UTOP, or va is not page-aligned.
//	-E_INVAL if perm is inappropriate (see above).
//	-E_NO_MEM if there's no memory to allocate the new page,
//		or to allocate any necessary page tables.
static int
sys_page_alloc(envid_t envid, void *va, int perm)
{
	// Hint: This function is a wrapper around page_alloc() and
	//   page_insert() from kern/pmap.c.
	//   Most of the new code you write should be to check the
	//   parameters for correctness.
	//   If page_insert() fails, remember to free the page you
	//   allocated!
	
	// LAB 4: Your code here.
	if(((perm & PTE_U) ==0) || ((perm & PTE_P) == 0)){
		cprintf("error 1.\n");
		return -E_INVAL;
	}
	if((perm & ~PTE_SYSCALL) != 0){
		cprintf("error 2.\n");
		return -E_INVAL;
	}
	if((intptr_t)va >= UTOP || PGOFF(va) != 0){
		cprintf("error 3.\n");
		return -E_INVAL;
	}

	struct PageInfo *p = page_alloc(ALLOC_ZERO);
	if(p == NULL){
		return -E_NO_MEM;
	}
	struct Env *e;
	envid_t e_id = envid2env(envid, &e, 1);

	if(e_id < 0){
		return -E_BAD_ENV;
	}

	e_id = (envid_t) page_insert(e->env_pgdir, p, va, perm);
	if(e_id < 0){
		page_free(p);
		return -E_NO_MEM;
	}

	return 0;
	// panic("sys_page_alloc not implemented");
}

```

```c

// Map the page of memory at 'srcva' in srcenvid's address space
// at 'dstva' in dstenvid's address space with permission 'perm'.
// Perm has the same restrictions as in sys_page_alloc, except
// that it also must not grant write access to a read-only
// page.
//
// Return 0 on success, < 0 on error.  Errors are:
//	-E_BAD_ENV if srcenvid and/or dstenvid doesn't currently exist,
//		or the caller doesn't have permission to change one of them.
//	-E_INVAL if srcva >= UTOP or srcva is not page-aligned,
//		or dstva >= UTOP or dstva is not page-aligned.
//	-E_INVAL is srcva is not mapped in srcenvid's address space.
//	-E_INVAL if perm is inappropriate (see sys_page_alloc).
//	-E_INVAL if (perm & PTE_W), but srcva is read-only in srcenvid's
//		address space.
//	-E_NO_MEM if there's no memory to allocate any necessary page tables.
static int
sys_page_map(envid_t srcenvid, void *srcva,
	     envid_t dstenvid, void *dstva, int perm)
{
	// Hint: This function is a wrapper around page_lookup() and
	//   page_insert() from kern/pmap.c.
	//   Again, most of the new code you write should be to check the
	//   parameters for correctness.
	//   Use the third argument to page_lookup() to
	//   check the current permissions on the page.
	
	// LAB 4: Your code here.
	if((uintptr_t)srcva >= UTOP || PGOFF(srcva) != 0){
		return -E_INVAL;
	}
	if((uintptr_t)dstva >= UTOP || PGOFF(dstva) != 0){
		return -E_INVAL;
	}
	if((perm & PTE_U) == 0 || (perm & PTE_P) == 0){
		return -E_INVAL;
	}
	if((perm & (~PTE_SYSCALL)) != 0){
		return -E_INVAL;
	}
	struct Env *src_e, *dst_e;
	if(envid2env(srcenvid, &src_e, 1) < 0){
		return -E_BAD_ENV;
	}
	if(envid2env(dstenvid, &dst_e, 1) < 0){
		return -E_BAD_ENV;
	}
	pte_t *src_ptab;
	struct PageInfo* p = page_lookup(src_e->env_pgdir, srcva, &src_ptab);

	if(!(*src_ptab & PTE_W) && (perm & PTE_W)){
		return -E_INVAL;
	}
	if(page_insert(dst_e->env_pgdir, p, dstva, perm) < 0){
		return -E_NO_MEM;
	}

	return 0;
	// panic("sys_page_map not implemented");
}

```

```c

// Unmap the page of memory at 'va' in the address space of 'envid'.
// If no page is mapped, the function silently succeeds.
//
// Return 0 on success, < 0 on error.  Errors are:
//	-E_BAD_ENV if environment envid doesn't currently exist,
//		or the caller doesn't have permission to change envid.
//	-E_INVAL if va >= UTOP, or va is not page-aligned.
static int
sys_page_unmap(envid_t envid, void *va)
{
	// Hint: This function is a wrapper around page_remove().

	// LAB 4: Your code here.
	if((uintptr_t)va >= UTOP || PGOFF(va) != 0){
		return -E_INVAL;
	}
	struct Env *e;
	if(envid2env(envid, &e, 1) < 0){
		return -E_BAD_ENV;
	}

	page_remove(e->env_pgdir, va);
	return 0;
      	// panic("sys_page_unmap not implemented");
}
```

最后记得更改syscall

```c

// Dispatches to the correct kernel function, passing the arguments.
int32_t
syscall(uint32_t syscallno, uint32_t a1, uint32_t a2, uint32_t a3, uint32_t a4, uint32_t a5)
{
	// Call the function corresponding to the 'syscallno' parameter.
	// Return any appropriate return value.
	// LAB 3: Your code here.

	// panic("syscall not implemented");

	switch (syscallno) {
	case SYS_cputs:
		return sys_cputs((const char*)a1, (size_t) a2);
	case SYS_cgetc:
		return sys_cgetc();
	case SYS_env_destroy:
		return sys_env_destroy((envid_t) a1);
	case SYS_getenvid:
		return sys_getenvid();
	case SYS_yield:
		return sys_yield();
	case SYS_exofork:
		return sys_exofork();
	case SYS_env_set_status:
		return sys_env_set_status(a1, a2);
	case SYS_page_alloc:
		return sys_page_alloc((envid_t)a1, (void *)a2, (int)a3);
	case SYS_page_map:
		return sys_page_map((envid_t)a1, (void *)a2, a3, (void *)a4, (int)a5);
	case SYS_page_unmap:
		return sys_page_unmap((envid_t)a1, (void *)a2);
	case NSYSCALLS:
		return 0;
	default:
		return -E_INVAL;
	}
}
```

Part A结束

# Part B: copy-on-write Fork

就和我们之前提到的一样，UNIX提供了fork系统调用，来作为系统调用进程创建原语，然后fork调用一个复制函数复制父进程的地址空间，以创建一个新进程。

xv6UNIX通过将所有的数据从父进程处拷贝所有的数据到新的子进程的页表中，这和dumbfork所做的事情一样，这个拷贝操作是fork函数中开销最大的操作。

但是在子进程中对fork的调用之后经常会立即调用exec，这将用一个新的程序替子进程的内存，通常shell就是这么做的，这种情况下，复制父进程空间所花费的时间基本上是浪费的，因为子进程在调用exec之后只会使用很少的内存。

因为这个原因，在最新的UNIX版本中，使用了共享内存，使父进程和子进程共享同一个内存，并映射到他们的地址空间内，直到其中一个进程改变了它。这个技术被称为写时复制。为了做到这一点，内核中fork函数会从父进程中拷贝地址空间的映射到子进程，而不是拷贝所有的内容。然后将所有的共享空间标记为只读。

当两个进程中的一个尝试去写共享的空间的时候，进程会产生一个pgfault ,然后UNIX内核会识别它到底是一个虚拟还是写时复制副本，因此它为出错程序生成一个新的私有的可写的页面副本，通过这种方式，单独的页表只有它在被写入的时候，才会真正的写入，这些优化使fork和exec开销更小。子进程在调用exec之前，只会复制一个page，也即是当前页的栈。

在这个lab的下一部分，我们会完成一个合适的，拥有写时复制的UNIX-LIKEfork函数，作为一个用户历程库。在用户空间实现fork和写时复制支持的有点是内核仍然特别简单，因此更有可能是正确的。它还允许单个用户模式程序为fork()定义自己的语义，需要稍微不同的实现的程序，例如昂贵的总是复制的版本，如dumbfork，可以很容易的提供自己的实现。

## User-level page fault handling

用户级的写时复制fork需要知道写保护页面上的页面错误，所以这才是我们首先需要实现的，写时复制只是用户级页面错误处理的多种可能用途之一。

通常会设置一个地址空间，以便页面错误指示合适需要执行某些操作，例如，大多数UNIX内核最初只映射新进程堆栈区域的单个页，然后随着进程堆栈的增加，按需分配其他堆栈页，并且在尚未映射的堆栈地址上发生页错误，典型的UNIX内核，必须跟踪在进程空间的每个区域发生页错误的时候要采取的操作，例如堆栈区域中的pgfault会分配和映射物理内存的新页面，bss区域中的错误通常会分配一个新页面，然后用0填充它，并且映射它，在按照需求分页的可执行文件的系统中，文本区域中的错误将从磁盘上读取二进制文件的相应页面，然后映射它。

内核需要跟踪很多信息。和传统的UNIX概念不同，你会去选择在用户空间中你对每一种page fault所做的操作，因为这写错误对用户空间的损害较小，这种设计的另外一个好处是允许程序在定义他们的内存区域时有着非常大的灵活性，稍后，我们将使用用户级的页面错误处理来映射和访问基于硬盘的文件系统上的文件。

### Setting the page fault handler

为了处理自己的页面错误，用户环境需要向JOS内核注册页面错误处理程序entrypoint，用户环境使用sys_env_set_pgfault_upcall系统调用注册其页面错误入口点。我们在Env结构中添加了一个新成员env_pgfault_upcall来记录这个信息。

### exercise 8:

实现sys_env_set_pgfault_upcall系统调用，确保在查找目标环境的环境ID时候启用权限检查，因为这是一个危险的系统调用。

```c

// Set the page fault upcall for 'envid' by modifying the corresponding struct
// Env's 'env_pgfault_upcall' field.  When 'envid' causes a page fault, the
// kernel will push a fault record onto the exception stack, then branch to
// 'func'.
//
// Returns 0 on success, < 0 on error.  Errors are:
//	-E_BAD_ENV if environment envid doesn't currently exist,
//		or the caller doesn't have permission to change envid.
static int
sys_env_set_pgfault_upcall(envid_t envid, void *func)
{
	// LAB 4: Your code here.
	struct Env *e = NULL;
	envid2env(envid, &e, 1);

	if(e == NULL){
		cprintf("Sys_env_set_tatus: envid2env.\n");
		return -E_BAD_ENV;
	}
	
	e->env_pgfault_upcall = func;

	return 0;
	// panic("sys_env_set_pgfault_upcall not implemented");
}

```

## Normal and exception stacks in User Environments

经过运行之后，一个JOS用户环境会运行在用户栈上，他的ESP寄存器在刚开始运行的时候，指向USTACKTOP，然后吧栈数据push进USTACKTOP -PGSIZE到USTACKTOP-1的中间之间，但是当用户模式下发生了页错误的时候，内核将重新启动在不同堆栈上运行指定的用户级页面处理程序的用户环境。在本质上，我们将使用JOS内核实现代表用户环境的自动堆栈切换，就像x86处理器从用户模式时实现了代表JOS的堆栈切换一样。

JOS用户异常栈页时一个页面大小，其顶部定义为虚拟地址UXSTACKTOP，所以用户异常栈的有效字节时UXSTACKTOP - PGSIZE到UXSTACKTOP -1(包括)。当我们运行在异常栈上的时候，用户态page fault处理函数使用JOS的系统调用来映射一个新的页或者调整映射，以便修复最初导致页面错误的任何问题，然后用户级的页面错误处理程序通过汇编语言的存根返回原是栈上的系统代码。

每个希望支持用户级别页面错误处理的用户环境都需要使用PartA中介绍的sys_page_alloc()系统调用为自己的异常堆栈分配内存。

### Invoking the user page fault handler

我们需要改变kern/trap.c中的pagefault处理函数来处理用户模式下的pagefault，如下所示，我们将在发生错误时将用户环境的状态称为trap时状态。

如果没有pagefault处理函数被寄存器储存，JOS内核会和之前一样销毁用户环境，但是如果有的话，内核会设置一个trapframe在异常栈上，它是一个struct Utrapframe结构体，这个结构体被定义在inc/trap.c上。

```
                    <-- UXSTACKTOP
trap-time esp
trap-time eflags
trap-time eip
trap-time eax       start of struct PushRegs
trap-time ecx
trap-time edx
trap-time ebx
trap-time esp
trap-time ebp
trap-time esi
trap-time edi       end of struct PushRegs
tf_err (error code)
fault_va            <-- %esp when handler is run
```

内核安排用户环境使用在异常栈上运行的页面错误处理函数，回复执行，你必须弄清楚如何执行这一点，fault_va是导致页面错误的虚拟地址。此处少了cs ip，但是多了fault_va，这表明这之间没有发生进程的切换。

如果用户环境在异常发生时已经在异常栈上，那么page fault处理函数本身就有错误。在这种情况下，我们应该在tf->tf_esp下启动新的堆栈框架，而不是在UXSTACKTOP，这时候我们应该先push一个空的32位的字，然后是一个strcut UTrapframe。

测试tf->tf_esp是不是已经存在了用户异常栈里面，检查它是不是在UXSTACKTOP-PGSIZE到TXSTACKTOP-1中间，

### Exerxcise 9

在kern/trap.c中实现page_fault_handler中的代码，需要将页面错误分派给用户模式处理程序，在写入异常栈的时候，一定要采取相当的措施，(如果用户环境耗尽堆栈上的空间，会发生什么？)

```c

void
page_fault_handler(struct Trapframe *tf)
{
	uint32_t fault_va;

	// Read processor's CR2 register to find the faulting address
	fault_va = rcr2();

	// Handle kernel-mode page faults.

	// LAB 3: Your code here.
	if(tf->tf_cs && 0x01 == 0){
		panic("Page_fault in kernel mode, fault adress %d.\n", fault_va);
	}

	// We've already handled kernel-mode exceptions, so if we get here,
	// the page fault happened in user mode.

	// Call the environment's page fault upcall, if one exists.  Set up a
	// page fault stack frame on the user exception stack (below
	// UXSTACKTOP), then branch to curenv->env_pgfault_upcall.
	//
	// The page fault upcall might cause another page fault, in which case
	// we branch to the page fault upcall recursively, pushing another
	// page fault stack frame on top of the user exception stack.
	//
	// It is convenient for our code which returns from a page fault
	// (lib/pfentry.S) to have one word of scratch space at the top of the
	// trap-time stack; it allows us to more easily restore the eip/esp. In
	// the non-recursive case, we don't have to worry about this because
	// the top of the regular user stack is free.  In the recursive case,
	// this means we have to leave an extra word between the current top of
	// the exception stack and the new stack frame because the exception
	// stack _is_ the trap-time stack.
	//
	// If there's no page fault upcall, the environment didn't allocate a
	// page for its exception stack or can't write to it, or the exception
	// stack overflows, then destroy the environment that caused the fault.
	// Note that the grade script assumes you will first check for the page
	// fault upcall and print the "user fault va" message below if there is
	// none.  The remaining three checks can be combined into a single test.
	//
	// Hints:
	//   user_mem_assert() and env_run() are useful here.
	//   To change what the user environment runs, modify 'curenv->env_tf'
	//   (the 'tf' variable points at 'curenv->env_tf').

	// LAB 4: Your code here.
	bool flag = true;
	if(!curenv->env_pgfault_upcall){
		flag = false;
	}
    // UXSTACKTOP = USTACKTOP + 2 * PGSZE
	if(fault_va >= UXSTACKTOP || fault_va < UXSTACKTOP - PGSIZE){
		flag = false;
	}

	if(flag){
		struct UTrapframe  *utf;
	
		if(tf->tf_esp < UXSTACKTOP && tf->tf_esp >= UXSTACKTOP-PGSIZE){
			utf = (struct UTrapframe*)(curenv->env_tf.tf_esp - sizeof(struct UTrapframe) - 4);
		}else{
			utf = (struct UTrapframe*)(UXSTACKTOP - sizeof(struct UTrapframe));
		}
		utf->utf_eflags = tf->tf_eflags;
		utf->utf_eip   = tf->tf_eip;
		utf->utf_err   = tf->tf_err;
		utf->utf_esp   = tf->tf_esp;
		utf->utf_fault_va = fault_va;
		utf->utf_regs  = tf->tf_regs;
		
		user_mem_assert(curenv, utf, sizeof(struct UTrapframe), PTE_W);

		tf->tf_eip = (uintptr_t) curenv->env_pgfault_upcall;
		tf->tf_esp = (uintptr_t) utf;

		env_run(curenv);
	}
	// Destroy the environment that caused the fault.
	cprintf("[%08x] user fault va %08x ip %08x\n",
		curenv->env_id, fault_va, tf->tf_eip);
	print_trapframe(tf);
	env_destroy(curenv);
}
```

## User-mode page fault entrypoint

接下来，我们需要完成汇编历程，来调用c语言的page fault处理程序，然后在原始错误处恢复执行，这个程序集处理程序将使用sys_env_set_pgfault_upcall()向内核注册。

### Exercise 10

在lib/pentry.s中实现_pgfault_upcall历程，有趣的部分是返回导致页面错误的用户代码中的原始位置。我们将直接返回哪里，而不需要内核返回，困难的部分是同时切换堆栈和重新加载EIP。

```nasm

// Page fault upcall entrypoint.

// This is where we ask the kernel to redirect us to whenever we cause
// a page fault in user space (see the call to sys_set_pgfault_handler
// in pgfault.c).
//
// When a page fault actually occurs, the kernel switches our ESP to
// point to the user exception stack if we're not already on the user
// exception stack, and then it pushes a UTrapframe onto our user
// exception stack:
//
//	trap-time esp
//	trap-time eflags
//	trap-time eip
//	utf_regs.reg_eax
//	...
//	utf_regs.reg_esi
//	utf_regs.reg_edi
//	utf_err (error code)
//	utf_fault_va            <-- %esp
//
// If this is a recursive fault, the kernel will reserve for us a
// blank word above the trap-time esp for scratch work when we unwind
// the recursive call.
//
// We then have call up to the appropriate page fault handler in C
// code, pointed to by the global variable '_pgfault_handler'.

.text
.globl _pgfault_upcall
_pgfault_upcall:
	// Call the C page fault handler.
	pushl %esp			// function argument: pointer to UTF
	movl _pgfault_handler, %eax
	call *%eax
	addl $4, %esp			// pop function argument
	
	// Now the C page fault handler has returned and you must return
	// to the trap time state.
	// Push trap-time %eip onto the trap-time stack.
	//
	// Explanation:
	//   We must prepare the trap-time stack for our eventual return to
	//   re-execute the instruction that faulted.
	//   Unfortunately, we can't return directly from the exception stack:
	//   We can't call 'jmp', since that requires that we load the address
	//   into a register, and all registers must have their trap-time
	//   values after the return.
	//   We can't call 'ret' from the exception stack either, since if we
	//   did, %esp would have the wrong value.
	//   So instead, we push the trap-time %eip onto the *trap-time* stack!
	//   Below we'll switch to that stack and call 'ret', which will
	//   restore %eip to its pre-fault value.
	//
	//   In the case of a recursive fault on the exception stack,
	//   note that the word we're pushing now will fit in the
	//   blank word that the kernel reserved for us.
	//
	// Throughout the remaining code, think carefully about what
	// registers are available for intermediate calculations.  You
	// may find that you have to rearrange your code in non-obvious
	// ways as registers become unavailable as scratch space.
	//
	// LAB 4: Your code here.
	// 使esp指向 reg_edi,等于是弹出 err和fault_va
	add $8, %esp
	
	// Restore the trap-time registers.  After you do this, you
	// can no longer modify any general-purpose registers.
	// LAB 4: Your code here.
	// 为了使eip指向正确的地址，
	// 保存了reg_eax的值
	movl 32(%esp), %eax
	// ebx中保存了utf_esp的值
	// 即是上一个栈的栈顶。
	movl 40(%esp), %ebx
	// 此时ebx指向的地址是utf_esp之下的空的那个word。
	subl $4, %ebx
	// 更新utf_esp指向原来的utf_esp-4的值，也就是将原来的空出来的字给包括了进来，
	movl %ebx, 40(%esp)
	// 将空的字里面放入了utf_eip的值。
	movl %eax, (%ebx)
	// Restore eflags from the stack.  After you do this, you can
	// no longer use arithmetic operations or anything else that
	// modifies eflags.
	// LAB 4: Your code here.
	// 回复寄存器，执行结束后 esp指向utf_eip，不是我们之前设置的那个空字里面的eip
	popal
	// Switch back to the adjusted trap-time stack.
	// LAB 4: Your code here.
	// 弹出utf_eip
	addl $4, %esp
	
	// 回复 eflags
	popfl
	// Return to re-execute the instruction that faulted.
	// LAB 4: Your code here.
	// 回复栈帧
	popl %esp
	// 此时esp指向的就是我们放入进去空字的那个utf_eip，然后依靠这个eip进行return。
	// 相当于popl %eip
	ret
```

### Exercise 11

在lib/pgfault.c中完成set_pgfault_handler。

```c

//
// Set the page fault handler function.
// If there isn't one yet, _pgfault_handler will be 0.
// The first time we register a handler, we need to
// allocate an exception stack (one page of memory with its top
// at UXSTACKTOP), and tell the kernel to call the assembly-language
// _pgfault_upcall routine when a page fault occurs.
//
void
set_pgfault_handler(void (*handler)(struct UTrapframe *utf))
{
	int r;

	if (_pgfault_handler == 0) {
		// First time through!
		// LAB 4: Your code here.
		envid_t env_id = sys_getenvid();
		r = sys_page_alloc(evn_id, (void *)(UXSTACKTOP - PGSIZE), PTE_U | PTE_W | PTE_P);
		if(r < 0){
			panic("pgfault_handler %e", r);
		}

		r = sys_env_set_pgfault_upcall(env_id, _pgfault_upcall);

		if(r < 0){
			panic("pgfault_hander %e", r);
		}
		// panic("set_pgfault_handler not implemented");
	}

	// Save handler pointer for assembly to call.
	_pgfault_handler = handler;
}

```

## Implementing Copying-on-Write Fork

我们现在有了在用户空间完成写时拷贝fork的能力。

我们在lib/fork.c中为我们的fork提供了一个框架，与dumbfork类似，fork应该创建一个新的空间，然后扫描父环境的整个地址空间，并在子环境中设置相应的页面映射。关键区别在于，虽然dumbfork复制了页面，但是fork最初只复制页面映射，fork只会在某个环境试图写入每个页面的时候复制它。

fork的基本流程如下：

1. 父进程把pgfalut作为一个c击毙嗯的pagefault处理程序，使用我们之前完成的set_pgfault_handler函数。

2. 父进程调用sys_exfork来创建子进程的环境。

3. 在UTOP之下所有的可写和写时复制的页面(用PTE_COW标示)，父进程调用duppage将其映射到子进程，同时将其权限修改为只读，并用PTE_COW位与一半的只读页面进行区别。

   异常栈的分配方式不同，你需要分配一个新的页来作为子进程的异常栈，因为page_fault_handler会实实在在的向异常栈写入内容，并且在异常栈上运行。如果异常栈页面都用COW机制，那么久没有能够执行拷贝这个过程的载体了。

4. 父进程会位子进程设置user page falut entry point

5. 子进程就绪，父进程将其设置为runnable

进程第一次往一个COW页面写入内容的时候，会发生page fault。其流程位

1. 内核将page fault传递至_pgfault_upcall，它会调用pgfault处理函数
2. pgfault检查错误类型，以及页面是否被标记为PTE_COW。
3. pgfault函数分配一个新的页面，并将fault page的内容拷贝过去，然后将旧的映射覆盖，使其映射到该新页面。

### Exercise 12

完成fork，duppag和pgfauli。然后使用forktree来进行测试。它会有如下输出：

```
	1000: I am ''
	1001: I am '0'
	2000: I am '00'
	2001: I am '000'
	1002: I am '1'
	3000: I am '11'
	3001: I am '10'
	4000: I am '100'
	1003: I am '01'
	5000: I am '010'
	4001: I am '011'
	2002: I am '110'
	1004: I am '001'
	1005: I am '111'
	1006: I am '101'
	
```

但是不一定是这个顺序，还有环境id可能会不一样。

```

//
// User-level fork with copy-on-write.
// Set up our page fault handler appropriately.
// Create a child.
// Copy our address space and page fault handler setup to the child.
// Then mark the child as runnable and return.
//
// Returns: child's envid to the parent, 0 to the child, < 0 on error.
// It is also OK to panic on error.
//
// Hint:
//   Use uvpd, uvpt, and duppage.
//   Remember to fix "thisenv" in the child process.
//   Neither user exception stack should ever be marked copy-on-write,
//   so you must allocate a new page for the child's user exception stack.
//
envid_t
fork(void)
{
	// LAB 4: Your code here.
	set_pgfault_handler(pgfault);
	envid_t env_id = sys_exofork();
	if(env_id < 0){
		panic("fork: %e", env_id);
	}

	if(env_id == 0){
		thisenv = &envs[(ENVX(sys_getenvid()))];
		return 0;
	}



	for(uintptr_t aadr = UTEXT; addr < USTACKTOP; addr += PGSIZE){
	    // 查看page table和page directory 是否存在
		if((uvpt[(PDX(addr))] & PTE_P) && (uvpd[PGNUM(addr)] & PTE_P)){
			duppage(env_id, PGNUM(addr));
		}
	}

	int r = sys_page_alloc(env_id, (void *)(UXSTACKTOP - PGSIZE), PTE_P | PTE_W | PTE_U);
	if(r < 0){
		panic("fork error: %e", r);
	}

	if((r = sys_env_set_status(env_id, ENV_RUNNABLE)) < 0){
		panic("syt_env_set_status error: %e", r);
	}

	return env_id;
	// panic("fork not implemented");
}
```

```c

//
// Map our virtual page pn (address pn*PGSIZE) into the target envid
// at the same virtual address.  If the page is writable or copy-on-write,
// the new mapping must be created copy-on-write, and then our mapping must be
// marked copy-on-write as well.  (Exercise: Why do we need to mark ours
// copy-on-write again if it was already copy-on-write at the beginning of
// this function?)
//
// Returns: 0 on success, < 0 on error.
// It is also OK to panic on error.
//
static int
duppage(envid_t envid, unsigned pn)
{
	int r;

	// LAB 4: Your code here.
	void *va = (void *)(pn*PGSIZE);
	envid_t env_id = sys_getenvid();

	int perm = uvpt[pn] & 0xFFF;
	if((perm & PTE_W) || (perm &PTE_COW)){
		perm|= PTE_COW;
		perm &= ~PTE_W;
	}
	// 非常重要，使它能被sys_call调用。
	perm &= PTE_SYSCALL;

	if((r = sys_page_map(env_id, va, envid, va, perm)) < 0){
		panic("duppage: %e", r);
	}
	// 修改权限，重新映射。
	if((r = sys_page_map(env_id, va, env_id, va, perm)) < 0){
		panic("duppage: %e", r);
	}
	// panic("duppage not implemented");
	return 0;
}
```

```c
// Custom page fault handler - if faulting page is copy-on-write,
// map in our own private writable copy.
//
static void
pgfault(struct UTrapframe *utf)
{
	void *addr = (void *) utf->utf_fault_va;
	uint32_t err = utf->utf_err;
	int r;

	// Check that the faulting access was (1) a write, and (2) to a
	// copy-on-write page.  If not, panic.
	// Hint:
	//   Use the read-only page table mappings at uvpt
	//   (see <inc/memlayout.h>).

	// LAB 4: Your code here.
	if((err & FEC_WR) == 0 || (uvpt[PGNUM(addr)] & PTE_COW) == 0){
		panic("pgfault: invalid user trap frame");
	}
	// Allocate a new page, map it at a temporary location (PFTEMP),
	// copy the data from the old page to the new page, then move the new
	// page to the old page's address.
	// Hint:
	//   You should make three system calls.

	// LAB 4: Your code here.
	envid_t env_id = sys_getenvid();
    // 分配一个页面，映射到了交换去PFTEMP这个虚拟地址，
	if((r = sys_page_alloc(env_id, (void *)PFTEMP, PTE_P | PTE_W| PTE_U)) < 0){
		panic("pgfault: page alloction failed %e", r);
	}

	addr = ROUNDDOWN(addr, PGSIZE);
	// 将addr所在页面拷贝至PFTEMP，此时有两个物理页保存了相同的内容。
	memmove(PFTEMP, addr, PGSIZE);
	// 解除对addr的映射
	if((r = sys_page_unmap(env_id, addr)) < 0){
		panic("pgfault: page unmap failed %e",r);
	}
	// 将addr映射到PFTEMP对应的物理页
	if((r = sys_page_map(env_id, PFTEMP, envid, addr, PTE_P | PTE_W | PTE_U)) < 0){
		panic("pgfault: page map failed %e", r);
	}
	// 接触了PFTEMP的映射。
	if((r = sys_page_unmap(env_id, PFTEMP)) < 0){
		panic("pgfault: page unmap %e", r);
	}
	// panic("pgfault not implemented");
}
```

