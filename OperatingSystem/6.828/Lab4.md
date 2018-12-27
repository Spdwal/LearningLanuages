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