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
	boot_map_region(kern_pgdir, base, ROUNDUP(size, PGSIZE), PTE_PCD | PTE_PWT | PTE_P);
    // 检测是否越过了limit。
	if((base + ROUNDUP(size, PGSIZE)) > MMIOLIM){
		panic("mmio_map_region failed:  TOO HIGH ADRESS.\n");
	}
	return base;
	// panic("mmio_map_region not implemented");
}
```



