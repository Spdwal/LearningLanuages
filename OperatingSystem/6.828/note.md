# Part 1: PC Bootstrap

## Exercise 1

### 了解x86汇编

#### 手册一本- -。。。。 The PC Assembly Language Book

TODO
这本书使用的汇编为nasm汇编，nasm汇编用的是intel语法，但是课程用的GCC汇编是AT&T语法，所以再附送手册一本
#### Brennan's Guide to Inline Assembly

##### 语法上的区别

+ 寄存器名：AT&T会添加一个%
    AT&T:  %eax
    Intel: eax

+ 语法顺序不同
    AT&T: 源寄存器在左边，结果寄存器在右边。
    Intel: 源寄存器在右边，结果寄存器在左边。

+ 两者对于const和立即数的处理不同
    例子： 将c静态变量booga的地址存入寄存器eax
    ```nasm
    AT&T: movl $_booga, %eax
    Intel: mov eax, _booga
    ```
    例子： 将0xd00d存入ebx
    ```nasm
    movl $0xd00d, %ebx        #AT&T
    mov ebx, doodh            #Intel
    ```

+ 在AT&T中，如果必须指定指令是针对那个字长进行操作，否则，编辑器会自己进行猜测。
    b,w,l分别对应byte, word, long word。切记不能忘记。

+ 地址计算

    ```nasm
    immed32(basepointer, indexpointer, indexscale)               #AT&T
    [basepointer + indexpointer * indexscale + immed32]          #Intel
    ```

    地址计算相当于：
    immed32 + basepointer + indexpointer * indexscale
    其中immed32和basepointer必须存在一个。
  * c变量的地址

      ```nasm
      _booga								#AT&T
      [_booga]							#Intel
      ```

  * 寄存器内指向的内存的地址：

      ```nasm
      (%eax) 								#AT&T
      [eax]								#Intel
      ```

  * 寄存器地址加上一个偏移量

    ```nasm
    _variable(%eax)					#AT&T: 
    [eax + _variable]				#Intel
    ```

  * 在一个数组中的索引的地址(按照4位对齐，此处针对32位机器)

    ```nasm
    _array(, %eax, 4)       #AT&T
    [eax * 4 + array]       #Intel
    ```

  * 也可以取立即数的偏移

    ```nasm
    *(p + 1) p is char *     #c code
    1(%eax)                  #AT&T
    Intel: [eax]             #Intel
    ```

  * 可以直接进行算数运算

    ```nasm
    _struct_prointer + 8       #AT&T
    ```

* 在一个8-char数组记录中找特定的char

     ```nasm
     _array(%ebx, %eax, 8)       #AT&T
     [ebx + eax * 8 + _array]    #Intel
     ```

##### 基本内嵌汇编

基础为asm("statements");

如果asm与其他变量名称相冲突，那么可以使用\_\_asm\_\_替代使用。如果需要一次嵌入多行汇编，那么可以使用：

```C
asm("push %eax\n\t"
   	"movl $0, %eax\n\t"
   	"popl %eax");
```

##### 内嵌汇编

```c
asm ("cld\n\t"
     "rep\n\t"
     "stosl"
     : /* no output registers */
     : "c" (count), "a" (fill_value), "D" (dest) /*将count传入%ecx 将fill_value传入%eax dest传入%edi*/
     : "%ecx", "%edi" );/* 当前内联语句会对%ecx和%edi进行修改，希望gcc会对它进行考虑时加入此句 */
```

在内联汇编时，不能直接使用byte寄存器和word寄存器。

## Exercise 2

__利用gdb里的si命令来调试bios来猜测它是做什么的。__

先上指令：

```nasm
[f000:fff0] 0xffff0: ljmp $0xf000,$0xe05b
[f000:e05b] 0xfe05b: cmpl $0x0,%cs:0x6547 #$cs: 0xf000
[f000:e062] 0xfe062: jne  0xfd2b6
[f000:e066] 0xfe066: xor  %ax,%ax         #$eax = 0
[f000:e068] 0xfe068: mov  %ax,%ss         #$ss = 0
[f000:e06a] 0xfe06a: mov  $0x7000,%esp    #$esp = 0x7000
[f000:0e70] 0xfe070: mov  $0xf3c24,%edx   #$edx = 0xf3c24
[f000:e076] 0xfe076: jmp  0xfd124
[f000:d124] 0xfd124: mov  %eax,%ecx       #$eax = 0, $ecx = 0
[f000:d127] 0xfd127: cli                  #禁止中断发生，只能在内核模式下使用，与sti命令配对
[f000:d128] 0xfd128: cld                  #
[f000:d129] 0xfd129: mov  $0x8f, %eax     #$eax = 0x8f
[f000:d12f] 0xfd12f: out  %al,$0x70       #将$al中的数据写入0x70端口中，$al=0x8f
[f000:d131] 0xfd131: in   $0x71,%al       #从0x70端口设备中读取一个数据，放入$al, $al = 0
#0x70端口和0x71端口是用于控制系统中一个叫做CMOS的设备，这个设备是一个低功耗的存储设备，它可以用于在计算机关闭时存储一些信息，它是由独立的电池供电的。
#当对0x70写入一个byte时，最高位为1，则NMI关闭，最高位为0，则NMI打开，这个关闭打开直到下一次往0x70输送数据为止。
#剩下的7位数据，是用来定位CMOS寄存器。此处我们即是要对0xf号寄存器进行读写，读写时，通过in指令对port0x71进行读写。
#CMOS的0xf寄存器是储存的关机状态，此时读出来数据为0，即表示为 normal execution of POST（软件重启）
#这个从CMOS中取出来的值没有被再次利用，所以应该只是单纯的关闭NMI中断。
[f000:d133] 0xfd133: in   $0x92,%al       #从0x92端口设备中读取一个数据，放入$al, $al = 0
[f000:d135] 0xfd135: or   $0x2,%al        #$al = 2，此处是为了不改变其他bit的情况下修改bit1，bit1功能为indicates A20 active，也即是打开A20位,为的是实现32位寻址。
[f000:d137] 0xfd137: out  %al,$0x92       #将$al中的数据写入端口0x92
#实模式下，CPU寻址范围为0~0xfffff，一共1m空间，需要0～19号共20根地址线，进入保护模式后，将使用32为寻址模式，即采用32根地址线进行寻址，第21根(A20)至第32根地址线的控制就意味着寻址模式的切换。
[f000:d139] 0xfd139: lidtw %cs:0x6690     #加载中断向量表寄存器，将目标地址处的6个数据加载进入idtr中。
#中断向量表即是一个表，中断向量是每一种中断对应的处理函数的地址，中断向量表即是保存它们的，此处gdb无法查看idtr的值。
# x/6bx 0xf6690
# 0x00 0x00 0x96 0x66 0x0f 0x00
[f000:d13f] 0xfd13f: lgdtw %cs:0x6650     #同上，加载全局描述符表。所谓全局，是表示无论在实模式还是保护模式下都可以使用，在进入保护模式之前，必须要定义gdt。
# x/6b 0xf6650
# 0x37 0x00 0x58 0x66 0x0f 0x00
[f000:d145] 0xfd145: mov %cr0, %eax       #$eax = 0x60000010
[f000:d148] 0xfd148: or  $0x1, %eax       #$eax = 0x60000011
[f000:d14c] 0xfd14c: mov %eax, %cr0.      #$cr0 = 0x60000011
#此段代码是为了将cr0的第一位置1，此处cr0第一位为PE位，当它置1时，进入保护模式。
[f000:d14f] 0xfd14f: ljmpl $0x8, $0xfd157
# Note that to complete the process of loading a new GDT, the segment registers need to be reloaded. The CS register must be loaded using a far jump。
#如果设置了GDT，那么必须在接下来充实设置段寄存器， CS寄存器设置后必须经过一个long jump指令。
# The target architecture is assumed to be i386
# 允许保护模式并不会马上改变处理器把逻辑地址翻译成物理地址的过程；只有当某个段寄存器加载了一个新的值，然后处理器通过这个值读取 GDT 的一项从而改变了内部的段设置。我们没法直接修改 %cs，所以使用了一个 ljmp 指令。跳转指令会接着在下一行执行，但这样做实际上将 %cs 指向了 gdt 中的一个代码描述符表项。该描述符描述了一个32位代码段，这样处理器就切换到了32位模式下。就这样，引导加载器让处理器从8088进化到80286，接着进化到了80386。
0xfd157: mov $0x10, %eax                  #$eax = 0x10
0xfd15c: mov %eax,  %dx      			  #$dx = 0x10
0xfd15e: mov %eax,  %es                   #$es = 0x10
0xfd160: mov %eax,  %ss                   #$ss = 0x10
0xfd162: mov %eax,  %fs                   #$fs = 0x10
0xfd164: mov %eax,  %gs                   #$gs = 0x10
0xfd166: mov %ecx,  %eax                  #$ecx = 0, $eax = 0
0xfd168: jmp *%edx                        #$edx = 0xf3c24
.....
```

###  整理下用到的知识

#### 段寄存器

>- Stack Segment (SS). Pointer to the stack.
>- Code Segment (CS). Pointer to the code.
>- Data Segment (DS). Pointer to the data.
>- Extra Segment (ES). Pointer to extra data ('E' stands for 'Extra').
>- F Segment (FS). Pointer to more extra data ('F' comes after 'E').
>- G Segment (GS). Pointer to still more extra data ('G' comes after 'F').

在实模式下，有8个16位寄存器可以使用，但是实际上处理器发送给内存的是20位，多出来的4位其实就是由这写段寄存器来传送。取指会用到%cs，读写数据会用到%ds，读写栈会用到%ss

在一些现代的操作系统中，使用内存模型，然后让所有的段寄存器只想同一个地方，使用分页来代替，然后阻止段寄存器的使用。

#### 实模式与保护模式

intel8086使用20bit地址，但是处理器本身只有16bit，所以intel发明了一种将20bit映射到16bit上的方法。现在cpu早就有了更宽的总线，但是每当操作系统或者重启的时候，都会进入一个实模式，它的行为和最早的8086非常相似，虽有有一点点不同，但是却是兼容的。在实模式中有一个CS段和IP段，最后的地址就是CS << 16 + ip。

但是0xFFFF:0xFFFF -> 0xFFFF0 + FFFF = FFFFF + FFF0 = 1MB = FFF0 bytes可以使用的地址范围为1MB + 0xFFF0 byte。8086有20个adress line(A0~A19)，当可打开A20总线后(A20~A31)，可以加大寻址范围。

在进入保护模式之前，需要打开A20总线。

在保护模式中，x86可以访问4GB大小的内存(32bit)，这些内存地址会被映射在物理RAM中，在i386中，保护模式被control registers所控制，也就是cr0,cr2,cr3,cr4。

>### CR0[[edit](https://en.wikibooks.org/w/index.php?title=X86_Assembly/Protected_Mode&action=edit&section=7)]
>
>The CR0 32-bit register has 6 bits that are of interest to us. The low 5 bits of the CR0 register, and the highest bit. Here is a representation of CR0:
>
>```
>CR0: |PG|----RESERVED----|NE|ET|TS|EM|MP|PE|
>```
>
>
>
>- PE
>
>  Bit 0. The Protected Environment flag. This flag puts the system into protected mode when set.
>
>- MP
>
>  Bit 1. The Monitor Coprocessor flag. This flag controls the operation of the "WAIT" instruction.
>
>- EM
>
>  Bit 2. The Emulate flag. When this flag is set, coprocessor instructions will generate an exception.
>
>- TS
>
>  Bit 3. The Task Switched flag. This flag is set automatically when the processor switches to a new task.
>
>- ET
>
>  Bit 4. The Extension Type flag. ET (also called "R") tells us which type of coprocessor is installed. If ET = 0, an 80287 is installed. if ET = 1, an 80387 is installed.
>
>- NE
>
>  Bit 5. New exceptions. If this flag is clear, FPU exceptions arrive as interrupts. If set, as exceptions.
>
>- PG
>
>  Bit 31. The Paging flag. When this flag is set, memory paging is enabled. We will talk more about that in a second.
>
>### CR2
>
>CR2 contains a value called the **Page Fault Linear Address** (PFLA). When a page fault occurs, the address that access was attempted on is stored in CR2.
>
>### CR3
>
>The upper 20 bits of CR3 are called the **Page Directory Base Register** (PDBR). The PDBR holds the physical address of the page directory.
>
>### CR4
>
>CR4 contains several flags controlling advanced features of the processor.

# Part 2: The Boot Loader

软盘或者硬盘被512bytes被称为一个扇区，扇区是硬盘最小的传输单位，每一个读写操作都必须至少使用一个扇区。如果一个扇区是可引导的，那么被称之为引导扇区(boot sector)，也是boot loader的所在地。当BIOS找到了boot扇区，它会将这512bytes的数据，拷贝到0x7c00~0x7dff，然后使用一个jmp指令跳转到0x7c00，将控制权交到boot loader手上。

从CD-ROM boot比正常的boot复杂一些，主要是因为CD-ROM的扇区大小是2048，这样的话BIOS会装载一个大得多的boot镜像。然后又丢给我一本手册，我稍微看了一下。。。WTF，skip it。

接下来是看两个代码，分别是boot/boot.s和boot/main.c。需要将它彻底弄懂。

```assembly
#include <inc/mmu.h>

# Start the CPU: switch to 32-bit protected mode, jump into C.
# The BIOS loads this code from the first sector of the hard disk into
# memory at physical address 0x7c00 and starts executing in real mode
# with %cs=0 %ip=7c00.

# 代码扇区扇区
.set PROT_MODE_CSEG, 0x8         # kernel code segment selector
# 数据扇区
.set PROT_MODE_DSEG, 0x10        # kernel data segment selector
.set CR0_PE_ON,      0x1         # protected mode enable flag

.globl start
start:
  .code16                     # Assemble for 16-bit mode
  # 屏蔽中断
  cli                         # Disable interrupts
  # cld和std指令一起字行块中使用的，他们决定了串传送的方向。此处置DF = 0，使地址从低位往高位走。
  cld                         # String operations increment

  # Set up the important data segment registers (DS, ES, SS).
  # 设置各个段寄存器
  xorw    %ax,%ax             # Segment number zero
  movw    %ax,%ds             # -> Data Segment
  movw    %ax,%es             # -> Extra Segment
  movw    %ax,%ss             # -> Stack Segment

  # Enable A20:
  #   For backwards compatibility with the earliest PCs, physical
  #   address line 20 is tied low, so that addresses higher than
  #   1MB wrap around to zero by default.  This code undoes this.
  #   这一部分主要是利用键盘控制器来打开A20端口，这个方法是4种打开键盘控制器的方法之一
  #   优点就是可移植性比较高
  #   CPU通过使用 0x60和0x64来和CPU进行通信。
seta20.1:
  # 此处0x64端口是KB controller read status。键盘控制器读取状态寄存器。
  # 从端口0x64读取一个字节
  inb     $0x64,%al               # Wait for not busy
  testb   $0x2,%al
  # %al AND 0x2 为0 则跳转，等于是一直读到bit1 = 0为止。
  # bit1 = 0,表示缓冲区有可写进去的空位。
  # 即是直到0x64端口的input缓冲区有空位
  jnz     seta20.1
  
  # 0x60接口是从PS/2硬件接口或者是从PS/2控制器本身读入数据
  # 或者是往PS/2硬件或者PS/2控制器写数据。
  # 而状态寄存器，也即是0x64，是为了设置或者读取PS/2控制器的状态使用。
  # 将0xd1这个数据写入0x64端口，是设置了状态，可以通过往0x60端口写入数据而控制PS/2的状态。
  #  write output port. next byte written  to 0060
  #  will be written to the 804x output port; the
  #  original IBM AT and many compatibles use bit 1 of
  #	 the output port to control the A20 gate.
  # 这个数据会写入804x的输出端口。
  movb    $0xd1,%al               # 0xd1 -> port 0x64
  outb    %al,$0x64

seta20.2:
  # 同上，检测缓冲区是否有空位
  inb     $0x64,%al               # Wait for not busy
  testb   $0x2,%al
  jnz     seta20.2
  # 向0x60端口写入0xdf
  #  DF	sngl  enable address line A20 (HP Vectra only???)
  #  这个数据是在0x64端口上查到的，打开A20总线接口。
  #  因为在上次对0x64写入了0xd1这个数据导致的。
  movb    $0xdf,%al               # 0xdf -> port 0x60
  outb    %al,$0x60
  #  存疑，在wiki.osdev.org上所查到的是读出状态码。然后or 2 打开bit1，再写入0x60即可以打开A20门
  #  但是在bochs.sourceforge.net上查到的是写入0xDF是打开A20门。
  #  可能是因为移植性的问题。
  
  
  # Switch from real to protected mode, using a bootstrap GDT
  # and segment translation that makes virtual addresses 
  # identical to their physical addresses, so that the 
  # effective memory map does not change during the switch.
  # 设置GDTR和CR0，同exercise2,打开保护模式，然后一个long jmp跳转进入安全模式
  # gdtdesc是一个内存地址。
  lgdt    gdtdesc    
  movl    %cr0, %eax
  orl     $CR0_PE_ON, %eax
  movl    %eax, %cr0
  
  # Jump to next instruction, but in 32-bit code segment.
  # Switches processor into 32-bit mode.
  # long jmp to 0x80:adress of protcseg
  ljmp    $PROT_MODE_CSEG, $protcseg

  .code32                     # Assemble for 32-bit mode
protcseg:
  # Set up the protected-mode data segment registers
  # data = 0x10
  movw    $PROT_MODE_DSEG, %ax    # Our data segment selector
  movw    %ax, %ds                # -> DS: Data Segment
  movw    %ax, %es                # -> ES: Extra Segment
  movw    %ax, %fs                # -> FS
  movw    %ax, %gs                # -> GS
  movw    %ax, %ss                # -> SS: Stack Segment
  
  # Set up the stack pointer and call into C.
  # 设置栈指针，然后调用bootmain，也就是boot/main.c
  movl    $start, %esp
  call bootmain

  # If bootmain returns (it shouldn't), loop.
  # 没啥好说的，跳转到下一个函数
spin:
  jmp spin

# Bootstrap GDT
.p2align 2                                # force 4 byte alignment
# 设置对齐
gdt:
  # 此处是在初始化一张gtd表
  SEG_NULL				# null seg
  SEG(STA_X|STA_R, 0x0, 0xffffffff)	# code seg
  SEG(STA_W, 0x0, 0xffffffff)	        # data seg
  # inc/mmu.h
  # #define SEG_NULL						\
  #  .word 0, 0;						\
  #  .byte 0, 0, 0, 0
  # #define SEG(type,base,lim)					\
  #  .word (((lim) >> 12) & 0xffff), ((base) & 0xffff);	\
  #  .byte (((base) >> 16) & 0xff), (0x90 | (type)),		\
  #   (0xC0 | (((lim) >> 28) & 0xf)), (((base) >> 24) & 0xff)
  #  此处SEG第一个参数是段的访问权限，第二个参数是起始地址，第三个参数是段的大小的界限

gdtdesc:
  .word   0x17                            # sizeof(gdt) - 1
  .long   gdt                             # address gdt

```



```c
#include <inc/x86.h>
#include <inc/elf.h>

/**********************************************************************
 * This a dirt simple boot loader, whose sole job is to boot
 * an ELF kernel image from the first IDE hard disk.
 * DE即Integrated Drive Electronics，它的本意是指把控制器与盘体集成在一起的硬盘驱动器，IDE是表示硬盘的传输接口。
 * DISK LAYOUT
 *  * This program(boot.S and main.c) is the bootloader.  It should
 *    be stored in the first sector of the disk.
 *    保存在disk的第一个扇区。
 *  * The 2nd sector onward holds the kernel image.
 *    第二个扇区保存内核镜像
 *  * The kernel image must be in ELF format.
 *    内核镜像必须是ELF格式
 * BOOT UP STEPS
 *  * when the CPU boots it loads the BIOS into memory and executes it
 *
 *  * the BIOS intializes devices, sets of the interrupt routines, and
 *    reads the first sector of the boot device(e.g., hard-drive)
 *    into memory and jumps to it.
 *    BIOS初始化硬件，设置中断，并且将boot device中的第一个扇区读取进内存并且跳转到它。
 *  * Assuming this boot loader is stored in the first sector of the
 *    hard-drive, this code takes over...
 *
 *  * control starts in boot.S -- which sets up protected mode,
 *    and a stack so C code then run, then calls bootmain()
 *
 *  * bootmain() in this file takes over, reads in the kernel and jumps to it.
 **********************************************************************/

/**********************************************************************
 * ELF结构
 Elf是使用来将程序储存在硬盘上的一种格式。
 Elf由4部分组成，分别是Elf头，程序表头，节，和节表头组成。
 headers是用来描述sections是怎样在程序中储存的。
 struct Elf{
 	uint32_t e_magic;               //保存了4个char，"\0x7FELF"，用来校验是否是一个Elf结构体
 	uint8_t  e_elf[12];             //应该是关于一些平台相关的设置，关系到如何译码和解释文件内容存疑。    
 	uint16_t e_type;                //该文件的类型
 	uint16_t e_machine;             //该文件需要的体系结构
 	uint32_t e_version;             //文件的版本
 	uint32_t e_entry;               //程序的入口地址
 	uint32_t e_phoff;               //表示Program header table在文件中的偏移量(以字节计算)
 	uint32_t e_shoff;               //表示Section header table在文件中的偏移量(以字节计算)
 	uint32_t e_flags;               //对IA32而言，此项为0。
 	uint16_t e_ehsize;              //表示ELF header大小
 	uint16_t e_phentsize;           //表示Program header table中每一个条目的大小
 	uint16_t e_phnum;               //表示Program header table中有多少个条目
 	uint16_t e_shentsize;           //表示Section header table中每一个条目的大小
 	uint16_t e_shnum;               //表示Section header table中有多少个条目
 	uint16_t e_shstrndx;            //表示包含节名称的字符串是第几个节
 }
 **********************************************************************/
#define SECTSIZE	512
//一个Elf表，存放在0x10000地址处
//其实就是第一个程序的开头。
//Elf文件头在文件最开始的地方。
#define ELFHDR		((struct Elf *) 0x10000) // scratch space

void readsect(void*, uint32_t);
void readseg(uint32_t, uint32_t, uint32_t);

void
bootmain(void)
{
    //program header是描述一个段在文件中的位置，大小以及他被放进内存后所在的位置和大小
    /***********************************************************
    struct Proghdr {
		uint32_t p_type;                  //当前program的段类型
		uint32_t p_offset;                //段的第一个字节在文件中的偏移
		uint32_t p_va;                    //段的第一个字节在文件中的虚拟地址
		uint32_t p_pa;                    //段的第一个字节在文件中的物理地址，在屋里内存定位相关的系统中使用
		uint32_t p_filesz;                //段在文件中的长度
		uint32_t p_memsz;                 //段在内存中的长度
		uint32_t p_flags;                 //与段相关的标识位
		uint32_t p_align;                 //根据此项来确定段在文件以及内存中如何对齐
	};

    ************************************************************/
	struct Proghdr *ph, *eph;

	// read 1st page off disk
    //读取从0开始的8个扇区放入ELFHDR位置
    //也就是程序起始位置
	readseg((uint32_t) ELFHDR, SECTSIZE*8, 0);

	// is this a valid ELF?
    //校验它是不是一个ELF结构体
	if (ELFHDR->e_magic != ELF_MAGIC)
		goto bad;

	// load each program segment (ignores ph flags)
    // 找到program header
	ph = (struct Proghdr *) ((uint8_t *) ELFHDR + ELFHDR->e_phoff);
	eph = ph + ELFHDR->e_phnum;
    //将每一段从屋里地址中放入内存中。
	for (; ph < eph; ph++)
		// p_pa is the load address of this segment (as well
		// as the physical address)
		readseg(ph->p_pa, ph->p_memsz, ph->p_offset);

	// call the entry point from the ELF header
	// note: does not return!
    // 运行程序入口点
	((void (*)(void)) (ELFHDR->e_entry))();

bad:
    //0x8A00写入0x8A00端口
   	//打开IOdebug
    //0x8E00写入0x8A00端口
    //如果系统挂了可以在bochs的调试器中看到状态
	outw(0x8A00, 0x8A00);
	outw(0x8A00, 0x8E00);
	while (1)
		/* do nothing */;
}

// Read 'count' bytes at 'offset' from kernel into physical address 'pa'.
// Might copy more than asked
// 将count个扇区从内核读取进物理地址pa处。
void
readseg(uint32_t pa, uint32_t count, uint32_t offset)
{
	uint32_t end_pa;

	end_pa = pa + count;

	// round down to sector boundary
	pa &= ~(SECTSIZE - 1);

	// translate from bytes to sectors, and kernel starts at sector 1
	offset = (offset / SECTSIZE) + 1;

	// If this is too slow, we could read lots of sectors at a time.
	// We'd write more to memory than asked, but it doesn't matter --
	// we load in increasing order.
	while (pa < end_pa) {
		// Since we haven't enabled paging yet and we're using
		// an identity segment mapping (see boot.S), we can
		// use physical addresses directly.  This won't be the
		// case once JOS enables the MMU.
		readsect((uint8_t*) pa, offset);
		pa += SECTSIZE;
		offset++;
	}
}

void
waitdisk(void)
{
	// wait for disk reaady
	while ((inb(0x1F7) & 0xC0) != 0x40)
		/* do nothing */;
}

void
readsect(void *dst, uint32_t offset)
{
	// wait for disk to be ready
	waitdisk();

	outb(0x1F2, 1);		// count = 1
	outb(0x1F3, offset);
	outb(0x1F4, offset >> 8);
	outb(0x1F5, offset >> 16);
	outb(0x1F6, (offset >> 24) | 0xE0);
	outb(0x1F7, 0x20);	// cmd 0x20 - read sectors

	// wait for disk to be ready
	waitdisk();

	// read a sector
	insl(0x1F0, dst, SECTSIZE/4);
}
```

## Exercise 3

__在地址0x7c00处打一个断点，此地址为boot扇区被加载的地址，然后跟踪调试boot/boot.s，同时用x/i命令来查看bootloader内的指令，并且将它们和obj/boot/boot.asm进行比较。__

第一个不同点在设置cr0的时候，

```nasm
and %al, %al
or $0x1, %ax
mov %eax, %cr0
ljmp 0xb866, $0x87c32
```

无伤大雅，基本上实现的功能一样。