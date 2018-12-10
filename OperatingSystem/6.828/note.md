# Part 1: PC Bootstrap

## Exercise 1

### 了解x86汇编

#### 手册一本- -。。。。 The PC Assembly Language Book

#### C inline 汇编

格式为:

```c
asm(code-strings)
```

但是单纯的这么作，可能因为优化而产生自己不想要得到的后果。

gcc同时提供了一种更加优异的嵌入汇编方式：

```c
asm(code-strings [: output-list [: input-list [: overwrite-list]]]);
```

方括号里的都是可选择的参数。

code-strings里面使用了类似于printf一样的参数格式化，例如%eax寄存器，使用的format-string为%%eax。

Output-list 有着如下的格式

```
[name] tag (expr)
```

这个格式确定了操作的名字，输出的内容，而后的C表达式指定指令的结果存储的变量。

tag指定了output操作的限制，具体限制如下表

| constraint | Meaning                          |
| ---------- | -------------------------------- |
| "=r"       | 更新寄存器内的数据               |
| "+r"       | 读取并且更新寄存器内的数据       |
| "=m"       | 更新内存中的数据                 |
| "+m"       | 读取并且更新内存中的数据         |
| "=rm"      | 更新寄存器或内存中的数据         |
| "+rm"      | 读取和更新及存取或者内存中的数据 |

expr中可以是任何可赋值的变量，也即是俗称的左值。

Input-list也是有着相同的格式。但是只有"r","m","rm"来指定是从寄存器，还是内存，或者两者都是中读取数据。

gcc甚至会对寄存器进行优化

```c
int tmult_ok3(long x, long y, long *dest){
    unsigned char bresult;
    *dest - x * y;
    asm("setae %[b]       #set result"
        : [b] "=r" (bresult)
        );
    return (int)  bresult;
}
```

编译器会自动为%[b]选择合适长度的寄存器。下面再看一个例子。

```c
int umult_ok(unsigned x, unsigned long y, unsigned long *dest){
    unsigned char bresult;
    
    asm("moveq %[x], %%rax           "
        "mulq %[y]                   "
        "movq %%rax                  "
        : [p] "=m" (*dest), [b] "=r" (bresult)    // outputs
        : [x] "r" (x), [y] "r" (y)                // inputs
        : "%rax", "rdx"                           //overwrites 
    )
}
```

最后的overwrites表示rax和rdx需要更新。我们通常在这里指定我们使用了哪些寄存器或者内存。

###### flags

| 分类       | 限定符  | 描述                                                         |
| ---------- | ------- | ------------------------------------------------------------ |
| 通用寄存器 | "a"     | 输入变量放入eax。如果eax已经被使用，就将eaxpush到堆栈，最后再pop出来。 |
|            | "b"     | 将输入变量放入ebx                                            |
|            | "c"     | 将输入变量放入ecx                                            |
|            | "d"     | 将输入变量放入edx                                            |
|            | "s"     | 将输入变量放入esi                                            |
|            | "d"     | 将输入变量放入edi                                            |
|            | "q"     | 将输入变量放入eax，ebx，ecx，edx中的一个                     |
|            | "r"     | 将输入变量放入通用寄存器，也就是eax，ebx，ecx, edx，esi，edi中的一个 |
|            | "A"     | 把eax和edx合成一个64 位的寄存器(use long longs)              |
| 内存       | "m"     | 内存变量                                                     |
|            | "o"     | 操作数为内存变量，但是其寻址方式是偏移量类型，也即是基址寻址，或者是基址加变址寻址 |
|            | "V"     | 操作数为内存变量，但寻址方式不是偏移量类型                   |
|            | " "     | 操作数为内存变量，但寻址方式为自动增量                       |
|            | "p"     | 操作数是一个合法的内存地址（指针）                           |
| 立即数     | "I"     | 0-31之间的立即数（用于32位移位指令）                         |
|            | "J"     | 0-63之间的立即数（用于64位移位指令                           |
|            | "N"     | 0-255之间的立即数（用于out指令）                             |
|            | "i"     | 立即数                                                       |
|            | "n"     | 立即数，有些系统不支持除字以外的立即数，这些系统应该使用"n"而不是"i" |
| 匹配       | " 0 "   | 表示用它限制的操作数与某个指定的操作数匹配，                 |
|            | "1" ... | 也即该操作数就是指定的那个操作数，例如"0"                    |
|            | "9"     | 去描述"％1"操作数，那么"%1"引用的其实就是"%0"操作数，注意作为限定符字母的0－9 与指令中的"％0"－"％9"的区别，前者描述操作数，后者代表操作数。 |
|            | &       | 该输出操作数不能使用过和输入操作数相同的寄存器               |
| 操作数类型 | "="     | 操作数在指令中是只写的（输出操作数）                         |
|            | "+"     | 操作数在指令中是读写类型的（输入输出操作数）                 |
| 浮点数     | "f"     | 浮点寄存器                                                   |
|            | "t"     | 第一个浮点寄存器                                             |
|            | "u"     | 第二个浮点寄存器                                             |
|            | "G"     | 标准的80387浮点常数                                          |
|            | %       | 该操作数可以和下一个操作数交换位置                           |
|            | #       | 部分注释，从该字符到其后的逗号之间所有字母被忽略             |
|            | *       | 表示如果选用寄存器，则其后的字母被忽略                       |

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
		uint32_t p_pa;                    //段的第一个字节在文件中的物理地址，在物理内存定位相关的系统中使用
		uint32_t p_filesz;                //段在文件中的长度
		uint32_t p_memsz;                 //段在内存中的长度
		uint32_t p_flags;                 //与段相关的标识位
		uint32_t p_align;                 //根据此项来确定段在文件以及内存中如何对齐
	};

    ************************************************************/
    // eph是end of program header table
    // 也就是program header tables 的最后一个 program header
    struct Proghdr *ph, *eph;

	// read 1st page off disk
    //读取从0开始的8个扇区放入ELFHDR位置
    //也就是程序起始位置
    // 此处应该也将program header tables 给读取进入了内存。
    // 没有找到官方资料，但是问过一些内核开发人员，可执行文件和共享文件都有 program header table。
	readseg((uint32_t) ELFHDR, SECTSIZE*8, 0);

	// is this a valid ELF?
    //校验它是不是一个ELF结构体
	if (ELFHDR->e_magic != ELF_MAGIC)
		goto bad;

	// load each program segment (ignores ph flags)
    // 找到program header的地址。 e_phoff是相对文件的offset
    // 同时ELF的地址也就是文件的地址。
	ph = (struct Proghdr *) ((uint8_t *) ELFHDR + ELFHDR->e_phoff);
	eph = ph + ELFHDR->e_phnum;
    //将每一个section从物理地址中放入内存中。
	for (; ph < eph; ph++)
		// p_pa is the load address of this segment (as well
		// as the physical address)
        // memsz -> memeory size;
        // 物理地址读取到program offset处。
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
    // 确认扇区边界，将其全部置0
	pa &= ~(SECTSIZE - 1);

	// translate from bytes to sectors, and kernel starts at sector 1
    // 计算扇区编号
	offset = (offset / SECTSIZE) + 1;

	// If this is too slow, we could read lots of sectors at a time.
	// We'd write more to memory than asked, but it doesn't matter --
	// we load in increasing order.
	while (pa < end_pa) {
		// Since we haven't enabled paging yet and we're using
		// an identity segment mapping (see boot.S), we can
		// use physical addresses directly.  This won't be the
		// case once JOS enables the MMU.
        // 从编号offset的扇区中读取数据进入文件的物理地址
		readsect((uint8_t*) pa, offset);
		pa += SECTSIZE;
		offset++;
	}
}

void
waitdisk(void)
{
	// wait for disk reaady
    // 一直从0x1f7中读取数据，直到他空闲为止。
    // 0x40这个位为1，就表示它空闲。
	while ((inb(0x1F7) & 0xC0) != 0x40)
		/* do nothing */;
}

void
readsect(void *dst, uint32_t offset)
{
	// wait for disk to be ready
	waitdisk();

    //读入一个扇区
	outb(0x1F2, 1);		// count = 1
    //将需要读取的扇区编号传入，因为扇区编号过长，所以分成4段传入
	outb(0x1F3, offset);
	outb(0x1F4, offset >> 8);
	outb(0x1F5, offset >> 16);
	outb(0x1F6, (offset >> 24) | 0xE0);
	outb(0x1F7, 0x20);	// cmd 0x20 - read sectors

	// wait for disk to be ready
	waitdisk();

	// read a sector
    // 0x1F0是硬盘接口的数据端口，而且还是一个16位的端口，一旦硬盘空闲且准备就绪，
    // 就可以连续从这额端口写入或者读取数据。
    // 因为一次操作会读取4个字节，所以这里需要除4.
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

__跟踪调试 boot/main.c中的bootmain函数，然后跟踪调试readsec函数，指出每个汇编指令是readsect中的哪一条语句，然后指出for循环开始和结尾从扇区中读入了什么，找出来循环结束后，运行了什么代码，在在那个代码出大一个断点，然后跟踪调试bootloader。__

直接make gdb，然后b bootmain没有用，找不到这个函数，我是在obj/boot/boot.asm中找到：

```nasm
00007d19 <bootmain>
```

找到bootmain的地址，而后直接

```gdb
b *0x7d19
continue
```

然后进入bootmain的运行。

```nasm
0x7d19: push %ebp                    # ebp = 0x0
0x7d1a: move %esp, %ebp              # ebp = esp = 0x7bf8 也就是0x7c00的前一个字节。
0x7d1c: push %esi                    # esi = 0
0x7d1d: push %ebx                    # ebx = 0
# 保存参数，准备调用函数
# 表示下面语句是调用一条c语言语句
readseg((uint32_t) ELFHDR, SECTSIZE*8, 0);
0x7d1e: push %edx                    # edx = 0;
# 通过栈传递参数
0x7d1f: push 0x0
0x7d21: push 0x1000
0x7d26: push 0x10000
0x7d2b: call 7cda <readseg>
# redseg,将头8个扇区放入ELFHDR处，
0x7cda: push %ebp                    # 保存地址，方便ret，ebp = 0x7cda
0x7cdb: mov %esp, %ebp               # ebp = esp = 0x7db8  栈往下延伸。
0x7cdd: push %edi                    # edi = 0
0x7cde: push %esi                    # esi = 0
0x7cdf: push %ebx                    # ebx = 0
0x7ce0: sub %0xc, %esp               # esp = 0x7bc0
# 取出三个参数
0x7ce3: mov 0x10(%ebp), %edi         # edi = 0
0x7ce6: mov 0x8(%ebp), %ebx          # ebx = 10000
0x7ce9: mov 0xc(ebp), %esi           # esi = 1000
# offset = (offset / SECTSIZE)
0x7cec: shr $0x9, $edi               # edi = 0
# end_pa = pa + count
0x7cef: add %ebx, %esi               # esi = 0x11000
# offset = (offset / SECTSIZE) + 1;
0x7cf1: inc %edi                     # edi = 1
# pa &= ~(SECTSIZE - 1)
0x7cf2: and $0xfffffe00, %ebx        # ebx = 65536
# while(pa < end_pa)
0x7cf8: cmp %esi, %ebx
0x7cfa: jae 0x7d11
# readsect((uint8_t*)pa, offset)    保存寄存器
0x7cfc: push %eax                    # eax = 0x10
0xfcfd: push %eax                    # eax = 0x10
0x7cfe: push %edi                    # edi = 1
# offset++
0x7cff: inc %edi                     # edi = 2
0x7d00: push %ebx                    # ebx = 0x10000
# pa += SECTSIZE
0x7d10: add $0x200, %ebx             # ebx = 0x10200
# call readsect
0x7d07: call 0x7c78   
# 保存参数
0x7c78: push %ebp
0x7c79: mov %esp, %ebp               
0x7c7b: push %edi                    
# offset = 1
0x7c7d: mov 0xc(%ebp), %ecx          # ecx = 1
# 它此处将inb和waitdisk顺序放置，如果inb执行结束，自动执行waitdisk
# call inb
0x7c80: call 0x7c6a
# inb(0x1F7) 将
0x7c6a: mov $0x1f7, %edx             # edx = 0x1f7
#将从0x1f7内读取出的数据放入%al中
0x7c6f: in (%dx), %al                # al = 0x50
# waitdisk 与之前的inb结合称完整的waitdisk函数
# while((inb(0x1F7) & 0xC0) != 0x40)
0x7c70: and $0xffffffc0, %eax
0x7c73: cmp 0x40, %al
0x7c75: jne 7c7f
0x7c77: ret
# outb(0x1f2, 1)
0x7c85: mov $0x1, %al
0x7c87: mov $0x1f2, %edx
0x7c8c: out %al, (%dx)
# oub(0x1f3, offset)
0x7c8d: mov $0x1f3, %edx
0x7c92: mov %ecx, %eax
0x7c94: out %al, (%dx)
# outb(0x1f4, offset >> 8)
0x7c97: mov $0x1f4, %edx
0x7c9c: shr $0x8, %eax               # eax = 0
0x7c9f: out %al, (%dx)
# outb(0x1f6, offset >> 16)
0x7ca0: mov %ecx, %eax
0x7ca2: mov $0x1f5, %edx             # eax = offset
0x7ca7: shr $0x10, %eax              # eax = offset >> 0x10 = 0
0x7caa: out %al, (%dx)
#outb(0x1f6, (offset >> 24) | 0xe0)
0x7cab: mov %ecs, %eax
0x7cad: mov %0x1f6, %edx
0x7cb2: shr $0x18, %eax
0x7cb5: or $0xffffffe0, %eax
0x7cb8: out %al, (%dx)
# outb(0x1f7, 0x20);
0x7cb9: mov $0x20, %al
0x7cbb: mov $0x1f7, %edx
0x7cc0: out %al, (%dx)
# call waitdisk
# 等待io
0x7cc1: call 0x7c6a
# 内嵌汇编，主要是调用 insl(0x1f0, dst, SECTSIZE/4)
0x7cc6: mov 0x8(%ebp), %edi
0x7cc9: mov $0x80, $ecx
0x7cce: mov $0x1f0, %edx
# cld清除标识位，表明一个串操作完成后源操作数和目的操作数的地址加1
0x7cd3: cld
# repnz叫重复串操作指令，它是一个浅灰，位于一条指令之前，这条指令会被一支重复执行。
# 并且知道计数寄存器的值满足某个条件。repnz重复后面的串操作指令%ecx次。
# dx为端口号0x1f0,%edi存放的是要呗存放的内存空间的起始地址，第一次循环时候也就是pa，0x10000。
# 此时 p *0x10000 0x464c457f，也就是ELF->e_magic的值
0x7cd4: repnz insl (%dx), %es:(%edi)
0x7cd6: pop %edx
0x7cd7: pop %edi
0x7cd8: pop %ebp
0x7cd9: ret
# continue to end of loop
# ...
# if(ELFHRD->e_magic != ELF_MAGIC)
0x7d30: add $0x10, $esp
0x7d33: cmpl $0x464c457f, 0x1000
0x7d3d: jne 0x7d77
# ph = (struct Proghdr *)((uint8_t *) ELFHDR + ELFHDR->e_phoff)
0x7d3f: mov 0x1001c, %eax            # 这里0x1001c没有加$，所以是将0x1001c处的数据放入eax = 0x34 此处存放的是ELFHDR->e_phoff
# eph = ph + ELFHRD->e_phnum
0x7d44: movzwl 0x1002c, %esi         # esi = 3 此处为ELFHDR->e_phnum
0x7d4b: lea 0x100000(%eax), %ebx     # ebx = 0x1003
0x7d51: shl $0x5, %esi               # 1个ptr 32位，所以此处左移5位， esi = 96
0x7d54: add %ebx, %esi               # esi = 0010094 也就是eph
0x7d58: jae 
0x7d5a: push %eax
# 进入循环，continue
# ...
# ((void (*)(void)) (ELFHDR->e_entry))()
0x7d71: call *0x10018                
# bootmain结束
```

## Exercise 4

让阅读K&R和了解指针，太简单，跳过。

需要阅读并理解boot/main.c，在这之前已经做过了。

此处有一本ELF的专门书籍需要阅读TODO。

一个ELF位于载入信息的头部，之后跟随着数个程序段。每一个程序段之间互相毗邻，它们将会被载入进内存的特定位置。Boot Loader不会改变代码或者数据，当它们被载入进内存后就会被运行。

一个ELF二进制文件，头部有一个固定长度的ELF HEADER，它后面跟着一个可变长度的program header，program header列出了所有将要被载入的程序节。在inc/elf.h中定义了ELF，在程序节中我们主要对以下几个部分感兴趣：

+ .text: 程序的运行指令
+ .rodata: 只读数据，比如字符串常量值等等。
+ .data: 程序里被初始化的数据。
+ 当连接器计算一个程序需要的内存的时候。它会为一些没有初始化的全局数据预留空间，这个程序节的名字叫做.bss，它位于.data后面。加载器或者程序本身，将会将这一段内存置0。在c语言中，未被初始化的全局变量都会被初始化为0。所以.bbs节并没有必要在ELF文件中占空间，作为替代，链接器只记录了.bss段的地址和大小。

利用objdump 来查看运行文件的布局：

```shell
objdump -h obj/kern/kernel
```

>
>
>obj/kern/kernel:     file format elf32-i386
>
>Sections:
>Idx Name          Size      VMA       LMA       File off  Algn
>  0 .text         000017bf  f0100000  00100000  00001000  2**4
>​                  CONTENTS, ALLOC, LOAD, READONLY, CODE
>  1 .rodata       00000714  f01017c0  001017c0  000027c0  2**5
>​                  CONTENTS, ALLOC, LOAD, READONLY, DATA
>  2 .stab         0000420d  f0101ed4  00101ed4  00002ed4  2**2
>​                  CONTENTS, ALLOC, LOAD, READONLY, DATA
>  3 .stabstr      000019b6  f01060e1  001060e1  000070e1  2**0
>​                  CONTENTS, ALLOC, LOAD, READONLY, DATA
>  4 .data         0000a300  f0108000  00108000  00009000  2**12
>​                  CONTENTS, ALLOC, LOAD, DATA
>  5 .bss          00000648  f0112300  00112300  00013300  2**5
>​                  CONTENTS, ALLOC, LOAD, DATA
>  6 .comment      0000002c  00000000  00000000  00013948  2**0
>​                  CONTENTS, READONLY

在这里我们可以看到一些除了我们之前提到的三个数据节之外的东西，这些东西中一大部分是为了调试准备的，它们在运行的之后并不会载入进内存。

终端关注一下.text节中的__VMA__(link adress or virtual memory adress)和__LMA__(load memory adress)，VMA的地址是在内存中程序将要运行的地址，CPU运行的时候都是虚拟地址，经过MMU转化为物理地址。当一段代码需要一个全局变量的地址，如果这个地址没有被链接，那么这个二进制文件就不会被正确执行。链接被现代链接库所使用，所但是它有着额外的性能和复杂度消耗，所以在6.828中并不会使用它。

一般来说，link和load adress都是相同的，接下来我们看看bootloader的内存布局：

```shell
objdump -h obj/boot/boot.out
```

>obj/boot/boot.out:     file format elf32-i386
>
>Sections:
>Idx Name          Size      VMA       LMA       File off  Algn
>  0 .text         0000018c  00007c00  00007c00  00000074  2**2
>​                  CONTENTS, ALLOC, LOAD, CODE
>  1 .eh_frame     0000009c  00007d8c  00007d8c  00000200  2**2
>​                  CONTENTS, ALLOC, LOAD, READONLY, DATA
>  2 .stab         00000870  00000000  00000000  0000029c  2**2
>​                  CONTENTS, READONLY, DEBUGGING
>  3 .stabstr      00000940  00000000  00000000  00000b0c  2**0
>​                  CONTENTS, READONLY, DEBUGGING
>  4 .comment      0000002c  00000000  00000000  0000144c  2**0
>​                  CONTENTS, READONLY

在这个文件中，VMA和LMA地址相同，我们也可以只看program headers

```shell
objdump -x obj/kern/kernel
```

>obj/kern/kernel:     file format elf32-i386
>obj/kern/kernel
>architecture: i386, flags 0x00000112:
>EXEC_P, HAS_SYMS, D_PAGED
>start address 0x0010000c
>
>Program Header:
>​    LOAD off    0x00001000 vaddr 0xf0100000 paddr 0x00100000 align 2**12
>​         filesz 0x00007a97 memsz 0x00007a97 flags r-x
>​    LOAD off    0x00009000 vaddr 0xf0108000 paddr 0x00108000 align 2**12
>​         filesz 0x0000a948 memsz 0x0000a948 flags rw-
>   STACK off    0x00000000 vaddr 0x00000000 paddr 0x00000000 align 2**4
>​         filesz 0x00000000 memsz 0x00000000 flags rwx
>
>Sections:
>
>...
>
>Symbol table:
>
>...

vaddr和paddr分别是虚拟地址和物理地址。

## Exercise 5

__BIOS加载boot扇区进入地址为0x7c00的内存，所以这里就是boot扇区的加载地址，boot扇区也是从这里开始运行，所以这里也是它的链接地址，我们在boot/Makefrag中将link adress利用指令 -Ttext 0x7c00进行设置，然后代码可以产生出正确的内存地址进行运行，在这个练习中，将link adress修改为错误的地址，然后再次跟踪调试boot loader，看一看会发生什么。__

我将0x7c00改成了0x6c00，然后make clean，make重新编译之后，发现多出来一条警告信息：

```
ld: warning: section '.bss' type changed to PROGBITS
```

然后我在stackoverflow对这条警告信息找到了如下的解释：

>When the BSS section is changed to PROGBITS, the effect is that there are more NUL bytes (zeroes) in the output file. When .bss is NOBITS (what it should be), the linker puts information in the output file that tell the operating system to wipe a section of memory to all zeroes when the program is loaded. If it's PROGBITS, then this information only tells the operating system to load the memory area from the file, and that section of the file is filled with zeroes. So the only negative effect is that the output file is bigger.

简单的来说就是：如果.bss是NOBITS的，那么链接器会在输出的文件里告诉操作系统当这个程序被加载的时候，根据提供的信息，将某一块内存给分配出来，并置0，但是如果是PROGBITS的话，就是告诉系统从文件里取出一块已经被置0的数据段存入内存中，所以区别就在NOBITS的文件中，.bbs数据段是不占用空间的，但是PROGBITS的数据段是占用空间的。虽然最后对运行的程序没什么影响，最大的影响是可执行文件多了一块被置零的数据段，需要占用更多的空间。

make qemu后，无限跳 Booting from Hard Disk。可以肯定是有错误了。

然后正常的make qemu-gdb，make gdb，还是和刚才一样无限跳一样的错误，完全不能跟踪到我打下的断点0x6c00，目测凡是把地址改成在0x7c00之前的，都会导致这样的情况。

现在将地址改成0c7d00。

BIOS会默认将程序装载在0x7c00处，打断点在0x7c00。随便跟踪几条语句：

```nasm
0c7c00: cli
0x7c01: cld
...
```

暂时看来没有任何区别，然后运行到这一条指令的时候

```nasm
0x7c1e: lgdtl (%esi)
```

通过打印信息发现，%esi中存储的值为0，然后使用

```
x/6xb ($esi)
0x0: 0x53 0xff 0x00 0xf0 0x53 0xff
```

虽然里面也有数据，但是用脑子想想中断描述符表也不应该在内存的0x0处，此处存疑，接下来往下走。

然后调试之后，遇到一句：

```nasm
0x7c2d: ljmp $0xb866, $0x87d32
```

这里错误就很明显了，正常情况下应该是跳转到0x7c2d的下一条语句，并且切换到保护模式，这里已经完全不知道跳转到了什么地方，程序从这里完全走进了错误的道路。