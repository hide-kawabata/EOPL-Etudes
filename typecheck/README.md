# The EOPL Compiler for KUE-CHIP2-S
## The EOPL Language

Here we call a variant of a programming language described in the EOPL Book [1] the EOPL Language (EOPL for short). EOPL is a simple functional language. The EOPL Book describes how to construct several types of interpreters of EOPL. 

[1] D.P. Friedman and M. Wand: Essentials of Programming Languages, 3rd ed., The MIT Press, 2008.

Currently, EOPL does not support types other than int and bool.
Algebraic data type definitions are not available.
Currying and partial evaluation of functions are not supported.

### Eamples

It is a functional language.

```
let f = proc (int x)
        let a = x
	    b = +(x,1)
        in let g = proc (int x)
                   -(+(a, x), b)
           in g
in let g = (f 3)
    in let h = (f 4)
       in +((g 5), (h 6))
```

Fibonacci.

    let rec int fib(int n) =
      if <(n, 2) then 1
      else +((fib -(n,1)), (fib -(n,2)))
    in
      (fib 10)


Mutual recursion.


    let rec bool even (int n) = if ==(n, 0) then true   else (odd -(n,1))
            bool odd (int n) = if ==(n, 0) then false else (even -(n,1))
    in (even 25)

CPS.

```
let rec int sum(int n, (int->int) k) =
  if ==(n, 0) then (k 0)
  else (sum -(n, 1) proc(int a) (k +(n, a)))
in
  output((sum 100 proc(int x) x))
```

## The KUE-CHIP2-S Processor


We designed the KUE-CHIP2-S architecture which is a simple enhancement of a tiny microprocessor KUE-CHIP2 [2][3].
KUE-CHIP2-S has PUSH/POP and CALL/RET instructions that enable the architecture usable for many purposes.
The EOPL Compiler targets the KUE-CHIP2-S architecture.

Note that the current design of KUE-CHIP2-S is rather abstract.
For example, several parameters such as word size, the number of registers, and supported memory size, are not defined.

[2] H. Kanbara and H. Yasuura, KUE-CHIP2: A microprocessor for education of LSI design and computer hardware, Proc. Synthesis and System Integration of Mixed Technologies (SASIMI'95) pp.233-240, 1995.
[3] H. Kanbara, KUE-CHIP: A Microprocessor for education of Computer Architecture and LSI design, Proc. IEEE ASIC Seminar and Exhibit, 1990.

## The Compiler

### Current Status

The interpreter is implemented as defined in the EOPL Book [1].

The compiler is underconstruction.
There are lots of oppotunities for optimization.

### How to build

 - Extract the package. You will see all source files in the top directory.
Type `make` to build `rep_loop` and  `gen2`. They are an interpreter and a compiler of EOPL, respectively. They perform as filters.

    ```
    $ ./rep_loop < source.eopl
    ```
    ```
    $ ./gen2 < source.eopl > obj.asm
    ```


 - `vm.pl` is a KUE-CHIP2-S simulator written in Perl. 

    ```
    $ perl vm.pl < obj.asm
    ```

### Conventions
- Both a frame pointer and a stack pointer are used.
Local variables are referenced to by using a frame pointer.
Arguments are passed to another function via stack.
A returned value from a function is passed via a register which is designated in advance.
- All expressions are evaluated by using the stack:
for example, operands for a binary operation are pushed
when the values are obtaind and
poped into two registers just before the operation is performed.
No full-scale register allocation algorithm is used.
- Frame size is fixed.
- The registers used for a stack pointer or a frame pointer
are designated in the source code of the compiler.
When you want to change the settings, you should also 
change the assignments defined in the KUE-CHIP2-S simulator program.


### Example
```
$ cat sample.eopl
output ((proc (int x, int y) +(x, y) 3 4))
$ ./gen2 < sample.eopl > sample.asm
$ cat sample.asm
start:
* Initialize Heap Pointer
        LD      R28,    255
* Initialize Frame Pointer
        LD      R31,    65535
* Initialize Stack Pointer
        LD      R30,    R31
        SUB     R30,    32
main:
* Evaluate operator
* Proc
        LD      R25,    heap_alloc
        LD      R24,    2       ! size
        PUSH    R24
        CALLR   R25
        POP     R25     ! cleanup
        LD      R25,    func0
        ST      R25,    [R29+0]         ! store func ptr
        LD      R25,    0       ! null
        ST      R25,    [R29+1]         ! store env ptr
        LD      R26,    R29     ! closure ptr
        LD      R25,    [R26+1]         ! envptr
* Prepare arguments
        LD      R24,    3       ! literal 3
        PUSH    R24
        LD      R24,    4       ! literal 4
        PUSH    R24
        PUSH    R25     ! envptr as arg
        LD      R26,    [R26+0]         ! addr
        CALLR   R26
* Obtain returned value
        LD      R27,    R29     ! returned value
        POP     R26     ! cleanup
        POP     R26     ! cleanup
        POP     R26     ! cleanup
        PUSH    R27
        POP     R27
        OUT     R27     ! output to somewhere
        LD      R29,    0       ! prim output
        HLT
func0:
* Save FP
        PUSH    R31
* Adjust FP and SP
        LD      R31,    R30     ! modify FP
        SUB     R30,    32      ! adjust SP
* Global variables
* Function body
        LD      R25,    [R31+4]         ! variable x
        PUSH    R25
        LD      R25,    [R31+3]         ! variable y
        PUSH    R25
        POP     R25
        POP     R29
        ADD     R29,    R25     ! prim add
* Restore FP and SP
        LD      R30,    R31
        POP     R31
        RET
heap_alloc:
* Save FP
        PUSH    R31
* Adjust FP and SP
        LD      R31,    R30     ! modify FP
        SUB     R30,    32      ! adjust SP
        LD      R29,    R28     ! returned ptr
        ADD     R28,    [R31+2]         ! size
* Restore FP and SP
        LD      R30,    R31
        POP     R31
        RET
        END
$
```

### Implementation

The interpreter and the compiler is written in OCaml.

The simulator of KUE-CHIP2-S is written in Perl.

