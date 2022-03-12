# Klaxon

Klaxon is a minimalist language designed to be minimal, modular and simple to implement.

![c++](https://img.shields.io/badge/c%2B%2B-%3E%3D17-blue.svg?style=flat)
[![license](https://img.shields.io/github/license/Jackojc/klaxon.svg?style=flat)](./LICENSE)
![code size](https://img.shields.io/github/languages/code-size/Jackojc/klaxon?style=flat-square)
[![issues](https://img.shields.io/github/issues/Jackojc/klaxon.svg?style=flat)](https://github.com/Jackojc/klaxon/issues)
[![discord](https://img.shields.io/discord/537732103765229590.svg?label=discord&style=flat)](https://discord.gg/H2qKkSd9gC)

### Philosophy & Goals
- Bare minimum of features needed to perform useful computation
- Features that compose [^1]
- Do not appeal to niche use cases [^2]
- Minimise configurability [^3]
- Modular pipeline built on top of Unix pipes with a standard IR format

[^1]: Features should not be very specific, they should compose well with other
features and give rise to emergent features.
[^2]: Klaxon is not designed to appeal to everyone's own particular needs, it is
designed to do as much as possible with the smallest surface area. This means your
feature request will most likely not be considered.
[^3]: Every flag that you introduce into a compiler in order to influence its
behaviour results in a combinatorial explosion that makes it virtually impossible
to properly bug test all possible paths of execution. Having one right way to
do something means that the compiler should (in theory) have less broken edge cases.

### Features:
- Register Allocation
- Procedures
- Macros
- Compiles to x86-64 assembly

### Semantics:
- Word Size Values
- UTF-8 Source Encoding
- Two's Complement Signed Arithmetic

### License
This project uses the MPL-2.0 license. (check [LICENSE](LICENSE))

### Progress & Discussion
You can join the discord server in order to follow progress and/or contribute to discussion of the project. (https://discord.gg/H2qKkSd9gC)

### Resources:
- https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/
- https://www.nasm.us/xdoc/2.13.03/html/nasmdoc0.html
- https://filippo.io/linux-syscall-table/
- https://www.felixcloutier.com/x86/
- https://www.youtube.com/watch?v=-ti07Z0xKKg
- https://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
- https://www3.nd.edu/~dthain/compilerbook/compilerbook.pdf
- https://min-lang.org/
- http://sovietov.com/app/forthwiz.html
- https://mcyoung.xyz/2021/06/01/linker-script/
- https://leahneukirchen.org/blog/archive/2020/04/brute-forthing-minimal-programs-for-stack-arrangements.html
- https://users.ece.cmu.edu/~koopman/stack_computers/appb.html
- https://courses.cs.washington.edu/courses/cse501/15sp/papers/massalin.pdf
- https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/
- https://uica.uops.info/
- https://users.ece.cmu.edu/~koopman/stack_compiler/stack_co.html
