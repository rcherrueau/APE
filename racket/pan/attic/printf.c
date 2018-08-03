#include <stdio.h>
// gcc -m32 -masm=intel -o - -S -fno-asynchronous-unwind-tables printf.c
// Check 32 bit, with `file a.out`
int main() {
   return printf("%d\n", 1);
}
