#include <stdio.h>

extern int _the_asm_code() asm("_the_asm_code");

int main(int argc, char** argv) {
  int result = _the_asm_code();
  printf("%d\n", result);
  return 0;
}
