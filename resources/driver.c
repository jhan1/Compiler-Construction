#include <stdio.h>
#include <stdint.h>
#include "printer.h"
#include <inttypes.h>
#include <stdlib.h>

int64_t bird_main(int64_t *ptr, int64_t heap_size) asm("bird_main");

int main(int argc, char** argv) {
  const int heap_size = 1148576 * sizeof(int64_t);
  int64_t* ptr = malloc(heap_size);
  // int64_t* ptr = malloc(heap_size);

  int64_t result = bird_main(ptr, heap_size);
  // printf("%"PRId64"\n", result);
  printValue(result);
  return 0;
}
