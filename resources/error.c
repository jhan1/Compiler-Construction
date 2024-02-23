#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Expected an int.\n");
      break;
    /* TODO: put your other error cases here */
    case 2:
       printf("Expected a boolean.\n");
      break;     
    case 3:
       printf("Expected a tuple.\n");
      break;     
    case 4:
       printf("Invalid index.\n");
      break;     
    case 5:
       printf("Expected a closure.\n");
      break;     
    case 6:
       printf("Divided by zero.\n");
      break;     
    case 7:
       printf("Out of memory.\n");
      break;     
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
