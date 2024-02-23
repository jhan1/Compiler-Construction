#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t* start_of_stack;
extern uint64_t* end_of_stack;
extern uint64_t* start_of_heap;
extern uint64_t* end_of_heap;
extern uint64_t* heap_cursor;

uint64_t is_pointer(uint64_t k) {
  return ((k&3)==1) && (k >= start_of_heap) && ((k-1) < end_of_heap);
}

uint64_t is_closure(uint64_t k) {
  return k>>63;
}

uint64_t get_object_size(uint64_t* k) {
  uint64_t val = *k;
  if (is_closure(val)) {
    val <<= 1;
    val >>= 1;
    return ((val+4) );
  } else  {
    return ((val+2) );
  }
}

uint64_t is_reachable(uint64_t* k) {
  return *(k+1);
}
/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
#define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__) ;  fflush(stdout)

// This macro enables all debugf statements.  (They become printf statements.)
// #define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__); fflush(stdout)


/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap() {
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t* p = (uint64_t*)((uint64_t)(end_of_heap - 100) & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1) {
    if (c==0) {
      debugf("%016" PRIx64 ":", p);
    }
    if (p >= start_of_heap) {
      debugf("    %016"PRIx64, *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==4) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}

void dump_stack() {
  debugf("STACK:\n");
  int c = 0;
  for (uint64_t* p = (uint64_t*)((uint64_t)end_of_stack & 0xFFFFFFFFFFFFFFFC);
       p < start_of_stack; p += 1) {
    if (c==0) {
      debugf("%016" PRIx64 ":", p);
    }
    if (p >= start_of_heap) {
      debugf("    %016"PRIx64, *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==4) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}

void dfs_mark(uint64_t *ptr) {;
  if (*(ptr+1)==1) {
    return;
  }
  *(ptr+1)=1;
  uint64_t size_ = *ptr;
  if (is_closure(size_)) {
    // If is closure
    size_ <<= 1;
    size_ >>= 1;
    for ( uint64_t i = 0 ; i < size_ ; i ++ ) {
      uint64_t val = *(ptr + 4 + i);
      
      if (is_pointer(val) ) {
        uint64_t addr = val-1;
        dfs_mark( (uint64_t*)addr );
      }
    } 
  } else {
    // If is tuple
    for ( uint64_t i = 0 ; i < size_ ; i ++ ) {
      uint64_t val = *(ptr + 2 + i);
      if (is_pointer(val) ) {
        uint64_t addr = val-1;
        dfs_mark( (uint64_t*)addr );
      }
    } 
  }
}

void gc(uint64_t desired_free) {

  // printf("called gc\n");
  // dump_heap()
  // Step 1: dfs mark
  for ( uint64_t i = end_of_stack ; i <= start_of_stack ; i += 8 ) {
    uint64_t val = *((uint64_t*)i);
    if ( is_pointer(val) ) {
      uint64_t addr = val - 1;
      dfs_mark( (uint64_t*)addr );
    }
  }

  // Step 2: forward
  uint64_t* next_heap_object = start_of_heap;
  uint64_t* next_live_destination = start_of_heap;
  while (next_heap_object < heap_cursor) {
    uint64_t object_size = get_object_size(next_heap_object);
    if (is_reachable(next_heap_object)) {
      *(next_heap_object+1) = (uint64_t)next_live_destination;
      next_live_destination += object_size;
    } 
    next_heap_object += object_size;
  }

  // Step 3: update
  for ( uint64_t i = end_of_stack ; i <= start_of_stack ; i += 8 ) {
    uint64_t val = *((uint64_t*)i);
    if ( is_pointer(val) ) {
      uint64_t addr = val -1;
      uint64_t new_addr = *(((uint64_t*)addr)+1);
      *((uint64_t*)i) = new_addr + 1;
    }
  }

  uint64_t* current_heap_object = start_of_heap;
  while ( current_heap_object < heap_cursor ) {
    uint64_t object_size = get_object_size(current_heap_object);
    if (!is_reachable(current_heap_object)) {
      current_heap_object += object_size;
      continue;
    }
    if ( is_closure(*current_heap_object) ) {
      for ( uint64_t i = 4 ; i < object_size ; i ++ ) {
        uint64_t val = *(i + current_heap_object);
        if ( is_pointer(val) ) {
          uint64_t addr = val -1;
          uint64_t new_addr = *(((uint64_t*)addr)+1);
          *(i + current_heap_object) = new_addr + 1;
        }        
      }
    } else {
      for ( uint64_t i = 2 ; i < object_size ; i ++ ) {
        uint64_t val = *(i + current_heap_object);
        if ( is_pointer(val) ) {
          uint64_t addr = val -1;
          uint64_t new_addr = *(((uint64_t*)addr)+1); 
          *(i + current_heap_object) = new_addr + 1;
        }        
      }
    }
    current_heap_object += object_size;
  }

  // Step 4 & 5: Compact & Unmark
  current_heap_object = start_of_heap;
  while ( current_heap_object < heap_cursor ) {
    uint64_t object_size = get_object_size(current_heap_object);
    if (is_reachable(current_heap_object) ) {
      uint64_t dst_ptr = *(current_heap_object+1) ;
      // Unmark
      *(current_heap_object+1) = 0;
      // Compact
      memmove((uint64_t*)dst_ptr, current_heap_object, sizeof(uint64_t)*(object_size) );
    } 
    current_heap_object += object_size;
  }

  heap_cursor = next_live_destination;
  
  // uint64_t freed_memory = (uint64_t)heap_cursor - (uint64_t)next_live_destination;
  uint64_t free_memory = (uint64_t)end_of_heap - (uint64_t)heap_cursor;
  memset(heap_cursor, 0, free_memory);
  // debugf("%016" PRIx64 "\n", free_memory);

  if ( free_memory < desired_free ) {
    stopWithError(7);
  }
  // heap_cursor = next_live_destination;
  
  
}
