
// This file implements a custom space in C, for use by the check_space test.
//

#ifndef C_SPACE_H
#define C_SPACE_H

#include <hyperon/hyperon.h>

typedef struct _custom_space_buf custom_space_t;

// Creates a new space using logic implemented in C
space_t custom_space_new();

typedef struct _custom_space_buf {
    struct _atom_list_item* atoms;
    size_t atom_count;
} custom_space_buf;

#endif /* C_SPACE_H */
