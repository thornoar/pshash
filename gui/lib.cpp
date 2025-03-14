#include <cstdlib>
#include "HsFFI.h"

void pshash_api_init () {
    int argc = 2;
    const char *argv[] = { "+RTS", "-A32m", NULL };
    // argv[0] = "+RTS";
    char **pargv = (char**) argv;

    // Initialize Haskell runtime
    hs_init(&argc, &pargv);

    // do any other initialization here and
    // return false if there was a problem
    // return HS_BOOL_TRUE;
}

void pshash_api_exit () {
    hs_exit();
}

int get_hash (int key) {
    return key+10;
}
