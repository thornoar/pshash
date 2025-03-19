#include "mini-gmp.h"

#define MAXSIZE_SMALL 128
#define MAXSIZE_BIG 2048

const char sourceLower[27] = "ckapzfitqdxnwehrolmbyvsujg";
const char sourceUpper[27] = "RQLIANBKJYVWPTEMCZSFDOGUHX";
const char sourceSpecial[13] = "=!*@?$%#&-+^";
const char sourceNumbers[11] = "1952074386";

struct source {
    char* elts;
    unsigned long amount;
};

struct configuration {
    unsigned long size;
    struct source* srcs;
};

// void print (mpz_t x);
void mpz_rel_fac_ui (mpz_t, unsigned long, unsigned long);
void mpz_cnk_ui (mpz_t, unsigned long, unsigned long);
void choose_ordered (char*, const struct source, mpz_t);
void multi_choose_ordered (char**, const struct source*, unsigned long, mpz_t);
void shuffleList (char*, const char*, mpz_t);
void merge_two_lists (char*, const char*, const char*, mpz_t);
void merge_lists (char*, char**, unsigned long, mpz_t);
void get_hash (char*, const struct configuration*, mpz_t, mpz_t);
void parse_key (mpz_t, const char*);
void get_public_key (mpz_t, const char*);
