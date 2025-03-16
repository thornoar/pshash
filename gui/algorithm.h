#include "mini-gmp.h"

#define MAXSIZE_SMALL 128
#define MAXSIZE_BIG 2048

const char sourceLower[27] = "ckapzfitqdxnwehrolmbyvsujg";
const char sourceUpper[27] = "RQLIANBKJYVWPTEMCZSFDOGUHX";
const char sourceSpecial[13] = "=!*@?$%#&-+^";
const char sourceNumbers[11] = "1952074386";

struct source {
    const char* elts;
    const unsigned long amount;
};

const struct source defaultConfiguration[] = { {sourceLower, 8}, {sourceUpper, 8}, {sourceSpecial, 5}, {sourceNumbers, 4} };
const struct source mediumConfiguration[] = { {sourceLower, 5}, {sourceUpper, 5}, {sourceSpecial, 5}, {sourceNumbers, 5} };
const struct source shortConfiguration[] = { {sourceLower, 4}, {sourceUpper, 4}, {sourceSpecial, 4}, {sourceNumbers, 4} };
const struct source anlongConfiguration[] = { {sourceLower, 7}, {sourceUpper, 7}, {sourceNumbers, 7} };
const struct source anshortConfiguration[] = { {sourceLower, 4}, {sourceUpper, 4}, {sourceNumbers, 4} };
const struct source pinCodeConfiguration[] = { {sourceNumbers, 4} };
const struct source mediumPinCodeConfiguration[] = { {sourceNumbers, 6} };
const struct source longPinCodeConfiguration[] = { {sourceNumbers, 8} };

void mpz_rel_fac_ui (mpz_t, unsigned long, unsigned long);
void mpz_cnk_ui (mpz_t, unsigned long, unsigned long);
void choose_ordered (char*, const struct source, mpz_t);
void multi_choose_ordered (char**, const struct source*, unsigned long, mpz_t);
void shuffleList (char*, const char*, mpz_t);
void merge_two_lists (char*, const char*, const char*, mpz_t);
void merge_lists (char*, char**, unsigned long, mpz_t);
void get_hash (char*, const struct source*, unsigned long, mpz_t, mpz_t);
