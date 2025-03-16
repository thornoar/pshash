#include "algorithm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print (mpz_t x) {
    char x_str[MAXSIZE_BIG];
    mpz_get_str(x_str, 10, x);
    printf("%s\n", x_str);
}

// Tested
unsigned long sum (const unsigned long arr[], const unsigned long size) {
    unsigned long sum = 0;
    for (int i = 0; i < size; i++) { sum += arr[i]; }
    return sum;
}

// Tested
void mpz_fac_prod (mpz_t to, const unsigned long arr[], const unsigned long size) {
    mpz_set_ui(to, 1);
    mpz_t curmul; mpz_init(curmul);
    for (int i = 0; i < size; i++) {
        mpz_fac_ui(curmul, arr[i]);
        mpz_mul(to, to, curmul);
    }
    mpz_clear(curmul);
}

// Tested
unsigned long shift (char* str) {
    unsigned long sum = 0;
    while (*str != '\0') {
        sum += *str;
        str++;
    }
    return sum;
}

// Tested
unsigned long shift2 (char** strs, const unsigned long size) {
    unsigned long sum = 0;
    for (int i = 0; i < size; i++) {
        sum += shift(strs[i]);
    }
    return sum;
}

// Tested
void exclude (char* arr) {
    while (*arr != '\0') { *arr = *(arr+1); arr++; }
}

// Tested
void mpz_rel_fac_ui (mpz_t to, unsigned long n, unsigned long m) {
    mpz_set_ui(to, 1);
    for (unsigned long i = 0; i < m; i++) {
        mpz_mul_ui(to, to, n-i);
    }
}

// Tested
void mpz_cnk_ui (mpz_t to, unsigned long n, unsigned long k) {
    mpz_t a, b;
    mpz_init(a); mpz_init(b);
    mpz_rel_fac_ui(a, n, k);
    mpz_fac_ui(b, k);
    mpz_divexact(to, a, b);
    mpz_clear(a); mpz_clear(b);
}

// Tested
void choose_ordered_spr (mpz_t to, unsigned long n, unsigned long m) {
    mpz_rel_fac_ui(to, n, m);
}

// Tested
void choose_ordered_spr_p (mpz_t to, const struct source src) {
    choose_ordered_spr(to, strlen(src.elts), src.amount);
}

// Tested
void choose_ordered (char* to, const struct source src, mpz_t key) {
    int i = 0;
    char elts_mut[MAXSIZE_SMALL] = {};
    strcpy(elts_mut, src.elts);
    unsigned int amount_mut = src.amount;
    unsigned long rem = 0;
    unsigned long len = strlen(elts_mut);
    while (amount_mut > 0) {
        rem = mpz_fdiv_q_ui(key, key, len);
        mpz_add_ui(key, key, elts_mut[rem]);
        to[i] = elts_mut[rem];
        exclude(elts_mut + rem);
        len -= 1;
        amount_mut -= 1;
        i++;
    }
    to[i] = '\0';
}

// Tested
void multi_choose_ordered (char** to, const struct source* srcs, unsigned long size, mpz_t key) {
    if (size == 0) { return; }
    mpz_t key_mod; mpz_init(key_mod);
    mpz_t spr; mpz_init(spr);
    choose_ordered_spr_p(spr, srcs[0]);
    mpz_fdiv_qr(key, key_mod, key, spr);
    choose_ordered(to[0], srcs[0], key_mod);
    mpz_add_ui(key, key, shift(to[0]));
    mpz_clear(key_mod); mpz_clear(spr);
    multi_choose_ordered(to+1, srcs+1, size-1, key);
}

// Tested
void shuffle_list (char* to, const char* lst, mpz_t key) {
    const struct source src = { lst, strlen(lst) };
    choose_ordered(to, src, key);
}

// Tested
void merge_two_lists_spr (mpz_t to, unsigned long n, unsigned long m) {
    mpz_t a; mpz_init(a); mpz_t b; mpz_init(b); mpz_t c; mpz_init(c);
    mpz_fac_ui(a, n + m);
    mpz_fac_ui(b, n);
    mpz_fac_ui(c, m);
    mpz_mul(b, b, c);
    mpz_divexact(to, a, b);
    mpz_clear(a); mpz_clear(b); mpz_clear(c);
}

// Tested
void merge_two_lists_spr_p (mpz_t to, const char* lst1, const char* lst2) {
    merge_two_lists_spr(to, strlen(lst1), strlen(lst2));
}

// Tested
void merge_two_lists (char* to, const char* lst1, const char* lst2, mpz_t key) {
    unsigned long len1 = strlen(lst1), len2 = strlen(lst2);
    int i = 0;
    mpz_t spr1; mpz_init(spr1);
    mpz_t spr2; mpz_init(spr2);
    mpz_t sum; mpz_init(sum);
    while (len1 + len2 > 0) {
        if (len1 == 0) { strcpy(to+i, lst2); return; }
        if (len2 == 0) { strcpy(to+i, lst1); return; }
        merge_two_lists_spr(spr1, len1 - 1, len2);
        merge_two_lists_spr(spr2, len1, len2 - 1);
        mpz_add(sum, spr1, spr2);
        mpz_mod(key, key, sum);
        if (mpz_cmp(key, spr1) < 0) {
            mpz_add_ui(key, key, *lst1);
            to[i] = *lst1;
            lst1 += 1;
            len1 -= 1;
        } else {
            mpz_sub(key, key, spr1);
            mpz_add_ui(key, key, *lst2);
            to[i] = *lst2;
            lst2 += 1;
            len2 -= 1;
        }
        i += 1;
    }
    to[i] = '\0';
    mpz_clear(spr1); mpz_clear(spr2); mpz_clear(sum);
}

// Tested
void merge_lists_spr (mpz_t to, unsigned long arr[], unsigned long size) {
    unsigned long cursum = sum(arr, size);
    mpz_t a; mpz_init(a);
    mpz_fac_ui(a, cursum);
    mpz_t b; mpz_init(b);
    mpz_fac_prod(b, arr, size);
    mpz_divexact(to, a, b);
    mpz_clear(a); mpz_clear(b);
}

// Tested
void merge_lists_spr_p (mpz_t to, char** lsts, const unsigned long size) {
    unsigned long arr[MAXSIZE_SMALL] = {};
    for (unsigned long i = 0; i < size; i++) { arr[i] = strlen(lsts[i]); }
    merge_lists_spr(to, arr, size);
}

// Tested
void merge_lists (char* to, char** lsts, unsigned long size, mpz_t key) {
    if (size == 0) { *to = '\0'; return; }
    if (size == 1) { strcpy(to, lsts[0]); return; }
    if (size == 2) { merge_two_lists(to, lsts[0], lsts[1], key); return; }
    mpz_t spr; mpz_init(spr);
    merge_lists_spr_p(spr, lsts+1, size-1);
    mpz_t key_mod; mpz_init(key_mod);
    mpz_fdiv_qr(key, key_mod, key, spr);
    mpz_add_ui(key, key, shift(*lsts));
    char to2[MAXSIZE_SMALL];
    merge_lists(to2, lsts+1, size-1, key_mod);
    merge_two_lists(to, lsts[0], to2, key);
    mpz_clear(spr); mpz_clear(key_mod);
}

// Tested
void choose_and_merge (char* to, const struct source* srcs, unsigned long size, mpz_t key) {
    char* selections[size];
    for (int i = 0; i < size; i++) {
        selections[i] = (char*) malloc(sizeof(char) * MAXSIZE_SMALL);
    }
    mpz_t spr; mpz_init(spr); mpz_set_ui(spr, 1);
    mpz_t curprod; mpz_init(curprod);
    for (int i = 0; i < size; i++) {
        mpz_rel_fac_ui(curprod, strlen(srcs[0].elts), srcs[0].amount);
        mpz_mul(spr, spr, curprod);
    }
    mpz_t key_mod; mpz_init(key_mod);
    mpz_fdiv_qr(key, key_mod, key, spr);
    multi_choose_ordered(selections, srcs, size, key_mod);
    mpz_add_ui(key, key, shift2(selections, size));
    merge_lists(to, selections, size, key);
    for (int i = 0; i < size; i++) {
        free(selections[i]);
    }
    mpz_clear(spr); mpz_clear(curprod); mpz_clear(key_mod);
}

// Tested
void get_hash (char* to, const struct source* srcs, unsigned long size, mpz_t key1, mpz_t key2) {
    char temp[MAXSIZE_SMALL] = {};
    choose_and_merge(temp, srcs, size, key1);
    shuffle_list(to, temp, key2);
}
