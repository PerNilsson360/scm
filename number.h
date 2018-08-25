#ifndef _NUMBER_H_
#define _NUMBER_H_

#include "type.h"

type* mk_number_from_int(int n);
type* mk_number(const char* symbol, unsigned int length, int positive);
type* mk_hex_number(const char* symbol, unsigned int length);
int is_number(const type* number);
int is_number_equal(const type* left, const type* right);
int is_number_lt(const type* left, const type* right);
int is_number_gt(const type* left, const type* right);
int is_number_lt_eq(const type* left, const type* right);
int is_number_gt_eq(const type* left, const type* right);
type* is_number_zero(const type* left);
type* is_number_positive(const type* left);
type* is_number_negative(const type* left);
type* is_number_odd(const type* left);
type* is_number_even(const type* left);
type* max_number(const type* left, const type* right);
type* min_number(const type* left, const type* right);
type* add_number(const type* left, const type* right);
type* mul_number(const type* left, const type* right);
type* sub_numbers(const type* numbers);
type* div_numbers(const type* numbers);
type* remainder_number(const type* left, const type* right);
type* number_to_string(const type* n);

unsigned int number_hash(const type* number);
#endif
