#ifndef _NUMBER_H_
#define _NUMBER_H_

#include "type.h"

TYPE* mk_number_from_int(int n);
TYPE* mk_number(const char* symbol, unsigned int length, int positive);
TYPE* mk_hex_number(const char* symbol, unsigned int length);
int is_number(const TYPE* number);
int is_number_equal(const TYPE* left, const TYPE* right);
int is_number_lt(const TYPE* left, const TYPE* right);
int is_number_gt(const TYPE* left, const TYPE* right);
int is_number_lt_eq(const TYPE* left, const TYPE* right);
int is_number_gt_eq(const TYPE* left, const TYPE* right);
TYPE* is_number_zero(const TYPE* left);
TYPE* is_number_positive(const TYPE* left);
TYPE* is_number_negative(const TYPE* left);
TYPE* is_number_odd(const TYPE* left);
TYPE* is_number_even(const TYPE* left);
TYPE* max_number(const TYPE* left, const TYPE* right);
TYPE* min_number(const TYPE* left, const TYPE* right);
TYPE* add_number(const TYPE* left, const TYPE* right);
TYPE* mul_number(const TYPE* left, const TYPE* right);
TYPE* sub_numbers(const TYPE* numbers);
TYPE* div_numbers(const TYPE* numbers);
TYPE* remainder_number(const TYPE* left, const TYPE* right);
TYPE* number_to_string(const TYPE* n);

unsigned int number_hash(const TYPE* number);
#endif
