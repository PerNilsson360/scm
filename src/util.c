// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
#include <assert.h>
#include <gc.h>

#include "error.h"
#include "symbol.h"
#include "util.h"
#include "common.h"

TYPE* 
it_bin_pred(const TYPE* list,
            int (pred) (const TYPE* left, const TYPE* right))
{
    
    int result = TRUE;
    if (IS_NIL(list)) {
	return mk_boolean(TRUE);
    }
    const TYPE* first = car(list);
    while (result) {
	list = cdr(list);
	if (IS_NIL(list)) {
	    break;
	}
	const TYPE* second = car(list);
	result = pred(first, second);
	first = second;
    }
    return mk_boolean(result);
}

TYPE* 
fold_right(TYPE* (* f) (const TYPE* left, const TYPE* right), 
           const TYPE* list, 
           const TYPE* identity)
{
    TYPE* result = nil(); 

    if (IS_NIL(list))
    {
        result = (TYPE*)identity;
    }
    else
    {
        result = fold_right(f, cdr(list), f(identity, car(list)));
    }
    
    return result;
}

TYPE* 
map1(TYPE* (*f) (const TYPE* arg), TYPE* list)
{
    TYPE* result = nil();

    if (!IS_NIL(list))
    {
        result = cons(f(car(list)), map1(f, cdr(list)));
    }

    return result;
}

static TYPE* 
_reverse_(const TYPE* list, const TYPE* result)
{
    if (IS_NIL(list))
    {
        return (TYPE*) result;
    }
    else
    {
        return _reverse_(cdr(list), cons(car(list), result));
    }
}

TYPE* 
reverse(const TYPE* list)
{
    if (IS_NIL(list) || IS_NIL(cdr(list)))
    {
        return (TYPE*) list;
    }
    else
    {
        return _reverse_(list, nil());
    }
}

TYPE* 
append(const TYPE* left, const TYPE* right)
{
    if (IS_NIL(left)) 
    {
        return (TYPE*) right;
    }
    else
    { 
        return cons(car(left), append(cdr(left), right));
    }
}

TYPE*
_unzip_(const TYPE* list, const TYPE* left, const TYPE* right)
{
    if (IS_NIL(list))
    {
        return cons(reverse(left), reverse(right));
    }
    else
    {
        return _unzip_(cdr(list), 
                       cons(car(car( list)), left),
                       cons(car(cdr(car( list))), right));
    }
}

TYPE* 
unzip(const TYPE* list)
{
    return _unzip_(list, nil(), nil());
}

TYPE* 
assq(const TYPE* obj, const TYPE* list)
{
    TYPE* result = nil();

    if (!is_list(list))
    {
        throw_error(CONSTRAINT_ERROR, "INTERNAL ASSQ: list is not a list");
    }

    if (!IS_NIL(list))
    {
        TYPE* a = car(list);
        
        if (!is_list(a))
        {
            throw_error(CONSTRAINT_ERROR, "INTERNAL ASSQ: elements are not pairs");
        }
        
        if (is_eq(obj, car(a)))
        {
            result = a;
        }
        else
        {
            result = assq(obj, cdr(list));
        }
    }
    
    return result;
}

void
nstrcpy(char* s1, const char* s2, unsigned int length)
{
    unsigned int i;

    for (i = 0; i < length; i++) 
        s1[i] = s2[i];

    s1[length] = '\0';
}

void*
mloc(size_t n)
{
    return GC_MALLOC(n);
}
