#include <assert.h>
#include <gc.h>

#include "error.h"
#include "symbol.h"
#include "util.h"
#include "common.h"

static 
int 
_it_bin_pred(const type* list,
            const type* first,
            int (pred) (const type* left, const type* right))
{
    int result;

    if (is_nil(list))
    {
        result = TRUE;
    }
    else if (pred(first, car(list)))
    {
        result = _it_bin_pred(cdr(list), car(list), pred);
    }
    else
    {
        result = FALSE;
    }

    return result;
}

type* 
it_bin_pred(const type* list,
            int (pred) (const type* left, const type* right))
{
    type*  result = nil();

    if (is_nil(list))
    {
        result = mk_boolean(TRUE);
    }
    else 
    {
        result = mk_boolean(_it_bin_pred(cdr(list), car(list), pred));
    }

    return result;
}

type* 
fold_right(type* (* f) (const type* left, const type* right), 
           const type* list, 
           type* identity)
{
    type* result = nil(); 

    if (is_nil(list))
    {
        result = identity;
    }
    else
    {
        result = fold_right(f, cdr(list), f(identity, car(list)));
    }
    
    return result;
}

type* 
map1(type* (*f) (const type* arg), type* list)
{
    type* result = nil();

    if (!is_nil(list))
    {
        result = cons(f(car(list)), map1(f, cdr(list)));
    }

    return result;
}

static type* 
_reverse_(const type* list, const type* result)
{
    if (is_nil(list))
    {
        return (type*) result;
    }
    else
    {
        return _reverse_(cdr(list), cons(car(list), result));
    }
}

type* 
reverse(const type* list)
{
    return _reverse_(list, nil());
}

type* 
append(const type* left, const type* right)
{
    if (is_nil(left)) 
    {
        return (type*) right;
    }
    else
    { 
        return cons(car(left), append(cdr(left), right));
    }
}

type*
_unzip_(const type* list, const type* left, const type* right)
{
    if (is_nil(list))
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

type* 
unzip(const type* list)
{
    return _unzip_(list, nil(), nil());
}

type* 
assq(const type* obj, const type* list)
{
    type* result = nil();

    if (!is_true(is_list(list)))
    {
        throw_error(CONSTRAINT_ERROR, "INTERNAL ASSQ: list is not a list");
    }

    if (!is_nil(list))
    {
        type* a = car(list);
        
        if (!is_true(is_list(a)))
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
