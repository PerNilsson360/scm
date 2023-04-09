#include <assert.h>
#include <gc.h>

#include "error.h"
#include "symbol.h"
#include "util.h"
#include "common.h"

static 
int 
_it_bin_pred(const TYPE* list,
            const TYPE* first,
            int (pred) (const TYPE* left, const TYPE* right))
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

TYPE* 
it_bin_pred(const TYPE* list,
            int (pred) (const TYPE* left, const TYPE* right))
{
    TYPE*  result = nil();

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

TYPE* 
fold_right(TYPE* (* f) (const TYPE* left, const TYPE* right), 
           const TYPE* list, 
           TYPE* identity)
{
    TYPE* result = nil(); 

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

TYPE* 
map1(TYPE* (*f) (const TYPE* arg), TYPE* list)
{
    TYPE* result = nil();

    if (!is_nil(list))
    {
        result = cons(f(car(list)), map1(f, cdr(list)));
    }

    return result;
}

static TYPE* 
_reverse_(const TYPE* list, const TYPE* result)
{
    if (is_nil(list))
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
    return _reverse_(list, nil());
}

TYPE* 
append(const TYPE* left, const TYPE* right)
{
    if (is_nil(left)) 
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

    if (!is_nil(list))
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
