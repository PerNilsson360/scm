#include <string.h>
#include <assert.h>

#include "common.h"
#include "error.h"
#include "number.h"
#include "symbol.h"
#include "type.h"
#include "env.h"
#include "env_ut.h"

void 
tst_env_lookup_variable_value()
{
    
}

void 
tst_env_extend_environment()
{
    const char* s1 = "v1";
    const char* s2 = "v2";

    type* var1 = mk_symbol(s1);
    type* var2 = mk_symbol(s2);
    type* vars = cons(var1, var2);

    type* val1 = mk_number_from_int(1);
    type* val2 = mk_number_from_int(2);
    type* val3 = mk_number_from_int(3);

    type* var2val = cons(val2, cons(val3, nil())); 
    type* vals = cons(val1, var2val);
    type* env = extend_environment(vars, vals, mk_env(nil()));
    
    assert(is_eq(lookup_variable_value(var1, env), val1));
    assert(is_eq(lookup_variable_value(var2, env), var2val));

    env = extend_environment(vars, cons(val1, nil()), mk_env(nil()));

    assert(is_eq(lookup_variable_value(var1, env), val1));
    assert(is_eq(lookup_variable_value(var2, env), nil()));
}

void 
tst_env_set_variable_value()
{
    type* env = extend_environment(nil(), nil(), mk_env(nil()));
    const char* s = "v1";
    
    type* var = mk_symbol(s);
    type* val1 = mk_number_from_int(100);
    type* val2 = mk_number_from_int(200);

    define_variable(var, val1, env);
    set_variable_value(var, val2, env);
    
    assert(is_eq(lookup_variable_value(var, env), val2));
}

static void
_tst_lookup_defined_varaible()
{
    type* env = extend_environment(nil(), nil(), mk_env(nil()));
    const char* s = "v1";
    
    type* var = mk_symbol(s);
    type* val = mk_number_from_int(100);
    
    define_variable(var, val, env);
    
    assert(is_eq(lookup_variable_value(var, env), val));
}

static void
_tst_lookup_undefined_variable()
{
    int success = FALSE;

    /* lookup throws exception when it fails */
    switch(setjmp(__c_env__))
    {
    case NO_ERROR:
    case PARSE_ERROR:
    case EVAL_ERROR:
    case APPLY_ERROR:
        success = TRUE;
        break;
    default:
        assert(FALSE && "CATCH_ERROR: not an implemented error");
    }

    if (!success)
    {
        type* env = extend_environment(nil(), nil(), mk_env(nil()));
        
        const char* s = "v1";
        
        type* var = mk_symbol(s);
        
        type* result = lookup_variable_value(var, env);
    }
}

void 
tst_env_define_variable()
{
    _tst_lookup_defined_varaible();
    _tst_lookup_undefined_variable();
}

