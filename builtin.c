#include "object.h"
#include "builtin.h"
#include <math.h>

/* debug */
opt builtin__PER_abort(vm_context *vm, unsigned argc, const opt *argv)
{
    fprintf(stderr, "%%abort\n");
    exit(1);
}

opt builtin__PER_prn(vm_context *vm, unsigned argc, const opt *argv)
{
    for (unsigned i = 0; i < argc; ++i) {
        write_sexpr(stdout, arg(i));
        fputc(' ', stdout);
    }
    fputc('\n', stdout);
    return OPT_FALSE;
}

/* 6.1 */

opt builtin_eq_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return arg(0) == arg(1) ? OPT_TRUE : OPT_FALSE;
}

/* 6.2 */

opt builtin_fixnum_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_fixnum(arg(0)) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_flonum_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_flonum(arg(0)) ? OPT_TRUE : OPT_FALSE;
}

#define OP_COMPARE(name, op)                                              \
opt name ## _flonum(double x, unsigned i, unsigned argc, const opt *argv) \
{                                                                         \
    for ( ; i < argc; ++ i) {                                             \
        opt p = arg(i);                                                   \
        double y;                                                         \
        if (is_fixnum(p)) {                                               \
            y = fixnum_value(p);                                          \
        } else if (is_flonum(p)) {                                        \
            y = flonum_value(p);                                          \
        } else {                                                          \
            error("not number");                                          \
            return OPT_FALSE;                                             \
        }                                                                 \
        if (x op y) {                                                     \
        } else {                                                          \
            return OPT_FALSE;                                             \
        }                                                                 \
    }                                                                     \
    return OPT_TRUE;                                                      \
}                                                                         \
                                                                          \
opt name ## _fixnum(int_t k, unsigned i, unsigned argc, const opt *argv)  \
{                                                                         \
    for ( ; i < argc; ++i) {                                              \
        opt p = arg(i);                                                   \
        if (is_fixnum(p)) {                                               \
            if (k op fixnum_value(p)) {                                   \
            } else {                                                      \
                return OPT_FALSE;                                         \
            }                                                             \
        } else if (is_flonum(p)) {                                        \
            name ## _flonum(k, i, argc, argv);                            \
        } else {                                                          \
            error("not number");                                          \
            return OPT_FALSE;                                             \
        }                                                                 \
    }                                                                     \
    return OPT_TRUE;                                                      \
}

OP_COMPARE(LESS,<)
OP_COMPARE(GREATER,>)
OP_COMPARE(LE,<=)
OP_COMPARE(GE,>=)

opt builtin__LESS_(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0))) {
        return LESS_fixnum(fixnum_value(arg(0)), 1, argc, argv);
    } else if (is_flonum(arg(0))) {
        return LESS_fixnum(flonum_value(arg(0)), 1, argc, argv);
    } else {
        error("not number");
        return OPT_FALSE;
    }
}

opt builtin__GRE_(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0))) {
        return GREATER_fixnum(fixnum_value(arg(0)), 1, argc, argv);
    } else if (is_flonum(arg(0))) {
        return GREATER_fixnum(flonum_value(arg(0)), 1, argc, argv);
    } else {
        error("not number");
        return OPT_FALSE;
    }
}

opt builtin__LESS__EQ_(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0))) {
        return LE_fixnum(fixnum_value(arg(0)), 1, argc, argv);
    } else if (is_flonum(arg(0))) {
        return LE_fixnum(flonum_value(arg(0)), 1, argc, argv);
    } else {
        error("not number");
        return OPT_FALSE;
    }
}

opt builtin__GRE__EQ_(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0))) {
        return GE_fixnum(fixnum_value(arg(0)), 1, argc, argv);
    } else if (is_flonum(arg(0))) {
        return GE_fixnum(flonum_value(arg(0)), 1, argc, argv);
    } else {
        error("not number");
        return OPT_FALSE;
    }
}

#define FOLD_OP2(NAME,OP)                                            \
opt fold_op2_ ## NAME ## _flonum                                     \
(double acc, unsigned i, unsigned argc, const opt *argv)             \
{                                                                    \
    for ( ; i < argc; ++i) {                                         \
        opt p = arg(i);                                              \
        double x;                                                    \
        if (is_fixnum(p)) {                                          \
            x = fixnum_value(p);                                     \
        } else if (is_flonum(p)) {                                   \
            x = flonum_value(p);                                     \
        } else {                                                     \
            error("not number");                                     \
            return OPT_FALSE;                                        \
        }                                                            \
        acc = acc OP x;                                              \
    }                                                                \
    return make_flonum(acc);                                         \
}                                                                    \
opt fold_op2_ ## NAME ## _fixnum                                     \
(int_t acc, unsigned i, unsigned argc, const opt *argv)              \
{                                                                    \
    for ( ; i < argc; ++i) {                                         \
        opt p = arg(i);                                              \
        if (is_fixnum(p)) {                                          \
            acc = acc OP fixnum_value(p);                            \
        } else if (is_flonum(p)) {                                   \
            return fold_op2_ ## NAME ## _flonum(acc, i, argc, argv); \
        } else {                                                     \
            error("not number");                                     \
            return OPT_FALSE;                                        \
        }                                                            \
    }                                                                \
    return make_fixnum(acc);                                         \
}

FOLD_OP2(MUL,*)
FOLD_OP2(ADD,+)
FOLD_OP2(SUB,-)
FOLD_OP2(DIV,/)

opt builtin__AST_(vm_context *vm, unsigned argc, const opt *argv)
{
    return fold_op2_MUL_fixnum(1, 0, argc, argv);
}

opt builtin__PLUS_(vm_context *vm, unsigned argc, const opt *argv)
{
    return fold_op2_ADD_fixnum(0, 0, argc, argv);
}

opt builtin__MINUS_(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    if (argc == 1) {
        if (is_fixnum(p)) {
            return make_fixnum(-fixnum_value(p));
        } else if (is_flonum(p)) {
            return make_flonum(-flonum_value(p));
        } else {
            error("not number");
            return OPT_FALSE;
        }
    } else {
        if (is_fixnum(p)) {
            return fold_op2_SUB_fixnum(fixnum_value(p), 1, argc, argv);
        } else if (is_flonum(p)) {
            return fold_op2_SUB_flonum(flonum_value(p), 1, argc, argv);
        } else {
            error("not number");
            return OPT_FALSE;
        }
    }
}

opt builtin__SL_(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    double x;
    if (is_fixnum(p)) {
        x = fixnum_value(p);
    } else if (is_flonum(p)) {
        x = flonum_value(p);
    } else {
        error("not number");
        return OPT_FALSE;
    }

    if (argc == 1) {
        return make_flonum(1.0 / x);
    } else {
        return fold_op2_DIV_flonum(x, 1, argc, argv);
    }
}

opt builtin_quotient(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0)) && is_fixnum(arg(1))) {
        int_t n0 = fixnum_value(arg(0));
        int_t n1 = fixnum_value(arg(1));
        if (n1 == 0)
          error("div by 0");
        return make_fixnum(n0 / n1);
    } else {
        error("not integer");
        return OPT_FALSE;
    }
}

opt builtin_modulo(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0)) && is_fixnum(arg(1))) {
        int_t n0 = fixnum_value(arg(0));
        int_t n1 = fixnum_value(arg(1));
        if (n1 == 0)
          error("div by 0");
        return make_fixnum(n0 % n1);
    } else {
        error("not integer");
        return OPT_FALSE;
    }
}

#define FLONUM_FUN_1(FUNNAME)                                            \
opt builtin_ ## FUNNAME (vm_context *vm, unsigned argc, const opt *argv) \
{                                                                        \
    opt p = arg(0);                                                      \
    double x;                                                            \
    if (is_fixnum(p)) {                                                  \
        x = fixnum_value(p);                                             \
    } else if (is_flonum(p)) {                                           \
        x = flonum_value(p);                                             \
    } else {                                                             \
        error("not number");                                             \
        return OPT_FALSE;                                                \
    }                                                                    \
    return make_flonum(FUNNAME(x));                                      \
}

FLONUM_FUN_1(exp)
FLONUM_FUN_1(sin)
FLONUM_FUN_1(cos)
FLONUM_FUN_1(asin)
FLONUM_FUN_1(acos)
FLONUM_FUN_1(sqrt)

FLONUM_FUN_1(log)               /* ### 2-args */
FLONUM_FUN_1(tan)               /* ### 2-args */
FLONUM_FUN_1(atan)              /* ### 2-args */

int_t intpow(int_t x, int_t n)
{
    if (n == 0)
      return 1;
    if (n < 0)
      return (x == 1) ? 1 : 0;

    int_t z = 1;
    while (n > 0) {
        if ((n % 2) == 0) {
            x *= x;
            n /= 2;
        } else {
            z *= x;
            n--;
        }
    }
    return z;
}

opt builtin_expt(vm_context *vm, unsigned argc, const opt *argv)
{
    if (is_fixnum(arg(0)) && is_fixnum(arg(1)))
      return make_fixnum(intpow(fixnum_value(arg(0)), fixnum_value(arg(1))));

    double x, y;
    if (is_fixnum(arg(0))) {
        x = fixnum_value(arg(0));
    } else if (is_flonum(arg(0))) {
        x = flonum_value(arg(0));
    } else {
        error("not number");
        return OPT_FALSE;
    }

    if (is_fixnum(arg(1))) {
        y = fixnum_value(arg(1));
    } else if (is_flonum(arg(1))) {
        y = flonum_value(arg(1));
    } else {
        error("not number");
        return OPT_FALSE;
    }

    return make_flonum(pow(x, y));
}

opt builtin_boolean_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return (arg(0) == OPT_TRUE || arg(0) == OPT_FALSE) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_pair_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_pair(arg(0)) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_cons(vm_context *vm, unsigned argc, const opt *argv)
{
    return cons(arg(0), arg(1));
}

opt builtin_car(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert(is_pair(arg(0)), "not pair");
    return car(arg(0));
}

opt builtin_cdr(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert(is_pair(arg(0)), "not pair");
    return cdr(arg(0));
}

opt builtin_set_MINUS_car_EX_(vm_context *vm, unsigned argc, const opt *argv)
{
    set_car(arg(0), arg(1));
    return OPT_FALSE;
}

opt builtin_set_MINUS_cdr_EX_(vm_context *vm, unsigned argc, const opt *argv)
{
    set_cdr(arg(0), arg(1));
    return OPT_FALSE;
}

opt builtin_symbol_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    return is_object(p) && is_symbol(ptr_to_header(p)) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_symbol_MINUS__GRE_string(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    xassert(is_object(p) && is_symbol(ptr_to_header(p)), "not symbol");
    return symbol_name(p);
}

opt builtin_string_MINUS__GRE_symbol(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    xassert(is_object(p) && is_string(ptr_to_header(p)), "not string");
    object_header *hp = ptr_to_header(p);
    uint_t length = string_length(hp);
    char *s = string_v(hp);
    return find_symbol(s, length, true);
}

opt builtin_char_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_char(arg(0)) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_char_MINUS__GRE_integer(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert(is_char(arg(0)), "not char");
    return make_fixnum(char_value(arg(0)));
}

opt builtin_integer_MINUS__GRE_char(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert(is_fixnum(arg(0)), "not integer");
    int_t k = fixnum_value(arg(0));
    return make_char(k);
}

opt builtin_string_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    return is_object(p) && is_string(ptr_to_header(p)) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_make_MINUS_string(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_fixnum(arg(0));
    int_t length = fixnum_value(arg(0));
    xassert(length >= 0 && length <= MAX_LENGTH_STRING, "invalid length");
    opt s = make_string(length);
    if (argc == 1)
      return s;
    else {
        if (!is_char(arg(1)))
          error("not char");
        int_t c = char_value(arg(1));
        char *v = string_v(ptr_to_header(s));
        for (int_t i = 0; i < length; ++i) {
            v[i] = c;
        }
        return s;
    }
}

opt builtin_string_MINUS_length(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    xassert_string(p);
    object_header *hp = ptr_to_header(p);
    uint_t length = string_length(hp);
    return make_fixnum(length);
}

opt builtin_string_MINUS_ref(vm_context *vm, unsigned argc, const opt *argv)
{
    opt p = arg(0);
    xassert_string(p);
    object_header *hp = ptr_to_header(p);
    uint_t length = string_length(hp);
    uint8_t *v = (uint8_t *)string_v(hp);
    xassert_fixnum(arg(1));
    int_t k = fixnum_value(arg(1));
    xassert(k >= 0 && k < length, "index out of range");
    return make_char(v[k]);
}

opt builtin_string_MINUS_set_EX_(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_string(arg(0));
    object_header *hp = ptr_to_header(arg(0));
    uint_t length = string_length(hp);
    char *v = string_v(hp);
    xassert_fixnum(arg(1));
    int_t k = fixnum_value(arg(1));
    xassert(k >= 0 && k < length, "index out of range");
    xassert_char(arg(2));
    uint_t c = char_value(arg(2));
    v[k] = c;
    return OPT_FALSE;   
}

opt builtin_vector_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_object(arg(0)) && is_vector(ptr_to_header(arg(0)))
      ? OPT_TRUE : OPT_FALSE;
}

opt builtin_make_MINUS_vector(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_fixnum(arg(0));
    int_t length = fixnum_value(arg(0));
    xassert(length >= 0 && length <= MAX_LENGTH_VECTOR, "invalid length");
    opt p = make_vector(length);
    if (argc == 1)
      return p;
    else {
        opt *v = vector_v(ptr_to_header(p));
        opt fill = arg(1);
        for (uint_t i = 0; i < length; ++i) {
            v[i] = fill;
        }
        return p;
    }
}

opt builtin_vector_MINUS_length(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_vector(arg(0));
    object_header *v = ptr_to_header(arg(0));
    return make_fixnum(vector_length(v));
}

opt builtin_vector_MINUS_ref(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_vector(arg(0));
    object_header *hp = ptr_to_header(arg(0));
    uint_t length = vector_length(hp);
    xassert_fixnum(arg(1));
    int_t k = fixnum_value(arg(1));
    xassert(k >= 0 && k < length, "index out of range");
    return vector_v(hp)[k];
}

opt builtin_vector_MINUS_set_EX_(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_vector(arg(0));
    object_header *hp = ptr_to_header(arg(0));
    uint_t length = vector_length(hp);
    xassert_fixnum(arg(1));
    int_t k = fixnum_value(arg(1));
    xassert(k >= 0 && k < length, "index out of range");
    vector_v(hp)[k] = arg(2);
    return OPT_FALSE;
}

opt builtin_bytevector_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_object(arg(0)) && is_bytevector(ptr_to_header(arg(0)))
      ? OPT_TRUE : OPT_FALSE;
}

opt builtin_make_MINUS_bytevector(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_fixnum(arg(0));
    int_t length = fixnum_value(arg(0));
    xassert(length >= 0 && length <= MAX_LENGTH_BYTEVECTOR, "invalid length");
    opt p = make_bytevector(length);
    if (argc == 1)
      return p;
    else {
        xassert_fixnum(arg(1));
        int_t fill = fixnum_value(arg(1));
        xassert(fill >= 0 && fill < 256, "not byte");
        uint8_t *v = bytevector_v(ptr_to_header(p));
        for (uint_t i = 0; i < length; ++i) {
            v[i] = fill;
        }
        return p;
    }
}

opt builtin_bytevector_MINUS_length(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_bytevector(arg(0));
    object_header *hp = ptr_to_header(arg(0));
    return make_fixnum(bytevector_length(hp));
}

opt builtin_bytevector_MINUS_u8_MINUS_ref(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_bytevector(arg(0));
    object_header *hp = ptr_to_header(arg(0));
    uint_t length = bytevector_length(hp);
    xassert_fixnum(arg(1));
    int_t k = fixnum_value(arg(1));
    xassert(k >= 0 && k < length, "index out of range");
    return make_fixnum(bytevector_v(hp)[k]);
}

opt builtin_bytevector_MINUS_u8_MINUS_set_EX_(vm_context *vm, unsigned argc, const opt *argv)
{
    xassert_bytevector(arg(0));
    object_header *hp = ptr_to_header(arg(0));
    uint_t length = bytevector_length(hp);
    xassert_fixnum(arg(1));
    int_t k = fixnum_value(arg(1));
    xassert(k >= 0 && k < length, "index out of range");
    xassert_fixnum(arg(2));
    int_t byte = fixnum_value(arg(1));
    xassert(byte >= 0 && byte < 256, "not byte");
    bytevector_v(hp)[k] = byte;
    return OPT_FALSE;
}

opt builtin_procedure_QUES_(vm_context *vm, unsigned argc, const opt *argv)
{
    return is_closure(arg(0)) ? OPT_TRUE : OPT_FALSE;
}

opt builtin_apply(vm_context *vm, unsigned argc, const opt *argv)
{
    opt proc = arg(0);
    opt p = arg(argc - 1);
    xassert(is_closure(proc), "not procedure");

    /* shift */
    for (uint_t i = 0; i < argc - 2; ++i) {
        *(vm->sp + argc - 1 - i) = *(vm->sp + argc - 2 - i);
    }
    uint_t i = 0;
    while (is_pair(p)) {
        *(vm->sp + 1 - i) = car(p);
        p = cdr(p);
        i++;
    }

    /* make trampoline */
    vm->r_num_args = i + argc - 2;
    vm->sp -= vm->r_num_args;
    vm->fp = (vm->fp + argc) - vm->r_num_args;
    vm->csp -= 3;
    vm->csp[2] = vref(proc, 0);
    vm->csp[1] = proc;
    vm->csp[0] = make_fixnum(vm->fp - vm->stack);
    vm->fp = vm->sp;
    return OPT_FALSE;
}

opt builtin_call_SL_cc(vm_context *vm, unsigned argc, const opt *argv)
{
    /* make stack copy */
    size_t stack_length = (vm->stack + STACK_LENGTH) - vm->sp;
    opt stack_save = make_vector(stack_length);
    memcpy(vector_v(ptr_to_header(stack_save)), vm->sp, stack_length * sizeof(opt));
    size_t cstack_length = (vm->cstack + CSTACK_LENGTH) - vm->csp;
    ipush(stack_save);
    opt cstack_save = make_vector(cstack_length);
    memcpy(vector_v(ptr_to_header(cstack_save)), vm->csp, cstack_length * sizeof(opt));
    ipush(cstack_save);

    /* make continuation procedure */
    opt code = make_vector(4);
    vset(code, 0, make_fixnum(VMI_BUILTIN));
    vset(code, 1, make_fixnum(PRIMITIVE_NO_sys_resume_cont));
    vset(code, 2, make_fixnum(1)); /* num_params */
    vset(code, 3, OPT_FALSE);      /* rest? */
    ipush(code);
    opt closure = make_closure(2);
    vset(closure, 0, ipop());   /* code */
    vset(closure, 2, ipop());   /* cstack save */
    vset(closure, 1, ipop());   /* stack save */

    /* make trampoline */
    opt proc = arg(0);
    *vm->sp = closure;
    vm->csp -= 3;
    vm->csp[2] = vref(proc, 0);
    vm->csp[1] = proc;
    vm->csp[0] = make_fixnum(vm->fp - vm->stack);
    vm->sp--;
    vm->fp = vm->sp;
    return OPT_FALSE;
}

opt builtin_sys_resume_cont(vm_context *vm, unsigned argc, const opt *argv)
{
    opt result = arg(0);
    opt stack_save = vref(vm->cur_closure, 1);
    opt cstack_save = vref(vm->cur_closure, 2);
    object_header *p = ptr_to_header(stack_save);
    object_header *q = ptr_to_header(cstack_save);
    uint_t stack_length = vector_length(p);
    uint_t cstack_length = vector_length(q);
    vm->sp = vm->stack + (STACK_LENGTH - stack_length);
    vm->csp = vm->cstack + (CSTACK_LENGTH - cstack_length);
    memcpy(vm->sp, vector_v(p), stack_length * sizeof(opt));
    memcpy(vm->csp, vector_v(q), cstack_length * sizeof(opt));
    vm->fp = vm->sp;
    return result;
}
