#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "vminst-defs.h"

#ifdef __cplusplus
extern "C" {
#endif

#define STACK_LENGTH               16384
#define CSTACK_LENGTH              (1024 * 3)
#define ISTACK_LENGTH              16384
#define SYMBOL_HTVEC_LENGTH        997
#define GLOBAL_ENV_HTVEC_LENGTH    997

enum extres_type {
    EXTRES_TYPE_FILE,
    NUM_EXTRES
};

#define MAX_TOKEN_LENGTH    256

typedef intptr_t int_t;
typedef uintptr_t uint_t;
#define PRI_d "ld"
#define PRI_u "lu"

typedef struct object *opt;

#define HEADER_LENGTH  1

typedef struct object_header {
  uint_t header;
} object_header;

#define BITS_TAG_PTR    3
#define MASK_TAG_PTR    ((uint_t)((1 << BITS_TAG_PTR) - 1))
#define TAG_PAIR        0x00
#define TAG_CLOSURE     0x02
#define TAG_OBJECT      0x04

#define BITS_TAG_FLONUM 4
#define MASK_TAG_FLONUM ((uint_t)((1 << BITS_TAG_FLONUM) - 1))
#define TAG_FLONUM      0x06

#define BITS_TAG_IMM    5
#define MASK_TAG_IMM    ((uint_t)((1 << BITS_TAG_IMM) - 1))
#define TAG_IMMEDIATE   0x0e
#define TAG_HEADER      0x1e

#define BITS_IMM_TYPE   8
#define MASK_IMM_TYPE   ((uint_t)((1 << BITS_IMM_TYPE) - 1))
#define TAG_FALSE       0x0e
#define TAG_TRUE        0x2e
#define TAG_NIL         0x4e
#define TAG_EOF         0x6e
#define TAG_CHAR        0x8e
#define TAG_EXTRES      0xae
#define TAG_RELOCMARK   0xce

#define OPT_FALSE       ((opt)TAG_FALSE)
#define OPT_TRUE        ((opt)TAG_TRUE)
#define OPT_NIL         ((opt)TAG_NIL)
#define OPT_EOF         ((opt)TAG_EOF)
#define OPT_RELOCMARK   ((opt)TAG_RELOCMARK)

#define BITS_TYPE       8
#define MASK_TYPE       ((uint_t)((1 << BITS_TYPE) - 1))
#define TYPE_VECTOR     0x1e
#define TYPE_CLOSURE    0x3e
#define TYPE_SYMBOL     0x5e
#define TYPE_RECORD     0x7e
#define TYPE_STRING     0x9e
#define TYPE_BYTEVECTOR 0xbe

/* relocmark:1 */
#define BITS_FLAGS      (BITS_TYPE + 2)
#define BIT_RELOCMARK   (1 << BITS_TYPE)
#define BIT_FIXEDLOC    (1 << (BITS_TYPE + 1))

#define BITS_FIXNUM     63
#define FIXNUM_MIN      ((int_t)((uint_t)1 << BITS_FIXNUM))
#define FIXNUM_MAX      ((int_t)(((uint_t)1 << BITS_FIXNUM) - 1))

static inline bool is_fixnum(opt p) { return (uint_t)p & 1; }
static inline int_t fixnum_value(opt p) { return (int_t)p >> 1; }
static inline opt make_fixnum(int_t k) { return (opt)((k << 1) | 1); }

static inline bool is_pair(opt p) { return ((uint_t)p & MASK_TAG_PTR) == TAG_PAIR; }
static inline bool is_closure(opt p) { return ((uint_t)p & MASK_TAG_PTR) == TAG_CLOSURE; }
static inline bool is_object(opt p) { return ((uint_t)p & MASK_TAG_PTR) == TAG_OBJECT; }

static inline opt copy_tag(void *p, opt q) {
    return (opt)((uint_t)p | ((uint_t)q & MASK_TAG_PTR));
}

static inline opt add_tag_pair(uint_t *p) { return (opt)((uint_t)p | TAG_PAIR); }
static inline opt add_tag_closure(object_header *p) { return (opt)((uint_t)p | TAG_CLOSURE); }
static inline opt add_tag_object(object_header *p) { return (opt)((uint_t)p | TAG_OBJECT); }

static inline bool is_flonum(opt p) { return ((uint_t)p & MASK_TAG_FLONUM) == TAG_FLONUM; }
static inline double flonum_value(opt o) {
	uint_t u = (uint_t)o & ~MASK_TAG_FLONUM;
	return *(double *)&u;
}
static inline opt make_flonum(double x) {
	uint_t k = *(uint_t *)&x;
	return (opt)((k & ~MASK_TAG_FLONUM) | TAG_FLONUM);
}

static inline bool is_immediate(opt p) { return ((uint_t)p & MASK_TAG_IMM) == TAG_IMMEDIATE; }
static inline bool is_header(uint_t h) { return (h & MASK_TAG_IMM) == TAG_HEADER; }

static inline bool is_relocmark(opt p) { return ((uint_t)p & MASK_IMM_TYPE) == TAG_RELOCMARK; }
static inline bool marked_p(object_header *p) { return p->header & BIT_RELOCMARK; }
static inline bool fixedloc_p(object_header *p) { return p->header & BIT_FIXEDLOC; }
static inline void mark(object_header *p) { p->header |= BIT_RELOCMARK; }
static inline void unmark(object_header *p) { p->header &= ~BIT_RELOCMARK; }

static inline bool is_char(opt p) { return ((uint_t)p & MASK_IMM_TYPE) == TAG_CHAR; }
static inline uint_t char_value(opt p) { return (uint_t)p >> BITS_IMM_TYPE; }
static inline opt make_char(uint_t c) { return (opt)((c << BITS_IMM_TYPE) | TAG_CHAR); }

/*
  extres
*/

typedef struct extres extres_t;

struct extres {
    void* object;
    extres_t *link;
    unsigned index;
    bool mark;
};

typedef bool (*extres_finalize_t)(void *);

typedef struct extres_book extres_book_t;

struct extres_book {
	extres_t **vector;
	extres_t *list;
	extres_t *free;
    extres_finalize_t finalize;
	unsigned watermark;
	unsigned size;
	unsigned count;
};

/*   LSB | tag:8 | type:8 | id:48 | MSB */
#define EXTRES_BITS_TYPE 8
#define EXTRES_MASK_TYPE ((1 << EXTRES_BITS_TYPE) - 1)
static inline bool is_extres(opt p) { return ((uint_t)p & MASK_IMM_TYPE) == TAG_EXTRES; }
static inline uint_t extres_type(opt p) { return ((uint_t)p >> BITS_IMM_TYPE) & EXTRES_MASK_TYPE; }
static inline uint_t extres_id(opt p) { return (uint_t)p >> (BITS_IMM_TYPE + EXTRES_BITS_TYPE); }
static inline opt make_extres(uint_t type, uint_t id) {
	return (opt)((((id << EXTRES_BITS_TYPE) | type) << BITS_IMM_TYPE) | TAG_EXTRES);
}

extern extres_book_t extres_book_table[];

static inline void *extres_get(int type, unsigned i)
{
	return extres_book_table[type].vector[i]->object;
}

static inline void extres_mark(extres_book_t *bp, unsigned i)
{
    assert(i < bp->watermark);
    bp->vector[i]->mark = true;
}

void extres_register_type(int extres_type, extres_finalize_t finalize);
unsigned extres_alloc(int extres_type, void *object);


static inline opt car(opt p) { return *(opt *)p; }
static inline opt cdr(opt p) { return *((opt *)p + 1); }
static inline void set_car(opt p, opt q) { *(opt *)p = q; }
static inline void set_cdr(opt p, opt q) { *((opt *)p + 1) = q; }

static inline bool is_box(opt p) { return is_pair(p); }
static inline opt unbox(opt box) { return car(box); }
static void box_set(opt box, opt p) { set_car(box, p); }

static object_header *ptr_to_header(opt p) {
    return (object_header *)((uint_t)p & ~MASK_TAG_PTR);
}

static inline bool is_vector(const object_header *p) {
	return (p->header & MASK_TYPE) == TYPE_VECTOR; }
static inline bool is_closure_obj(const object_header *p) {
	return (p->header & MASK_TYPE) == TYPE_CLOSURE; }
static inline bool is_symbol(const object_header *p) {
	return (p->header & MASK_TYPE) == TYPE_SYMBOL; }
static inline bool is_record(const object_header *p) {
	return (p->header & MASK_TYPE) == TYPE_RECORD; }
static inline bool is_string(const object_header *p) {
	return (p->header & MASK_TYPE) == TYPE_STRING; }
static inline bool is_bytevector(const object_header *p) {
	return (p->header & MASK_TYPE) == TYPE_BYTEVECTOR; }

static inline bool is_vector_like(const object_header *hp) {
	return !is_bytevector(hp) && !is_string(hp);
}

static opt vref(opt p, int_t k) { return *((opt *)(ptr_to_header(p) + 1) + k); }
static void vset(opt p, int_t k, opt q) { *((opt *)(ptr_to_header(p) + 1) + k) = q; }

/* vm context */
typedef struct {
  opt reg;
  opt cur_closure;
  opt code;
  opt *sp;
  opt *fp;
  opt *csp;
  uint_t r_num_args;
  opt *stack;
  opt *cstack;
} vm_context;

typedef opt (*pf_builtin_t)(vm_context *vm, unsigned argc, const opt *argv);
extern pf_builtin_t builtin_procedure[];

void error(const char *msg, ...);

static inline void check_num_args(unsigned num_args, unsigned expected) {
	if (num_args != expected)
	  error("wrong num of args");
}

static inline void xassert(bool b, const char *msg) {
	if (!b)
	  error(msg);
}


#define MAX_LENGTH_VECTOR (1 << 24)
static inline uint_t vector_length(const object_header *p) { return p->header >> BITS_FLAGS; }
static inline opt *vector_v(object_header *p) { return (opt *)(p + 1); }

#define MAX_LENGTH_CLOSURE (1 << 24)
static inline uint_t closure_length(const object_header *p) { return p->header >> BITS_FLAGS; }
static inline opt *closure_v(object_header *p) { return (opt *)(p + 1); }

#define MAX_LENGTH_RECORD (1 << 24)
static inline uint_t record_length(const object_header *p) { return p->header >> BITS_FLAGS; }
static inline opt *record_v(object_header *p) { return (opt *)(p + 1); }

#define SYMBOL_LENGTH 1
static inline void symbol_name_set(object_header *p, opt s) { *(opt *)(p + 1) = s; }
static inline opt symbol_name(opt sym) { return vref(sym, 0); }

#define MAX_LENGTH_BYTEVECTOR (1 << 24)
static inline uint_t bytevector_length(const object_header *p) { return p->header >> BITS_FLAGS; }
static inline uint8_t *bytevector_v(object_header *p) { return (uint8_t *)(p + 1); }

#define MAX_LENGTH_STRING (1 << 24)
static inline uint_t string_length(const object_header *p) { return p->header >> BITS_FLAGS; }
static inline char *string_v(object_header *p) { return (char *)(p + 1); }

extern opt *istack;
extern opt *isp;
extern opt *isp_sup;

extern opt symbol_htvec;
extern opt global_env_htvec;
extern opt g_drive_proc;
extern opt g_drive_argv;

extern bool option_debug;
extern bool option_verbose;

static inline void ipush(opt p) {
	assert(isp < isp_sup);
	*isp++ = p;
}

static inline opt ipop(void) {
	assert(isp > istack);
	return *--isp;
}

void init_istack(void);
void init_environments(vm_context *vm);
void init_vm_context(vm_context *vm);
void init_object_space(void);
void init_load(vm_context *vm, const char *filename, bool b_init_path);
opt call_thunk(vm_context *vm, const char *sym_name);
void drive(void);

uint_t *obj_alloc(vm_context *vm, uint_t length);
object_header *vobj_alloc(vm_context *vm, uint_t length);
object_header *bvobj_alloc(vm_context *vm, uint_t length);
opt cons(vm_context *vm, opt p, opt q);
opt make_box(vm_context *vm, opt o);
opt make_vector(vm_context *vm, uint_t length);
opt make_closure(vm_context *vm, uint_t num_closed);
opt make_symbol(vm_context *vm, opt string);
opt make_bytevector(vm_context *vm, uint_t length);
opt make_string(vm_context *vm, uint_t length);

uint_t string_hash(const char *s, uint_t len);
opt make_symbol_for_reader(vm_context *vm, const char *s, uint_t length);
opt find_symbol(vm_context *vm, const char *s, uint_t length, bool create);

opt read_sexpr(vm_context *vm, FILE *fp);
void write_sexpr(FILE *fp, opt p);
opt list_to_vector(vm_context *vm, opt p);
opt list_to_bytevector(vm_context *vm, opt p);

void register_extres_type(int extres_type, extres_finalize_t finalize);

static inline void xassert_fixnum(opt p) {
	xassert(is_fixnum(p), "not integer");
}
static inline void xassert_flonum(opt p) {
	xassert(is_flonum(p), "not real number");
}
static inline void xassert_char(opt p) {
	xassert(is_char(p), "not char");
}
static inline void xassert_extres(int type, opt p) {
	xassert(is_extres(p) && extres_type(p) == type, "not extres");
}
static inline void xassert_pair(opt p) {
	xassert(is_pair(p), "not pair");
}
static inline void xassert_box(opt p) {
	xassert(is_box(p), "not box");
}
static inline void xassert_string(opt p) {
	xassert(is_object(p) && is_string(ptr_to_header(p)), "not string");
}
static inline void xassert_symbol(opt p) {
	xassert(is_object(p) && is_symbol(ptr_to_header(p)), "not symbol");
}
static inline void xassert_vector(opt p) {
	xassert(is_object(p) && is_vector(ptr_to_header(p)), "not vector");
}
static inline void xassert_closure(opt p) {
	xassert(is_closure(p), "not closure");
}
static inline void xassert_bytevector(opt p) {
	xassert(is_object(p) && is_bytevector(ptr_to_header(p)), "not bytevector");
}
static inline void xassert_real(opt p) {
	xassert(is_fixnum(p) || is_flonum(p), "not real");
}

static inline opt *get_argv(void)
{
    return (opt *)(ptr_to_header(g_drive_argv) + 1);
}

static inline opt arg_ref(unsigned k, unsigned argc, const opt *argv) {
    return argv[argc - 1 - k];
}

#define arg(k) arg_ref((k), argc, argv)

static inline double real_value(opt p) {
	if (is_fixnum(p))
	  return fixnum_value(p);
	else
	  return flonum_value(p);
}

#ifdef __GNUC__
static inline unsigned long long rdtsc(void)
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 );
}
#endif

#ifdef __cplusplus
}
#endif
