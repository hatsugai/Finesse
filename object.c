#include "object.h"

#define HEAP_LENGTH (16 * 1024  * 1024)

uint_t *heap0;
uint_t *heap1;
uint_t *heap;
uint_t *heap_sup;
uint_t *heap_cur;
uint_t *heap_ptr;

extres_book_t extres_book_table[NUM_EXTRES];

object_header vector0		= { BIT_FIXEDLOC | TYPE_VECTOR };
object_header bytevector0	= { BIT_FIXEDLOC | TYPE_BYTEVECTOR };
object_header string0		= { BIT_FIXEDLOC | TYPE_STRING };

opt g_drive_proc;
opt g_drive_argv;

void extres_init(extres_book_t *bp, extres_finalize_t finalize)
{
	bp->finalize = finalize;
    bp->list = 0;
    bp->free = 0;
    bp->count = 0;
    bp->watermark = 0;
    bp->size = 16;	/* default size */
    bp->vector = malloc(bp->size * sizeof(extres_t *));
    for (unsigned i = 0; i < bp->size; i++)
	  bp->vector[i] = NULL;
}

unsigned extres_alloc2(extres_book_t *bp, void *object)
{
    extres_t *p;
    if (bp->free) {
        p = bp->free;
        bp->free = p->link;
    } else {
        if (bp->watermark == bp->size) {
            bp->size *= 2;
            extres_t **v = malloc(bp->size * sizeof(extres_t *));
            unsigned i;
            for (i = 0; i < bp->watermark; i++)
			  v[i] = bp->vector[i];
            for ( ; i < bp->size; i++)
			  v[i] = NULL;
            free(bp->vector);
            bp->vector = v;
        }
        p = malloc(sizeof(extres_t));
		p->index = bp->watermark;
        bp->vector[bp->watermark++] = p;
    }
    p->object = object;
    p->mark = false;
	p->link = bp->list;
	bp->list = p;
    bp->count++;
    return p->index;
}

unsigned extres_alloc(int extres_type, void *object)
{
	return extres_alloc2(&extres_book_table[extres_type], object);
}

void extres_register_type(int extres_type, extres_finalize_t finalize)
{
	extres_init(&extres_book_table[extres_type], finalize);
}

uint_t obj_length(const object_header *p)
{
    if (is_vector(p))
        return vector_length(p);
    else if (is_record(p))
        return record_length(p);
    else if (is_closure_obj(p))
        return closure_length(p);
    else if (is_symbol(p))
        return SYMBOL_LENGTH;
    else if (is_string(p))
        return (string_length(p) + sizeof(uint_t) - 1) / sizeof(uint_t);
    else if (is_bytevector(p))
        return (bytevector_length(p) + sizeof(uint_t) - 1) / sizeof(uint_t);
    else {
        xassert(false, "unknown object");
        return 0;
    }
}

opt gc_copy(opt p)
{
	if (is_extres(p)) {
		uint_t type = extres_type(p);
		extres_book_t *bp = &extres_book_table[type];
		extres_mark(bp, extres_id(p));
		return p;
	} else if (is_fixnum(p) || is_flonum(p) || is_immediate(p)) {
        return p;
    } else if (is_pair(p)) {
        if (is_relocmark(car(p))) {
            return cdr(p);
        } else {
            opt *q = (opt *)heap_ptr;
            heap_ptr += 2;
            q[0] = car(p);
            q[1] = cdr(p);
            set_car(p, OPT_RELOCMARK);
            set_cdr(p, (opt)q);
            return (opt)q;
        }
    } else {
        object_header *hp = ptr_to_header(p);
		uint_t length = obj_length(hp);
        if (marked_p(hp)) {
            return vref(p, 0);
		} else if (length == 0) {
			return p;
        } else {
            uint_t *q = heap_ptr;
            heap_ptr += HEADER_LENGTH + length;
            *q = hp->header;
            memcpy(q + 1, hp + 1, length * sizeof(uint_t));
            opt r = copy_tag(q, p);
            mark(hp);
            vset(p, 0, r);		/* length > 0 */
            return r;
        }
    }
}

void sweep_extres(void)
{
	for (int i = 0; i < NUM_EXTRES; ++i) {
		extres_book_t *bp = &extres_book_table[i];
		extres_t *p, *q, dummy;
		dummy.link = bp->list;
		p = &dummy;
		q = bp->list;
		while (q != NULL) {
			if (q->mark) {
				q->mark = false;
				p = q;
				q = q->link;
			} else if ((*bp->finalize)(q->object)) {
				p->link = q->link;
				q->link = bp->free;
				bp->free = q;
				q = p->link;
			} else {
				p = q;
				q = q->link;
			}
		}
	}
}

void gc(vm_context *vm)
{
    uint_t *tospace;
    uint_t *scan;

    if (heap == heap0) {
        tospace = heap1;
    } else {
        tospace = heap0;
    }
    heap_ptr = tospace;

    /* copy roots */
    symbol_htvec = gc_copy(symbol_htvec);
    global_env_htvec = gc_copy(global_env_htvec);
    vm->reg = gc_copy(vm->reg);
    vm->cur_closure = gc_copy(vm->cur_closure);
    vm->code = gc_copy(vm->code);
    opt *sup = isp;
    for (opt *p = istack; p < sup; ++p)
        *p = gc_copy(*p);
    sup = vm->stack + STACK_LENGTH;
    for (opt *p = vm->sp; p < sup; ++p)
        *p = gc_copy(*p);
    sup = vm->cstack + CSTACK_LENGTH;
    for (opt *p = vm->csp; p < sup; ++p)
        *p = gc_copy(*p);

    g_drive_proc = gc_copy(g_drive_proc);
    g_drive_argv = gc_copy(g_drive_argv);

    scan = tospace;
    while (scan < heap_ptr) {
        object_header *hp = (object_header *)scan;
        if (is_header(hp->header)) { /* object */
            uint_t length = obj_length(hp);
            if (is_vector_like(hp)) {
                opt *v = (opt *)(scan + 1);
                for (uint_t i = 0; i < length; ++i) {
                    v[i] = gc_copy(v[i]);
                }
            }
            scan += HEADER_LENGTH + length;
        } else {                 /* pair */
            opt *v = (opt *)scan;
            v[0] = gc_copy(v[0]);
            v[1] = gc_copy(v[1]);
            scan += 2;
        }
    }

	sweep_extres();

    heap = tospace;
    heap_sup = heap + HEAP_LENGTH;
    heap_cur = heap_ptr;

	double x = heap_cur - heap;
	x /= HEAP_LENGTH;
	fprintf(stderr, "GC %.1f\n", x * 100.0);
}

uint_t *obj_alloc(vm_context *vm, uint_t length)
{
	if (heap_cur + length > heap_sup) {
        gc(vm);
		if (heap_cur + length > heap_sup) {
			fprintf(stderr, "heap exhausted\n");
			abort();
		}
    }
	uint_t *p = heap_cur;
	heap_cur += length;
	return p;
}

void init_object_space(void)
{
	heap0 = malloc(HEAP_LENGTH * sizeof(uint_t));
	heap1 = malloc(HEAP_LENGTH * sizeof(uint_t));
    heap = heap0;
	heap_sup = heap + HEAP_LENGTH;
	heap_cur = heap;
}

object_header *vobj_alloc(vm_context *vm, uint_t length)
{
	return (object_header *)obj_alloc(vm, length + HEADER_LENGTH);
}

object_header *bvobj_alloc(vm_context *vm, uint_t length)
{
	uint_t wlen = (length + sizeof(uint_t) - 1) / sizeof(uint_t);
	return (object_header *)obj_alloc(vm, wlen + HEADER_LENGTH);
}

opt make_pair(vm_context *vm)
{
	return add_tag_pair(obj_alloc(vm, 2));
}

opt cons(vm_context *vm, opt x, opt y)
{
	ipush(x);
	ipush(y);
	opt p = make_pair(vm);
	y = ipop();
	x = ipop();
	set_car(p, x);
	set_cdr(p, y);
	return p;
}

opt make_box(vm_context *vm, opt o)
{
	ipush(o);
	opt p = make_pair(vm);
	o = ipop();
	set_car(p, o);
	set_cdr(p, OPT_NIL);
	return p;
}

opt make_closure(vm_context *vm, uint_t num_closed)
{
	object_header *p = vobj_alloc(vm, num_closed + 1); /* for code */
	p->header = ((num_closed + 1) << BITS_FLAGS) | TYPE_CLOSURE;
	return add_tag_closure(p);
}

opt make_vector(vm_context *vm, uint_t length)
{
	if (length == 0) {
		return add_tag_object(&vector0);
	} else {
		object_header *p = vobj_alloc(vm, length);
		p->header = (length << BITS_FLAGS) | TYPE_VECTOR;
		return add_tag_object(p);
	}
}

opt make_bytevector(vm_context *vm, uint_t length)
{
	if (length == 0) {
		return add_tag_object(&bytevector0);
	} else {
		object_header *p = bvobj_alloc(vm, length);
		p->header = (length << BITS_FLAGS) | TYPE_BYTEVECTOR;
		return add_tag_object(p);
	}
}

opt make_string(vm_context *vm, uint_t length)
{
	if (length == 0) {
		return add_tag_object(&string0);
	} else {
		object_header *p = bvobj_alloc(vm, length);
		p->header = (length << BITS_FLAGS) | TYPE_STRING;
		return add_tag_object(p);
	}
}

opt make_symbol(vm_context *vm, opt str)
{
	ipush(str);
	object_header *p = vobj_alloc(vm, SYMBOL_LENGTH);
	str = ipop();
	p->header = TYPE_SYMBOL;
	symbol_name_set(p, str);
	return add_tag_object(p);
}
