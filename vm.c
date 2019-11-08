#include "object.h"
#include "builtin.h"

opt *istack;
opt *isp;
opt *isp_sup;

opt symbol_htvec;
opt global_env_htvec;

vm_context vm;

bool option_debug;
bool option_verbose;

char init_path[PATH_MAX];

void error(const char *msg, ...)
{
	va_list ap;
	va_start(ap, msg);
	vprintf(msg, ap);
	va_end(ap);
	printf("\n");
	exit(1);
}

opt find_binding(vm_context *vm, opt sym, bool create)
{
    object_header *hp = ptr_to_header(global_env_htvec);
	opt *v = vector_v(hp);
    opt str = symbol_name(sym);
    object_header *str_hp = ptr_to_header(str);
	uint_t k = string_hash(string_v(str_hp), string_length(str_hp)) % vector_length(hp);
	for (opt p = v[k]; p != OPT_NIL; p = cdr(p)) {
		opt q = car(p);
		if (car(q) == sym)
		  return cdr(q);			/* binding box */
	}

	if (create) {
		ipush(sym);
		opt box = make_box(vm, OPT_FALSE);
		sym = ipop();
		ipush(box);
		opt p = cons(vm, sym, box);
		v[k] = cons(vm, p, v[k]);
		box = ipop();
		return box;
	} else {
		return OPT_FALSE;
	}
}

const char *inst_name(unsigned i)
{
	extern const char *instruction_name[];
	if (i < NUM_INSTRUCTIONS) {
		return instruction_name[i];
	} else {
		return "*INVALID_INSTRUCTION*";
	}
}

#ifdef NDEBUG
#define PRINT_STACK(vm)
#else
#define PRINT_STACK(vm) print_stack(vm)
void print_stack(vm_context *vm)
{
	if (option_debug) {
		printf("------ %p\n", vm->sp);
		for (uint_t i = 0; i < vm->r_num_args; ++i) {
			printf("%d ", (int)i);
			write_sexpr(stdout, *(vm->sp + vm->r_num_args - 1 - i));
			printf("\n");
		}
		printf("------\n");
	}
}
#endif

opt execute(vm_context *vm, opt thunk)
{
	vm->cur_closure = thunk;
	vm->code = vref(thunk, 0);

	vm->reg = OPT_FALSE;
	vm->sp = vm->stack + STACK_LENGTH;
	vm->fp = vm->sp;
	opt *csp_sup = vm->cstack + CSTACK_LENGTH;
	vm->csp = csp_sup;

	while (true) {
		unsigned instruction_code = fixnum_value(vref(vm->code, 0));
#ifndef NDEBUG
		if (option_debug) {
			printf("%s ", inst_name(instruction_code));
			write_sexpr(stdout, vm->reg);
			printf("  ");
			write_sexpr(stdout, vm->code);
			printf("\n");
		}
#endif
		switch (instruction_code) {
		case VMI_BUILTIN:
		  {
			  unsigned k = fixnum_value(vref(vm->code, 1));
			  unsigned num_params = fixnum_value(vref(vm->code, 2));
			  unsigned num_args = vm->r_num_args; /* save */
			  opt rest = vref(vm->code, 3);
			  if ((rest == OPT_FALSE && num_args != num_params) ||
				  (rest != OPT_FALSE && num_args + 1 < num_params))
				error("wrong num of args");
			  vm->reg = (*builtin_procedure[k])(vm, num_args, vm->fp);
			  vm->sp = vm->fp + num_args;
			  if (vm->csp == csp_sup)
				return vm->reg;
			  vm->code = vm->csp[2];
			  vm->cur_closure = vm->csp[1];
			  vm->fp = vm->stack + fixnum_value(vm->csp[0]);
			  vm->csp += 3;
		  }
		  break;
		case VMI_CONSTANT:
		  vm->reg = vref(vm->code, 2);
		  vm->code = vref(vm->code, 1);
		  break;
		case VMI_PUSH:
		  xassert(vm->sp > vm->stack, "stack overflow");
		  *--(vm->sp) = vm->reg;
		  vm->code = vref(vm->code, 1);
		  break;
		case VMI_INDIRECT:
		  vm->reg = unbox(vm->reg);
		  vm->code = vref(vm->code, 1);
		  break;
		case VMI_REF_PARAM:
		  {
			  unsigned k = fixnum_value(vref(vm->code, 2));
			  vm->reg = *(vm->fp + k);
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_REF_CLOSED:
		  {
			  unsigned k = fixnum_value(vref(vm->code, 2));
			  vm->reg = vref(vm->cur_closure, k + 1);
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_REF_GLOBAL_SYMBOL:
		  {
			  opt symbol = vref(vm->code, 2);
			  opt box = find_binding(vm, symbol, false);
			  if (box == OPT_FALSE) {
				  write_sexpr(stderr, symbol);
				  error(": unbound var");
			  }
			  vm->reg = unbox(box);
			  /* rewrite instruction */
			  vset(vm->code, 0, make_fixnum(VMI_REF_GLOBAL));
			  vset(vm->code, 2, box);
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_REF_GLOBAL:
		  vm->reg = unbox(vref(vm->code, 2));
		  vm->code = vref(vm->code, 1);
		  break;
		case VMI_ASSIGN_PARAM:
		  {
			  unsigned k = fixnum_value(vref(vm->code, 2));
			  box_set(*(vm->fp + k), vm->reg);
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_ASSIGN_CLOSED:
		  {
			  unsigned k = fixnum_value(vref(vm->code, 2));
			  box_set(vref(vm->cur_closure, k + 1), vm->reg);
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_ASSIGN_GLOBAL:
		  box_set(vref(vm->code, 2), vm->reg);
		  vm->code = vref(vm->code, 1);
		  break;
		case VMI_ASSIGN_GLOBAL_SYMBOL:
		  {
			  opt symbol = vref(vm->code, 2);
			  opt box = find_binding(vm, symbol, false);
			  if (box == OPT_FALSE) {
				  error("unbound var", symbol);
			  }
			  box_set(box, vm->reg);
			  /* rewrite instruction */
			  vset(vm->code, 0, make_fixnum(VMI_ASSIGN_GLOBAL));
			  vset(vm->code, 2, box);
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_CLOSE:
		  {
			  unsigned num_closed = fixnum_value(vref(vm->code, 2));
			  vm->reg = make_closure(vm, num_closed);
			  vset(vm->reg, 0, vref(vm->code, 3)); /* closure code */
			  for (int i = 0; i < num_closed; ++i) {
				  vset(vm->reg, i + 1, *(vm->sp + i));
			  }
			  vm->sp += num_closed;
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_BRANCH:
		  if (vm->reg == OPT_FALSE) {
			  vm->code = vref(vm->code, 2);
		  } else {
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_BOX:
		  {
			  unsigned k = fixnum_value(vref(vm->code, 2));
			  *(vm->fp + k) = make_box(vm, *(vm->fp + k));
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_APPLY:
		  if (!is_closure(vm->reg)) {
			  write_sexpr(stderr, vm->reg);
			  error(": not closure", vm->reg);
		  } else {
			  vm->r_num_args = fixnum_value(vref(vm->code, 2)); /* refered to by enter */
			  xassert(vm->csp > vm->cstack, "cstack overflow");
			  vm->csp -= 3;
			  vm->csp[2] = vref(vm->code, 1); /* next */
			  vm->csp[1] = vm->cur_closure;
			  vm->csp[0] = make_fixnum(vm->fp - vm->stack);
			  vm->fp = vm->sp;
			  vm->cur_closure = vm->reg;
			  vm->code = vref(vm->cur_closure, 0);
		  }
		  break;
		case VMI_SHIFTJUMP:
		  if (!is_closure(vm->reg)) {
			  write_sexpr(stderr, vm->reg);
			  error(": not closure", vm->reg);
		  } else {
			  unsigned num_params = fixnum_value(vref(vm->code, 2));
			  vm->r_num_args = fixnum_value(vref(vm->code, 1));
			  for (unsigned i = 0; i < vm->r_num_args; ++i) {
				  *(vm->fp + num_params - 1 - i) = *(vm->sp + vm->r_num_args - 1 - i);
			  }
			  vm->sp = vm->fp + num_params - vm->r_num_args;
			  vm->fp = vm->sp;
			  vm->cur_closure = vm->reg;
			  vm->code = vref(vm->cur_closure, 0);
		  }
		  break;
		case VMI_ENTER:
		  PRINT_STACK(vm);
		  {
			  unsigned num_params = fixnum_value(vref(vm->code, 2));
			  if (num_params != vm->r_num_args) {
				  error("wrong num of args");
			  }
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_ENTER_REST:
		  PRINT_STACK(vm);
		  {
			  unsigned num_params = fixnum_value(vref(vm->code, 2));
			  if (num_params - 1 > vm->r_num_args) {
				  error("wrong num of args");
			  }
			  /* make rest arg */
			  opt rest = OPT_NIL;
			  for (unsigned i = 0; i < vm->r_num_args - num_params + 1; ++i) {
				  rest = cons(vm, *(vm->sp + i), rest);
			  }
			  vm->sp = vm->sp + vm->r_num_args - num_params;
			  *vm->sp = rest;
			  vm->fp = vm->sp;
			  vm->code = vref(vm->code, 1);
		  }
		  break;
		case VMI_RETURN:
		  {
			  if (vm->csp == csp_sup)
				return vm->reg;
			  unsigned num_params = fixnum_value(vref(vm->code, 1));
			  vm->sp = vm->fp + num_params;
			  vm->code = vm->csp[2];
			  vm->cur_closure = vm->csp[1];
			  vm->fp = vm->stack + fixnum_value(vm->csp[0]);
			  vm->csp += 3;
		  }
		  break;
		default:
		  error("unknown instruction %d", instruction_code);
		}
	}
}

void init_vector(opt p)
{
	object_header *hp = ptr_to_header(p);
    opt *v = vector_v(hp);
	uint_t n = vector_length(hp);
	for (uint_t i = 0; i < n; ++i) {
		v[i] = OPT_NIL;
	}
}

void init_istack(void)
{
	istack = malloc(ISTACK_LENGTH * sizeof(uint_t));
	isp_sup = istack + ISTACK_LENGTH;
	isp = istack;
}

void init_environments(vm_context *vm)
{
	symbol_htvec = make_vector(vm, SYMBOL_HTVEC_LENGTH);
	init_vector(symbol_htvec);
	global_env_htvec = make_vector(vm, GLOBAL_ENV_HTVEC_LENGTH);
	init_vector(global_env_htvec);
}

opt symbol_value_from_nstring(const char *sym_name)
{
	opt sym = find_symbol(NULL, sym_name, strlen(sym_name), false);
    xassert_symbol(sym);
	opt box = find_binding(NULL, sym, false);
	xassert_box(box);
	return unbox(box);
}

opt call_thunk(vm_context *vm, const char *sym_name)
{
	opt thunk = symbol_value_from_nstring(sym_name);
    xassert_closure(thunk);
    vm->r_num_args = 0;
	opt result = execute(vm, thunk);
	return result;
}

void init_vm_context(vm_context *vm)
{
	vm->reg = OPT_FALSE;
	vm->cur_closure = OPT_FALSE;
	vm->code = OPT_FALSE;
	vm->stack = malloc(STACK_LENGTH * sizeof(uint_t));
	vm->cstack = malloc(CSTACK_LENGTH * sizeof(uint_t));
	vm->sp = vm->stack + STACK_LENGTH;
	vm->fp = vm->fp;
	vm->csp = vm->cstack + CSTACK_LENGTH;
	vm->r_num_args = 0;
}

void init_load(vm_context *vm, const char *filename, bool b_init_path)
{
    char path[PATH_MAX];
    if (b_init_path) {
        strcpy(path, init_path);
        strcat(path, filename);
    } else {
        strcpy(path, filename);
    }

	FILE *fp = fopen(path, "r");
	if (fp == NULL) {
		fprintf(stderr, "cannot open %s\n", path);
		exit(1);
	}

	opt p;
	while ((p = read_sexpr(vm, fp)) != OPT_EOF) {
		opt name = car(p);
		ipush(name);
		opt x = car(cdr(p));
		if (is_object(x) && is_vector(ptr_to_header(x))) {		/* code */
			ipush(x);
			x = make_closure(vm, 0);
			vset(x, 0, ipop());
		}
		name = ipop();
		ipush(x);
		opt box = find_binding(vm, name, true);
		box_set(box, ipop());
	}
	fclose(fp);
}
