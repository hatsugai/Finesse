#include "object.h"

uint_t string_hash(const char *s, uint_t len)
{
    const uint8_t *p = (const uint8_t *)s;
    uint_t hv = 0;
    while (len > 0) {
        --len;
        hv = (hv * 7) + p[len];
    }
    return hv;
}

opt find_symbol(const char *s, uint_t length, bool create)
{
    object_header *hp = ptr_to_header(symbol_htvec);
    opt *v = vector_v(hp);
    uint_t k = string_hash(s, length) % vector_length(hp);
    for (opt p = v[k]; p != OPT_NIL; p = cdr(p)) {
        opt sym = car(p);
        opt sym_name = symbol_name(sym);
        assert(is_object(sym_name));
        object_header *q = ptr_to_header(sym_name);
        assert(is_string(q));
        if (string_length(q) == length &&
            memcmp(string_v(q), s, length) == 0) {
            return sym;
        }
    }

    if (create) {
        opt str = make_string(length);
        memcpy(string_v(ptr_to_header(str)), s, length);
        opt sym = make_symbol(str);
        v[k] = cons(sym, v[k]);
        return sym;
    } else {
        return OPT_FALSE;
    }
}

opt make_symbol_for_reader(const char *s, uint_t length)
{
    return find_symbol(s, length, true);
}

uint_t list_length(opt p, bool *p_dotted)
{
    uint_t c = 0;
    while (is_pair(p)) {
        p = cdr(p);
        c++;
    }
    if (p_dotted != NULL) {
        *p_dotted = (p != OPT_NIL);
    }
    return c;
}

opt list_to_vector(opt p)
{
    uint_t length;
    bool dotted;
    length = list_length(p, &dotted);
    if (dotted)
      error("invalid vector");
    ipush(p);
    opt v = make_vector(length);
    p = ipop();
    for (uint_t i = 0; i < length; ++i) {
        vset(v, i, car(p));
        p = cdr(p);
    }
    return v;
}

opt list_to_bytevector(opt p)
{
    /* check bytes */
    opt q;
    uint_t length = 0;
    for (q = p; is_pair(q); q = cdr(q), length++) {
        opt x = car(q);
        if (!is_fixnum(x))
          error("invalid bytevector");
        int_t k = fixnum_value(x);
        if (k < 0 || k >= 256)
          error("invalid bytevector");
    }
    if (q != OPT_NIL)
      error("invalid bytevector");

    ipush(p);
    opt bv = make_bytevector(length);
    p = ipop();
    uint8_t *v = bytevector_v(ptr_to_header(bv));
    for (uint_t i = 0; i < length; ++i) {
        v[i] = fixnum_value(car(p));
        p = cdr(p);
    }
    return bv;
}

bool is_delimiter(int c)
{
    return c == EOF || c == ' ' || c == '\t' || c == '\n' ||
      c == '|' || c == '(' || c == ')' || c == '"' || c == ';';
}

int get_vchar0(FILE *fp)
{
    int c;
    while ((c = getc(fp)) == ' ' || c == '\t' || c == '\n')
      ;
    return c;
}

int get_vchar(FILE *fp)
{
    int c;
    while (true) {
        c = get_vchar0(fp);
        if (c == ';') {
            while ((c = getc(fp)) != EOF && c != '\n')
              ;
            if (c == EOF)
              return EOF;
        } else {
            return c;
        }
    }
}

bool strchr_until(const char *start, const char *end, int c)
{
    while (start < end) {
        if (*start == c)
            return true;
        start++;
    }
    return false;
}

opt read_atom(int c, FILE *fp)
{
    char buf[MAX_TOKEN_LENGTH];
    int i;
    for (i = 0; i < MAX_TOKEN_LENGTH && !is_delimiter(c); ++i) {
        buf[i] = c;
        c = getc(fp);
    }
    if (i == MAX_TOKEN_LENGTH)
      error("too long atom");
    ungetc(c, fp);
    buf[i] = 0;

    /* try number */
    char *p;
    double x = strtod(buf, &p);
    if (p != buf) {
        if (strchr_until(buf, p, '.')) {
            return make_flonum(x);
        } else {
            int_t k = x;
            double y = k;
            if (x == y && k >= FIXNUM_MIN && k <= FIXNUM_MAX) {
                return make_fixnum(k);
            } else {
                return make_flonum(x);
            }
        }
    }
    return make_symbol_for_reader(buf, i);
}

opt read_string(FILE *fp)
{
    char buf[MAX_TOKEN_LENGTH];
    int i = 0;
    int c = getc(fp);
    while (i < MAX_TOKEN_LENGTH && c != EOF && c != '"') {
        if (c == '\\') {
            c = getc(fp);
            if (c == EOF)
              error("missing '\"'");
        }
        buf[i] = c;
        c = getc(fp);
        i++;
    }
    if (i == MAX_TOKEN_LENGTH)
      error("too long string");
    if (c == EOF)
      error("missing '\"'");
    opt p = make_string(i);
    memcpy(string_v(ptr_to_header(p)), buf, i);
    return p;
}

opt read_special(FILE *fp)
{
    int c = getc(fp);
    if (c == EOF) {
        error("unexpected EOF");
    } else if (c == 'f') {
        return OPT_FALSE;
    } else if (c == 't') {
        return OPT_TRUE;
    } else if (c == '(') {      /* vector */
        ungetc(c, fp);
        opt p = read_sexpr(fp);
        return list_to_vector(p);
    } else if (c == 'u') {      /* bytevector */
        c = getc(fp);
        if (c == '8') {
            c = getc(fp);
            if (c == '(') {
                ungetc(c, fp);
                opt p = read_sexpr(fp);
                return list_to_bytevector(p);
            }
        }
        error("invalid token: '%c'", c);
    } else {
        error("invalid token: '%c'", c);
    }
    return OPT_FALSE;           /* to avoid warning */
}

opt read_list(int c, FILE *fp)
{
    opt p = read_sexpr(fp);
    opt head = cons(p, OPT_NIL);
    opt tail = head;
    ipush(head);
    while (true) {
        int c = get_vchar(fp);
        if (c == EOF) {
        } else if (c == ')') {
            break;
        } else if (c == '.') {
            ipush(tail);
            p = read_sexpr(fp);
            tail = ipop();
            set_cdr(tail, p);
            c = get_vchar(fp);
            if (c == ')')
              break;
            error("missing ')'");
        } else {
            ungetc(c, fp);
            ipush(tail);
            p = read_sexpr(fp);
            p = cons(p, OPT_NIL);
            tail = ipop();
            set_cdr(tail, p);
            tail = p;
        }
    }
    head = ipop();
    return head;
}

opt read_sexpr(FILE *fp)
{
    int c;
    c = get_vchar(fp);
    if (c == EOF) {
        return OPT_EOF;
    } else if (c == '(') {
        c = get_vchar(fp);
        if (c == ')') {
            return OPT_NIL;
        } else {
            ungetc(c, fp);
            return read_list(c, fp);
        }
    } else if (c == '#') {
        return read_special(fp);
    } else if (c == '"') {
        return read_string(fp);
    } else if (c == ')') {
        error("unexpected ')'");
        return OPT_FALSE;
    } else {
        return read_atom(c, fp);
    }
}

void write_list(FILE *fp, opt p)
{
    fprintf(fp, "(");
    write_sexpr(fp, car(p));
    p = cdr(p);
    while (is_pair(p)) {
        fprintf(fp, " ");
        write_sexpr(fp, car(p));
        p = cdr(p);
    }
    if (p == OPT_NIL) {
        fprintf(fp, ")");
    } else {
        fprintf(fp, " . ");
        write_sexpr(fp, p);
        fprintf(fp, ")");
    }
}

void write_vector(FILE *fp, opt p)
{
    uint_t n = vector_length(ptr_to_header(p));
    if (n == 0) {
        fprintf(fp, "#()");
    } else {
        fprintf(fp, "#(");
        write_sexpr(fp, vref(p, 0));
        for (uint_t i = 1; i < n; ++i) {
            fprintf(fp, " ");
            write_sexpr(fp, vref(p, i));
        }
        fprintf(fp, ")");
    }
}

void write_bytevector(FILE *fp, opt p)
{
    object_header *q = ptr_to_header(p);
    uint_t n = bytevector_length(q);
    uint8_t *v = bytevector_v(q);
    if (n == 0) {
        fprintf(fp, "#u8()");
    } else {
        fprintf(fp, "#u8(%u", v[0]);
        for (uint_t i = 1; i < n; ++i) {
            fprintf(fp, " %u", v[i]);
        }
        fprintf(fp, ")");
    }
}

void write_string(FILE *fp, opt p)
{
    object_header *hp = ptr_to_header(p);
    char *s = string_v(hp);
    uint_t n = string_length(hp);
    for (uint_t i = 0; i < n; ++i) {
        putc(s[i], fp);
    }
}

void write_symbol(FILE *fp, opt p)
{
    write_string(fp, symbol_name(p));
}

void write_record(FILE *fp, opt p)
{
    fprintf(fp, "#<record:%p>", p);
}

void write_sexpr(FILE *fp, opt p)
{
    if (p == OPT_FALSE) {
        fprintf(fp, "#f");
    } else if (p == OPT_TRUE) {
        fprintf(fp, "#t");
    } else if (p == OPT_NIL) {
        fprintf(fp, "()");
    } else if (p == OPT_EOF) {
        fprintf(fp, "#eof");
    } else if (is_fixnum(p)) {
        fprintf(fp, "%" PRId64, (int64_t)fixnum_value(p));
    } else if (is_flonum(p)) {
        fprintf(fp, "%g", flonum_value(p));
    } else if (is_char(p)) {
        uint_t c = char_value(p);
        if (c < 256)
          fprintf(fp, "#\\%c", (int)c);
        else
          fprintf(fp, "#\\x%" PRIx64, (uint64_t)c);
    } else if (is_extres(p)) {
        fprintf(fp, "#<extres:%" PRI_u ":%" PRI_u ">", extres_type(p), extres_id(p));
    } else if (is_pair(p)) {
        write_list(fp, p);
    } else if (is_closure(p)) {
        fprintf(fp, "#<closure:%p>", p);
    } else if (is_object(p)) {
        object_header *q  = ptr_to_header(p);
        if (is_vector(q)) {
            write_vector(fp, p);
        } else if (is_symbol(q)) {
            write_symbol(fp, p);
        } else if (is_record(q)) {
            write_record(fp, p);
        } else if (is_string(q)) {
            putc('"', fp);
            write_string(fp, p);
            putc('"', fp);
        } else if (is_bytevector(q)) {
            write_bytevector(fp, p);
        } else {
            assert(false);
        }
    } else {
            assert(false);
    }
}
