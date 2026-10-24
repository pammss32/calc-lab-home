/*
 * Name: Pamir Ali Koc
 * Student ID: 260ADM044
 * Repository: https://github.com/pammss32/calc-lab-home
 * Resources used: OpenAI ChatGPT, for explanation, debugging, and implementation guidance.
 * Prompt: Help me implement and understand an arithmetic parser in C for the RTU graded lab.
 * Conversation history: Add your shared conversation URL before submission.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <limits.h>
#include <dirent.h>
#include <sys/stat.h>

#define STUDENT_NAME "pamir_koc"
#define STUDENT_ID "260ADM044"
#define MAX_DEPTH 1000

typedef struct {
    double d;
    long long i;
    int is_float;
} Value;

typedef struct {
    const char *s;
    size_t len;
    size_t pos;
    size_t error;
    int depth;
} Parser;

static void skip_ws(Parser *p)
{
    while (p->pos < p->len &&
           (p->s[p->pos] == ' ' ||
            p->s[p->pos] == '\t' ||
            p->s[p->pos] == '\r')) {
        p->pos++;
    }
}

static Value make_int(long long n)
{
    Value v;
    v.i = n;
    v.d = (double)n;
    v.is_float = 0;
    return v;
}

static Value make_float(double n)
{
    Value v;
    v.d = n;
    v.i = 0;
    v.is_float = 1;
    return v;
}

static Value parse_expr(Parser *p);

static void set_error(Parser *p, size_t at)
{
    if (!p->error) {
        p->error = at + 1;
    }
}

static int at_end(Parser *p)
{
    return p->pos >= p->len || p->s[p->pos] == '\n';
}

static Value parse_primary(Parser *p)
{
    Value v = make_int(0);
    skip_ws(p);

    if (p->error) return v;

    if (p->pos >= p->len || p->s[p->pos] == '\n') {
        set_error(p, p->pos);
        return v;
    }

    if (p->s[p->pos] == '(') {
        p->pos++;

        if (++p->depth > MAX_DEPTH) {
            set_error(p, p->pos - 1);
            return v;
        }

        v = parse_expr(p);
        p->depth--;
        skip_ws(p);

        if (p->error) return v;

        if (p->pos < p->len && p->s[p->pos] == ')') {
            p->pos++;
        } else {
            set_error(p, p->pos);
        }

        return v;
    }

    if (p->s[p->pos] == ')') {
        set_error(p, p->pos);
        return v;
    }

    /* Unary operators and leading-dot floats are not supported. */
    if (p->s[p->pos] == '+' ||
        p->s[p->pos] == '-' ||
        p->s[p->pos] == '.') {
        set_error(p, p->pos);
        return v;
    }

    if (!isdigit((unsigned char)p->s[p->pos])) {
        set_error(p, p->pos);
        return v;
    }

    size_t start = p->pos;

    while (p->pos < p->len &&
           isdigit((unsigned char)p->s[p->pos])) {
        p->pos++;
    }

    if (p->pos < p->len && p->s[p->pos] == '.') {
        size_t dot = p->pos++;

        if (p->pos >= p->len ||
            !isdigit((unsigned char)p->s[p->pos])) {
            set_error(p, dot);
            return v;
        }

        while (p->pos < p->len &&
               isdigit((unsigned char)p->s[p->pos])) {
            p->pos++;
        }

        errno = 0;
        char *end = NULL;
        double d = strtod(p->s + start, &end);

        if (errno == ERANGE || !isfinite(d)) {
            set_error(p, start);
            return v;
        }

        v = make_float(d);
    } else {
        errno = 0;
        char *end = NULL;
        long long n = strtoll(p->s + start, &end, 10);

        if (errno == ERANGE || end != p->s + p->pos) {
            set_error(p, start);
            return v;
        }

        v = make_int(n);
    }

    return v;
}

static Value apply_power(Parser *p, Value a, Value b, size_t op_pos)
{
    if (a.is_float || b.is_float) {
        double base = a.is_float ? a.d : (double)a.i;
        double exponent = b.is_float ? b.d : (double)b.i;

        if (base == 0.0 && exponent < 0.0) {
            set_error(p, op_pos);
            return make_float(0.0);
        }

        double result = pow(base, exponent);

        if (!isfinite(result)) {
            set_error(p, op_pos);
            return make_float(0.0);
        }

        return make_float(result);
    }

    if (b.i < 0) {
        set_error(p, op_pos);
        return make_int(0);
    }

    long long result = 1;
    long long base = a.i;
    long long exponent = b.i;

    while (exponent > 0 && !p->error) {
        if (exponent & 1) {
            if (__builtin_mul_overflow(result, base, &result)) {
                set_error(p, op_pos);
                return make_int(0);
            }
        }

        exponent >>= 1;

        if (exponent &&
            __builtin_mul_overflow(base, base, &base)) {
            set_error(p, op_pos);
            return make_int(0);
        }
    }

    return make_int(result);
}

static Value parse_power(Parser *p)
{
    Value left = parse_primary(p);
    skip_ws(p);

    if (p->error) return left;

    if (p->pos + 1 < p->len &&
        p->s[p->pos] == '*' &&
        p->s[p->pos + 1] == '*') {
        size_t op = p->pos;
        p->pos += 2;
        skip_ws(p);

        if (at_end(p)) {
            set_error(p, p->pos);
            return left;
        }

        size_t exponent_start = p->pos;
        Value right = parse_power(p);

        if (!p->error && !right.is_float && right.i < 0) {
            set_error(p, exponent_start);
            return left;
        }

        if (!p->error) {
            left = apply_power(p, left, right, op);
        }
    }

    return left;
}

static Value parse_term(Parser *p)
{
    Value left = parse_power(p);

    while (!p->error) {
        skip_ws(p);

        if (p->pos >= p->len ||
            p->s[p->pos] == '\n' ||
            p->s[p->pos] == '+' ||
            p->s[p->pos] == '-' ||
            p->s[p->pos] == ')') {
            break;
        }

        if (p->s[p->pos] != '*' && p->s[p->pos] != '/') {
            break;
        }

        if (p->s[p->pos] == '*' &&
            p->pos + 1 < p->len &&
            p->s[p->pos + 1] == '*') {
            break;
        }

        size_t op = p->pos;
        char oper = p->s[p->pos++];
        skip_ws(p);

        if (at_end(p)) {
            set_error(p, p->pos);
            break;
        }

        if (p->s[p->pos] == '+' ||
            p->s[p->pos] == '-' ||
            p->s[p->pos] == '*' ||
            p->s[p->pos] == '/') {
            set_error(p, p->pos);
            break;
        }

        size_t right_start = p->pos;
        Value right = parse_power(p);

        if (p->error) break;

        if (oper == '/') {
            if ((right.is_float && right.d == 0.0) ||
                (!right.is_float && right.i == 0)) {
                set_error(p, right_start);
                break;
            }

            if (left.is_float || right.is_float) {
                double a = left.is_float ? left.d : (double)left.i;
                double b = right.is_float ? right.d : (double)right.i;
                double r = a / b;

                if (!isfinite(r)) set_error(p, op);
                left = make_float(r);
            } else if (left.i == LLONG_MIN && right.i == -1) {
                set_error(p, op);
            } else {
                left = make_int(left.i / right.i);
            }
        } else {
            if (left.is_float || right.is_float) {
                double a = left.is_float ? left.d : (double)left.i;
                double b = right.is_float ? right.d : (double)right.i;
                double r = a * b;

                if (!isfinite(r)) set_error(p, op);
                left = make_float(r);
            } else {
                long long r;

                if (__builtin_mul_overflow(left.i, right.i, &r)) {
                    set_error(p, op);
                } else {
                    left = make_int(r);
                }
            }
        }
    }

    return left;
}

static Value parse_expr(Parser *p)
{
    Value left = parse_term(p);

    while (!p->error) {
        skip_ws(p);

        if (p->pos >= p->len ||
            p->s[p->pos] == '\n' ||
            p->s[p->pos] == ')') {
            break;
        }

        if (p->s[p->pos] != '+' && p->s[p->pos] != '-') {
            break;
        }

        size_t op = p->pos;
        char oper = p->s[p->pos++];
        skip_ws(p);

        if (at_end(p)) {
            set_error(p, p->pos);
            break;
        }

        if (p->s[p->pos] == '+' ||
            p->s[p->pos] == '-' ||
            p->s[p->pos] == '*' ||
            p->s[p->pos] == '/') {
            set_error(p, p->pos);
            break;
        }

        Value right = parse_term(p);

        if (p->error) break;

        if (left.is_float || right.is_float) {
            double a = left.is_float ? left.d : (double)left.i;
            double b = right.is_float ? right.d : (double)right.i;
            double r = a + (oper == '+' ? b : -b);

            if (!isfinite(r)) set_error(p, op);
            left = make_float(r);
        } else {
            long long r;
            int bad;

            if (oper == '+') {
                bad = __builtin_add_overflow(left.i, right.i, &r);
            } else {
                bad = __builtin_sub_overflow(left.i, right.i, &r);
            }

            if (bad) {
                set_error(p, op);
            } else {
                left = make_int(r);
            }
        }
    }

    return left;
}

static int eval_line(const char *data, size_t len, size_t base,
                     size_t *err, Value *out)
{
    Parser p;
    p.s = data;
    p.len = len;
    p.pos = 0;
    p.error = 0;
    p.depth = 0;

    while (p.pos < len &&
           (data[p.pos] == ' ' ||
            data[p.pos] == '\t' ||
            data[p.pos] == '\r')) {
        p.pos++;
    }

    if (p.pos >= len || data[p.pos] == '\n' || data[p.pos] == '#') {
        return 0;
    }

    *out = parse_expr(&p);
    skip_ws(&p);

    if (!p.error && p.pos < len && data[p.pos] != '\n') {
        set_error(&p, p.pos);
    }

    if (p.error) {
        *err = base + p.error - 1;
        return -1;
    }

    return 1;
}

static char *read_all(FILE *f, size_t *length)
{
    size_t cap = 8192;
    size_t n = 0;
    char *buf = malloc(cap + 1);

    if (!buf) return NULL;

    for (;;) {
        if (n == cap) {
            if (cap > (size_t)-1 / 2) {
                free(buf);
                return NULL;
            }

            cap *= 2;
            char *tmp = realloc(buf, cap + 1);

            if (!tmp) {
                free(buf);
                return NULL;
            }

            buf = tmp;
        }

        size_t got = fread(buf + n, 1, cap - n, f);
        n += got;

        if (got == 0) {
            if (ferror(f)) {
                free(buf);
                return NULL;
            }
            break;
        }
    }

    buf[n] = '\0';
    *length = n;
    return buf;
}

static int ends_txt(const char *s)
{
    size_t n = strlen(s);
    return n >= 4 && strcmp(s + n - 4, ".txt") == 0;
}

static char *base_name(const char *path)
{
    const char *s = strrchr(path, '/');
    s = s ? s + 1 : path;

    size_t n = strlen(s);

    if (n > 4 && strcmp(s + n - 4, ".txt") == 0) {
        n -= 4;
    }

    char *r = malloc(n + 1);

    if (r) {
        memcpy(r, s, n);
        r[n] = '\0';
    }

    return r;
}

static int ensure_dir(const char *path)
{
    struct stat st;

    if (stat(path, &st) == 0) {
        return S_ISDIR(st.st_mode);
    }

    return mkdir(path, 0777) == 0;
}

static char *join_path(const char *a, const char *b)
{
    size_t n = strlen(a) + strlen(b) + 2;
    char *r = malloc(n);

    if (r) {
        snprintf(r, n, "%s/%s", a, b);
    }

    return r;
}

static int process_file(const char *input_path, const char *outdir)
{
    FILE *in = fopen(input_path, "rb");

    if (!in) {
        fprintf(stderr, "calc: cannot open input: %s\n", input_path);
        return 1;
    }

    size_t len = 0;
    char *data = read_all(in, &len);
    fclose(in);

    if (!data) {
        fprintf(stderr, "calc: cannot read input: %s\n", input_path);
        return 1;
    }

    char *base = base_name(input_path);

    if (!base) {
        free(data);
        return 1;
    }

    size_t outn = strlen(outdir) + strlen(base) +
                  strlen(STUDENT_NAME) + strlen(STUDENT_ID) + 8;

    char *outpath = malloc(outn);

    if (!outpath) {
        free(data);
        free(base);
        return 1;
    }

    snprintf(outpath, outn, "%s/%s_%s_%s.txt",
             outdir, base, STUDENT_NAME, STUDENT_ID);

    FILE *out = fopen(outpath, "w");

    if (!out) {
        fprintf(stderr, "calc: cannot create output: %s\n", outpath);
        free(data);
        free(base);
        free(outpath);
        return 1;
    }

    size_t pos = 0;
    size_t first_error = 0;

    /* Validate the entire file before writing any results. */
    while (pos < len) {
        size_t start = pos;

        while (pos < len && data[pos] != '\n') pos++;

        size_t line_end = pos;

        if (pos < len) pos++;

        size_t line_len = (line_end - start) +
                          (line_end < len ? 1 : 0);

        Value v;
        size_t err = 0;

        int status = eval_line(data + start, line_len,
                               start + 1, &err, &v);

        if (status < 0) {
            first_error = err;
            break;
        }
    }

    if (first_error) {
        fprintf(out, "ERROR:%zu\n", first_error);
    } else {
        pos = 0;

        while (pos < len) {
            size_t start = pos;

            while (pos < len && data[pos] != '\n') pos++;

            size_t line_end = pos;

            if (pos < len) pos++;

            size_t line_len = (line_end - start) +
                              (line_end < len ? 1 : 0);

            Value v;
            size_t err = 0;

            int status = eval_line(data + start, line_len,
                                   start + 1, &err, &v);

            if (status > 0) {
                if (v.is_float) {
                    fprintf(out, "%.15g\n", v.d);
                } else {
                    fprintf(out, "%lld\n", v.i);
                }
            }
        }
    }

    fclose(out);
    free(data);
    free(base);
    free(outpath);

    return 0;
}

static void usage(void)
{
    fprintf(stderr,
            "Usage: calc [-d DIR | --dir DIR] "
            "[-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
}

int main(int argc, char **argv)
{
    const char *indir = NULL;
    const char *outdir = NULL;
    const char *input = NULL;

    for (int i = 1; i < argc; i++) {
        if ((!strcmp(argv[i], "-d") ||
             !strcmp(argv[i], "--dir")) && i + 1 < argc) {
            indir = argv[++i];
        } else if ((!strcmp(argv[i], "-o") ||
                    !strcmp(argv[i], "--output-dir")) && i + 1 < argc) {
            outdir = argv[++i];
        } else if (argv[i][0] == '-') {
            usage();
            return 1;
        } else if (!input) {
            input = argv[i];
        } else {
            usage();
            return 1;
        }
    }

    if ((indir && input) || (!indir && !input)) {
        usage();
        return 1;
    }

    char *default_out = NULL;

    if (!outdir) {
        if (indir) {
            size_t dir_len = strlen(indir);

            while (dir_len > 1 && indir[dir_len - 1] == '/') {
                dir_len--;
            }

            const char *start = indir;

            for (size_t j = 0; j < dir_len; j++) {
                if (indir[j] == '/') {
                    start = indir + j + 1;
                }
            }

            size_t name_len = (size_t)((indir + dir_len) - start);
            size_t n = name_len + strlen(STUDENT_NAME) +
                       strlen(STUDENT_ID) + 3;

            default_out = malloc(n + 1);

            if (default_out) {
                snprintf(default_out, n + 1, "%.*s_%s_%s",
                         (int)name_len, start,
                         STUDENT_NAME, STUDENT_ID);
            }
        } else {
            char *b = base_name(input);

            if (!b) return 1;

            size_t n = strlen(b) + strlen(STUDENT_NAME) +
                       strlen(STUDENT_ID) + 4;

            default_out = malloc(n);

            if (default_out) {
                snprintf(default_out, n, "%s_%s_%s",
                         b, STUDENT_NAME, STUDENT_ID);
            }

            free(b);
        }

        outdir = default_out;
    }

    if (!outdir || !ensure_dir(outdir)) {
        fprintf(stderr, "calc: cannot create output directory: %s\n",
                outdir ? outdir : "(null)");
        free(default_out);
        return 1;
    }

    int rc = 0;

    if (input) {
        rc = process_file(input, outdir);
    } else {
        DIR *d = opendir(indir);

        if (!d) {
            fprintf(stderr, "calc: cannot open directory: %s\n", indir);
            free(default_out);
            return 1;
        }

        struct dirent *entry;

        while ((entry = readdir(d)) != NULL) {
            if (!ends_txt(entry->d_name)) continue;

            char *full = join_path(indir, entry->d_name);

            if (!full) {
                rc = 1;
                continue;
            }

            struct stat st;

            if (stat(full, &st) == 0 && S_ISREG(st.st_mode)) {
                if (process_file(full, outdir) != 0) {
                    rc = 1;
                }
            }

            free(full);
        }

        closedir(d);
    }

    free(default_out);
    return rc;
}