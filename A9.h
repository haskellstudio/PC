struct exp;
typedef struct exp exp;
struct exp {
  enum {
    _const_exp,
    _var_exp,
    _if_exp,
    _mult_exp,
    _subr1_exp,
    _zero_exp,
    _capture_exp,
    _return_exp,
    _let_exp,
    _lambda_exp,
    _app_exp
  } tag;
  union {
    struct { void *_n; } _const;
    struct { void *_v; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_randr1; void *_randr2; } _mult;
    struct { void *_rand; } _subr1;
    struct { void *_rand; } _zero;
    struct { void *_body; } _capture;
    struct { void *_vexp; void *_kexp; } _return;
    struct { void *_vexp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *expr_const(void *n);
void *expr_var(void *v);
void *expr_if(void *test, void *conseq, void *alt);
void *expr_mult(void *randr1, void *randr2);
void *expr_subr1(void *rand);
void *expr_zero(void *rand);
void *expr_capture(void *body);
void *expr_return(void *vexp, void *kexp);
void *expr_let(void *vexp, void *body);
void *expr_lambda(void *body);
void *expr_app(void *rator, void *rand);

void *env, *num, *expr, *v, *c, *a, *k;

void (*pc)();

void valuer__m__of();
struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _ratorr__m__k_kt,
    _randr__m__k_kt,
    _letr__m__k_kt,
    _retr__m__k_kt,
    _zeror__m__k_kt,
    _subr1r__m__k_kt,
    _multr__m__innerr__m__k_kt,
    _multr__m__outerr__m__k_kt,
    _ifr__m__k_kt,
    _emptyr__m__k_kt
  } tag;
  union {
    struct { void *_rand; void *_envr__ex__; void *_kr__ex__; } _ratorr__m__k;
    struct { void *_clos; void *_kr__ex__; } _randr__m__k;
    struct { void *_body; void *_envr__ex__; void *_kr__ex__; } _letr__m__k;
    struct { void *_vexp; void *_envr__ex__; } _retr__m__k;
    struct { void *_kr__ex__; } _zeror__m__k;
    struct { void *_kr__ex__; } _subr1r__m__k;
    struct { void *_vr__ex__; void *_kr__ex__; } _multr__m__innerr__m__k;
    struct { void *_randr2; void *_envr__ex__; void *_kr__ex__; } _multr__m__outerr__m__k;
    struct { void *_conseq; void *_alt; void *_envr__ex__; void *_kr__ex__; } _ifr__m__k;
    struct { void *_dismount; } _emptyr__m__k;
  } u;
};

void *ktr_ratorr__m__k(void *rand, void *envr__ex__, void *kr__ex__);
void *ktr_randr__m__k(void *clos, void *kr__ex__);
void *ktr_letr__m__k(void *body, void *envr__ex__, void *kr__ex__);
void *ktr_retr__m__k(void *vexp, void *envr__ex__);
void *ktr_zeror__m__k(void *kr__ex__);
void *ktr_subr1r__m__k(void *kr__ex__);
void *ktr_multr__m__innerr__m__k(void *vr__ex__, void *kr__ex__);
void *ktr_multr__m__outerr__m__k(void *randr2, void *envr__ex__, void *kr__ex__);
void *ktr_ifr__m__k(void *conseq, void *alt, void *envr__ex__, void *kr__ex__);
void *ktr_emptyr__m__k(void *dismount);

void appr__m__k();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _empty_envr,
    _extend_envr
  } tag;
  union {
    struct { char dummy; } _empty;
    struct { void *_arg; void *_envr__ex__; } _extend;
  } u;
};

void *envrr_empty();
void *envrr_extend(void *arg, void *envr__ex__);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_code; void *_envr__ex__; } _closure;
  } u;
};

void *closr_closure(void *code, void *envr__ex__);

void applyr__m__closure();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

