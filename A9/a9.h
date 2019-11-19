void *er__t__, *envr__t__, *kr__t__, *yr__t__, *cr__t__, *ar__t__, *vr__t__;

void (*pcr__t__)();

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

void valuer__m__ofr__m__cps();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _emptyr__m__env_envr,
    _extendr__m__env_envr
  } tag;
  union {
    struct { char dummy; } _emptyr__m__env;
    struct { void *_ar__ex__; void *_envr__ex__; } _extendr__m__env;
  } u;
};

void *envrr_emptyr__m__env();
void *envrr_extendr__m__env(void *ar__ex__, void *envr__ex__);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_b; void *_env; } _closure;
  } u;
};

void *closr_closure(void *b, void *env);

void applyr__m__closure();
struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _emptyr__m__k_kt,
    _appr__m__outerr__m__k_kt,
    _appr__m__innerr__m__k_kt,
    _letr__m__k_kt,
    _throwr__m__k_kt,
    _ifr__m__k_kt,
    _zeror__m__k_kt,
    _subr1r__m__k_kt,
    _multr__m__outerr__m__k_kt,
    _multr__m__innerr__m__k_kt
  } tag;
  union {
    struct { void *_jumpout; } _emptyr__m__k;
    struct { void *_rand; void *_env; void *_k; } _appr__m__outerr__m__k;
    struct { void *_c; void *_k; } _appr__m__innerr__m__k;
    struct { void *_b; void *_env; void *_k; } _letr__m__k;
    struct { void *_vr__m__exp; void *_env; } _throwr__m__k;
    struct { void *_conseq; void *_alt; void *_env; void *_k; } _ifr__m__k;
    struct { void *_k; } _zeror__m__k;
    struct { void *_k; } _subr1r__m__k;
    struct { void *_xr2; void *_env; void *_k; } _multr__m__outerr__m__k;
    struct { void *_xr1; void *_k; } _multr__m__innerr__m__k;
  } u;
};

void *ktr_emptyr__m__k(void *jumpout);
void *ktr_appr__m__outerr__m__k(void *rand, void *env, void *k);
void *ktr_appr__m__innerr__m__k(void *c, void *k);
void *ktr_letr__m__k(void *b, void *env, void *k);
void *ktr_throwr__m__k(void *vr__m__exp, void *env);
void *ktr_ifr__m__k(void *conseq, void *alt, void *env, void *k);
void *ktr_zeror__m__k(void *k);
void *ktr_subr1r__m__k(void *k);
void *ktr_multr__m__outerr__m__k(void *xr2, void *env, void *k);
void *ktr_multr__m__innerr__m__k(void *xr1, void *k);

void applyr__m__k();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

