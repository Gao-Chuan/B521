#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *ktr_emptyr__m__k(void *jumpout) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._jumpout = jumpout;
  return (void *)_data;
}

void *ktr_appr__m__outerr__m__k(void *rand, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__outerr__m__k_kt;
  _data->u._appr__m__outerr__m__k._rand = rand;
  _data->u._appr__m__outerr__m__k._env = env;
  _data->u._appr__m__outerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_appr__m__innerr__m__k(void *c, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__innerr__m__k_kt;
  _data->u._appr__m__innerr__m__k._c = c;
  _data->u._appr__m__innerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_letr__m__k(void *b, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letr__m__k_kt;
  _data->u._letr__m__k._b = b;
  _data->u._letr__m__k._env = env;
  _data->u._letr__m__k._k = k;
  return (void *)_data;
}

void *ktr_throwr__m__k(void *vr__m__exp, void *env) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__k_kt;
  _data->u._throwr__m__k._vr__m__exp = vr__m__exp;
  _data->u._throwr__m__k._env = env;
  return (void *)_data;
}

void *ktr_ifr__m__k(void *conseq, void *alt, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ifr__m__k_kt;
  _data->u._ifr__m__k._conseq = conseq;
  _data->u._ifr__m__k._alt = alt;
  _data->u._ifr__m__k._env = env;
  _data->u._ifr__m__k._k = k;
  return (void *)_data;
}

void *ktr_zeror__m__k(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zeror__m__k_kt;
  _data->u._zeror__m__k._k = k;
  return (void *)_data;
}

void *ktr_subr1r__m__k(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1r__m__k_kt;
  _data->u._subr1r__m__k._k = k;
  return (void *)_data;
}

void *ktr_multr__m__outerr__m__k(void *xr2, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outerr__m__k_kt;
  _data->u._multr__m__outerr__m__k._xr2 = xr2;
  _data->u._multr__m__outerr__m__k._env = env;
  _data->u._multr__m__outerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_multr__m__innerr__m__k(void *xr1, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__innerr__m__k_kt;
  _data->u._multr__m__innerr__m__k._xr1 = xr1;
  _data->u._multr__m__innerr__m__k._k = k;
  return (void *)_data;
}

void *closr_closure(void *b, void *env) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._b = b;
  _data->u._closure._env = env;
  return (void *)_data;
}

void *envrr_emptyr__m__env() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envr;
  return (void *)_data;
}

void *envrr_extendr__m__env(void *ar__ex__, void *envr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._ar__ex__ = ar__ex__;
  _data->u._extendr__m__env._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
envr__t__ = (void *)envrr_emptyr__m__env();
er__t__ = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
pcr__t__ = &valuer__m__ofr__m__cps;
mount_tram();
printf("Fact 5: %d\n", (int)vr__t__);}

void applyr__m__k()
{
kt* _c = (kt*)kr__t__;
switch (_c->tag) {
case _emptyr__m__k_kt: {
void *jumpout = _c->u._emptyr__m__k._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
case _appr__m__outerr__m__k_kt: {
void *rand = _c->u._appr__m__outerr__m__k._rand;
void *env = _c->u._appr__m__outerr__m__k._env;
void *k = _c->u._appr__m__outerr__m__k._k;
er__t__ = (void *)rand;
envr__t__ = (void *)env;
kr__t__ = (void *)ktr_appr__m__innerr__m__k(vr__t__,k);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _appr__m__innerr__m__k_kt: {
void *c = _c->u._appr__m__innerr__m__k._c;
void *k = _c->u._appr__m__innerr__m__k._k;
cr__t__ = (void *)c;
ar__t__ = (void *)vr__t__;
kr__t__ = (void *)k;
pcr__t__ = &applyr__m__closure;
break; }
case _letr__m__k_kt: {
void *b = _c->u._letr__m__k._b;
void *env = _c->u._letr__m__k._env;
void *k = _c->u._letr__m__k._k;
er__t__ = (void *)b;
envr__t__ = (void *)envrr_extendr__m__env(vr__t__,env);
kr__t__ = (void *)k;
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _throwr__m__k_kt: {
void *vr__m__exp = _c->u._throwr__m__k._vr__m__exp;
void *env = _c->u._throwr__m__k._env;
er__t__ = (void *)vr__m__exp;
envr__t__ = (void *)env;
kr__t__ = (void *)vr__t__;
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _ifr__m__k_kt: {
void *conseq = _c->u._ifr__m__k._conseq;
void *alt = _c->u._ifr__m__k._alt;
void *env = _c->u._ifr__m__k._env;
void *k = _c->u._ifr__m__k._k;
if(vr__t__) {
  er__t__ = (void *)conseq;
envr__t__ = (void *)env;
kr__t__ = (void *)k;
pcr__t__ = &valuer__m__ofr__m__cps;

} else {
  er__t__ = (void *)alt;
envr__t__ = (void *)env;
kr__t__ = (void *)k;
pcr__t__ = &valuer__m__ofr__m__cps;

}
break; }
case _zeror__m__k_kt: {
void *k = _c->u._zeror__m__k._k;
kr__t__ = (void *)k;
vr__t__ = (void *)(vr__t__ == 0);
pcr__t__ = &applyr__m__k;
break; }
case _subr1r__m__k_kt: {
void *k = _c->u._subr1r__m__k._k;
kr__t__ = (void *)k;
vr__t__ = (void *)(void *)((int)vr__t__ - 1);
pcr__t__ = &applyr__m__k;
break; }
case _multr__m__outerr__m__k_kt: {
void *xr2 = _c->u._multr__m__outerr__m__k._xr2;
void *env = _c->u._multr__m__outerr__m__k._env;
void *k = _c->u._multr__m__outerr__m__k._k;
er__t__ = (void *)xr2;
envr__t__ = (void *)env;
kr__t__ = (void *)ktr_multr__m__innerr__m__k(vr__t__,k);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _multr__m__innerr__m__k_kt: {
void *xr1 = _c->u._multr__m__innerr__m__k._xr1;
void *k = _c->u._multr__m__innerr__m__k._k;
kr__t__ = (void *)k;
vr__t__ = (void *)(void *)((int)xr1 * (int)vr__t__);
pcr__t__ = &applyr__m__k;
break; }
}
}

void applyr__m__closure()
{
clos* _c = (clos*)cr__t__;
switch (_c->tag) {
case _closure_clos: {
void *b = _c->u._closure._b;
void *env = _c->u._closure._env;
er__t__ = (void *)b;
envr__t__ = (void *)envrr_extendr__m__env(ar__t__,env);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)envr__t__;
switch (_c->tag) {
case _emptyr__m__env_envr: {
fprintf(stderr, "unbound variable");
 exit(1);
break; }
case _extendr__m__env_envr: {
void *ar__ex__ = _c->u._extendr__m__env._ar__ex__;
void *envr__ex__ = _c->u._extendr__m__env._envr__ex__;
if((yr__t__ == 0)) {
  vr__t__ = (void *)ar__ex__;
pcr__t__ = &applyr__m__k;

} else {
  envr__t__ = (void *)envr__ex__;
yr__t__ = (void *)(void *)((int)yr__t__ - 1);
pcr__t__ = &applyr__m__env;

}
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)er__t__;
switch (_c->tag) {
case _const_expr: {
void *cexp = _c->u._const._cexp;
vr__t__ = (void *)cexp;
pcr__t__ = &applyr__m__k;
break; }
case _mult_expr: {
void *nexpr1 = _c->u._mult._nexpr1;
void *nexpr2 = _c->u._mult._nexpr2;
er__t__ = (void *)nexpr1;
kr__t__ = (void *)ktr_multr__m__outerr__m__k(nexpr2,envr__t__,kr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *nexp = _c->u._subr1._nexp;
er__t__ = (void *)nexp;
kr__t__ = (void *)ktr_subr1r__m__k(kr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *nexp = _c->u._zero._nexp;
er__t__ = (void *)nexp;
kr__t__ = (void *)ktr_zeror__m__k(kr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
er__t__ = (void *)test;
kr__t__ = (void *)ktr_ifr__m__k(conseq,alt,envr__t__,kr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
er__t__ = (void *)body;
envr__t__ = (void *)envrr_extendr__m__env(kr__t__,envr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kexp = _c->u._throw._kexp;
void *vexp = _c->u._throw._vexp;
er__t__ = (void *)kexp;
kr__t__ = (void *)ktr_throwr__m__k(vexp,envr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *exp = _c->u._let._exp;
void *body = _c->u._let._body;
er__t__ = (void *)exp;
kr__t__ = (void *)ktr_letr__m__k(body,envr__t__,kr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *n = _c->u._var._n;
yr__t__ = (void *)n;
pcr__t__ = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
vr__t__ = (void *)closr_closure(body,envr__t__);
pcr__t__ = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
er__t__ = (void *)rator;
kr__t__ = (void *)ktr_appr__m__outerr__m__k(rand,envr__t__,kr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
kr__t__= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
pcr__t__();
}
}
return 0;
}
