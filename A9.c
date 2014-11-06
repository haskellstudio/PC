#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "A9.h"

void *closr_closure(void *code, void *envr__ex__) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._code = code;
  _data->u._closure._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *envrr_empty() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_envr;
  return (void *)_data;
}

void *envrr_extend(void *arg, void *envr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extend_envr;
  _data->u._extend._arg = arg;
  _data->u._extend._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *ktr_ratorr__m__k(void *rand, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ratorr__m__k_kt;
  _data->u._ratorr__m__k._rand = rand;
  _data->u._ratorr__m__k._envr__ex__ = envr__ex__;
  _data->u._ratorr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_randr__m__k(void *clos, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _randr__m__k_kt;
  _data->u._randr__m__k._clos = clos;
  _data->u._randr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_letr__m__k(void *body, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letr__m__k_kt;
  _data->u._letr__m__k._body = body;
  _data->u._letr__m__k._envr__ex__ = envr__ex__;
  _data->u._letr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_retr__m__k(void *vexp, void *envr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _retr__m__k_kt;
  _data->u._retr__m__k._vexp = vexp;
  _data->u._retr__m__k._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *ktr_zeror__m__k(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zeror__m__k_kt;
  _data->u._zeror__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_subr1r__m__k(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1r__m__k_kt;
  _data->u._subr1r__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_multr__m__innerr__m__k(void *vr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__innerr__m__k_kt;
  _data->u._multr__m__innerr__m__k._vr__ex__ = vr__ex__;
  _data->u._multr__m__innerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_multr__m__outerr__m__k(void *randr2, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outerr__m__k_kt;
  _data->u._multr__m__outerr__m__k._randr2 = randr2;
  _data->u._multr__m__outerr__m__k._envr__ex__ = envr__ex__;
  _data->u._multr__m__outerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_ifr__m__k(void *conseq, void *alt, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ifr__m__k_kt;
  _data->u._ifr__m__k._conseq = conseq;
  _data->u._ifr__m__k._alt = alt;
  _data->u._ifr__m__k._envr__ex__ = envr__ex__;
  _data->u._ifr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_emptyr__m__k(void *dismount) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._dismount = dismount;
  return (void *)_data;
}

void *expr_const(void *n) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_exp;
  _data->u._const._n = n;
  return (void *)_data;
}

void *expr_var(void *v) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_exp;
  _data->u._var._v = v;
  return (void *)_data;
}

void *expr_if(void *test, void *conseq, void *alt) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_exp;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *expr_mult(void *randr1, void *randr2) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_exp;
  _data->u._mult._randr1 = randr1;
  _data->u._mult._randr2 = randr2;
  return (void *)_data;
}

void *expr_subr1(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_exp;
  _data->u._subr1._rand = rand;
  return (void *)_data;
}

void *expr_zero(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_exp;
  _data->u._zero._rand = rand;
  return (void *)_data;
}

void *expr_capture(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _capture_exp;
  _data->u._capture._body = body;
  return (void *)_data;
}

void *expr_return(void *vexp, void *kexp) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _return_exp;
  _data->u._return._vexp = vexp;
  _data->u._return._kexp = kexp;
  return (void *)_data;
}

void *expr_let(void *vexp, void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_exp;
  _data->u._let._vexp = vexp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *expr_lambda(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_exp;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *expr_app(void *rator, void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_exp;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
expr = (void *)expr_app(expr_app(expr_lambda(expr_lambda(expr_var((void *)1))),expr_const((void *)5)),expr_const((void *)6));
env = (void *)envrr_empty();
pc = &valuer__m__of;
mount_tram();
printf("%d\n", (int)v);env = (void *)envrr_empty();
expr = (void *)expr_app(expr_lambda(expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5))),expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))));
pc = &valuer__m__of;
mount_tram();
printf("%d\n", (int)v);env = (void *)envrr_empty();
expr = (void *)expr_mult(expr_const((void *)2),expr_capture(expr_mult(expr_const((void *)5),expr_return(expr_mult(expr_const((void *)2),expr_const((void *)6)),expr_var((void *)0)))));
pc = &valuer__m__of;
mount_tram();
printf("%d\n", (int)v);env = (void *)envrr_empty();
expr = (void *)expr_let(expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))),expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5)));
pc = &valuer__m__of;
mount_tram();
printf("%d\n", (int)v);}

void applyr__m__closure()
{
clos* _c = (clos*)c;
switch (_c->tag) {
case _closure_clos: {
void *code = _c->u._closure._code;
void *envr__ex__ = _c->u._closure._envr__ex__;
expr = (void *)code;
env = (void *)envrr_extend(a,envr__ex__);
pc = &valuer__m__of;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)env;
switch (_c->tag) {
case _empty_envr: {
fprintf(stderr, "unbound variable");
 exit(1);
break; }
case _extend_envr: {
void *arg = _c->u._extend._arg;
void *envr__ex__ = _c->u._extend._envr__ex__;
if((num == 0)) {
  v = (void *)arg;
pc = &appr__m__k;

} else {
  env = (void *)envr__ex__;
num = (void *)(void *)((int)num - 1);
pc = &applyr__m__env;

}
break; }
}
}

void appr__m__k()
{
kt* _c = (kt*)k;
switch (_c->tag) {
case _multr__m__outerr__m__k_kt: {
void *randr2 = _c->u._multr__m__outerr__m__k._randr2;
void *envr__ex__ = _c->u._multr__m__outerr__m__k._envr__ex__;
void *kr__ex__ = _c->u._multr__m__outerr__m__k._kr__ex__;
expr = (void *)randr2;
env = (void *)envr__ex__;
k = (void *)ktr_multr__m__innerr__m__k(v,kr__ex__);
pc = &valuer__m__of;
break; }
case _multr__m__innerr__m__k_kt: {
void *vr__ex__ = _c->u._multr__m__innerr__m__k._vr__ex__;
void *kr__ex__ = _c->u._multr__m__innerr__m__k._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(void *)((int)vr__ex__ * (int)v);
pc = &appr__m__k;
break; }
case _subr1r__m__k_kt: {
void *kr__ex__ = _c->u._subr1r__m__k._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(void *)((int)v - (int)(void *)1);
pc = &appr__m__k;
break; }
case _zeror__m__k_kt: {
void *kr__ex__ = _c->u._zeror__m__k._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(v == 0);
pc = &appr__m__k;
break; }
case _retr__m__k_kt: {
void *vexp = _c->u._retr__m__k._vexp;
void *envr__ex__ = _c->u._retr__m__k._envr__ex__;
expr = (void *)vexp;
env = (void *)envr__ex__;
k = (void *)v;
pc = &valuer__m__of;
break; }
case _letr__m__k_kt: {
void *body = _c->u._letr__m__k._body;
void *envr__ex__ = _c->u._letr__m__k._envr__ex__;
void *kr__ex__ = _c->u._letr__m__k._kr__ex__;
expr = (void *)body;
env = (void *)envrr_extend(v,envr__ex__);
k = (void *)kr__ex__;
pc = &valuer__m__of;
break; }
case _randr__m__k_kt: {
void *clos = _c->u._randr__m__k._clos;
void *kr__ex__ = _c->u._randr__m__k._kr__ex__;
c = (void *)clos;
a = (void *)v;
k = (void *)kr__ex__;
pc = &applyr__m__closure;
break; }
case _ratorr__m__k_kt: {
void *rand = _c->u._ratorr__m__k._rand;
void *envr__ex__ = _c->u._ratorr__m__k._envr__ex__;
void *kr__ex__ = _c->u._ratorr__m__k._kr__ex__;
expr = (void *)rand;
env = (void *)envr__ex__;
k = (void *)ktr_randr__m__k(v,kr__ex__);
pc = &valuer__m__of;
break; }
case _ifr__m__k_kt: {
void *conseq = _c->u._ifr__m__k._conseq;
void *alt = _c->u._ifr__m__k._alt;
void *envr__ex__ = _c->u._ifr__m__k._envr__ex__;
void *kr__ex__ = _c->u._ifr__m__k._kr__ex__;
if(v) {
  expr = (void *)conseq;
env = (void *)envr__ex__;
k = (void *)kr__ex__;
pc = &valuer__m__of;

} else {
  expr = (void *)alt;
env = (void *)envr__ex__;
k = (void *)kr__ex__;
pc = &valuer__m__of;

}
break; }
case _emptyr__m__k_kt: {
void *dismount = _c->u._emptyr__m__k._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
}
}

void valuer__m__of()
{
exp* _c = (exp*)expr;
switch (_c->tag) {
case _const_exp: {
void *n = _c->u._const._n;
v = (void *)n;
pc = &appr__m__k;
break; }
case _var_exp: {
void *v = _c->u._var._v;
num = (void *)v;
pc = &applyr__m__env;
break; }
case _if_exp: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
expr = (void *)test;
k = (void *)ktr_ifr__m__k(conseq,alt,env,k);
pc = &valuer__m__of;
break; }
case _mult_exp: {
void *randr1 = _c->u._mult._randr1;
void *randr2 = _c->u._mult._randr2;
expr = (void *)randr1;
k = (void *)ktr_multr__m__outerr__m__k(randr2,env,k);
pc = &valuer__m__of;
break; }
case _subr1_exp: {
void *rand = _c->u._subr1._rand;
expr = (void *)rand;
k = (void *)ktr_subr1r__m__k(k);
pc = &valuer__m__of;
break; }
case _zero_exp: {
void *rand = _c->u._zero._rand;
expr = (void *)rand;
k = (void *)ktr_zeror__m__k(k);
pc = &valuer__m__of;
break; }
case _capture_exp: {
void *body = _c->u._capture._body;
expr = (void *)body;
env = (void *)envrr_extend(k,env);
pc = &valuer__m__of;
break; }
case _return_exp: {
void *vexp = _c->u._return._vexp;
void *kexp = _c->u._return._kexp;
expr = (void *)kexp;
k = (void *)ktr_retr__m__k(vexp,env);
pc = &valuer__m__of;
break; }
case _let_exp: {
void *vexp = _c->u._let._vexp;
void *body = _c->u._let._body;
expr = (void *)vexp;
k = (void *)ktr_letr__m__k(body,env,k);
pc = &valuer__m__of;
break; }
case _lambda_exp: {
void *body = _c->u._lambda._body;
v = (void *)closr_closure(body,env);
pc = &appr__m__k;
break; }
case _app_exp: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
expr = (void *)rator;
k = (void *)ktr_ratorr__m__k(rand,env,k);
pc = &valuer__m__of;
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
k= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
pc();
}
}
return 0;
}
