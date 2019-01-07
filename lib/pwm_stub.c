#include<stdio.h>

#include<caml/mlvalues.h>
#include<caml/memory.h>
#include<caml/callback.h>
#include<caml/fail.h>
#include<caml/alloc.h>
#include<caml/misc.h>

value gzt_pwm_scan(value mat, value seq, value caml_tol) {
  CAMLparam3(mat, seq, caml_tol);
  CAMLlocal3(r,tmp,hit);
  int n = Wosize_val(seq);
  int m = Wosize_val(mat);
  double tol = Double_val(caml_tol);
  int i,j;

  r = Val_int(0); // empty list
  for(i = n - m; i >= 0; i--) {
    double score = 0.;
    for(j = 0; j < m; j++) {
      score += Double_field(Field(mat, j), Int_val(Field(seq,i+j)));
    }
    if(score > tol) {
      tmp = r;
      hit = caml_alloc(2,0);
      Store_field(hit,0,Val_int(i));
      Store_field(hit,1,caml_copy_double(score));
      r = caml_alloc(2,0);
      Store_field(r,0,hit);
      Store_field(r,1,tmp);
    }
  }
  CAMLreturn (r);
}


double array_max(value arr, int n) {
  double best = Double_field(arr, 0);
  for(int j = 1; j < n; j++) {
    double candidate = Double_field(arr, j);
    if(candidate > best) best = candidate;
  }
  return best;
}

value gzt_opt_pwm_scan(value mat, value seq, value caml_tol) {
  CAMLparam3(mat, seq, caml_tol);
  CAMLlocal3(r,tmp,hit);
  int n = Wosize_val(seq);
  int m = Wosize_val(mat);
  double tol = Double_val(caml_tol);
  int i,j;

  double bs[m];
  bs[m - 1] = array_max(Field(mat, m - 1), 4);
  for(int i = m - 2; i >= 0; i--) {
    bs[i] = bs[i + 1] + array_max(Field(mat, i), 4);
  }

  r = Val_int(0); // empty list
  for(i = n - m; i >= 0; i--) {
    double score = 0.;
    for(j = 0; j < m; j++) {
      if(score + bs[j] < tol) break;
      score += Double_field(Field(mat, j), Int_val(Field(seq,i+j)));
    }
    if(score > tol) {
      tmp = r;
      hit = caml_alloc(2,0);
      Store_field(hit,0,Val_int(i));
      Store_field(hit,1,caml_copy_double(score));
      r = caml_alloc(2,0);
      Store_field(r,0,hit);
      Store_field(r,1,tmp);
    }
  }
  CAMLreturn (r);
}
