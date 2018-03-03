#ifndef EXTCLASS_H
#define EXTCLASS_H

#include <stdio.h>
#include "basic.h"

#define CMA ,
#define ARRSZ(ARR)  (sizeof(ARR)/sizeof((ARR)[0]))
#define CLASSRETURN(VAL) return ret=VAL, 1;

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
//                                                      //
//            ~*~ CLASSMACRO use exmple: ~*~            //
//                                                      //
// CLASSMACRO(spy,                                      //
//            {"age" CMA "rank"},{24 CMA 007},          //
//            {"hear" CMA "message"},{HEAR_ CMA MSG_})  //
//                                                      //
//   [ spy.age=24, spy.rank=7                   ]       //
//   [ spy.hear()=HEAR_(), spy.message()=MSG_() ]       //
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

#define CLASSMACRO(NAME,\
                   VARNAMES,VARVALS,\
                   FUNCNAMES,FUNCEXEC)\
char NAME##_vn[][SYMSZ]=VARNAMES;\
Val NAME##_vl[]=VARVALS;\
char NAME##_fn[][SYMSZ]=FUNCNAMES;\
int (*NAME##_ex[])()=FUNCEXEC;\
int NAME##LOAD_() { *--sp=NAME##_vl[PCV]; STEP; }\
int NAME##STORE_() { NAME##_vl[PCV]=*sp++; STEP; }\
EXTCLASS NAME={#NAME,\
                     ARRSZ(NAME##_vn), ARRSZ(NAME##_fn),\
                     NAME##_vn, NAME##_vl,\
                     NAME##_fn, NAME##_ex,\
                     NAME##LOAD_,NAME##STORE_};


typedef struct {
    char name[SYMSZ];
    int varcount,funccount;
    char (*varname)[SYMSZ];
    Val *varval;
    char (*funcname)[SYMSZ];
    int (**funcexec)();
    int	(*LOAD)();
    int	(*STORE)();
} EXTCLASS;

useclass(EXTCLASS *clss,
            char *member_name,
            bool use_as_expr) {
  int i; char (*tmpn)[SYMSZ];
  i=(*clss).funccount;
  tmpn=(*clss).funcname;
  while (i--)
    if(!strcasecmp(tmpn[i],member_name)) {
      (*clss).funcexec[i]();
      if (use_as_expr) emit(RV_);
      return 0;
    }
  i=(*clss).varcount;
  tmpn=(*clss).varname;
  while (i--)
    if(!strcasecmp(tmpn[i],member_name)) {
      if (use_as_expr) inst((*clss).LOAD,i);
      else need(EQ), expr(), inst((*clss).STORE,i);
      break;
    }
  if (i==-1) bad("UNEXIST CLASS MEMBER");
  return 0;
}

#endif // !EXTCLASS_H
