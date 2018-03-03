#include <stdio.h>
#include "basic.h"
#include "extclass.h"

// HOW TO CREATE NEW EXTERNAL CLASS:
POW_() {
	int pwr=*sp++,n=*sp++,res=1,neg=pwr<0? pwr*=-1,1 : 0;
	while (pwr--)	res*=n;
	if (neg) res=1/res;
	CLASSRETURN(res);
}
math_pow() {
	int n=want(LP);
	expr(), need(COMMA), expr(), emit(POW_);
	if (n) need(RP);
}
CLASSMACRO(math,
           {"pi" CMA "circle"},{314 CMA 360},
           {"pow"},{math_pow})

// HOW TO CREATE NEW KEYWORD:
OK_() { puts("OK!"); STEP; }

kwdhook_(char *msg) {
	if (!strcmp("OK",msg)) emit(OK_);
	else if(!strcasecmp(math.name,msg)) {
		need(DOT), need(NAME);
		useclass(&math,tokn,false);
	} else return 0;
	return 1;
}

exprhook_(char *msg) {
	if(!strcasecmp(math.name,msg)) {
		need(DOT), need(NAME);
		useclass(&math,tokn,true);
	} else return 0;
	return 1;
}

main(int argc, char **argv) {
	FILE *sf=stdin;
	initbasic(0);
	kwdhook=kwdhook_;
	exprhook=exprhook_;
	if (argv[1])
		if (sf=fopen(argv[1],"r"))
			compile++;
		else {
			printf("CANNOT OPEN: %s\n", argv[1]);
			return 255;
		}
	return interp(sf);
}
