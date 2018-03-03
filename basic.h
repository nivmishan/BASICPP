#pragma once
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define SYMSZ	16			/* SYMBOL SIZE */
#define PRGSZ	65536			/* PROGRAM SIZE */
#define STKSZ	256			/* STACK SIZE */
#define STRSZ	4096			/* STRING TABLE SIZE */
#define VARS	512			/* VARIABLE COUNT */
#define LOCS	16			/* LOCAL COUNT */
#define LINESZ	1024	/* SINGLE LINE STATEMENT SIZE */

typedef ptrdiff_t	Val;		/* SIGNED INT/POINTER */
typedef int		(*Code)();	/* BYTE-CODE */
#ifndef __cplusplus
typedef enum BOOLEAN { false=0, true=1 } bool;
#endif

#define NONE_MODE    0 /* DO NOT CHANGE NONE MODE VALUE! */
#define DIM_MODE     1
#define SUB_MODE     2
#define CLASS_MODE   3
#define OBJ_MODE     4

enum { NAME=1,NUMBER,STRING,NOT,LP,RP,COMMA,DOT,ADD,SUBS,MUL,DIV,MOD,
	EQ,LT,GT,NE,LE,GE,AND,OR,FORMAT,SUB,END,RETURN,LOCAL,WHILE,FOR,
	TO,IF,ELSE,THEN,DIM,UBOUND,BYE,BREAK,CONTINUE,CLASS,THIS,PRINT,
  EXIT,STOP };
char	*kwd[]={ "AND","OR","FORMAT","SUB","END","RETURN","LOCAL","WHILE",
	"FOR","TO","IF","ELSE","THEN","DIM","UBOUND","BYE","BREAK","CONTINUE",
	"CLASS","THIS","PRINT","EXIT","STOP",0 };

char	flbuf[LINESZ]={0},lbuf[LINESZ],tokn[SYMSZ],*flp,*lp;	/* LEXER STATE */
int	cnum,lnum,tok,tokv,ungot;		/* LEXER STATE */
int	(*prg[PRGSZ])(),(**pc)(),cpc,fmap[PRGSZ][2]; /* COMPILED PROGRAM */
Val	stk[STKSZ],*sp;			/* RUN-TIME STACK */
Val	*value[VARS], data[VARS];			/* VARIABLE VALUES */
char	name[VARS][SYMSZ];		/* VARIABLE NAMES */
int	owner[VARS]; /* VAIRABLE OWNERS */
int	sub[VARS][LOCS+2];		/* N,LOCAL VAR INDEXES */
int	mode[VARS];
Val	ret;				/* FUNCTION RETURN VALUE */
int	cstk[STKSZ], *csp;		/* COMPILER STACK */
bool local; /* USE LOCAL VAIRABLE ONLY */
int	exitcode,nvar,cursub,curclass,temp,
    compile,ipc,(**opc)(); /* COMPILER STATE */
char	stab[STRSZ], *stabp;		/* STRING TABLE */
jmp_buf	trap;				/* TRAP ERRORS */

#define A	sp[1]			/* LEFT OPERAND */
#define B	sp[0]			/* RIGHT OPERAND */
#define PCV	((Val)*pc++)		/* GET IMMEDIATE */
#define STEP	return 1		/* CONTINUE RUNNING */
#define DRIVER	while ((*pc++)())	/* RUN PROGRAM */
#define EMPTY (-1) /* WHEN GENRAL THEN OWNER=(-1)*/
#define CURSUB (cursub!=EMPTY) /* CURCSUB DEFINED */
#define CURCLASS (curclass!=EMPTY) /* CURCLASS DEFINED */
#define LOC(N) (*value)[sub[v][N+2]]	/* SUBROUTINE LOCAL */
#define THIS(V,N) value[sub[V][N+2]]  /* CLASS POINTER TO LOCAL */
#define SCLS sub[cls][n+2] /* LOCAL N OF CLASS */
#define SOBJ sub[obj][n+2] /* LOCAL N OF OBJECT */
Val *bound(Val *mem, int n) { if (n<1 || n>*mem) err("BOUNDS"); return mem+n;  }

int	(*kwdhook)(char *kwd);		/* KEYWORD HOOK */
int	(*exprhook)(char *kwd);	/* FUNCTION CALL HOOK */
int	(*errhook)(int code);	/* ERROR FUNCTION HOOK */

initbasic(int comp) {
	cursub=curclass=EMPTY; flp=flbuf; pc=prg;
	sp=stk+STKSZ; csp=cstk+STKSZ; stabp=stab; compile=comp;
	int i=VARS; while (i--)	value[i]=&(data[i]);
}
#define ERRMSG "ERROR [%d:%d]: %s\n" // ,line, column, error-msg
bad(char *msg) { printf(ERRMSG, lnum, cnum, msg); longjmp(trap,1); }
err(char *msg) {
	printf("RUNTIME "ERRMSG,fmap[pc-prg-1][0],fmap[pc-prg-1][1],msg);
	longjmp(trap,2);
}
emit(int opcode()) { fmap[cpc][0]=lnum,fmap[cpc][1]=cnum,prg[cpc++]=opcode; }
inst(int opcode(), Val x) { emit(opcode); emit((Code)x); }
BYE_() { longjmp(trap,4); }
STOP_() { longjmp(trap,3); }
NUMBER_() { *--sp=PCV; STEP; }
LOAD_() { *--sp=(*value)[PCV]; STEP; }
STORE_() { (*value)[PCV]=*sp++; STEP; }
ECHO_() { printf("%d\n",*sp++); }
PRINTNL_() { putchar('\n'); STEP; }
PRINT_() { char *f; Val n=PCV, *ap=(sp+=n)-1;
	for (f=stab + *sp++; *f; f++)
		if (*f=='%') printf("%d", (int)*ap--);
		else if (*f=='$') printf("%s", (char*)*ap--);
		else putchar(*f);
	STEP;
}
ADD_() { A+=B; sp++; STEP; };
SUBS_() { A-=B; sp++; STEP; };
MUL_() { A*=B; sp++; STEP; };
DIV_() { if (!B) sp+=2,err("DIVISION BY ZERO"); A/=B; sp++; STEP; };
MOD_() { if (!B) sp+=2,err("MODULUS OF ZERO"); A%=B; sp++; STEP; };
NOT_() { A=B? 0: 1; sp++; STEP; };
EQ_() { A=(A==B)? 1: 0; sp++; STEP; };
LT_() { A=(A<B)? 1: 0; sp++; STEP; };
GT_() { A=(A>B)? 1: 0; sp++; STEP; };
NE_() { A=(A!=B)? 1: 0; sp++; STEP; };
LE_() { A=(A<=B)? 1: 0; sp++; STEP; };
GE_() { A=(A>=B)? 1: 0; sp++; STEP; };
AND_() { A&=B; sp++; STEP; };
OR_() { A|=B; sp++; STEP; };
JMP_() { pc=prg+(int)*pc; STEP; }
FALSE_() { if (*sp++) pc++; else pc=prg+(int)*pc; STEP; }
FOR_() { if ((*value)[PCV] >= *sp) pc=prg+(int)*pc, sp++; else PCV; STEP; }
BREAK_() {
	if (prg[(int)*pc-2]==FOR_) sp++;
	pc=prg+(int)prg[(int)*pc];
	STEP;
}
CONTINUE_() { pc=prg+(int)prg[(int)*pc]-4; STEP; }
NEXT_() { (*value)[PCV]++; STEP; }
CALLSUB_() { Val v=PCV, n=sub[v][1], x, *ap=sp;
	while (n--) { x=LOC(n); LOC(n)=*ap; *ap++=x; }
	for (n=sub[v][1]; n<sub[v][0]; n++) *--sp=LOC(n);
	*--sp=pc-prg;
	pc=prg+(*value)[v];
	STEP;
}
RETURN_() { int v=PCV, n=sub[v][0];
	pc=prg+*sp++;
	while (n--) LOC(n)=*sp++;
	STEP;
}
SETRET_() { ret=*sp++; STEP; }
RV_() { *--sp=ret; STEP; }
classpoint(int cls, int obj) {
	int n=sub[cls][0];
	while (n--) {
		if (mode[SCLS]==OBJ_MODE)
			classpoint(SCLS,SOBJ);
		else if(mode[SCLS]!=SUB_MODE)
			THIS(cls,n)=THIS(obj,n);
	}
}
CLASSPOINT_() { Val obj=PCV; classpoint((*value)[obj],obj); STEP; }
CALLCLASS_() {
	Val v=PCV, n=sub[v][1];
	while (n--)	LOC(n)=*sp++;
	classpoint((*value)[v], v); /* (CLASS, OBJ) */
	*--sp=pc-prg;
	pc=prg+(*value)[(*value)[v]];
	STEP;
}
ENDCLASS_() { Val v=PCV; pc=prg+*sp++; STEP; }
copyobj(int src, int dst) {
		int n=sub[src][0];
		while (n--) {
			(*value)[sub[dst][n+2]]=(*value)[sub[src][n+2]];
			if (mode[sub[src][n+2]]==OBJ_MODE)
				copyobj(sub[src][n+2],sub[dst][n+2]);
		}
}
COPYOBJ_() {
	int src=*sp++, dst=*sp++;
	if (mode[dst]!=OBJ_MODE)	err("DESTINATION MUST BE AN OBJECT");
	if (mode[src]!=OBJ_MODE)	err("SOURCE MUST BE AN OBJECT");
	if ((*value)[src]!=(*value)[dst])	err("CANT COPY OBJECTS OF DIFFERENT CLASSES");
	copyobj(src,dst); STEP;
}
DROP_() { sp+=PCV; STEP; }
EXIT_() { exitcode=*sp++; longjmp(trap,5); STEP; }
DIM_() { int v=PCV, n=*sp++; Val *mem=calloc(sizeof(Val),n+1); /*FIXME*/
	mem[0]=n; (*value)[v]=(Val)mem;
}
LOADI_() { Val x=*sp++; x=*bound((Val*)(*value)[PCV],x); *--sp=x; STEP; }
STOREI_() { Val x=*sp++, i=*sp++; *bound((Val*)(*value)[PCV],i)=x; STEP; }
UBOUND_() { *--sp=*(Val*)(*value)[PCV]; STEP; }
classdef(){ return (!CURSUB && CURCLASS); } /* IF IN THE MIDDLE OF CLASS DEF */
createobj(Val cls, Val obj) { /* CREATE OBJECT BY CLASS */
	int v, n, bu=curclass;
	mode[curclass=obj]=OBJ_MODE, (*value)[obj]=cls;
	sub[obj][0]=sub[cls][0], sub[obj][1]=sub[cls][1]; /* LOCAL,PARAM */
	for (n=0; n<sub[cls][0]; n++) {
		v=SOBJ=find(name[SCLS]), mode[v]=mode[SCLS];
		if (mode[v]==OBJ_MODE)
			createobj((*value)[SCLS],SOBJ); /* COPY SUB OBJECTS */
		else if (mode[v]==SUB_MODE) {
			*THIS(obj,n)=SCLS;
			sub[SOBJ][0]=sub[SCLS][0]; /* SET LOCAL COUNT */
			sub[SOBJ][1]=sub[SCLS][1]; /* SET PARAM COUNT */
		}
	}
	curclass=bu;
}
int relatefind(char *var, int own) { /* FIND NAMES OF SPECIFIC OWNER */
	int i=nvar;
	while (i-- && (owner[i]!=own || strcmp(var,name[i])));
	return i;
}
find(char *var) {
	int	i=EMPTY;
	if (CURSUB)	i=relatefind(var,cursub); /* SEARCH IN LOCAL */
	if (!local && i==EMPTY) {
		if(CURSUB)
			i=((i=relatefind(var,EMPTY))!=EMPTY || !CURCLASS) ?i :relatefind(var,curclass);
		else	i=relatefind(var,curclass); /* CURCLASS MAY BE 'EMPTY' */
	}
	if (i==EMPTY) {
		if (strlen(var)>=SYMSZ)	bad("SYMBOL NAME TOO LONG");
		i=nvar++, strcpy(name[i], var);
		if (local) owner[i]=cursub;
		else if (classdef())
			owner[i]=curclass, sub[curclass][sub[curclass][0]+++2]=i;
		else	owner[i]=EMPTY;
	}
	return i;
}
escape() { /* READ ESCAPE SEQUENCE */
	long n;
	char *p, tmpstr[17], *tsp=tmpstr;
	static const char *escp="\a\b\f\n\r\t\v\e\\\'\"\?",
	                  *escpmap="abfnrtve\\\'\"\?",
	                  unisz[]={16,4,8},
	                  *unimap="xuU";
	if (*lp++=='\\')
		if (p=strchr(escpmap,(char)*lp)) /* e.g '\n','\t'... */
			n=escp[p-escpmap], ++lp;
		else if (p=strchr(unimap,*lp)) { /* e.g '\x6AB2','\u654'... */
			// TODO: add real unicode escape support
			n=unisz[p-unimap], ++lp;
			while (isxdigit(*lp) && n--) *tsp++=*lp++;
			*tsp=0, n=strtol(tmpstr,NULL,16);
		} else if (isdigit(*lp) && *lp<'8' && (n=4)) { /* e.g '\345', '\0'... */
			while (isdigit(*lp) && *lp<'8' && n--)	*tsp++=*lp++;
			p=1, *tsp=0, n=strtol(tmpstr,NULL,8);
		}

	if (!p)	bad("UNKNOWN ESCAPE SEQUENCE");
	return n;
}
read() {	/* READ TOKEN */
	unsigned int i;
	char *p,*d,**k, *pun="!(),.+-*/%=<>", *dub="!=<=>=";
	if (ungot) return ungot=0, tok;	/* UNGOT PREVIOUS */
	while (isspace(*lp)) lp++;	/* SKIP SPACE */
	if (!*lp) return tok=0; /* END OF LINE */
	cnum=flp-flbuf-strlen(lbuf)+lp-lbuf+1;
	if (isdigit(*lp))		/* NUMBER */
		return tokv=strtol(lp,&lp,0), tok=NUMBER;
	if ((p=strchr(pun,*lp)) && lp++) { /* PUNCTUATION */
		for (d=dub; *d && strncmp(d,lp-1,2); d+=2);
		if (!*d) return tok=(p-pun)+NOT;
		return lp++, tok=(d-dub)/2+NE;
	} else if (isalpha(*lp)) {	/* IDENTIFIER */
		for (p=tokn; isalnum(*lp) || *lp=='_'; ) *p++=toupper(*lp++);
		for (*p=0, k=kwd; *k && strcmp(tokn,*k); k++);
		if (*k) return tok=(k-kwd)+AND;
		return tokv=find(tokn), tok=NAME;
	} else if ((*lp=='"' || *lp=='\'') && (d=lp) && lp++) {	/* STRING */
		for (p=stabp; *lp && *lp!=*d; )
			*stabp++=(*lp=='\\' ?escape() :*lp++);
		return *stabp++=0, lp++, tokv=p-stab, tok=STRING;
	} else	return bad("BAD TOKEN");
}
want(int type) { return !(ungot=read()!=type); }
need(int type) { if (!want(type)) bad("SYNTAX ERROR"); }
purgename(int v) { int n;
	n=sub[v][0]; while (n--)	purgename(sub[v][n+2]), sub[v][n+2]=NULL;
	name[v][0]=(*value)[v]=mode[v]=owner[v]=sub[v][0]=sub[v][1]=NULL;
}
#define LIST(BODY) if (!want(0)) do {BODY;} while (want(COMMA))
loadname(int var) {
	int n=0, v;
	switch (mode[var]) {
	case DIM_MODE:
		need(LP), expr(), need(RP), inst(LOADI_, var); break;
	case SUB_MODE:
		need(LP);
		LIST(if (tok==RP) break; expr(); n++);
		need(RP);
		if (n!=sub[var][1])	bad("BAD ARG COUNT");
		if (owner[var]!=EMPTY)
			inst(CLASSPOINT_, owner[var]), var=(*value)[var];
		inst(CALLSUB_, var), emit(RV_);
		break;
	case OBJ_MODE:
	case CLASS_MODE:
		need(DOT), n=nvar, need(NAME);
		if ((v=relatefind(name[tokv],var))==EMPTY)
			bad("UNEXIST CLASS MEMBER");
		if (n<nvar)	purgename(--nvar);
		return loadname(v);
		break;
	default:
		if (want(LP))	return 1;
		inst(LOAD_, var);
	}
	return 0;
}
base() {		/* BASIC EXPRESSION */
	int n=0,var, nt=want(NOT)? (inst(NUMBER_,0),1): 0,
	    neg=want(SUBS)? (inst(NUMBER_,0),1): 0;
	if (want(NUMBER))	inst(NUMBER_, tokv);
	else if (want(STRING))	inst(NUMBER_, (Val)(stab+tokv));
	else if (want(THIS)) {
		if (!CURCLASS)	bad("\"THIS\" STATEMENT MUST BE INSIDE CLASS");
		loadname(curclass);
	} else if (want(NAME)) {
		if (!exprhook || !exprhook(name[var=tokv]))
			if(mode[var]==CLASS_MODE || loadname(var))
				bad("BAD SUB/ARG COUNT");
	} else if (want(LP))	expr(), need(RP);
	else if (want(UBOUND))	need(LP),need(NAME),need(RP),inst(UBOUND_,tokv);
	else			bad("BAD EXPRESSION");
	if (neg)		emit(SUBS_);	/* NEGATE */
	if (nt)		emit(NOT_);	/* NEGATE */
}
int (*bin[])()={ADD_,SUBS_,MUL_,DIV_,MOD_,EQ_,LT_,GT_, NE_,LE_,GE_,AND_,OR_};
#define BIN(NAME,LO,HI,ELEM)  NAME() { int (*o)(); \
	ELEM(); \
	while (want(0), LO<=tok && tok<=HI) \
		o=bin[tok-ADD], read(), ELEM(), emit(o); \
	return 0; }
BIN(factor,MUL,MOD,base)
BIN(addition,ADD,SUBS,factor)
BIN(relation,EQ,GE,addition)
BIN(expr,AND,OR,relation)
char * strcasestr(char *str, const char *substr) {
  const char *sp, *sbp;
  char sb0=toupper(*substr);
  if (sb0==0)  return str;
  do {
    if (toupper(*str)==sb0)
      for (sp=str+1, sbp=substr+1;
           toupper(*sp)==toupper(*sbp) || !*sbp;
           sp++, sbp++)
        if (!*sbp) return str;
  } while (*++str);
  return (char*)0;
}
usename(var) {
	int n,v;
	if (mode[var]==OBJ_MODE || mode[var]==CLASS_MODE) {
		if (want(EQ))
			need(NAME),inst(NUMBER_,var),inst(NUMBER_,tokv),emit(COPYOBJ_);
		else {
			need(DOT),n=nvar,need(NAME);
			if ((v=relatefind(name[tokv],var))==EMPTY)
				bad("UNEXIST CLASS MEMBER");
			if (n<nvar)	purgename(--nvar);
			return usename(v);
		}
	} else if (want(EQ))
		expr(), inst(STORE_, var);
	else if (mode[var]==SUB_MODE) {
		if (classdef())	bad("BAD CLASS MEMBER");
		v=want(LP), n=0;
		LIST(if (v && tok==RP) break; expr(); n++);
		if (v)	need(RP);
		if (n!=sub[var][1])	bad("BAD ARG COUNT");
		if (owner[var]!=EMPTY)
			inst(CLASSPOINT_, owner[var]), var=(*value)[var];
		inst(CALLSUB_, var);
	} else if (want(LP))
		expr(),need(RP),need(EQ),expr(),inst(STOREI_,var);
	else	return 1;
	return 0;
}
stmt() {	/* STATEMENT */
	int	n,var,type,*intp;
	char *tmpp;
	switch (type=read()) {
	case SUB:	/* CSTK: {CLASS,INDEX,JMP} */
		if (CURSUB)	bad("SUB CAN NOT BE NESTED");
		if (!compile) bad("SUB MUST BE COMPILED");
		compile++;			/* MUST BALANCE WITH END */
		need(NAME), mode[cursub=var=tokv]=SUB_MODE; /* SUB NAME */
		n=0; LIST(need(NAME); sub[var][n+++2]=tokv); /* PARAMS */
		*--csp=cpc+1, inst(JMP_,0);	/* JUMP OVER CODE */
		sub[var][0]=sub[var][1]=n;	/* LOCAL=PARAM COUNT */
		(*value)[var]=cpc;			/* ADDRESS */
		*--csp=var, *--csp=SUB;		/* FOR "END" CLAUSE */
		break;
	case CLASS: /* CSTK: {SUB,INDEX,JMP} */
		if (CURCLASS)	bad("CLASS CAN NOT BE NESTED");
		if (!compile)	bad("CLASS MUST BE COMPILED");
		compile++;			/* MUST BALANCE WITH END */
		need(NAME), mode[curclass=var=tokv]=CLASS_MODE; /* CLASS NAME */
		n=0; LIST(need(NAME); sub[var][n+++2]=tokv); /* PARAMS */
		*--csp=cpc+1, inst(JMP_,0);	/* JUMP OVER CODE */
		sub[var][0]=sub[var][1]=n;	/* LOCAL=PARAM COUNT */
		(*value)[var]=cpc;			/* ADDRESS */
		*--csp=var, *--csp=CLASS;	/* FOR "END" CLAUSE */
		break;
	case NAME:
		var=tokv;
		int dst;
		if (!classdef() && kwdhook && kwdhook(tokn));
		else if (mode[var]==CLASS_MODE) {
				need(NAME),createobj(var,dst=tokv);
				n=0; LIST(expr(); n++);
				if (n!=sub[var][1])	bad("BAD ARG COUNT");
				inst(CALLCLASS_,dst);
		} else if (!usename(var));
		else bad("BAD SUB"); /*FIXME:add purgename?*/
		break;
	case THIS:
		if (!CURCLASS)	bad("\"THIS\" STATEMENT MUST BE INSIDE CLASS");
		usename(curclass);
		break;
	case DIM:
		need(NAME), mode[var=tokv]=DIM_MODE;	/* SET VAR MODE TO DIM */
		need(LP), expr(), need(RP), inst(DIM_, var);
		break;
	case STOP: emit(STOP_); break;
	case END:
		need(*csp++), compile--;		/* MATCH BLOCK */
		if (csp[-1]==SUB) {
			inst(RETURN_, *csp++);
			cursub=EMPTY;
			prg[*csp++]=(Code)cpc;		/* PATCH JUMP */
		} else if (csp[-1]==CLASS) {
			inst(ENDCLASS_, *csp++);
			curclass=EMPTY;
			prg[*csp++]=(Code)cpc;		/* PATCH JUMP */
		} else if (csp[-1]==WHILE) {
			prg[*csp++]=(Code)(cpc+2);	/* PATCH TEST */
			inst(JMP_, *csp++);		/* LOOP TO TEST */
		} else if (csp[-1]==FOR) {
			prg[*csp++]=(Code)(cpc+4);	/* PATCH TEST */
			inst(NEXT_, *csp++);		/* INCREMENT */
			inst(JMP_, *csp++);		/* LOOP TO TEST */
			temp--;				/* ONE LESS TEMP */
		} else if (csp[-1]==IF) {
			for (n=*csp++; n--; )		/* PATCH BLOCK ENDS */
				prg[*csp++]=(Code)cpc;
			if (n=*csp++) prg[n]=(Code)cpc; /* PATCH "ELSE" */
		}
		break;
	default:
		if (classdef() && tok)	bad("BAD CLASS MEMBER");
		switch(type) {
		case FORMAT:
		case PRINT:
			need(STRING), inst(NUMBER_, tokv);
			n=0; if (want(COMMA)) LIST(expr(); n++);
			inst(PRINT_, n);
			if (type==FORMAT) emit(PRINTNL_);
			break;
		case EXIT:
			if (want(0)) inst(NUMBER_, 0);
			else expr();
			emit(EXIT_);
			break;
		case LOCAL:
			local=true;
			LIST(need(NAME); sub[cursub][sub[cursub][0]+++2]=tokv;);
			local=false;
			break;
		case RETURN:
			if (temp) inst(DROP_, temp);
			if (!want(0))	expr(), emit(SETRET_);
			inst(RETURN_, cursub);
			break;
		case WHILE:	/* CSTK: {WHILE,TEST-FALSE,TOP} */
			compile++;			/* BODY IS COMPILED */
			*--csp=cpc, expr();
			*--csp=cpc+1, *--csp=WHILE, inst(FALSE_, 0);
			break;
		case FOR:	/* CSTK: {FOR,TEST-FALSE,I,TOP}; STK:{HI} */
			compile++;			/* BODY IS COMPILED */
			need(NAME), var=tokv, temp++;
			need(EQ), expr(), inst(STORE_,var);
			need(TO), expr();
			*--csp=cpc, inst(FOR_,var), emit(0);
			*--csp=var, *--csp=cpc-1, *--csp=FOR;
			break;
		case IF:	/* CSTK: {IF,N,ENDS...,TEST-FALSE} */
			expr(), inst(FALSE_,0), *--csp=cpc-1;
			if (want(THEN)) { stmt(); prg[*csp++]=(Code)cpc; }
			else	compile++, *--csp=0, *--csp=IF;
			break;
		case ELSE:
			n=csp[1]+1;
			inst(JMP_,0);			/* JUMP OVER "ELSE" */
			*--csp=IF, csp[1]=n, csp[2]=cpc-1; /* ADD A FIXUP */
			prg[csp[2+n]]=(Code)cpc;	/* PATCH "ELSE" */
			csp[2+n]=!want(IF)? 0:		/* "ELSE IF" */
				(expr(), inst(FALSE_,0), cpc-1);
			break;
		case BREAK:
		case CONTINUE:
			intp=csp;
			do if (*intp==FOR || *intp==WHILE) break;
			while (cstk+STKSZ-intp++);
			if (*intp!=FOR && *intp!=WHILE)
				bad("BREAK/CONTINUE ARE ONLY LOOP MEMBERS");
			if (type==BREAK) inst(BREAK_,intp[1]);
			else { // if (type==CONTINUE)
				if (*intp==WHILE) inst(JMP_,intp[2]);
				else inst(CONTINUE_,intp[1]); // if (*intp==FOR)
			}
			break;
		case BYE:		emit(BYE_); break;
		case GT:		expr(); emit(ECHO_); break;
		default:		if (tok) bad("BAD STATEMENT");
		}
	}
	if (!want(0))		bad("TOKENS AFTER STATEMENT");
}
/* FIXME: find the bug - when writing "A=12/0" and then
   "break", its tell that the "break" is an error */
interp(FILE *sf) {	/* INTERPRETER LOOP */
	for (;;) {
		int code=setjmp(trap);			/* RETURN ON ERROR */
		if (errhook) code=errhook(code);
		if (code==1) /* FILE SYNTAX ERROR */
			if (sf==stdin) {
				*flp='\0'; /* SET READ NEW LINE */
				if (compile) {
					printf("%d ENDING COMPILE MODE\n",compile);
					cursub=EMPTY, curclass=EMPTY; /* END CLASS OR SUB DEFINITION */
					compile=0, code=3/*=BREAK_*/;
				}
			} else return 1;	/* FILE SYNTAX ERROR */
		if (code==2) opc=pc, !compile? cpc=ipc: 0;			/* FAULT */
		if (code==3) pc=opc?opc:pc, cpc=ipc;	/* "BREAK" */
		if (code==4) return 0;			/* "BYE" */
		if (code==5) exit(exitcode);
		for (;;) {
			if (!(*flp)) {
				if (sf==stdin)
					printf("%3d%*c ",lnum+1,compile*2+1,'>');
				if (!fgets(flp=flbuf,sizeof flbuf,sf)) break;
				if (lp=strchr(flp,'#')) *lp=0;
				lnum++;
			}
			lp=lbuf;
			while (*flp && *flp!=';') *lp++=*flp++;
			if (*flp==';') flp++;
			*lp='\0', lp=lbuf; /* SET SINGLE STATEMENT */
			ungot=0, stmt();	/* PARSE AND COMPILE */
			if (compile) continue;		/* CONTINUE COMPILING */
			opc=pc, pc=prg+ipc;		/* START OF IMMEDIATE */
			emit(STOP_); DRIVER;		/* RUN STATEMENT */
		}
		ipc=cpc+1, compile=0, fclose(sf), sf=stdin; /* DONE COMPILING */
		emit(BYE_); DRIVER;			/* RUN PROGRAM */
	}
}
