%{
%}
{
"|"		 OR
"&"		 AND
"="		 EQ
"<"		 LT
">"		 GT
","		 CM
"."		 DOT
":"		 MMB
";"		 SM
"!"		 NOT
"+"		 ADD
"-"		 SUB
"*"		 MUL
"/"		 DIV
"%"		 MOD
"("		 LP
")"		 RP
"["		 LB
"]"		 RB
"{"		 LC
"}"		 RC
"A"		 ALPHA
"0"		 NUMBER

"^"			HAT
"?"			QUE

"EOF"				EOF
}

Goal
	: Token
	;
Token
	: "<"					{{ $$ = $1; ((TTknLax)$$).tknSym = _PHTMLParser.eLT; }}
	| ">"					{{ $$ = $1; ((TTknLax)$$).tknSym = _PHTMLParser.eGT; }}
	| "<" "/"				{{ $$ = $1; ((TTknLax)$$).tknLen++; ((TTknLax)$$).tknSym = _PHTMLParser.eETAG; }}
	| Identifier			{{ $$ = $1; ((TTknLax)$$).tknSym = _PHTMLParser.eIDENTIFIER; }}
	;
Identifier
	: "A"
	| Identifier "A"				{{  $$ = $1; ((TTknLax)$$).tknLen++; }}
	;
