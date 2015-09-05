%{
%}
{
"!="		NE
"<="		LE
">="		GE
"="			EQ
"<"			LT
">"			GT
","			CM
"."			DOT
":"			MMB
";"			SM
"!"			EXC
"+"			ADD
"-"			SUB
"*"			MUL
"/"			DIV
"%"			MOD
"("			LP
")"			RP
"["			LB
"]"			RB
"{"			LC
"}"			RC

"^"			HAT
"||"		OR2
"&&"		AND2
"?"			QUE
":="		ASN
"*="		MULEQ
"/="		DIVEQ
"%="		MODEQ
"+="		ADDEQ
"-="		SUBEQ
"<<="		LSHEQ
">>="		RSHEQ
">>>="		URSHEQ
"&="		ANDEQ
"^="		HATEQ
"|="		OREQ
"++"		INC
"--"		DEC
"->"		RARROW
"~"			NOT
">>"		RSH
"<<"		LSH
">>>"		URSH
"=="		EQ2
"|-"		IMPLY

"else"		ELSE
"free"		FREE
"if"		IF
"iif"		IIF
"trace"		TRACE
"vars"		VARS

"ÅÕ"			ALL
"all"		ALL_2
"Å»"			AND
"&"			AND_2
"ÅŒ"			EXIST
"exist"		EXIST_2
"Å∏"			IN
"in"		IN_2
"Å…"			OR
"|"			OR_2
"Å∫"			SUBSET
"subset"	SUBSET_2
"Åø"			INTERSECTION
"Åæ"			UNION
"Å~"			CART

"$Head___Comment"		HEAD_COMMENT
"$Tail___Comment"		TAIL_COMMENT
"$IntegerLiteral"		INT_L
"$FloatingPointLiteral"	FLOAT_L
"$CharacterLiteral"		CHAR_L
"$StringLiteral"		STRING_L
"$Identifier"			IDENTIFIER

"EOF"				EOF
}

Goal
	: CompilationUnit
	;
HeadComment
	: "$Head___Comment"
	;
TailComment
	: "$Tail___Comment"
	;
Literal
	: "$IntegerLiteral"			{{ $$ = ZF.makeInt((TTknW)$1); }}
	| "$FloatingPointLiteral"	{{ $$ = ZF.makeFlt((TTknW)$1); }}
	| "$CharacterLiteral"		{{ $$ = ZF.makeChar((TTknW)$1); }}
	| "$StringLiteral"			{{ $$ = ZF.makeStr((TTknW)$1); }}
	;
Type
	: Term
	;
CompilationUnit
	: TheoremList
	| LogicList
	| "vars" VariableList				{{ $$ = $2; }}
	| Trace								
	;
TheoremList
	: Theorem							{{ $$ = ZF.makeVThr((TThr)$1); }}
	| TheoremList Theorem				{{ $$ = ZF.addThr((VThr)$1, (TThr)$2); }}
	;
Theorem
	: HeadComment_opt "$StringLiteral" "(" LogicSequence ")"		{{ $$ = ZF.makeThr2((TTknW)$1, (TTknW)$2, (TPrp)$4); }}
	;
FreeLogic
	: "free" VariableList "(" LogicSequence ")"			{{ $$ = ZF.makeQnt(ZF.e_log_free, (VVar)$2, (TLog)$4); }}
	;
VariableList
	: VariableComment												{{ $$ = ZF.makeVVar((TVar)$1); }}
	| VariableList "," TailComment_opt VariableComment				{{ $$ = ZF.addTailCommentVar((VVar)$1, (TTknW)$3, (TVar)$4); }}
	;
Variable
	: "$Identifier"									{{ $$ = ZF.makeVar((TTknW)$1, null); }}
	;
VariableComment
	: HeadComment_opt Variable TailComment_opt		{{ $$ = ZF.setComment((TTknW)$1, (TVar)$2, (TTknW)$3); }}
	;
Block
	: "{" Logic_opt "}"						{{ $$ = $2; }}
	;
Set
	: "{" TermList_opt "}"								{{ $$ = ZF.makeList((TObj)$2, true); }}
	| "{" TermList_opt "|" Logic  "}"					{{ $$ = ZF.makeSet2($2, (TLog)$4); }}
	;	
TermList
	: Term									{{ $$ = ZF.makeVTrm1((TTrm)$1); }}
	| TermList "," Term						{{ ZF.seqAdd((VTrm)$1, (TTrm)$3); $$ = $1; }}
	;
TermList2
	: Term "," Term							{{ $$ = ZF.makeVTrm2((TTrm)$1, (TTrm)$3); }}
	| TermList2 "," Term					{{ ZF.seqAdd((VTrm)$1, (TTrm)$3); $$ = $1; }}
	;
Tuple
	: "(" TermList_opt ")"					{{ $$ = ZF.makeTpl((TObj)$2); }}
	;
Sequence
	: "[" TermList_opt "]"					{{ $$ = ZF.makeList((TObj)$2, false); }}
	;
FieldAccess
	: Primary "." "$Identifier"				{{ $$ = ZF.makeAppDot((TObj)$1, (TTknW)$3); }}
	;
MethodInvocation
	: Primary Tuple							{{ $$ = ZF.makeApp((TTrm)$1 , (TObj)$2); }}
	;
Primary
	: Literal
	| "$Identifier"							{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| "(" Term ")"							{{ $$ = $2; }}
	| "(" TermList2 ")"						{{ $$ = ZF.makeTpl2((TObj)$2); }}
	| Sequence
	| Set
	| FieldAccess
	| MethodInvocation
	;
Unary
	: Primary
	| "+" Unary								{{ $$ = ZF.makeOpr1(ZF.e_opr_add, (TTrm)$2); }}
	| "-" Unary								{{ $$ = ZF.makeOpr1(ZF.e_opr_sub, (TTrm)$2); }}
	;
Multiplicative
	: Unary
	| Multiplicative "*" Unary				{{ $$ = ZF.makeOpr2(ZF.e_opr_mul, (TTrm)$1, (TTrm)$3); }}
	| Multiplicative "/" Unary				{{ $$ = ZF.makeOpr2(ZF.e_opr_div, (TTrm)$1, (TTrm)$3); }}
	| Multiplicative "%" Unary				{{ $$ = ZF.makeOpr2(ZF.e_opr_mod, (TTrm)$1, (TTrm)$3); }}
	| Multiplicative "Åø" Unary				{{ $$ = ZF.makeOpr2(ZF.e_opr_intersection, (TTrm)$1, (TTrm)$3); }}
	| Multiplicative "Å~" Unary				{{ $$ = ZF.makeAppCart((TTrm)$1, (TTrm)$3); }}
	;
Additive
	: Multiplicative
	| Additive "+" Multiplicative			{{ $$ = ZF.makeOpr2(ZF.e_opr_add, (TTrm)$1, (TTrm)$3); }}
	| Additive "-" Multiplicative			{{ $$ = ZF.makeOpr2(ZF.e_opr_sub, (TTrm)$1, (TTrm)$3); }}
	| Additive "Åæ" Multiplicative			{{ $$ = ZF.makeOpr2(ZF.e_opr_union, (TTrm)$1, (TTrm)$3); }}
	;
Term
	: Additive
	| Term "<<" Additive					{{ $$ = ZF.makeOpr2(ZF.e_opr_lsh, (TTrm)$1, (TTrm)$3); }}
	| Term ">>" Additive					{{ $$ = ZF.makeOpr2(ZF.e_opr_rsh, (TTrm)$1, (TTrm)$3); }}
	| Term ">>>" Additive					{{ $$ = ZF.makeOpr2(ZF.e_opr_ursh, (TTrm)$1, (TTrm)$3); }}
	;

Relation
	: Term "<" Term								{{ $$ = ZF.makeRel(ZF.e_rel_lt, (TTrm)$1, (TTrm)$3); }}
	| Term ">" Term								{{ $$ = ZF.makeRel(ZF.e_rel_gt, (TTrm)$1, (TTrm)$3); }}
	| Term "<=" Term							{{ $$ = ZF.makeRel(ZF.e_rel_le, (TTrm)$1, (TTrm)$3); }}
	| Term ">=" Term							{{ $$ = ZF.makeRel(ZF.e_rel_ge, (TTrm)$1, (TTrm)$3); }}
	| Term "=" Term								{{ $$ = ZF.makeRel(ZF.e_rel_eq, (TTrm)$1, (TTrm)$3); }}
	| Term "!=" Term							{{ $$ = ZF.makeRel(ZF.e_rel_ne, (TTrm)$1, (TTrm)$3); }}
	| Term "in" Type							{{ $$ = ZF.makeRel(ZF.e_rel_in, (TTrm)$1, (TTrm)$3); }}
	| Term "Å∏" Type								{{ $$ = ZF.makeRel(ZF.e_rel_in, (TTrm)$1, (TTrm)$3); }}
	| Term "subset" Type						{{ $$ = ZF.makeRel(ZF.e_rel_subset, (TTrm)$1, (TTrm)$3); }}
	| Term "Å∫" Type								{{ $$ = ZF.makeRel(ZF.e_rel_subset, (TTrm)$1, (TTrm)$3); }}
	| Term ":=" Term							{{ $$ = ZF.makeRelExt(ZF.e_rel_eq, TAlgo.e_rel_ext_asn, (TTrm)$1, (TTrm)$3); }}
	;
IfStatement
	: "if" "(" Logic ")" Block "else" Block			{{ $$ = ZF.makePrp3(ZF.e_log_if, (TLog)$3, (TLog)$5, (TLog)$7); }}
	| "if" "(" Logic ")" Block "else" IfStatement	{{ $$ = ZF.makePrp3(ZF.e_log_if, (TLog)$3, (TLog)$5, (TLog)$7); }}
	| "if" "(" Logic ")" Block						{{ $$ = ZF.makePrp2(ZF.e_log_if, (TLog)$3, (TLog)$5); }}
	;
	
Macro
	: "$Identifier" Sequence					{{ $$ = ZF.makeApp((TTrm)$1 , (TObj)$2); }}
	;
Statement
	: IfStatement
	| Relation
	| Assignment
	| "all" VariableList "(" Logic ")"			{{ $$ = ZF.makeQnt(ZF.e_log_all, (VVar)$2, (TLog)$4); }}
	| "ÅÕ" VariableList "(" Logic ")"			{{ $$ = ZF.makeQnt(ZF.e_log_all, (VVar)$2, (TLog)$4); }}
	| "exist" VariableList "(" Logic ")" 		{{ $$ = ZF.makeQnt(ZF.e_log_exist, (VVar)$2, (TLog)$4); }}
	| "ÅŒ" VariableList "(" Logic ")"	 		{{ $$ = ZF.makeQnt(ZF.e_log_exist, (VVar)$2, (TLog)$4); }}
	| Macro							{{ $$ = ZF.makeAsn(ZF.e_asn_eq, null, (TApp)$1); }}
	| "(" Or ")"				{{ $$ = $2; }}
	;
StatementComment
	: Statement TailComment_opt		{{ $$ = ZF.makeLog(null, (TObj)$1, (TTknW)$2); }}
	| HeadComment Statement TailComment_opt		{{ $$ = ZF.makeLog((TTknW)$1, (TObj)$2, (TTknW)$3); }}
	;
Not
	: StatementComment
	| "~" Not					{{ $$ = ZF.makePrp1(ZF.e_log_not, (TLog)$2); }}
	;
And
	: Not
	| And "Å»" TailComment_opt Not	{{ $$ = ZF.addTailCommentPrp2(ZF.e_log_and, (TLog)$1, (TTknW)$3, (TLog)$4); }}
	| And "&"  TailComment_opt Not	{{ $$ = ZF.addTailCommentPrp2(ZF.e_log_and, (TLog)$1, (TTknW)$3, (TLog)$4); }}
	;
Or
	: And
	| Or "Å…" And				{{ $$ = ZF.makePrp2(ZF.e_log_or, (TLog)$1, (TLog)$3); }}
	| Or "|"  And				{{ $$ = ZF.makePrp2(ZF.e_log_or, (TLog)$1, (TLog)$3); }}
	;
Logic
	: Or
	| Logic "|-" Or		{{ $$ = ZF.makePrp2(ZF.e_log_imply, (TLog)$1, (TLog)$3); }}
	| Logic "iif" Or			{{ $$ = ZF.makePrp2(ZF.e_log_iif, (TLog)$1, (TLog)$3); }}
	;
LogicList
	: Logic						{{ $$ = ZF.makeVLog((TLog)$1); }}
	| LogicList "," Logic		{{ ZF.seqAddUp((VLog)$1, (TLog)$3); $$ = $1; }}
	;
LogicSequence
	: LogicList_opt "->" LogicList_opt	{{ $$ = ZF.makeLogSeq((VLog)$1, (VLog)$3); }}
	;
Assignment
	: LeftHandSide "*="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_mul , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "/="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_div , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "%="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_mod , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "+="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_add , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "-="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_sub , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "<<="  Term			{{ $$ = ZF.makeAsn(ZF.e_asn_lsh , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide ">>="  Term			{{ $$ = ZF.makeAsn(ZF.e_asn_rsh , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide ">>>=" Term			{{ $$ = ZF.makeAsn(ZF.e_asn_ursh, (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "&="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_and , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "^="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_hat , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "|="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_or  , (TTrm)$1, (TTrm)$3); }}
	| PostIncrementExpression				{{ $$ = ZF.makeAsn(ZF.e_asn_eq  , null    , (TApp)$1); }}
	| PostDecrementExpression				{{ $$ = ZF.makeAsn(ZF.e_asn_eq  , null    , (TApp)$1); }}
	;
LeftHandSide
	: "$Identifier"			{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| FieldAccess
	| MethodInvocation
	;
PostIncrementExpression
	: Primary "++"								{{ $$ = ZF.makeOpr1(ZF.e_opr_inc, (TTrm)$1); }}
	;
PostDecrementExpression
	: Primary "--"								{{ $$ = ZF.makeOpr1(ZF.e_opr_dec, (TTrm)$1); }}
	;

Trace
	: "trace" "{" "$IntegerLiteral" "$IntegerLiteral" "{" PosSeq_opt "}" "$StringLiteral" "{" TraceList_opt "}" "}"	{{ $$ = ZF.makeTrc((TTknW)$3, (TTknW)$4, (VPos)$6, (TTknW)$8, (VTrc)$10); }}
	;
PosList
	: "$IntegerLiteral"						{{ $$ = ZF.makePos(null, (TTknW)$1); }}
	| PosList "," "$IntegerLiteral"			{{ $$ = ZF.makePos((SPos)$1, (TTknW)$3); }}
	;
PosSeq
	: "(" PosList ")"				{{ $$ = ZF.newVPos(5,5); ZF.seqAddUp((VPos)$$, (SPos)$2); }}
	| PosSeq "," "(" PosList ")"	{{ ZF.seqAddUp((VPos)$1, (SPos)$4); $$ = $1; }}
	;
TraceList
	: Trace						{{ $$ = ZF.newVTrc(5,5); ZF.seqAddUp((VTrc)$$, (STrc)$1); }}
	| TraceList "," Trace		{{ ZF.seqAddUp((VTrc)$1, (STrc)$3); $$ = $1; }}
	;
