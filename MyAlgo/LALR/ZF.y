%{
%}
{
"!="		 NE
"<="		 LE
">="		 GE
"|"		 OR
"&"		 AND
"="		 EQ
"<"		 LT
">"		 GT
","		 CM
"."		 DOT
":"		 MMB
";"		 SM
"!"		 EXC
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

"^"			HAT
"||"			OR2
"&&"			AND2
"?"			QUE
":="			ASN
"*="			MULEQ
"/="			DIVEQ
"%="			MODEQ
"+="			ADDEQ
"-="			SUBEQ
"<<="		LSHEQ
">>="		RSHEQ
">>>="		URSHEQ
"&="			ANDEQ
"^="			HATEQ
"|="			OREQ
"++"			INC
"--"			DEC
"->"			RARROW
"~"			NOT
">>"			RSH
"<<"			LSH
">>>"		URSH
"=="			EQ2

"abstract"				ABSTRACT
"all"					ALL
"break"					BREAK
"case"						CASE
"catch"					CATCH
"continue"				CONTINUE
"default"				DEFAULT
"do"							DO
"each"					EACH
"else"						ELSE
"exist"				EXIST
"extends"				EXTENDS
"final"					FINAL
"finally"				FINALLY
"for"						FOR
"free"						FREE
"if"							IF
"iif"							IIF
"implements"			IMPLEMENTS
"imply"					IMPLY
"import"					IMPORT
"in"							IN
"instanceof"			INSTANCEOF
"interface"			INTERFACE
"native"					NATIVE
"package"				PACKAGE
"private"				PRIVATE
"protected"			PROTECTED
"public"					PUBLIC
"set"					SET
"static"					STATIC
"subset"					SUBSET
"super"					SUPER
"switch"					SWITCH
"synchronized"		SYNCHRONIZED
"there"						THERE
"throw"					THROW
"throws"					THROWS
"trace"				TRACE
"transient"			TRANSIENT
"try"						TRY
"vars"				VARS
"volatile"				VOLATILE
"while"					WHILE

"$Head___Comment"		HEAD_COMMENT
"$Tail___Comment"		TAIL_COMMENT
"$ZFstart"				ZF_START
"$ZFend"				ZF_END
"$IntegerLiteral"		INT_L
"$FloatingPointLiteral"		FLOAT_L
"$CharacterLiteral"		CHAR_L
"$StringLiteral"			STRING_L
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
	| "vars" VariableList				{{ $$ = $2; }}
	| Trace								
	;
TheoremList
	: Theorem							{{ $$ = ZF.makeVThr((TThr)$1); }}
	| TheoremList Theorem				{{ $$ = ZF.addThr((VThr)$1, (TThr)$2); }}
	;
Theorem
	: HeadComment_opt "$StringLiteral" HeadComment_opt FreeLogic		{{ $$ = ZF.makeThr((TTknW)$1, (TTknW)$2, (TTknW)$3, (TQnt)$4); }}
	;
FreeLogic
	: "free" "(" VariableList ")" "{" LogicSequence "}"			{{ $$ = ZF.makeQnt(ZF.e_log_free, (VVar)$3, (TLog)$6); }}
	;
VariableList
	: VariableComment												{{ $$ = ZF.makeVVar((TVar)$1); }}
	| VariableList "," TailComment_opt VariableComment				{{ $$ = ZF.addTailCommentVar((VVar)$1, (TTknW)$3, (TVar)$4); }}
	;
Variable
	: "$Identifier" "in" Type						{{ $$ = ZF.makeVar((TTknW)$1, (TTrm)$3); }}
	| "$Identifier"									{{ $$ = ZF.makeVar((TTknW)$1, null); }}
	;
VariableComment
	: HeadComment_opt Variable TailComment_opt		{{ $$ = ZF.setComment((TTknW)$1, (TVar)$2, (TTknW)$3); }}
	;
Block
	: "{" Logic_opt "}"						{{ $$ = $2; }}
	;
Set
	: "set" "(" VariableList_opt ")" "(" VariableList_opt ")" Block	{{ $$ = ZF.makeSet(2, (VVar)$3, (VVar)$6, (TLog)$8); }}
	| "set" "(" VariableList_opt ")" Block								{{ $$ = ZF.makeSet(1, (VVar)$3, null, (TLog)$5); }}
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
	;
Additive
	: Multiplicative
	| Additive "+" Multiplicative			{{ $$ = ZF.makeOpr2(ZF.e_opr_add, (TTrm)$1, (TTrm)$3); }}
	| Additive "-" Multiplicative			{{ $$ = ZF.makeOpr2(ZF.e_opr_sub, (TTrm)$1, (TTrm)$3); }}
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
	| Term "subset" Type						{{ $$ = ZF.makeRel(ZF.e_rel_subset, (TTrm)$1, (TTrm)$3); }}
	;
IfStatement
	: "if" "(" Or2 ")" Block "else" Block			{{ $$ = ZF.makePrp3(ZF.e_log_if, (TLog)$3, (TLog)$5, (TLog)$7); }}
	| "if" "(" Or2 ")" Block "else" IfStatement	{{ $$ = ZF.makePrp3(ZF.e_log_if, (TLog)$3, (TLog)$5, (TLog)$7); }}
	| "if" "(" Or2 ")" Block						{{ $$ = ZF.makePrp2(ZF.e_log_if, (TLog)$3, (TLog)$5); }}
	;
Statement2
	: Relation
	| "(" Or2 ")"				{{ $$ = $2; }}
	;
Not2
	: Statement2
	| "~" Not2					{{ $$ = ZF.makePrp1(ZF.e_log_not, (TLog)$2); }}
	;
And2
	: Not2
	| And2 "&" Not2				{{ $$ = ZF.makePrp2(ZF.e_log_and, (TLog)$1, (TLog)$3); }}
	;
Or2
	: And2
	| Or2 "|" And2				{{ $$ = ZF.makePrp2(ZF.e_log_or , (TLog)$1, (TLog)$3); }}
	;
Statement
	: IfStatement
	| Relation
	| Assignment
	| "all" "(" VariableList ")" Block			{{ $$ = ZF.makeQnt(ZF.e_log_all, (VVar)$3, (TLog)$5); }}
	| "exist" "(" VariableList ")" Block		{{ $$ = ZF.makeQnt(ZF.e_log_exist, (VVar)$3, (TLog)$5); }}
	| MethodInvocation							{{ $$ = ZF.makeAsn(ZF.e_asn_eq, null, (TApp)$1); }}
	| Block "imply" Block		{{ $$ = ZF.makePrp2(ZF.e_log_imply, (TLog)$1, (TLog)$3); }}
	| Block "iif" Block			{{ $$ = ZF.makePrp2(ZF.e_log_iif, (TLog)$1, (TLog)$3); }}
	| "for" "(" Assignment_opt ";" Or2_opt ";" Assignment_opt ")" Block	{{ $$ = ZF.makeFor((TAsn)$3, (TLog)$5, (TAsn)$7, (TLog)$9); }}
	;
StatementComment
	: HeadComment_opt Statement TailComment_opt		{{ $$ = ZF.makeLog((TTknW)$1, (TObj)$2, (TTknW)$3); }}
	;
Not
	: StatementComment
	| "~" Not					{{ $$ = ZF.makePrp1(ZF.e_log_not, (TLog)$2); }}
	;
And
	: Not
	| And "&" TailComment_opt Not	{{ $$ = ZF.addTailCommentPrp2(ZF.e_log_and, (TLog)$1, (TTknW)$3, (TLog)$4); }}
	;
Or
	: And
	| Or "|" And				{{ $$ = ZF.makePrp2(ZF.e_log_or, (TLog)$1, (TLog)$3); }}
	;
Logic
	: Or
	;
LogicList
	: Logic						{{ $$ = ZF.makeVLog((TLog)$1); }}
	| LogicList "," Logic		{{ ZF.seqAddUp((VLog)$1, (TLog)$3); $$ = $1; }}
	;
LogicSequence
	: LogicList_opt "->" LogicList_opt	{{ $$ = ZF.makeLogSeq((VLog)$1, (VLog)$3); }}
	;
Assignment
	: LeftHandSide ":="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_eq  , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "*="   Term			{{ $$ = ZF.makeAsn(ZF.e_asn_mul , (TTrm)$1, (TTrm)$3); }}
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
