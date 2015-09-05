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

"^"			HAT
"||"			OR2
"&&"			AND2
"?"			QUE
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
"~"			TIL
">>"			RSH
"<<"			LSH
">>>"		URSH
"=="			EQ2

"abstract"				ABSTRACT
"all"					ALL
"boolean"				BOOLEAN
"break"					BREAK
"byte"						BYTE
"case"						CASE
"catch"					CATCH
"char"						CHAR
"class"					CLASS
"continue"				CONTINUE
"default"				DEFAULT
"do"							DO
"double"					DOUBLE
"each"					EACH
"else"						ELSE
"exists"				EXISTS
"extends"				EXTENDS
"final"					FINAL
"finally"				FINALLY
"float"					FLOAT
"for"						FOR
"if"							IF
"iif"							IIF
"implements"			IMPLEMENTS
"imply"					IMPLY
"import"					IMPORT
"instanceof"			INSTANCEOF
"int"						INT
"interface"			INTERFACE
"long"						LONG
"native"					NATIVE
"new"						NEW
"package"				PACKAGE
"private"				PRIVATE
"protected"			PROTECTED
"public"					PUBLIC
"return"					RETURN
"short"					SHORT
"static"					STATIC
"super"					SUPER
"switch"					SWITCH
"synchronized"		SYNCHRONIZED
"there"						THERE
"this"						THIS
"throw"					THROW
"throws"					THROWS
"transient"			TRANSIENT
"try"						TRY
"void"						VOID
"volatile"				VOLATILE
"while"					WHILE
"null"				NULL_L
"true"				TRUE_L
"false"				FALSE_L

"$Head___Comment"		HEAD_COMMENT
"$Tail___Comment"		TAIL_COMMENT
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
	| "null"					{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| "true"					{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| "false"					{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	;
Type
	: PrimitiveType				
	| ReferenceType
	;
PrimitiveType
	: NumericType				{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| "boolean"					{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	;
NumericType
	: IntegralType
	| FloatingPointType
	;
IntegralType
	: "byte"
	| "short"
	| "int"
	| "long"
	| "char"
	;
FloatingPointType
	: "float"
	| "double"
	;
ReferenceType
	: Name						
	| ArrayType
	;
ArrayType
	: PrimitiveType "[" "]"		{{ $$ = ZF.makeArrayType((TTrm)$1); }}
	| Name "[" "]"				{{ $$ = ZF.makeArrayType((TTrm)$1); }}
	| ArrayType "[" "]"			{{ $$ = ZF.incArrayType((TApp)$1); }}
	;
Name
	: "$Identifier"				{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| Name "." "$Identifier"		{{ $$ = ZF.makeAppDot((TObj)$1, (TTknW)$3); }}		
	;
CompilationUnit
	: PackageDeclaration_opt ImportDeclarations_opt TypeDeclarations_opt		{{ $$ = ZF.makeSrc((String)$1, (TObj)$2, (TObj)$3); }}
	| PackageDeclaration_opt ImportDeclarations_opt TypeDeclarations_opt ";"	{{ ZF.makeErr(1); }}
	;
ImportDeclarations
	: ImportDeclaration						
	| ImportDeclarations ImportDeclaration					{{ $$ = ZF.makeVTrm((TObj)$1, (TTrm)$2); }}
	;
TypeDeclarations
	: HeadComment_opt TypeDeclaration						{{ $$ = ZF.setComment((TTknW)$1, (TObj)$2, null); }}
	| TypeDeclarations HeadComment_opt TypeDeclaration	
		{{ $$ = ZF.appendVVar((TObj)$1, ZF.setComment((TTknW)$2, (TObj)$3, null)); }}
	;
PackageDeclaration
	: "package" PackageName ";"								{{ $$ = $2; }}
	;
PackageName
	: "$Identifier"											{{ $$ = ((TTknW)$1).tknName; }}
	| PackageName "." "$Identifier"							{{ $$ = ((String)$1) + "." + ((TTknW)$3).tknName; }}		
	;
ImportDeclaration
	: "import" Name ";"						{{ ZF.makeErr(1); }}
	| "import" Name "." "*" ";"				{{ $$ = $2; }}
	;
TypeDeclaration
	: ClassDeclaration
	| InterfaceDeclaration
	;
ClassDeclaration
	: Modifiers_opt "class" "$Identifier" Super_opt Interfaces_opt ClassBody	{{ $$ = ZF.makeClass((TInt)$1, (TTknW)$3, (TRef)$4, (TObj)$6); }} 
	  
	;
InterfaceDeclaration
	: Modifiers_opt "interface" "$Identifier" ExtendsInterfaces_opt "{" InterfaceMemberDeclarations_opt "}"	{{ ZF.makeErr(1); }}
    ;
ClassBody
        : "{" ClassBodyDeclarations_opt "}"					{{ $$ = $2; }}
        ;
Modifiers
	: Modifier				
	| Modifiers Modifier									{{ $$ = ZF.addInt((TInt)$1, (TInt)$2); }}
	;
Modifier
	: "public"												{{ $$ = ZF.newInt(ZF.PUBLIC); }}
	| "protected" 											{{ $$ = null; }}
	| "private"												{{ $$ = null; }}
	| "static"												{{ $$ = ZF.newInt(ZF.STATIC); }}
	| "abstract" 											{{ $$ = ZF.newInt(ZF.ABSTRACT); }}
	| "final"												{{ $$ = ZF.newInt(ZF.FINAL); }}
	| "native" 												{{ $$ = null; }}
	| "synchronized" 										{{ $$ = null; }}
	| "transient" 											{{ $$ = null; }}
	| "volatile"											{{ $$ = null; }}
	;
Super
	: "extends" Name										{{ $$ = $2; }}
	;
Interfaces
	: "implements" InterfaceTypeList						{{ ZF.makeErr(1); }}
	;
InterfaceTypeList
	: Name			  	
	| InterfaceTypeList "," Name							{{ ZF.makeErr(1); }}
	;
ClassBodyDeclarations
	: HeadComment_opt ClassBodyDeclaration	   				{{ $$ = ZF.setComment((TTknW)$1, (TObj)$2, null); }}
	| ClassBodyDeclarations HeadComment_opt ClassBodyDeclaration	
		{{ $$ = ZF.appendVVar((TObj)$1, ZF.setComment((TTknW)$2, (TObj)$3, null)); }}
	;
ClassBodyDeclaration
	: ClassMemberDeclaration
	| "static" Block										{{ ZF.makeErr(1); }}
	| Modifiers_opt "$Identifier" "(" FormalParameterList_opt ")" Throws_opt Block		{{ $$ = ZF.makeMethod((TInt)$1, $2, (TTknW)$2, (VVar)$4, (TLog)$7); }}
	| Modifiers_opt Type "$Identifier" "(" FormalParameterList_opt ")" Dims_opt Throws_opt  MethodBody		{{ $$ = ZF.makeMethod((TInt)$1, $2, (TTknW)$3, (VVar)$5, (TLog)$9); }}
	| Modifiers_opt "void" "$Identifier" "(" FormalParameterList_opt ")" Dims_opt Throws_opt  MethodBody		{{ $$ = ZF.makeMethod((TInt)$1, null, (TTknW)$3, (VVar)$5, (TLog)$9); }}
	| ";"													{{ ZF.makeErr(1); }}
	;
ClassMemberDeclaration
	: FieldDeclaration
	| ClassDeclaration										{{ ZF.makeErr(1); }}
	;
FieldDeclaration
	: Modifiers_opt Type VariableDeclarators ";" TailComment_opt	{{ $$ = ZF.setVVarType((TInt)$1, (TTrm)$2, (TObj)$3, (TTknW)$5); }}
	;
VariableDeclarators
	: VariableDeclarator							
	| VariableDeclarators "," VariableDeclarator					{{ $$ = ZF.makeVObj((TObj)$1, (TObj)$3); }}
	;
VariableDeclarator
	: VariableDeclaratorId
	| VariableDeclaratorId "=" VariableInitializer					{{ $$ = ZF.setVarConst((TVar)$1, (TTrm)$3); }}
	;
VariableDeclaratorId
	: "$Identifier"													{{ $$ = ZF.makeVar((TTknW)$1, null); }}
	| VariableDeclaratorId "[" "]"									{{ ZF.makeErr(1); }}
	;
VariableInitializer
	: Expression
	| ArrayInitializer
	;
MethodHeader
	: Modifiers_opt Type "$Identifier" "(" FormalParameterList_opt ")" Dims_opt Throws_opt
	| Modifiers_opt "void" "$Identifier" "(" FormalParameterList_opt ")" Dims_opt Throws_opt
	;
FormalParameterList
	: FormalParameter							
	| FormalParameterList "," FormalParameter						{{ $$ = ZF.appendVVar((TObj)$1, (TObj)$3); }}
	;
FormalParameter
	: Type VariableDeclaratorId										{{ $$ = ZF.setVVarType(null, (TTrm)$1, (TObj)$2, null); }}
	;
Throws
	: "throws" ClassTypeList										{{ ZF.makeErr(1); }}
	;
ClassTypeList
	: Name						
	| ClassTypeList "," Name										{{ ZF.makeErr(1); }}
	;
ExplicitConstructorInvocation
	: "this" "(" ArgumentList_opt ")" ";"							{{ ZF.makeErr(1); }}
	| "super" "(" ArgumentList_opt ")" ";"							{{ ZF.makeErr(1); }}
	;
ExtendsInterfaces
	: "extends" Name												{{ ZF.makeErr(1); }}
	| ExtendsInterfaces "," Name									{{ ZF.makeErr(1); }}
	;
InterfaceMemberDeclarations
	: InterfaceMemberDeclaration								
	| InterfaceMemberDeclarations InterfaceMemberDeclaration		{{ ZF.makeErr(1); }}
	;
InterfaceMemberDeclaration
	: FieldDeclaration												{{ ZF.makeErr(1); }}
	| MethodHeader ";"												{{ ZF.makeErr(1); }}
	| ClassDeclaration												{{ ZF.makeErr(1); }}
	| ";"															{{ ZF.makeErr(1); }}
	;
ArrayInitializer
	: "{" VariableInitializers_opt "}"								{{ ZF.makeErr(1); }}
	| "{" VariableInitializers "," "}"								{{ ZF.makeErr(1); }}
	;
VariableInitializers
	: VariableInitializer								
	| VariableInitializers "," VariableInitializer					{{ ZF.makeErr(1); }}
	;
MethodBody
	: Block
	| ";"		
	;

Block
	: "{" BlockStatements_opt "}"										{{ $$ = ZF.makeBlock((TObj)$2); }}
	;
BlockStatements
	: CommentBlockStatement
	| BlockStatements CommentBlockStatement								{{ $$ = ZF.makeVObj((TObj)$1, (TObj)$2); }}
	;
CommentBlockStatement
	: HeadComment_opt BlockStatement TailComment_opt					{{ $$ = ZF.setComment((TTknW)$1, (TObj)$2, (TTknW)$3); }}
	;	
BlockStatement
	: Statement
	| LocalVariableDeclaration ";"										{{ $$ = $1; }}
	| ClassDeclaration													{{ ZF.makeErr(1); }}
	;
LocalVariableDeclaration
	: Type VariableDeclarators											{{ $$ = ZF.setVVarType(null, (TTrm)$1, (TObj)$2, null); }}
	| "final" Type VariableDeclarators									{{ ZF.makeErr(1); }}
	;
Statement
	: StatementWithoutTrailingSubstatement
	| "$Identifier" ":" Statement										{{ ZF.makeErr(1); }}
	| "while" "(" Expression ")" Statement								{{ ZF.makeErr(1); }}
	| "for" "(" ForInit_opt ";" Expression_opt ";" ForUpdate_opt ")"	Statement	{{ $$ = ZF.makeFor((TAsn)$3, (TLog)$5, (TAsn)$7, (TLog)$9); }}
		
	| "for" "(" ForInit_opt ";" Expression_opt ";" ForUpdate_opt ")"	LocalVariableDeclaration ";"	{{ ZF.makeErr(1); }}
	| IfStatement
	| ExplicitConstructorInvocation
	;
IfStatement
	: "if" "(" Expression ")" Block									{{ $$ = ZF.makeIf((TLog)$3, (TLog)$5, null    , false); }}
	| "if" "(" Expression ")" Block "else" Block					{{ $$ = ZF.makeIf((TLog)$3, (TLog)$5, (TLog)$7, false); }}
	| "if" "(" Expression ")" Block "else" IfStatement				{{ $$ = ZF.makeIf((TLog)$3, (TLog)$5, (TLog)$7, true); }}
	;
	
StatementNoShortIf
	: StatementWithoutTrailingSubstatement
	| "$Identifier" ":" StatementNoShortIf							{{ ZF.makeErr(1); }}
	| "while" "(" Expression ")" StatementNoShortIf					{{ ZF.makeErr(1); }}
	| "for" "(" ForInit_opt ";" Expression_opt ";" ForUpdate_opt ")"	StatementNoShortIf	{{ $$ = ZF.makeFor((TAsn)$3, (TLog)$5, (TAsn)$7, (TLog)$9); }}
	;
StatementWithoutTrailingSubstatement
	: Block
	| ";"															{{ ZF.makeErr(1); }}
	| StatementExpression ";"										{{ $$ = $1; }}
	| "switch" "(" Expression ")" "{" LabelStatements  "}"
	  {{ $$ = ZF.makeSwi((TTrm)$3, (TObj)$6); }}
	| "do" Statement "while" "(" Expression ")" ";"					{{ ZF.makeErr(1); }}
	| BreakStatement
	| ContinueStatement
	| "return" Expression_opt ";"									{{ $$ = ZF.makeReturn((TTrm)$2); }}
	| "synchronized" "(" Expression ")" Block						{{ ZF.makeErr(1); }}
	| "throw" Expression ";"										{{ ZF.makeErr(1); }}
	| TryStatement
	;
StatementExpression
	: Assignment
	| PreIncrementExpression
	| PreDecrementExpression
	| MethodInvocation												{{ $$ = ZF.makeAsn(ZF.e_asn_eq, null, (TApp)$1); }}
	| ClassInstanceCreationExpression
	;
LabelStatements
	: CommentLabelStatement
	| LabelStatements CommentLabelStatement								{{ $$ = ZF.makeVObj((TObj)$1, (TObj)$2); }}
	;
CommentLabelStatement
	: HeadComment_opt Statement TailComment_opt					{{ $$ = ZF.setComment((TTknW)$1, (TObj)$2, (TTknW)$3); }}
	| HeadComment_opt SwitchLabel TailComment_opt					{{ $$ = ZF.setComment((TTknW)$1, (TObj)$2, (TTknW)$3); }}
	;	
SwitchLabel
	: "case" Expression ":" 			{{ $$ = $2; }}
	| "default" ":"						{{ $$ = null; }}
	;
ForInit
	: StatementExpressionList
	| LocalVariableDeclaration										{{ ZF.makeErr(1); }}
	;
ForUpdate
	: StatementExpressionList
	;
StatementExpressionList
	: StatementExpression								
	| StatementExpressionList "," StatementExpression		{{ ZF.makeErr(1); }}
	;
BreakStatement
	: "break" ";"											{{ $$ = ZF.makeBreak(); }}
	| "break" "$Identifier" ";"								{{ ZF.makeErr(1); }}
	;
ContinueStatement
	: "continue" ";"										{{ ZF.makeErr(1); }}
	| "continue" "$Identifier" ";"							{{ ZF.makeErr(1); }}
	;
TryStatement
	: "try" Block Catches									{{ ZF.makeErr(1); }}
	| "try" Block Catches_opt "finally" Block				{{ ZF.makeErr(1); }}
	;
Catches
	: CatchClause				
	| Catches CatchClause									{{ ZF.makeErr(1); }}
	;
CatchClause
	: "catch" "(" FormalParameter ")" Block					{{ ZF.makeErr(1); }}
	;
Primary
	: PrimaryNoNewArray
	| ArrayCreationExpression
	;
PrimaryNoNewArray
	: Literal
	| "this"												{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| "(" Expression ")"									{{ $$ = $2; }}
	| ClassInstanceCreationExpression
	| FieldAccess
	| MethodInvocation
	| ArrayAccess
	| Name "." "this"										{{ ZF.makeErr(1); }}
	| Name "." "class"										{{ ZF.makeErr(1); }}
	| PrimitiveType "." "class"								{{ ZF.makeErr(1); }}
	| "void" "." "class"									{{ ZF.makeErr(1); }}
	;
ClassInstanceCreationExpression
	: "new" Name "(" ArgumentList_opt ")"					{{ $$ = ZF.makeNew((TTrm)$2, (TObj)$4); }}
	| "new" Name "(" ArgumentList_opt ")" ClassBody			{{ ZF.makeErr(1); }}
	;
ArgumentList
	: Expression						
	| ArgumentList "," Expression							{{ $$ = ZF.makeVTrm((TObj)$1, (TTrm)$3); }}
	;
ArrayCreationExpression
	: "new" PrimitiveType DimExprs Dims_opt					{{ $$ = ZF.makeNewArray((TTrm)$2, (TObj)$3, (TInt)$4); }}
	| "new" Name DimExprs Dims_opt							{{ $$ = ZF.makeNewArray((TTrm)$2, (TObj)$3, (TInt)$4); }}				
	| "new" ArrayType ArrayInitializer						{{ ZF.makeErr(1); }}			
	;
DimExprs
	: DimExpr				
	| DimExprs DimExpr										{{ $$ = ZF.makeVTrm((TObj)$1, (TTrm)$2); }}
	;
DimExpr
	: "[" Expression "]"									{{ $$ = $2; }}
	;
Dims
	: "[" "]"												{{ $$ = ZF.newInt(1); }}
	| Dims "[" "]"											{{ ((TInt)$1).intVal++; $$ = $1; }}
	;
FieldAccess
	: Primary "." "$Identifier"								{{ $$ = ZF.makeAppDot((TObj)$1, (TTknW)$3); }}
	| "super" "." "$Identifier"	
	;
MethodInvocation
	: Name "(" ArgumentList_opt ")"							{{ $$ = ZF.makeApp((TTrm)$1 , (TObj)$3); }}
	| Primary "." "$Identifier" "(" ArgumentList_opt ")"		{{ $$ = ZF.makeApp( ZF.makeAppDot((TObj)$1, (TTknW)$3) , (TObj)$5); }}
	| "super" "." "$Identifier" "(" ArgumentList_opt ")"	  
	;
ArrayAccess
	: Name "[" Expression "]"								{{ $$ = ZF.makeAppArray((TTrm)$1, (TTrm)$3); }}
	| PrimaryNoNewArray "[" Expression "]"					{{ $$ = ZF.makeAppArray((TTrm)$1, (TTrm)$3); }}
	;
PostfixExpression
	: Primary
	| Name						
	;
PreIncrementExpression
	: "++" UnaryExpression									{{ $$ = ZF.makeOpr1(ZF.e_opr_inc, (TTrm)$2); }}
	;
PreDecrementExpression
	: "--" UnaryExpression									{{ $$ = ZF.makeOpr1(ZF.e_opr_dec, (TTrm)$2); }}
	;
CastExpression
	: "(" PrimitiveType Dims_opt ")" UnaryExpression		{{ $$ = ZF.makeCast((TTrm)$2, (TInt)$3, (TTrm)$5); }}
	| "(" Expression ")" UnaryExpressionNotPlusMinus		{{ $$ = ZF.makeCast((TTrm)$2, null	  , (TTrm)$4); }}
	| "(" Name Dims ")" UnaryExpressionNotPlusMinus			{{ $$ = ZF.makeCast((TTrm)$2, (TInt)$3, (TTrm)$5); }}
	;
UnaryExpressionNotPlusMinus
	: PostfixExpression
	| "~" UnaryExpression									{{ $$ = ZF.makeOpr1(ZF.e_opr_til, (TTrm)$2); }}
	| "!" UnaryExpression									{{ $$ = ZF.makePrp1(ZF.e_log_not, (TLog)$2); }}
	| CastExpression
	;
UnaryExpression
	: PreIncrementExpression
	| PreDecrementExpression
	| "+" UnaryExpression									{{ $$ = ZF.makeOpr1(ZF.e_opr_add, (TTrm)$2); }}
	| "-" UnaryExpression									{{ $$ = ZF.makeOpr1(ZF.e_opr_sub, (TTrm)$2); }}
	| UnaryExpressionNotPlusMinus
	;
MultiplicativeExpression
	: UnaryExpression
	| MultiplicativeExpression "*" UnaryExpression			{{ $$ = ZF.makeOpr2(ZF.e_opr_mul, (TTrm)$1, (TTrm)$3); }}
	| MultiplicativeExpression "/" UnaryExpression			{{ $$ = ZF.makeOpr2(ZF.e_opr_div, (TTrm)$1, (TTrm)$3); }}
	| MultiplicativeExpression "%" UnaryExpression			{{ $$ = ZF.makeOpr2(ZF.e_opr_mod, (TTrm)$1, (TTrm)$3); }}
	;
AdditiveExpression
	: MultiplicativeExpression
	| AdditiveExpression "+" MultiplicativeExpression		{{ $$ = ZF.makeOpr2(ZF.e_opr_add, (TTrm)$1, (TTrm)$3); }}
	| AdditiveExpression "-" MultiplicativeExpression		{{ $$ = ZF.makeOpr2(ZF.e_opr_sub, (TTrm)$1, (TTrm)$3); }}
	;
ShiftExpression
	: AdditiveExpression
	| ShiftExpression "<<" AdditiveExpression				{{ $$ = ZF.makeOpr2(ZF.e_opr_lsh, (TTrm)$1, (TTrm)$3); }}
	| ShiftExpression ">>" AdditiveExpression				{{ $$ = ZF.makeOpr2(ZF.e_opr_rsh, (TTrm)$1, (TTrm)$3); }}
	| ShiftExpression ">>>" AdditiveExpression				{{ $$ = ZF.makeOpr2(ZF.e_opr_ursh, (TTrm)$1, (TTrm)$3); }}
	;
RelationalExpression
	: ShiftExpression
	| RelationalExpression "<" ShiftExpression				{{ $$ = ZF.makeRel(ZF.e_rel_lt, (TTrm)$1, (TTrm)$3); }}
	| RelationalExpression ">" ShiftExpression				{{ $$ = ZF.makeRel(ZF.e_rel_gt, (TTrm)$1, (TTrm)$3); }}
	| RelationalExpression "<=" ShiftExpression				{{ $$ = ZF.makeRel(ZF.e_rel_le, (TTrm)$1, (TTrm)$3); }}
	| RelationalExpression ">=" ShiftExpression				{{ $$ = ZF.makeRel(ZF.e_rel_ge, (TTrm)$1, (TTrm)$3); }}
	| RelationalExpression "instanceof" ReferenceType		{{ $$ = ZF.makeRel(ZF.e_rel_in, (TTrm)$1, (TTrm)$3); }}
	;
EqualityExpression
	: RelationalExpression
	| EqualityExpression "==" RelationalExpression			{{ $$ = ZF.makeRel(ZF.e_rel_eq, (TTrm)$1, (TTrm)$3); }}
	| EqualityExpression "!=" RelationalExpression			{{ $$ = ZF.makeRel(ZF.e_rel_ne, (TTrm)$1, (TTrm)$3); }}
	;
AndExpression
	: EqualityExpression
	| AndExpression "&" EqualityExpression					{{ $$ = ZF.makeOpr2(ZF.e_opr_and, (TTrm)$1, (TTrm)$3); }}
	;
ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression "^" AndExpression				{{ $$ = ZF.makeOpr2(ZF.e_opr_hat, (TTrm)$1, (TTrm)$3); }}
	;
InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression "|" ExclusiveOrExpression		{{ $$ = ZF.makeOpr2(ZF.e_opr_or, (TTrm)$1, (TTrm)$3); }}
	;
ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression "&&" InclusiveOrExpression	{{ $$ = ZF.makePrp2(ZF.e_log_and, (TLog)$1, (TLog)$3); }}
	  
	;
ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression "||" ConditionalAndExpression	{{ $$ = ZF.makePrp2(ZF.e_log_or, (TLog)$1, (TLog)$3); }}
	  
	;
ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression "?" Expression ":" ConditionalExpression	{{ ZF.makeErr(1); }}
	  
	;
Expression
	: ConditionalExpression
	| Assignment
	;
Assignment
	: LeftHandSide "=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_eq  , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "*=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_mul , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "/=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_div , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "%=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_mod , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "+=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_add , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "-=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_sub , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "<<=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_lsh , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide ">>=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_rsh , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide ">>>=" Expression		{{ $$ = ZF.makeAsn(ZF.e_asn_ursh, (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "&=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_and , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "^=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_hat , (TTrm)$1, (TTrm)$3); }}
	| LeftHandSide "|=" Expression			{{ $$ = ZF.makeAsn(ZF.e_asn_or  , (TTrm)$1, (TTrm)$3); }}
	| PostIncrementExpression				{{ $$ = ZF.makeAsn(ZF.e_asn_eq  , null    , (TApp)$1); }}
	| PostDecrementExpression				{{ $$ = ZF.makeAsn(ZF.e_asn_eq  , null    , (TApp)$1); }}
	;
LeftHandSide
	: Name			
	| FieldAccess
	| ArrayAccess
	;
PostIncrementExpression
	: PostfixExpression "++"								{{ $$ = ZF.makeOpr1(ZF.e_opr_inc, (TTrm)$1); }}
	;
PostDecrementExpression
	: PostfixExpression "--"								{{ $$ = ZF.makeOpr1(ZF.e_opr_dec, (TTrm)$1); }}
	;


 
