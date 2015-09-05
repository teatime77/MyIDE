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
"class"					CLASS
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
"return"					RETURN
"set"					SET
"static"					STATIC
"super"					SUPER
"switch"					SWITCH
"synchronized"		SYNCHRONIZED
"there"						THERE
"this"						THIS
"throw"					THROW
"throws"					THROWS
"trace"				TRACE
"transient"			TRANSIENT
"try"						TRY
"vars"				VARS
"void"						VOID
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
"$Member"                  	MEMBER_STR
"$Fields"                       FIELDS_STR
"$POST_INC"			POST_INC
"$POST_DEC"			POST_DEC

"$type"		TYPE
"$class::"	CLASS_CL2
"$constructor"	CONSTRUCTOR
"$destructor"	DESTRUCTOR
"$new_ptr"	NEW_PTR
"$template_name"	TEMPLATE_NAME
"$typedef"	TYPEDEF
"$operator"	OPERATOR

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
CompilationUnit
	: PackageDeclaration_opt TypeDeclarations_opt
	;
PackageDeclaration
	: "package" Name ";"	
	;	
Name
	: "$Identifier"
	| Name "." "$Identifier"		
	;
TypeDeclarations
	: HeadComment_opt TypeDeclaration					
	| TypeDeclarations HeadComment_opt TypeDeclaration	
	;
TypeDeclaration
	: Modifiers_opt "class" "$Identifier" Super_opt Interfaces_opt ClassBody	{{ TSys.log("class’è‹`\n"); }} 
	| Modifiers_opt "interface" "$Identifier" ExtendsInterfaces_opt "{" InterfaceMemberDeclarations_opt "}"
    ;
ClassBody
    : "{" ClassBodyDeclarations_opt "}"     
    ;
Modifiers
	: Modifier				
	| Modifiers Modifier	
	;
Modifier
	: "public" 
	| "protected" 
	| "private"
	| "static"
	| "abstract" 
	| "final" 
	| "native" 
	| "synchronized" 
	| "transient" 
	| "volatile"
	;
	
Type
	: PrimitiveType
	| ReferenceType
	;
PrimitiveType
	: NumericType				
	| "boolean"					
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
	: PrimitiveType "[" "]"		
	| Name "[" "]"				
	| ArrayType "[" "]"			
	;
	
Super
	: "extends" Name 
	;
Interfaces
	: "implements" InterfaceTypeList 
	;
InterfaceTypeList
	: Name			  	
	| InterfaceTypeList "," Name	
	;
ClassBodyDeclarations
	: HeadComment_opt ClassBodyDeclaration	   			
	| ClassBodyDeclarations HeadComment_opt ClassBodyDeclaration	
	;
ClassBodyDeclaration
	: ClassMemberDeclaration
	| "static" Block			
	| Modifiers_opt "$Identifier" "(" FormalParameterList_opt ")" Throws_opt "{" BlockStatements_opt "}"
	  
	| ";"			
	;
ClassMemberDeclaration
	: FieldDeclaration
	| MethodHeader MethodBody	
	;
MethodBody
	: Block         
	| ";"		
	;
FieldDeclaration
	: Modifiers_opt Type VariableDeclarators ";" TailComment_opt
	;
VariableDeclarators
	: VariableDeclarator							
	| VariableDeclarators "," VariableDeclarator	
	;
VariableDeclarator
	: VariableDeclaratorId
	| VariableDeclaratorId "=" VariableInitializer	
	;
VariableDeclaratorId
	: "$Identifier"					
	| VariableDeclaratorId "[" "]"	
	;
VariableInitializer
	: Term
	| ArrayInitializer
	;
MethodHeader
	: Modifiers_opt Type MethodDeclarator Throws_opt
	| Modifiers_opt "void" MethodDeclarator Throws_opt
	;
MethodDeclarator
	: "$Identifier" "(" FormalParameterList_opt ")"	
	| MethodDeclarator "[" "]"						
	;
FormalParameterList
	: FormalParameter							
	| FormalParameterList "," FormalParameter	
	;
FormalParameter
	: Type VariableDeclaratorId		
	;
Throws
	: "throws" ClassTypeList	
	;
ClassTypeList
	: Name						
	| ClassTypeList "," Name	
	;
ExtendsInterfaces
	: "extends" Name				
	| ExtendsInterfaces "," Name	
	;
InterfaceMemberDeclarations
	: InterfaceMemberDeclaration								
	| InterfaceMemberDeclarations InterfaceMemberDeclaration	
	;
InterfaceMemberDeclaration
	: FieldDeclaration
	| MethodHeader ";"	
	| ";"			
	;
ArrayInitializer
	: "{" VariableInitializers_opt "}"		
	| "{" VariableInitializers "," "}"		
	;
VariableInitializers
	: VariableInitializer								
	| VariableInitializers "," VariableInitializer		
	;
Block
	: "{" BlockStatements_opt "}"		
	;
BlockStatements
	: HeadComment_opt BlockStatement TailComment_opt
	| BlockStatements HeadComment_opt BlockStatement TailComment_opt
	;
BlockStatement
	: LocalVariableDeclaration ";"		
	| Statement
	;
LocalVariableDeclaration
	: Type VariableDeclarators	
	| "final" Type VariableDeclarators
	;
Statement
	: IfStatement
	| Assignment ";"
	| "for" "(" Assignment_opt ";" Logic_opt ";" Assignment_opt ")" Block	{{ $$ = ZF.makeFor((TAsn)$3, (TLog)$5, (TAsn)$7, (TLog)$9); }}
	| "break" ";"					
	| "switch" "(" Term ")" "{" SwitchBlockStatementGroups SwitchLabels_opt "}"
	| "continue" ";"				
	| "return" Term_opt ";"						
	| "throw" Term ";"					
	| TryStatement
	;
IfStatement
	: "if" "(" Logic ")" Block "else" Block			{{ $$ = ZF.makePrp3((TTknW)$1, (TLog)$3, (TLog)$5, (TLog)$7); }}
	| "if" "(" Logic ")" Block "else" IfStatement	{{ $$ = ZF.makePrp3((TTknW)$1, (TLog)$3, (TLog)$5, (TLog)$7); }}
	| "if" "(" Logic ")" Block						{{ $$ = ZF.makePrp2((TTknW)$1, (TLog)$3, (TLog)$5); }}
	;


SwitchBlockStatementGroups
	: SwitchBlockStatementGroup									
	| SwitchBlockStatementGroups SwitchBlockStatementGroup		
	;
SwitchBlockStatementGroup
	: SwitchLabels BlockStatements	
	;
SwitchLabels
	: SwitchLabel					
	| SwitchLabels SwitchLabel		
	;
SwitchLabel
	: "case" Term ":"			
	| "default" ":"					
	;
TryStatement
	: "try" Block Catches						
	| "try" Block Catches_opt "finally" Block	
	;
Catches
	: CatchClause				
	| Catches CatchClause		
	;
CatchClause
	: "catch" "(" FormalParameter ")" Block		
	;



	
	
	
	
TermList
	: Term
	| TermList "," Term						{{ $$ = ZF.makeVTrm((TObj)$1, (TTrm)$3); }}
	;
Tuple
	: "(" TermList_opt ")"					{{ $$ = ZF.makeTpl((TObj)$2); }}
	;
FieldAccess
	: Primary "." "$Identifier"				{{ $$ = ZF.makeMemberApp((TObj)$1, (TTknW)$2, ZF.makeRefTkn((TTknW)$3)); }}
	;
MethodInvocation
	: "$Identifier" Tuple	{{ $$ = ZF.makeApp(ZF.makeRefTkn((TTknW)$1) , (TObj)$2); }}
	;
Primary
	: Literal
	| "$Identifier"							{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| FieldAccess
	| MethodInvocation
	;
Unary
	: Primary
	| "+" Unary								{{ $$ = ZF.makeOpr1((TTknW)$1, (TTrm)$2); }}
	| "-" Unary								{{ $$ = ZF.makeOpr1((TTknW)$1, (TTrm)$2); }}
	;
Multiplicative
	: Unary
	| Multiplicative "*" Unary				{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Multiplicative "/" Unary				{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Multiplicative "%" Unary				{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	;
Additive
	: Multiplicative
	| Additive "+" Multiplicative			{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Additive "-" Multiplicative			{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	;
Term
	: Additive
	| Term "<<" Additive					{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term ">>" Additive					{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term ">>>" Additive					{{ $$ = ZF.makeOpr2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	;

Relation
	: Term "<" Term								{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term ">" Term								{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term "<=" Term							{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term ">=" Term							{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term "==" Term							{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term "!=" Term							{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	| Term "instanceof" Type					{{ $$ = ZF.makeRel2((TTknW)$2, (TTrm)$1, (TTrm)$3); }}
	;
Not
	: Relation
	| "!" Not					{{ $$ = ZF.makePrp1((TTknW)$1, (TLog)$2); }}
	;
And
	: Not
	| And "&&" Not				{{ $$ = ZF.makePrp2((TTknW)$2, (TLog)$1, (TLog)$3); }}
	;
Or
	: And
	| Or "||" And				{{ $$ = ZF.makePrp2((TTknW)$2, (TLog)$1, (TLog)$3); }}
	;
Logic
	: Or
	;
Assignment
	: LeftHandSide AssignmentOperator Term		{{ $$ = ZF.makeAsn((TTrm)$1, (TTrm)$3); }}
	| MethodInvocation						{{ $$ = ZF.makeAsn(null, (TApp)$2); }}
	;
LeftHandSide
	: "$Identifier"			{{ $$ = ZF.makeRefTkn((TTknW)$1); }}
	| FieldAccess
	;
AssignmentOperator
	: "=" 
	| "*=" 
	| "/=" 
	| "%=" 
	| "+=" 
	| "-=" 
	| "<<=" 
	| ">>=" 
	| ">>>=" 
	| "&=" 
	| "^=" 
	| "|="
	;

