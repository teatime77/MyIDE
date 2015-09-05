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
"::"	 CL2		 
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
"break"					BREAK						
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
"this"						THIS						
"throw"					THROW						
"throws"					THROWS					
"transient"			TRANSIENT				
"try"						TRY							
"void"						VOID						
"volatile"				VOLATILE				
"while"					WHILE						

"$Head___Comment"		HEAD_COMMENT		
"$Tail___Comment"		TAIL_COMMENT		
"$IntegerLiteral"		INT_L		
"$FloatingPointLiteral"		FLOAT_L		
"$CharacterLiteral"		CHAR_L		
"$StringLiteral"			STRING_L	
"$Identifier"			IDENTIFIER	
"$Member"                  	MEMBER_STR      
"$Fields"                       FIELDS_STR      
"$POST_INC"			POST_INC			
"$POST_DEC"			POST_DEC			

"$ZFstart"				ZF_START			
"$ZFend"				ZF_END				

"$assembly_instruction"	ASSEMBLY_INSTRUCTION	
"$future"	FUTURE	
"$implementations"	IMPLEMENTATIONS	
"$reserved"	RESERVED	
"$rmodel"	RMODEL	
"..." DDDOT 
"->"	PTR	
":>"	SEG		
".*"	DOTAST	
"->*"	PTRAST	
"=>"	EQGT	
"auto"	AUTO	
"const"	CONST	
"const_cast"	CONST_CAST	
"delete"	DELETE	
"dllexport"	DLLEXPORT	
"dllimport"	DLLIMPORT	
"dynamic_cast"	DYNAMIC_CAST	
"enum"	ENUM	
"extern"	EXTERN	
"friend"	FRIEND	
"goto"	GOTO	
"inline"	INLINE	
"__inline"	__INLINE	
"naked"	NAKED	
"namespace"	NAMESPACE	
"operator"	OPERATOR	
"register"	REGISTER	
"reinterpret_cast"	REINTERPRET_CAST	
"signed"	SIGNED	
"sizeof"	SIZEOF	
"static_cast"	STATIC_CAST	
"struct"	STRUCT	
"template"	TEMPLATE	
"thread"	THREAD	
"typedef"	TYPEDEF	
"typeid"	TYPEID	
"union"	UNION	
"unsigned"	UNSIGNED	
"using"	USING	
"virtual"	VIRTUAL	
"__asm"	__ASM	
"__based"	__BASED	
"_cdecl"	_CDECL	
"__cdecl"	__CDECL	
"__declspec"	__DECLSPEC	
"__except"	__EXCEPT	
"__fastcall"	__FASTCALL	
"__finally"	__FINALLY	
"__oldcall"	__OLDCALL	
"__stdcall"	__STDCALL	
"__syscall"	__SYSCALL	
"__try"	__TRY	
"__unaligned"	__UNALIGNED	

"$type"		TYPE	
"$class::"	CLASS_CL2	
"$constructor"	CONSTRUCTOR	
"$destructor"	DESTRUCTOR	
"$template_name"	TEMPLATE_NAME	
"$new_ptr"	NEW_PTR	
"__int8"	__INT8	
"__int16"	__INT16	
"__int32"	__INT32	
"__int64"	__INT64	

"$null for java"				NULL_L		
"$true for java"				TRUE_L		
"$false for java"				FALSE_L		

"EOF"				EOF		
}
						    
Goal
	: declarations	{{ TSys.Print("parse end"); }}
	;
declarations
	: declaration
	| declarations declaration
	;
TailComment
	: "$Tail___Comment"
	;
compound_statement
	: "{" BlockStatements_opt "}"
	;
BlockStatements
	: BlockStatement TailComment_opt
	| BlockStatements BlockStatement TailComment_opt
	;
BlockStatement
	: Statement
	;
Statement
	: StatementWithoutTrailingSubstatement
	| "$Identifier" ":" Statement		
	| "if" "(" expression ")" Statement								
	| "if" "(" expression ")" StatementNoShortIf "else" Statement	
	| "while" "(" expression ")" Statement						
	| "for" "(" ForInit_opt ";" expression_opt ";" ForUpdate_opt ")" Statement
	| "each" "(" "$Identifier" ";" expression ")"	Statement
	;
StatementNoShortIf
	: StatementWithoutTrailingSubstatement
	| "$Identifier" ":" StatementNoShortIf	
	| "if" "(" expression ")" StatementNoShortIf "else" StatementNoShortIf
	| "while" "(" expression ")" StatementNoShortIf		
	| "for" "(" ForInit_opt ";" expression_opt ";" ForUpdate_opt ")"	StatementNoShortIf
	| "each" "(" "$Identifier" ";" expression ")"	StatementNoShortIf
	;
StatementWithoutTrailingSubstatement
	: var_fnc_declaration ";"
	| decl_specifiers ";"
	| function_definition
	| ";"
	| function_pointer ";"
	| compound_statement
	| StatementExpression ";"		
	| "switch" "(" expression ")" "{" SwitchBlockStatementGroups SwitchLabels_opt "}"
	| "do" Statement "while" "(" expression ")" ";"		
	| BreakStatement
	| ContinueStatement
	| "return" expression_opt ";"						
	| "synchronized" "(" expression ")" compound_statement	
	| TryStatement
	| asm_statement
	;
StatementExpression
	: expression
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
	: "case" expression ":"			
	| "default" ":"					
	;
ForInit
	: expression
	| var_fnc_declaration
	;
ForUpdate
	: expression
	;
BreakStatement
	: "break" ";"					
	| "break" "$Identifier" ";"		
	;
ContinueStatement
	: "continue" ";"				
	| "continue" "$Identifier" ";"	
	| "goto"  "$Identifier"  ";"
	;
TryStatement
	: "try" compound_statement Catches						
	| "try" compound_statement Catches_opt "finally" compound_statement	
	;
Catches
	: CatchClause				
	| Catches CatchClause		
	;
CatchClause
	: "catch" "(" exception_declaration ")" compound_statement		
	;
exception_declaration
	: decl_specifiers declarator
	| decl_specifiers
	;
asm_statement
	: "__asm" assembly_instruction
	| "__asm" "{" assembly_instruction_list "}"
	;
assembly_instruction_list
	: assembly_instruction sm_opt
	| assembly_instruction_list assembly_instruction
	;
assembly_instruction
	: "int" "$IntegerLiteral"
	;
expression
	: assignment_expression
	| expression  ","  assignment_expression
	;
assignment_expression
	: conditional_expression
	| cast_expression assignment_operator assignment_expression	
	| "throw"
	| "throw" assignment_expression
	;
assignment_operator
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
conditional_expression
	: logical_or_expression
	| logical_or_expression "?" expression ":" conditional_expression
	;
logical_or_expression
	: logical_and_expression
	| logical_or_expression "||" logical_and_expression
	;
logical_and_expression
	: inclusive_or_expression
	| logical_and_expression "&&" inclusive_or_expression
	;
inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression "|" exclusive_or_expression	
	;
exclusive_or_expression
	: and_expression
	| exclusive_or_expression "^" and_expression	
	;
and_expression
	: equality_expression
	| and_expression "&" equality_expression	
	;
equality_expression
	: relational_expression
	| equality_expression "==" relational_expression	
	| equality_expression "!=" relational_expression	
	;
relational_expression
	: shift_expression
	| relational_expression "<" shift_expression	
	| relational_expression ">" shift_expression	
	| relational_expression "<=" shift_expression	
	| relational_expression ">=" shift_expression	
	;
shift_expression
	: additive_expression
	| shift_expression "<<" additive_expression	
	| shift_expression ">>" additive_expression	
	| shift_expression ">>>" additive_expression	
	;
additive_expression
	: multiplicative_expression
	| additive_expression "+" multiplicative_expression	
	| additive_expression "-" multiplicative_expression	
	;
multiplicative_expression
	: segment_expression
	| multiplicative_expression "*" segment_expression	
	| multiplicative_expression "/" segment_expression	
	| multiplicative_expression "%" segment_expression	
	;
segment_expression
	: pm_expression
	| segment_expression  ":>"  pm_expression
	;
pm_expression
	: cast_expression 
	| pm_expression  ".*"  cast_expression 
	| pm_expression  "->*"  cast_expression
	;

cast_expression
	: "(" type_name ")" cast_expression	
	| unary_expression
	;
unary_expression
	: postfix_expression
	| "++" unary_expression
	| "--" unary_expression
	| unary_operator cast_expression
	| "sizeof" unary_expression
	| "sizeof" "(" type_name ")"
	| allocation_expression
	| deallocation_expression
	;
unary_operator
	: "+"
	| "-"
	| "*"
	| "&"
	| "!"
	| "~"
	;
allocation_expression
	: cl2_opt "new"	placement_opt "$constructor" "("  expression_opt  ")"
	| allocation_expression1 "$type" "$new_ptr" dmy new_declarator_opt
	| allocation_expression1 "$type"  dmy new_declarator_opt
	;
placement
	: "("  expression_opt  ")"
	;
allocation_expression1
	: cl2_opt "new" placement_opt	{{ new___flg = true; }}
	;
dmy
	:	{{ new___flg = false; }}
	;
new_declarator
	: "[" expression "]"
	| new_declarator "[" expression "]"
	;
new_initializer
	: "("  initializer_list  ")"
	;
deallocation_expression
	: cl2_opt  "delete" cast_expression 
	| cl2_opt  "delete"  "["  "]" cast_expression
	;
postfix_expression
	: primary_expression
	| postfix_expression  "["  expression  "]" 
	| postfix_expression  "("  expression_opt  ")"
	| postfix_expression "." name	
	| postfix_expression "->" name	
	| postfix_expression "++"	
	| postfix_expression "--"	
	;
primary_expression
	: literal
	| "this"							
	| name						
	| "(" expression ")"				
	;
name
	: "$Identifier"
	| class_cl2 "$Identifier"
	| "$constructor" "("  expression_opt  ")"
	| "$destructor" "$constructor" "("  expression_opt  ")"
	| operator_function_name
	| class_cl2 operator_function_name
	| "::" "$Identifier"
	;
literal
	: "$IntegerLiteral"			
	| "$FloatingPointLiteral"	
	| "$CharacterLiteral"		
	| string_literal			
	;
string_literal
	: "$StringLiteral"
	| string_literal "$StringLiteral"
	;
declaration
	: decl_specifiers ";"
	| var_fnc_declaration ";"
	| function_definition
	| "extern" "$StringLiteral" "{" declarations_opt "}"
	| template_declaration
	| constructor_definition
	| typedef_declaration
	| ";"
	;
var_fnc_declaration
	: decl_specifiers  declarator_list
	;
decl_specifiers
	: type_specifier											{{ $$ = new TDeclSpec(null, $1, null); }}
	| type_specifier decl_specifier_list						{{ $$ = new TDeclSpec(null, $1, $2); }}
	| decl_specifier_list type_specifier						{{ $$ = new TDeclSpec($1, $2, null); }}
	| decl_specifier_list type_specifier decl_specifier_list	{{ $$ = new TDeclSpec($1, $2, $3); }}
	;
decl_specifier_list
	: decl_specifier						{{ $$ = new TIntList(((TTkn)$1).tknTkn); }}
	| decl_specifier_list decl_specifier	{{ $$ = ((TIntList)$1).appendInt(((TTkn)$2).tknTkn); }}
	;
decl_specifier
	: storage_class_specifier
	| fct_specifier
	| cv_qualifier
	;
storage_class_specifier
	: "auto"
	| "register"
	| "static"
	| "extern"
	| "extern" "$StringLiteral"	{{ $$ = $1; }}//??????????
	| "friend" 																					
	| "__declspec" "(" extended_decl_modifier_seq ")"	{{ $$ = $1; }}//??????????
	;
fct_specifier
	: "inline" 																					
	| "__inline" 																				
	| "virtual"																					
	;
type_specifier
	: simple_type_name
	| class_specifier				{{ $$ = new TRCla("?????", pcInteger); }}//???????
	| enum_specifier				{{ $$ = new TRCla("?????", pcInteger); }}//???????
	| elaborated_type_specifier		{{ $$ = new TRCla("?????", pcInteger); }}//???????
	;
extended_decl_modifier_seq
	: extended_decl_modifier
	| extended_decl_modifier_seq extended_decl_modifier
	;
extended_decl_modifier
	: "thread"
	| "naked"
	| "dllimport"
	| "dllexport"
	;
simple_type_name
	: qualified_class_name
	| "char" 						{{ $$ = new TRCla("char", pcCharacter); }}
	| "short"  						{{ $$ = new TRCla("short", pcShort); }}
	| "int"  						{{ $$ = new TRCla("int", pcInteger); }}
	| "long"  						{{ $$ = new TRCla("long", pcInteger ); }}
	| "signed" "char" 				{{ $$ = new TRCla("signed char", pcCharacter); }}
	| "signed" "short" 				{{ $$ = new TRCla("signed short", pcShort); }}
	| "signed" "int" 				{{ $$ = new TRCla("signed int", pcInteger); }}
	| "signed" "long" 				{{ $$ = new TRCla("signed long", pcInteger ); }}
	| "signed" "__int8"				{{ $$ = new TRCla("signed __int8", pcCharacter); }}
	| "signed" "__int16"			{{ $$ = new TRCla("signed __int16", pcShort); }}
	| "signed" "__int32"			{{ $$ = new TRCla("signed __int32", pcInteger); }}
	| "signed" "__int64"			{{ $$ = new TRCla("signed __int64", pcInt64); }}

	| "unsigned" "char" 			{{ $$ = new TRCla("unsigned char", pcCharacterU); }}
	| "unsigned" "short" 			{{ $$ = new TRCla("unsigned short", pcShortU); }}
	| "unsigned" "int" 				{{ $$ = new TRCla("unsigned int", pcIntegerU); }}
	| "unsigned" "long" 			{{ $$ = new TRCla("unsigned long", pcIntegerU); }}
	| "unsigned" "__int8"			{{ $$ = new TRCla("unsigned __int8", pcCharacterU); }}
	| "unsigned" "__int16"			{{ $$ = new TRCla("unsigned __int16", pcShortU); }}
	| "unsigned" "__int32"			{{ $$ = new TRCla("unsigned __int32", pcIntegerU); }}
	| "unsigned" "__int64"			{{ $$ = new TRCla("unsigned __int64", pcInt64U); }}

	| "long" "int" 					{{ $$ = new TRCla("long int", pcInteger ); }}
	| "short" "int" 				{{ $$ = new TRCla("short int", pcShort); }}
	| "signed" "long" "int" 		{{ $$ = new TRCla("signed long int", pcInteger); }}
	| "signed" "short" "int" 		{{ $$ = new TRCla("signed short int", pcShort); }}
	| "unsigned" "long" "int" 		{{ $$ = new TRCla("unsigned long int", pcIntegerU); }}
	| "unsigned" "short" "int" 		{{ $$ = new TRCla("unsigned short int", pcShortU); }}

	| "signed" 						{{ $$ = new TRCla("signed", pcInteger); }}
	| "unsigned" 					{{ $$ = new TRCla("unsigned", pcIntegerU); }}
	| "float" 						{{ $$ = new TRCla("float", pcFloat); }}
	| "double"						{{ $$ = new TRCla("double", pcDouble); }}
	| "void"						{{ $$ = new TRCla("void", pcVoid); }}

	| "__int8"						{{ $$ = new TRCla("__int8", pcCharacter); }}
	| "__int16"						{{ $$ = new TRCla("__int16", pcShort); }}
	| "__int32"						{{ $$ = new TRCla("__int32", pcInteger); }}
	| "__int64"						{{ $$ = new TRCla("__int64", pcInt64); }}
	;
elaborated_type_specifier
	: class_key  "$Identifier"	{{ ((JZF)this).setTypeTkn((TTkn)$2); }} 
	| class_key  qualified_class_name
	;
class_key
	: "struct"
	| "union"
	| "class"
	;
qualified_class_name
	: class_name
	| class_cl2 class_name	{{ $$ = $2; }}//?????
	;
enum_specifier
	: "enum" "{" enumerator_list cm_opt "}"
	| "enum" "$Identifier" "{" enumerator_list cm_opt "}"	{{ ((JZF)this).setTypeTkn((TTkn)$2); }} 
	| "enum" "$Identifier"	{{ ((JZF)this).setTypeTkn((TTkn)$2); }} 
	;
enumerator_list
	: enumerator
	| enumerator_list "," enumerator
	; 
enumerator
	: "$Identifier"
	| "$Identifier" "=" constant_expression 
	;
constant_expression
	: conditional_expression
	;
declarator_list
	: init_declarator
	| declarator_list "," init_declarator
	;
template_declaration
	: "template" "<" template_argument_list ">" declaration
	;
template_argument_list
	: template_argument
	| template_argument_list "," template_argument
	;
template_argument
	: decl_specifiers declarator template_initializer_opt
	| decl_specifiers
	;
template_initializer
	: "=" shift_expression
	| "=" "{" initializer_list "}"
	| "=" "{" initializer_list "," "}"
	;
template_class_name
	: "$template_name" "<" template_arg_list ">"	{{ $$ = ((CZF)this).makeRCla((TTkn)$1); }}//??????
	;
template_arg_list
	: template_arg
	| template_arg_list "," template_arg
	;
template_arg
	: shift_expression		
	| type_name
	;
init_declarator
	: declarator		{{	if (tdefflg)	((JZF)this).setTypeTkn(typedef_tkn);	}}
	| declarator initializer
	;
declarator
	: direct_declarator
	| ptr_operator				{{ $$ = ((CZF)this).makePtr($1,null); }}
	| ptr_operator declarator	{{ $$ = ((CZF)this).makePtr($1,  $2); }}
	;
direct_declarator
	: "$Identifier"		
		{{ $$ = ((CZF)this).makeVar(null, null, false, (TTkn)$1); }}
	| class_cl2 "$Identifier"		
		{{ $$ = ((CZF)this).makeVar(null, $1, false, (TTkn)$2); }}
	| ms_modifier_list "$Identifier"		
		{{ $$ = ((CZF)this).makeVar((TIntList)$1, null, false, (TTkn)$2); }}
	| ms_modifier_list class_cl2 "$Identifier"		
		{{ $$ = ((CZF)this).makeVar((TIntList)$1, $2, false, (TTkn)$3); }}
	| operator_function_name
		{{ $$ = ((CZF)this).makeVar(null, null, true, (TTkn)$1); }}
	| class_cl2 operator_function_name
		{{ $$ = ((CZF)this).makeVar(null, $1, true, (TTkn)$2); }}
	| ms_modifier_list operator_function_name
		{{ $$ = ((CZF)this).makeVar((TIntList)$1, null, true, (TTkn)$2); }}
	| ms_modifier_list class_cl2 operator_function_name
		{{ $$ = ((CZF)this).makeVar((TIntList)$1, $2, true, (TTkn)$3); }}
	| direct_declarator a1 "(" expression ")" cv_qualifier_opt
		{{	fnc_nest--; $$ = $1; }}//?????
	| direct_declarator "[" constant_expression_opt "]"
		{{ $$ = $1; }}//?????
	| direct_declarator a1 "(" argument_declaration_list_opt ")" cv_qualifier_opt
		{{ $$ = ((CZF)this).makeFncPtr(null, null, false, $1, $4, $6); }}
	| "(" declarator ")" a1 "(" argument_declaration_list_opt ")" cv_qualifier_opt
		{{ $$ = ((CZF)this).makeFncPtr(null, null, false, $2, $6, $8); }}
	| "(" ms_modifier_list "*" ")" a1 "(" argument_declaration_list_opt ")" cv_qualifier_opt
		{{ $$ = ((CZF)this).makeFncPtr((TIntList)$2, null, true, null, $7, $9); }}
	| "(" ms_modifier_list "*" "$Identifier" ")" a1 "(" argument_declaration_list_opt ")" cv_qualifier_opt
		{{ $$ = ((CZF)this).makeFncPtr((TIntList)$2, null, true, $4, $8, $10); }}
			// typedef void (__stdcall *fnc) ();
	| "(" class_cl2 "*" ")"	 a1 "(" argument_declaration_list_opt ")" cv_qualifier_opt
		{{ $$ = ((CZF)this).makeFncPtr(null, $2, true, null, $7, $9); }}
			//void ( CWnd::*)(UINT, CPoint)
	| "(" class_cl2 "*" "$Identifier" ")" a1 "(" argument_declaration_list_opt ")" cv_qualifier_opt
		{{ $$ = ((CZF)this).makeFncPtr(null, $2, true, $4, $8, $10); }}
	//( CCmdTarget::*AFX_PMSG)
	;
a1
	:  			{{	fnc_nest++; }}
	;
ptr_operator
	: "*" cv_qualifier_opt	{{ $$ = ((CZF)this).makeTkn2($1, $2); }}
	| "&" cv_qualifier_opt	{{ $$ = ((CZF)this).makeTkn2($1, $2); }}
	;
cv_qualifier
	: "const"
	| "volatile"
	;
argument_declaration_list
	: arg_declaration_list
	| arg_declaration_list "," "..."		{{ $$ = ((VDeclTion)$1).addDeclTion(new TDeclTion(null,new TDecl("..."),null)); }}
	;
arg_declaration_list
	: argument_declaration_com							{{ $$ = new VDeclTion((TDeclTion)$1); }}
	| arg_declaration_list "," argument_declaration_com	{{ $$ = ((VDeclTion)$1).addDeclTion((TDeclTion)$3); }}
	;
argument_declaration_com
	: argument_declaration
	| argument_declaration "$Tail___Comment"		{{ ((TDeclTion)$1).setTailCom($2); $$ = $1; }}
	;
argument_declaration
	: decl_specifiers							{{ $$ = new TDeclTion($1,null,null); }}
	| decl_specifiers declarator				{{ $$ = new TDeclTion($1,  $2,null); }}
	| decl_specifiers declarator initializer	{{ $$ = new TDeclTion($1,  $2,  $3); }}
	| decl_specifiers initializer				{{ $$ = new TDeclTion($1,null,  $2); }}
	| function_pointer
	;
function_definition
	: function_definition_com					{{ ((CZF)this).dumpFnc($1); $$ = $1; }}
	| "$Head___Comment" function_definition_com	{{ ((TDeclTion)$2).setHeadCom($1); ((CZF)this).dumpFnc($2); $$ = $2; }}
	;
function_definition_com
	: decl_specifiers declarator fct_body	{{ $$ = new TDeclTion($1, $2, null); }}//?????
	;
fct_body
	: compound_statement
	;
type_name
	: decl_specifiers
	| decl_specifiers declarator
	| function_pointer
	;
initializer
	: "=" initializer_item
	;
initializer_list
	: initializer_item
	| initializer_list "," initializer_item
	;
initializer_item
	: assignment_expression
	| "{" initializer_list "}"
	| "{" initializer_list "," "}"
	;
class_specifier
	: class_head "{" member_list "}"			{{	if (tdefflg)	struct_nest--;	}}
	;
class_head
	: class_key 					{{	if (tdefflg)	struct_nest++;	}}
	| class_key  "$Identifier" base_spec_opt 	{{	if (tdefflg)	struct_nest++;	((JZF)this).setTypeTkn((TTkn)$2); }} 
	| class_key  class_name	 base_spec_opt	{{	if (tdefflg)	struct_nest++; }} 		
	| class_key storage_class_specifier					{{	if (tdefflg)	struct_nest++;	}}
	| class_key storage_class_specifier "$Identifier" base_spec_opt 	{{	if (tdefflg)	struct_nest++;	((JZF)this).setTypeTkn((TTkn)$3); }} 
	| class_key storage_class_specifier class_name	 base_spec_opt	{{	if (tdefflg)	struct_nest++; }} 		
	;
member_list
	: member_declaration
	| member_list member_declaration
	| access_specifier ":" 
	| member_list access_specifier ":" 
	;
member_declaration
	: decl_specifiers member_declarator_list_opt ";"	//?? member_declarator_list‚ªstruct‚È‚ç–³–¼‚Å‚à‚æ‚¢
	| function_definition
	| constructor_definition
	| constructor_declaration
	| typedef_declaration
	| ";"
	;
member_declarator_list
	: member_declarator
	| member_declarator_list "," member_declarator
	;
member_declarator
	: declarator pure_specifier_opt
	| declarator ":" constant_expression
	;
pure_specifier
	: "="  "$IntegerLiteral"
	;
class_name
	: "$type"				{{ $$ = ((CZF)this).makeRCla((TTkn)$1); }}
	| template_class_name
	;
base_spec
	: ":"  base_list
	;
base_list
	: base_specifier 
	| base_list "," base_specifier
	;
base_specifier
	: class_name
	| virtual access_specifier_opt  class_name 
	| access_specifier virtual_opt  class_name
	;
access_specifier
	: "private" 
	| "protected" 
	| "public"
	;
conversion_function_name
	: "operator" simple_type_name 
	| conversion_function_name ptr_operator
	;
ctor_initializer
	: ":" mem_initializer_list
	;
mem_initializer_list
	: mem_initializer 
	| mem_initializer_list "," mem_initializer  
	;
mem_initializer
	: "$constructor"   "(" expression_opt  ")"		 	
	| "$Identifier" "(" expression_opt  ")"
	;
operator_function_name
	: "operator" operator_kind1		{{ $$ = $2; }}
	;
operator_kind1
	: "new"
	| "delete"
	| "[" "]"	{{ $$ = $1; }}
	| "="
	| "+"
	| "+="
	| "<<"
	| ">>"
	| "-"
	| "*"
	| "/"
	| "%"
	| "^"
	| "&"
	| "|"
	| "~"
	| "!"
	| "<"
	| ">"
	| "-="
	| "*="
	| "/="
	| "%="
	| "^="
	| "&="
	| "|="
	| ">>="
	| "<<="
	| "=="
	| "!="
	| "<="
	| ">="
	| "&&"
	| "||"
	| "++"
	| "--"
	| ","
	| "->*"
	| "->"
	| "(" ")"	{{ $$ = $1; }}
	;
ms_modifier_list
	: ms_modifier					{{ $$ = new TIntList(((TTkn)$1).tknTkn); }}
	| ms_modifier_list ms_modifier	{{ $$ = ((TIntList)$1).appendInt(((TTkn)$2).tknTkn); }}
	;
ms_modifier
	: "_cdecl"
	| "__cdecl"
	| "__fastcall"
	| "__stdcall"
	;
constructor_definition
	: declarator2 ctor_initializer_opt  fct_body
	| decl_specifier_list declarator2 ctor_initializer_opt  fct_body
	;
constructor_declaration
	: declarator2 ";"
	| decl_specifier_list declarator2 ";"
	;
declarator2
	: declarator21
	| class_cl2 declarator21
	| ms_modifier_list declarator21
	| ms_modifier_list class_cl2 declarator21
	;
declarator21
	: "$constructor" "(" argument_declaration_list_opt ")"
	| function_pointer
	| "$destructor" "$constructor" "(" argument_declaration_list_opt ")"
	| conversion_function_name "(" argument_declaration_list_opt ")" cv_qualifier_opt
	;
destructor
	: "$destructor"
	;
typedef_declaration
	: typedef_head decl_specifiers declarator_list ";"
		{{	tdefflg = false; struct_nest = fnc_nest = 0;	}}
	;
typedef_head
	: "typedef"		{{ struct_nest = fnc_nest = 0; typedef_tkn = null; tdefflg = true; }}
	;
function_pointer
	: "$constructor" "(" ms_modifier_list "*" "$Identifier" ")" "(" argument_declaration_list_opt ")"
		{{ $$ = ((CZF)this).makeFncDeclTion($1, (TIntList)$3, null, $5, $8); }}
 		//BOOL (__stdcall* lpfn)(HDC, int)
 	| "$constructor" "(" class_cl2 "*" ")" "(" argument_declaration_list_opt ")"
		{{ $$ = ((CZF)this).makeFncDeclTion($1, null, $3, null, $7); }}
 		//BOOL ( CWnd::*)(CWnd*, UINT, UINT)
	;	
cm
	: ","
	;
sm	
	: ";"
	;
class_cl2
	: "$class::" "::"				{{ $$ = ((CZF)this).makeRCla((TTkn)$1); }}//????????
	| template_class_name "::"		{{ $$ = $1; }}
	| class_cl2 "$class::" "::"		{{ $$ = ((CZF)this).makeRCla((TTkn)$2); }}//????????
	;
virtual
	: "virtual"
	;
cl2
	: "::"
	;