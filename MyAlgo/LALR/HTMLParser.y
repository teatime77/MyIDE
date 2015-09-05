%{
%}
{
"</"	ETAG
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
"$Identifier"			IDENTIFIER

"EOF"				EOF
}

Goal
	: CompilationUnit
	;
CompilationUnit
	: Tags
	;
Tags
	: Tag
	| Tags Tag
	;
Tag
	: "<" "$Identifier" ">"			{{ TSys.Log("start tag " + LaxLex.getText((TTknLax)$2) + "\n"); }} 
	| "</" "$Identifier" ">"		{{ TSys.Log("end   tag " + LaxLex.getText((TTknLax)$2) + "\n"); }} 
	;
