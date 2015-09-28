Imports System.Diagnostics

Public Enum EClass
    eClassCla
    eEnumCla
    eStructCla
    eInterfaceCla
    eDelegateCla
End Enum

Public Enum EGeneric
    UnknownClass
    SimpleClass
    ParameterizedClass
    ArgumentClass
    SpecializedClass
End Enum

Public Enum EApply
    eUnknownApp
    eCallApp
    eArrayApp
    eStringApp
    eListApp
    eDictionaryApp
End Enum

Public Enum EAggregateFunction
    eSum
    eMax
    eMin
End Enum

Public Enum EToken
    eProtected
    eFriend
    eAddressOf
    eAnp
    eMns
    eADD
    eAnd
    eAppCall
    eASN
    eAt
    eBase
    eBaseNew
    eBaseCall
    eBreak
    eCast
    eComma
    eComment
    eConst
    eCType
    eDefault
    eDIV
    eExtends
    eFor
    eForeach
    eGE
    eGoto
    eGT
    eHAT
    eBlockComment
    eLineComment
    eIIF
    eImplements
    eLabel
    eLC
    eLE
    eLT
    eMMB
    eMOD
    eMUL
    eNamespace
    eNE
    eNext
    eNL
    eOR
    eOut
    eQUE
    eRC
    eRef
    eSM
    eShared
    eSub
    eTilde
    eTo
    eTypeof
    eVirtual
    eVLine
    eAbstract
    eAggregate
    eAs
    eByVal
    eCall
    eCase
    eCatch
    eClass
    eDelegate
    eDim
    eDo
    eEach
    eElse
    eElseIf
    eEnd
    eEndIf
    eEndSub
    eEndFunction
    eEndOperator
    eEndClass
    eEndStruct
    eEndInterface
    eEndEnum
    eEndModule
    eEndSelect
    eEndTry
    eEndWith
    eEnum
    eExit
    eExitDo
    eExitFor
    eExitSub
    eFrom
    eFunction
    eGet
    eGetType
    eHandles
    eIf
    eIfBlock
    eImports
    eIn
    eInherits
    eInterface
    eInto
    eIs
    eIsNot
    eIterator
    eLoop
    eMustOverride
    eModule
    eNew
    eNot
    eOf
    eOperator
    eOverride
    eParamArray
    ePartial
    ePrivate
    ePublic
    eReDim
    eReturn
    eSelect
    eStep
    eStruct
    eTake
    eThen
    eThrow
    eTry
    eUnknown
    eVar
    eVarDecl
    eWhere
    eWhile
    eWith
    eYield
    eDot
    eEq
    eLP
    eRP
    eLB
    eRB
    eId
    eInt
    eHex
    eSet
    eChar
    eString
    eRegEx
    eADDEQ
    eSUBEQ
    eMULEQ
    eDIVEQ
    eMODEQ
    eEOT
    eMAX_ETkn
End Enum

Public Class TToken
    Public SpcTkn As Integer
    Public TypeTkn As EToken
    Public StrTkn As String
    Public PosTkn As Integer

    Public tknSym As Integer
    Public ObjTkn As Object

    Public Sub New()
    End Sub

    Public Sub New(type1 As EToken, str1 As String, pos1 As Integer)
        TypeTkn = type1
        StrTkn = str1
        PosTkn = pos1
    End Sub

    Public Sub New(type1 As EToken, str1 As String)
        TypeTkn = type1
        StrTkn = str1
        PosTkn = 0
    End Sub

    Public Sub New(str1 As String, obj As Object)
        StrTkn = str1
        ObjTkn = obj
    End Sub

    Public Sub New(obj As Object)
        ObjTkn = obj
    End Sub

    Public Sub New(type1 As EToken, obj As Object)
        TypeTkn = type1
        ObjTkn = obj
    End Sub
End Class

' -------------------------------------------------------------------------------- TModifier
Public Class TModifier
    Public ValidMod As Boolean
    Public isPartial As Boolean
    Public isPublic As Boolean
    Public isShared As Boolean
    Public isConst As Boolean
    Public isMustOverride As Boolean
    Public isOverride As Boolean
    Public isAbstract As Boolean
    Public isVirtual As Boolean
    Public isIterator As Boolean

    Public isXmlIgnore As Boolean

    Public TokenListMod As List(Of TToken)
End Class

' -------------------------------------------------------------------------------- TTerm
Public Class TTerm
    Public UpTrm As Object
    Public TokenList As List(Of TToken)
    Public FunctionTrm As TFunction
    Public TypeTrm As TClass
    Public ProjectTrm As TProject

    Public Sub New()
    End Sub

    Public Function IsApp() As Boolean
        If TypeOf Me Is TApply Then
            Select Case CType(Me, TApply).TypeApp
                Case EToken.eCast, EToken.eAppCall, EToken.eADD, EToken.eBaseNew, EToken.eNew, EToken.eMns, EToken.eMUL, EToken.eBaseCall, EToken.eDIV, EToken.eMOD, EToken.eGetType
                    Return True
            End Select
        End If

        Return False
    End Function

    Public Function IsRel() As Boolean
        If TypeOf Me Is TApply Then
            Select Case CType(Me, TApply).TypeApp
                Case EToken.eEq, EToken.eNE, EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE, EToken.eIsNot, EToken.eTypeof, EToken.eIs
                    Return True
            End Select
        End If

        Return False
    End Function

    Public Function IsOpr() As Boolean
        Dim app1 As TApply

        If TypeOf Me Is TApply Then
            app1 = CType(Me, TApply)
            Select Case app1.TypeApp
                Case EToken.eAnd, EToken.eOR, EToken.eNot, EToken.eAnp
                    Return True
            End Select
            If app1.Negation Then
                Return True
            End If
        End If

        Return False
    End Function

    Public Function IsLog() As Boolean
        Return IsOpr() OrElse IsRel()
    End Function
End Class

' -------------------------------------------------------------------------------- TVariable
Public Class TVariable
    Public Shared VarCnt As Integer
    Public UpVar As Object
    Public IdxVar As Integer
    Public ModVar As TModifier
    Public NameVar As String
    Public TypeVar As TClass
    Public InitVar As TTerm
    Public RefVar As New TList(Of TReference)
    Public ByRefVar As Boolean = False
    Public ParamArrayVar As Boolean = False
    Public ComVar As TComment
    Public NoType As Boolean = False
    Public UsedVar As Boolean = False
    ' public List<TReference> UseVar = new List<TReference>();
    ' public List<TReference> DefVar = new List<TReference>();

    Public TokenListVar As List(Of TToken)

    Public Sub CopyVarMem(var1 As TVariable)
        var1.ModVar = ModVar
        var1.NameVar = NameVar
        var1.TypeVar = TypeVar
        Debug.Assert(InitVar Is Nothing)
        var1.InitVar = Nothing
        var1.ByRefVar = ByRefVar
    End Sub

    Sub IniVar(name1 As String)
        IdxVar = VarCnt
        VarCnt = VarCnt + 1
        NameVar = name1
    End Sub

    Public Sub New()
    End Sub

    Public Sub New(mod1 As TModifier, id1 As TToken, type1 As TClass)
        ModVar = mod1
        IniVar(id1.StrTkn)
        TypeVar = type1
    End Sub

    Public Sub New(id1 As TToken, type1 As TClass)
        IniVar(id1.StrTkn)
        TypeVar = type1
    End Sub

    Public Sub New(name1 As String, type1 As TClass)
        IniVar(name1)
        TypeVar = type1
    End Sub

    Public Function GetClassVar() As TClass
        If TypeOf Me Is TField Then
            Return CType(Me, TField).ClaFld
        ElseIf TypeOf Me Is TFunction Then
            Return CType(Me, TFunction).ClaFnc
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Overrides Function ToString() As String
        Return NameVar
    End Function
End Class

' -------------------------------------------------------------------------------- TClass
Public Class TClass
    Inherits TVariable
    Public ProjectCla As TProject
    Public KndCla As EClass = EClass.eClassCla
    Public OrgCla As TClass
    Public GenCla As TList(Of TClass)
    Public SuperClassList As New TList(Of TClass)
    Public SubClasses As New TList(Of TClass)
    Public AllSuperCla As TList(Of TClass)
    Public InterfaceList As New TList(Of TClass)
    Public FldCla As New TList(Of TField)
    Public FncCla As New TList(Of TFunction)
    Public DimCla As Integer = 0
    Public SrcCla As TSourceFile
    Public IsParamCla As Boolean = False
    Public GenericType As EGeneric
    Public Parsed As Boolean = False

    Public TokenListCls As List(Of TToken)

    Public Sub New()
        Debug.WriteLine("@a")
    End Sub

    Public Sub New(prj1 As TProject, name1 As String)
        MyBase.New(name1, CType(Nothing, TClass))
        ProjectCla = prj1
    End Sub

    Public Sub New(prj1 As TProject, mod1 As TModifier, name1 As String)
        MyBase.New(name1, CType(Nothing, TClass))
        ProjectCla = prj1
        ModVar = mod1
    End Sub

    Public Function NameCla() As String
        Return NameVar
    End Function

    Public Function ModCla() As TModifier
        If ModVar Is Nothing Then
            ModVar = New TModifier()
        End If
        Return ModVar
    End Function

    Public Function ComCla() As TComment
        Return ComVar
    End Function

    Public Sub AddFld(fld As TField)
        FldCla.Add(fld)
        fld.ClaFld = Me
    End Sub

    Public Shared Function MakeArray(cla1 As TClass, dim1 As Integer) As TClass
        Dim cla2 As TClass

        Debug.Assert(cla1 IsNot Nothing)
        cla2 = New TClass(cla1.ProjectCla, "Array")
        cla2.GenCla = New TList(Of TClass)()
        cla2.GenCla.Add(cla1)
        cla2.DimCla = dim1
        cla2.GenericType = EGeneric.SpecializedClass

        Return cla2
    End Function

    Public Sub AllSuperCla2(vcla As TList(Of TClass))
        vcla.AddRange(SuperClassList)
        vcla.AddRange(InterfaceList)
        ' for Each cla1 In SuperClassList Call cla1
        For Each cla1 In SuperClassList
            cla1.AllSuperCla2(vcla)
        Next

        ' for Each cla1 In InterfaceList Call cla1
        For Each cla1 In InterfaceList
            cla1.AllSuperCla2(vcla)
        Next
    End Sub

    Public Sub SetAllSuperClass()
        AllSuperCla = New TList(Of TClass)()
        AllSuperCla2(AllSuperCla)
    End Sub

    ' このクラスが引数のクラスのサブクラスならTrueを返す
    Public Function IsSubcla(cla1 As TClass) As Boolean
        Return AllSuperCla.Contains(cla1)
    End Function

    ' このクラスが引数のクラスと同じかサブクラスならTrueを返す
    Public Function IsSubsetOf(cla1 As TClass) As Boolean
        Return cla1 Is Me OrElse AllSuperCla.Contains(cla1)
    End Function

    Public Function IsArray() As Boolean
        Return DimCla <> 0
    End Function

    Public Function IsList() As Boolean
        Return NameType() = "List" OrElse NameType() = "TList"
    End Function

    Public Function IsDictionary() As Boolean
        Return NameType() = "Dictionary"
    End Function

    Public Function HasIndex() As Boolean
        Select Case NameType()
            Case "List", "TList", "Dictionary", "String", "Array"
                Return True
            Case Else
                Return False
        End Select
    End Function

    Public Function NameType() As String
        Return NameCla()
    End Function

    Public Function IsAtomType() As Boolean
        Select Case NameType()
            Case "bool", "byte", "char", "int", "float", "double", "string", "Boolean", "Byte", "Char", "Integer", "Single", "Double", "String"
                Return True
        End Select

        If KndCla = EClass.eEnumCla Then
            Return True
        End If

        Return False
    End Function

    Public Function LongName() As String
        Dim sw As TStringWriter

        If GenCla Is Nothing Then
            If DimCla = 0 Then
                Return NameVar
            Else
                Return String.Format("{0}({1})", NameVar, DimCla)
            End If
        End If

        sw = New TStringWriter()
        sw.Write(NameVar + "<")
        For Each cla1 In GenCla
            sw.Write(" " + cla1.LongName())
        Next
        sw.Write(" >")
        If DimCla <> 0 Then
            sw.Write("({0})", DimCla)
        End If

        Return sw.ToString()
    End Function

    Public Overrides Function ToString() As String
        Return String.Format("{0} {1}", LongName(), IdxVar)
    End Function
End Class

' -------------------------------------------------------------------------------- TDelegate
Public Class TDelegate
    Inherits TClass
    Public RetDlg As TClass
    Public ArgDlg As New TList(Of TVariable)

    Public Sub New(prj1 As TProject, name1 As String)
        MyBase.New(prj1, name1)
    End Sub

    Public Sub New(prj1 As TProject, fnc1 As TFunction)
        MyBase.New(prj1, "Delegate@" + fnc1.NameVar)
        RetDlg = fnc1.RetType
        ArgDlg = New TList(Of TVariable)(fnc1.ArgFnc)
    End Sub

End Class

' -------------------------------------------------------------------------------- TField
Public Class TField
    Inherits TVariable
    Public ClaFld As TClass
    Public TailCom As String
    Public OrgFld As TField

    Public Sub New()
    End Sub

    Public Sub New(mod1 As TModifier, id1 As TToken, type1 As TClass)
        MyBase.New(mod1, id1, type1)
    End Sub

    Public Sub New(name1 As String, type1 As TClass)
        MyBase.New(name1, type1)
    End Sub

    Public Sub New(name1 As String, type1 As TClass, init As TTerm)
        MyBase.New(name1, type1)
        InitVar = init
    End Sub

    Public Function FullFldName() As String
        If ClaFld Is Nothing Then
            Return NameVar
        Else
            Return ClaFld.NameCla() + "_" + NameVar
        End If
    End Function

    Public Overrides Function ToString() As String
        If ClaFld Is Nothing Then
            Return NameVar
        Else
            Return ClaFld.NameCla() + "." + NameVar
        End If
    End Function
End Class

' -------------------------------------------------------------------------------- TFunction
Public Class TFunction
    Inherits TVariable
    Public TypeFnc As EToken
    Public IsNew As Boolean = False
    Public OpFnc As EToken = EToken.eUnknown
    Public ClaFnc As TClass
    Public RetType As TClass
    Public InterfaceFnc As TClass
    Public ImplFnc As TReference
    Public ArgFnc As New TList(Of TVariable)
    Public ThisFnc As TVariable
    Public BlcFnc As TBlock
    Public OrgFnc As TFunction

    Public CallFrom As New TList(Of TFunction)
    Public CallTo As New TList(Of TFunction)
    Public CallToAll As New TList(Of TFunction)
    Public OvrFnc As TFunction
    Public OvredFnc As New TList(Of TFunction)
    Public EqOvredFncAll As New TList(Of TFunction)
    Public Reachable As Boolean
    Public RefFnc As New TList(Of TReference)
    Public LabelCount As Integer

    Public Sub CopyFncMem(fnc1 As TFunction)
        CopyVarMem(fnc1)
        fnc1.IsNew = IsNew
    End Sub

    Public Sub New()
    End Sub

    Public Sub New(id1 As TToken, type1 As TClass)
        MyBase.New(id1, Nothing)
        RetType = type1
    End Sub

    Public Sub New(name1 As String, ret_type As TClass)
        MyBase.New(name1, CType(Nothing, TClass))
        RetType = ret_type
    End Sub

    Public Overrides Function ToString() As String
        Return FullName()
    End Function

    Public Function ModFnc() As TModifier
        Return ModVar
    End Function

    Public Sub SetModFnc(mod1 As TModifier)
        ModVar = mod1
    End Sub

    Public Function NameFnc() As String
        Return NameVar
    End Function

    Public Function FullName() As String
        If ClaFnc Is Nothing Then
            Return NameFnc()
        Else
            Return ClaFnc.LongName() + "." + NameFnc()
        End If
    End Function

    Public Function OpName() As String
        Return "_OP_" + OpFnc.ToString().Substring(1)
    End Function

    Public Function IsGenerated() As Boolean
        Return NameFnc() = "Class@Initializer" OrElse NameFnc() = "Instance@Initializer" OrElse NameFnc() = "Implicit@New"
    End Function
End Class

' -------------------------------------------------------------------------------- TAtom
Public Class TAtom
    Inherits TTerm
    Public Shared AtmCnt As Integer
    Public IdxAtm As Integer
    Public NameRef As String = "noname_ref"

    Public Sub New()
        IdxAtm = AtmCnt
        AtmCnt += 1
    End Sub

    Public Sub New(name1 As String)
        NameRef = name1
        IdxAtm = AtmCnt
        AtmCnt += 1
    End Sub
End Class

' -------------------------------------------------------------------------------- TConstant
Public Class TConstant
    Inherits TAtom
    Public TypeAtm As EToken

    Public Sub New()
    End Sub

    Public Sub New(type1 As EToken, name As String)
        MyBase.New(name)
        TypeAtm = type1
    End Sub
End Class

' -------------------------------------------------------------------------------- TReference
Public Class TReference
    Inherits TAtom
    Public VarRef As TVariable
    Public DefRef As Boolean = False
    Public IsAddressOf As Boolean = False


    Public Sub New()
        MyBase.New()
    End Sub

    Public Sub New(name1 As String)
        MyBase.New(name1)
    End Sub

    Public Sub New(tkn1 As TToken)
        MyBase.New(tkn1.StrTkn)
    End Sub

    Public Sub New(var1 As TVariable)
        MyBase.New()
        VarRef = var1
        NameRef = var1.NameVar
    End Sub

    Public Overrides Function ToString() As String
        If VarRef Is Nothing Then
            Return NameRef
        Else
            Return VarRef.ToString()
        End If
    End Function
End Class

' -------------------------------------------------------------------------------- TParenthesis
Public Class TParenthesis
    Inherits TTerm
    Public TrmPar As TTerm

    Public Sub New(trm1 As TTerm)
        TrmPar = trm1
    End Sub
End Class

' -------------------------------------------------------------------------------- TApply
Public Class TApply
    Inherits TTerm
    Public TypeApp As EToken = EToken.eUnknown
    Public Negation As Boolean = False
    Public ArgApp As New TList(Of TTerm)
    Public KndApp As EApply = EApply.eUnknownApp
    Public FncApp As TTerm
    Public ClassApp As TClass
    Public NewApp As TClass
    Public IniApp As TArray

    Public Sub AddInArg(trm1 As TTerm)
        ArgApp.Add(trm1)
    End Sub

    Public Shared Function MakeAppNew(type1 As TClass) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        app1.TypeApp = EToken.eNew
        app1.NewApp = type1
        app1.FncApp = New TReference("New@" + type1.NameCla())

        Return app1
    End Function

    Public Shared Function MakeAppBase(tkn1 As TToken) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        If tkn1.TypeTkn = EToken.eNew Then
            app1.TypeApp = EToken.eBaseNew
        Else
            app1.TypeApp = EToken.eBaseCall
        End If
        app1.FncApp = New TReference(tkn1)

        Return app1
    End Function

    Public Shared Function MakeAppCall(fnc1 As TTerm) As TApply
        Dim app1 As TApply

        Debug.Assert(fnc1 IsNot Nothing)
        If TypeOf fnc1 Is TDot OrElse TypeOf fnc1 Is TReference OrElse fnc1.IsApp() Then
        Else
            Debug.Assert(False, "make app call:new type")
        End If
        app1 = New TApply()
        app1.TypeApp = EToken.eAppCall
        app1.FncApp = fnc1

        Return app1
    End Function

    Public Shared Function MakeApp1Opr(tkn1 As TToken, trm1 As TTerm) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        app1.TypeApp = tkn1.TypeTkn

        app1.FncApp = New TReference(tkn1)

        app1.AddInArg(trm1)

        Return app1
    End Function

    Public Shared Function MakeApp2Opr(tkn1 As TToken, trm1 As TTerm, trm2 As TTerm) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        app1.TypeApp = tkn1.TypeTkn

        app1.FncApp = New TReference(tkn1)

        app1.AddInArg(trm1)
        app1.AddInArg(trm2)

        Return app1
    End Function

    Public Shared Function MakeApp3Opr(tkn1 As TToken, trm1 As TTerm, trm2 As TTerm, trm3 As TTerm) As TApply
        Dim app1 As TApply

        Debug.Assert(tkn1.TypeTkn = EToken.eQUE)
        app1 = New TApply()
        app1.TypeApp = tkn1.TypeTkn

        app1.FncApp = New TReference(tkn1)

        app1.AddInArg(trm1)
        app1.AddInArg(trm2)
        app1.AddInArg(trm3)
        Return app1
    End Function

    Public Shared Function MakeAppCastClass(type1 As TClass) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        app1.TypeApp = EToken.eCast
        app1.ClassApp = type1

        Return app1
    End Function

    Public Shared Function MakeAppGetType(type1 As TClass) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        app1.TypeApp = EToken.eGetType
        app1.ClassApp = type1

        Return app1
    End Function

    Public Shared Function NewOpr(type1 As EToken) As TApply
        Dim app1 As New TApply

        app1.TypeApp = type1

        Return app1
    End Function

    Public Shared Function NewOpr2(type1 As EToken, trm1 As TTerm, trm2 As TTerm) As TApply
        Dim app1 As New TApply

        app1.TypeApp = type1
        app1.AddInArg(trm1)
        app1.AddInArg(trm2)

        Return app1
    End Function

    Public Shared Function NewTypeOf(trm1 As TTerm, type2 As TClass) As TApply
        Dim app1 As New TApply

        app1.TypeApp = EToken.eTypeof
        app1.AddInArg(trm1)
        app1.AddInArg(New TReference(type2))

        Return app1
    End Function

    Public Sub TrmSrcSet(mk As TCodeGenerator, trm1 As TTerm)
        Select Case KndApp

            Case EApply.eListApp, EApply.eDictionaryApp
                Debug.Assert(ArgApp.Count = 1)

                mk.TrmSrc(FncApp)
                mk.WordAdd(".", EFigType.eSymFig, Me)
                If KndApp = EApply.eListApp Then
                    mk.WordAdd("set", EFigType.eRefFig, Me)
                Else
                    mk.WordAdd("put", EFigType.eRefFig, Me)
                End If
                mk.WordAdd("(", EFigType.eSymFig, Me)
                mk.TrmSrc(ArgApp(0))
                mk.WordAdd(",", EFigType.eSymFig, Me)
                mk.TrmSrc(trm1)
                mk.WordAdd(")", EFigType.eSymFig, Me)
            Case Else
                Debug.Assert(False)
        End Select
    End Sub
End Class

' -------------------------------------------------------------------------------- TDot
Public Class TDot
    Inherits TReference
    Public TrmDot As TTerm
    Public TypeDot As TClass

    Public Sub New(trm1 As TTerm, name1 As String)
        MyBase.New(name1)
        '        Debug.Assert(trm1 IsNot Nothing)
        TrmDot = trm1
    End Sub

    Public Sub New(trm1 As TTerm, var1 As TVariable)
        MyBase.New(var1)
        TrmDot = trm1
    End Sub

    Public Function IsEnumDot() As Boolean
        Dim fld As TField, fnc As TFunction

        If TypeOf VarRef Is TField Then
            fld = CType(VarRef, TField)
            If fld.ClaFld IsNot Nothing AndAlso fld.ClaFld.KndCla = EClass.eEnumCla Then
                ' ジェネリック型の場合ClaFldはNothing
                Return True
            End If
        ElseIf TypeOf VarRef Is TFunction Then
            fnc = CType(VarRef, TFunction)
            If fnc.ClaFnc IsNot Nothing AndAlso fnc.ClaFnc.KndCla = EClass.eEnumCla Then
                Return True
            End If
        End If

        Return False
    End Function
End Class

' -------------------------------------------------------------------------------- TFrom
' From i In v1 Where i Mod 2 = 0 Select AA(i)
Public Class TFrom
    Inherits TTerm
    Public VarFrom As TVariable
    Public SeqFrom As TTerm
    Public CndFrom As TTerm
    Public SelFrom As TTerm
    Public TakeFrom As TTerm
End Class

' -------------------------------------------------------------------------------- TAggregate
' Aggregate x In v Into Sum(x.Value)
Public Class TAggregate
    Inherits TTerm
    Public VarAggr As TVariable
    Public SeqAggr As TTerm
    Public FunctionAggr As EAggregateFunction
    Public IntoAggr As TTerm
End Class

' -------------------------------------------------------------------------------- TArray
Public Class TArray
    Inherits TTerm
    Public TrmArr As New TList(Of TTerm)

    Public Sub New()
    End Sub
End Class

' -------------------------------------------------------------------------------- TStatement
Public Class TStatement
    Shared StmtCnt As Integer
    Public ParentStmt As Object
    Public ComStmt As TList(Of TToken)
    Public TailCom As String
    Public vTknStmt As TList(Of TToken)
    Public StmtIdx As Integer
    Public TypeStmt As EToken
    Public BlcStmt As TBlock
    Public RefStmt As New TList(Of TReference)
    Public IsGenerated As Boolean
    Public UsedStmt As Boolean
    Public ValidStmt As Boolean = True
    Public BeforeSrc As String
    Public AfterSrc As String = ""

    Public TokenList As List(Of TTokenLine)
    Public TabStmt As Integer
    Public FunctionStmt As TFunction

    Public Sub New()
        StmtIdx = StmtCnt
        StmtCnt = StmtCnt + 1
    End Sub

    Public Overridable Function JumpEnd() As Boolean
        Select Case TypeStmt
            Case EToken.eReturn, EToken.eExitDo, EToken.eExitFor, EToken.eExitSub
                ' ジャンプする場合

                Return True
        End Select

        Return False
    End Function

    Public Function ProjectStmt() As TProject
        Return FunctionStmt.ClaFnc.ProjectCla
    End Function
End Class

' -------------------------------------------------------------------------------- TVariableDeclaration
Public Class TVariableDeclaration
    Inherits TStatement
    Public ModDecl As TModifier
    Public TypeDecl As TClass
    Public VarDecl As New TList(Of TVariable)

    Public Sub New()
    End Sub

    Public Sub New(vvar1 As TList(Of TVariable))
        VarDecl = vvar1
    End Sub

End Class

' -------------------------------------------------------------------------------- TBlock
Public Class TBlock
    Inherits TStatement
    Public VarBlc As New TList(Of TVariable)
    Public StmtBlc As New TList(Of TStatement)

    Public Sub AddStmtBlc(stmt1 As TStatement)
        StmtBlc.Add(stmt1)
        stmt1.BlcStmt = Me
    End Sub

    Public Overrides Function JumpEnd() As Boolean
        Dim stmt1 As TStatement, i1 As Integer

        i1 = StmtBlc.Count - 1
        Do While 0 <= i1
            stmt1 = StmtBlc(i1)

            Select Case stmt1.TypeStmt
                Case EToken.eComment
                    ' コメント文の場合

                    i1 = i1 - 1
                Case Else
                    ' コメント文でない場合

                    Return stmt1.JumpEnd()
            End Select
        Loop

        Return False
    End Function
End Class

' -------------------------------------------------------------------------------- TIf
Public Class TIf
    Inherits TStatement
    Public IfBlc As New TList(Of TIfBlock)

    Public Sub New()
        TypeStmt = EToken.eIf
    End Sub

    Public Overrides Function JumpEnd() As Boolean
        ' for Find
        For Each if_blc In IfBlc
            If Not if_blc.BlcIf.JumpEnd() Then
                Return False
            End If
        Next

        Return True
    End Function
End Class

' -------------------------------------------------------------------------------- TIfBlock
Public Class TIfBlock
    Inherits TStatement
    Public BlcIf As TBlock
    Public CndIf As TTerm

    Public Sub New()
        TypeStmt = EToken.eIfBlock
    End Sub

    Public Sub New(cnd As TTerm, blc As TBlock)
        TypeStmt = EToken.eIfBlock
        CndIf = cnd
        BlcIf = blc
    End Sub
End Class

' -------------------------------------------------------------------------------- TCase
Public Class TCase
    Inherits TStatement
    Public TrmCase As New TList(Of TTerm)
    Public DefaultCase As Boolean
    Public BlcCase As TBlock

    Public Sub New()
        TypeStmt = EToken.eCase
    End Sub

    Public Sub AddTrmCase(trm As TTerm)
        TrmCase.Add(trm)
    End Sub
End Class

' -------------------------------------------------------------------------------- TSelect
Public Class TSelect
    Inherits TStatement
    Public TrmSel As TTerm
    Public CaseSel As New TList(Of TCase)

    Public Sub New()
        TypeStmt = EToken.eSelect
    End Sub
End Class

' -------------------------------------------------------------------------------- TTry
Public Class TTry
    Inherits TStatement
    Public BlcTry As TBlock
    Public BlcCatch As TBlock
    Public VarCatch As TList(Of TVariable)
End Class

' -------------------------------------------------------------------------------- TFor
Public Class TFor
    Inherits TStatement
    Public IdxVarFor As TVariable
    Public InVarFor As TVariable
    Public IdxFor As TReference
    Public FromFor As TTerm
    Public ToFor As TTerm
    Public StepFor As TTerm
    Public InTrmFor As TTerm
    Public IniFor As TStatement
    Public CndFor As TTerm
    Public StepStmtFor As TStatement
    Public BlcFor As TBlock
    Public IsDo As Boolean = False
    Public LabelFor As Integer

    Public Sub New()
        TypeStmt = EToken.eFor
    End Sub

    Public Sub New(prj1 As TProject, idx_name As String, from1 As TTerm, to1 As TTerm)
        TypeStmt = EToken.eFor

        IdxVarFor = New TVariable(idx_name, prj1.IntType)
        IdxFor = New TReference(IdxVarFor)
        FromFor = from1
        ToFor = to1
    End Sub
End Class

' -------------------------------------------------------------------------------- TWith
Public Class TWith
    Inherits TStatement

    Public TermWith As TTerm
    Public BlcWith As TBlock

    Public Sub New()
        TypeStmt = EToken.eWith
    End Sub
End Class

' -------------------------------------------------------------------------------- TWith
Public Class TWithStmt
    Inherits TStatement

    Public TermWith As TTerm
End Class

' -------------------------------------------------------------------------------- TAssignment
Public Class TAssignment
    Inherits TStatement
    Public RelAsn As TApply

    Public Sub New(rel1 As TApply)
        TypeStmt = EToken.eASN
        RelAsn = rel1
    End Sub

    Public Sub New(ref1 As TReference, trm1 As TTerm)
        TypeStmt = EToken.eASN
        RelAsn = TApply.NewOpr2(EToken.eASN, ref1, trm1)
    End Sub
End Class

' -------------------------------------------------------------------------------- TCall
Public Class TCall
    Inherits TStatement
    Public AppCall As TApply

    Public Sub New(app1 As TApply)
        TypeStmt = EToken.eCall
        AppCall = app1
    End Sub
End Class

' -------------------------------------------------------------------------------- TReturn
Public Class TReturn
    Inherits TStatement
    Public TrmRet As TTerm
    Public YieldRet As Boolean

    Public Sub New(trm1 As TTerm, yield_ret As Boolean)
        TypeStmt = EToken.eReturn
        TrmRet = trm1
        YieldRet = yield_ret
    End Sub
End Class

' -------------------------------------------------------------------------------- TSourceFile
Public Class TSourceFile
    Public vUsing As New TList(Of String)
    Public ClaSrc As New TList(Of TClass)
    Public vTextSrc As String()
    Public FileDir As String
    Public FileSrc As String
    Public LineTkn As TList(Of TList(Of TToken))
    Public StmtSrc As TList(Of TStatement)
    Public FigSrc As TBasicCodeGenerator

    Public Sub New(path1 As String)
        FileSrc = path1
    End Sub

    Public Sub New(path1 As String, vtext As String())
        FileSrc = path1
        vTextSrc = vtext
    End Sub
End Class
