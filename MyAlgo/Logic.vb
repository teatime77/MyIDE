Imports System.Diagnostics

Public Enum ELanguage
    TypeScript
    CSharp
    JavaScript
    Java
    Basic
End Enum

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
    eAbstract
    eADD
    eADDEQ
    eAddressOf
    eAggregate
    eAnd
    eAnp
    eAny
    eAppCall
    eAs
    eASN
    eAsync
    eAt
    Attribute
    eAwait
    eBase
    eBaseCall
    eBaseNew
    eBlockComment
    eBreak
    eByVal
    eCall
    eCase
    eCast
    eCatch
    eChar
    eClass
    eComma
    eComment
    eConst
    eConstructor
    eContinue
    eCType
    eDebugger
    eDeclare
    eDefault
    eDelegate
    eDelete
    eDEC
    eVar
    eDIV
    eDIVEQ
    eDo
    eDot
    eEach
    eElse
    eElseIf
    eEnd
    eEndClass
    eEndEnum
    eEndFunction
    eEndIf
    eEndInterface
    eEndModule
    eEndOperator
    eEndSelect
    eEOL
    eEndStruct
    eEndSub
    eEndTry
    eEndWith
    eEnum
    eEOT
    eEq
    eExit
    eExitDo
    eExitFor
    eExitSub
    eExtends
    eFor
    eForeach
    eFriend
    eFrom
    eFunction
    eGE
    eGet
    eGetType
    eGoto
    eGT
    eHandles
    eHAT
    eHex
    eId
    eIf
    eIfBlock
    eImplements
    eImports
    eIn
    eInstanceof
    eInt
    eInterface
    eInto
    eINC
    eIs
    eIsNot
    eIterator
    eLabel
    eLB
    eLC
    eLE
    eLineComment
    eLoop
    eLowLine
    eLP
    eLT
    eMMB
    eMns
    eMOD
    eMODEQ
    eModule
    eMUL
    eMULEQ
    eMustOverride
    eNamespace
    eNE
    eNew
    eNext
    eNL
    eNot
    eOf
    eOperator
    eOR
    eOut
    eOverride
    ePackage
    eParamArray
    ePartial
    ePrivate
    eProtected
    ePublic
    Question
    eRB
    eRC
    eReDim
    eRef
    eRegEx
    eReturn
    eRP
    eSelect
    eSet
    eShared
    eSM
    eStep
    eString
    eStruct
    eSub
    eSUBEQ
    eSwitch
    eTab
    eTake
    eThen
    eThrow
    eTilde
    eTo
    eTry
    eUnknown
    eUsing
    eVarDecl
    eVirtual
    eBitOR
    eWhere
    eWhile
    eWith
    eYield

    eMAX_ETkn
End Enum

Public Class TToken
    Public SpcTkn As Integer
    Public TypeTkn As EToken = EToken.eUnknown
    Public StrTkn As String
    Public PosTkn As Integer
    Public TabTkn As Integer

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
    Public isWeak As Boolean
    Public isParent As Boolean
    Public isInvariant As Boolean

    Public isXmlIgnore As Boolean

    Public TokenListMod As List(Of TToken)

    Public Function isStrong() As Boolean
        Return Not isWeak AndAlso Not isParent
    End Function

End Class

' -------------------------------------------------------------------------------- TTerm
Public Class TTerm
    <_Parent()> Public UpTrm As Object
    <_Weak()> Public TokenList As List(Of TToken)
    <_Weak()> Public FunctionTrm As TFunction
    <_Weak()> Public TypeTrm As TClass
    <_Weak()> Public ProjectTrm As TProject
    <_Weak()> Public RefPathTrm As TRefPath
    <_Weak()> Public CastType As TClass

    Public Sub New()
    End Sub

    Public Function IsApp() As Boolean
        If TypeOf Me Is TApply Then
            Select Case CType(Me, TApply).TypeApp
                Case EToken.eCast, EToken.eAppCall, EToken.eADD, EToken.eBaseNew, EToken.eNew, EToken.eMns, EToken.eMUL, EToken.eBaseCall, EToken.eDIV, EToken.eMOD, EToken.eGetType, EToken.eINC, EToken.eDEC, EToken.eBitOR, EToken.Question
                    Return True
            End Select
        End If

        Return False
    End Function

    Public Function IsRel() As Boolean
        If TypeOf Me Is TApply Then
            Select Case CType(Me, TApply).TypeApp
                Case EToken.eEq, EToken.eNE, EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE, EToken.eIsNot, EToken.eInstanceof, EToken.eIs
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
    <_Parent()> Public UpVar As Object

    Public IdxVar As Integer
    Public ModVar As TModifier
    Public NameVar As String
    <_Weak()> Public TypeVar As TClass
    Public InitVar As TTerm
    <_Weak()> Public RefVar As New TList(Of TReference)
    Public ByRefVar As Boolean = False
    Public ParamArrayVar As Boolean = False
    Public ComVar As TComment
    Public NoType As Boolean = False
    Public UsedVar As Boolean = False
    <_Weak()> Public RefPathVar As TRefPath
    ' public List<TReference> UseVar = new List<TReference>();
    ' public List<TReference> DefVar = new List<TReference>();

    <_Weak()> Public TokenListVar As List(Of TToken)

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

    Public FldCla As New TList(Of TField)
    Public FncCla As New TList(Of TFunction)

    <_Weak()> Public ProjectCla As TProject
    <_Weak()> Public OrgCla As TClass
    <_Weak()> Public GenCla As TList(Of TClass)
    <_Weak()> Public SuperClassList As New TList(Of TClass)
    <_Weak()> Public SubClassList As New TList(Of TClass)
    <_Weak()> Public AllSuperClassList As TList(Of TClass)
    <_Weak()> Public InterfaceList As New TList(Of TClass)
    <_Weak()> Public SrcCla As TSourceFile
    <_Weak()> Public TokenListCls As List(Of TToken)
    <_Weak()> Public GenTokenListCls As List(Of TToken)

    Public DimCla As Integer = 0
    Public KndCla As EClass = EClass.eClassCla
    Public IsParamCla As Boolean = False
    Public ContainsArgumentClass As Boolean = False
    Public GenericType As EGeneric
    Public Parsed As Boolean = False


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
        AllSuperClassList = New TList(Of TClass)()
        AllSuperCla2(AllSuperClassList)
    End Sub

    ' このクラスが引数のクラスのサブクラスならTrueを返す
    Public Function IsSubcla(cla1 As TClass) As Boolean
        If AllSuperClassList.Contains(cla1) Then
            Return True
        End If

        If cla1.GenericType = EGeneric.SpecializedClass Then
            Dim vcla = From cla2 In AllSuperClassList Where cla2.GenericType = EGeneric.SpecializedClass AndAlso cla2.OrgCla Is cla1.OrgCla AndAlso Not (From idx In TNaviUp.IndexList(cla1.GenCla) Where Not cla2.GenCla(idx).IsSubcla(cla1.GenCla(idx))).Any()
            If vcla.Any() Then
                Return True
            End If

            For Each cla2 In AllSuperClassList
                If cla2.GenericType = EGeneric.SpecializedClass AndAlso cla2.OrgCla Is cla1.OrgCla Then

                    If Not (From idx In TNaviUp.IndexList(cla1.GenCla) Where Not cla2.GenCla(idx).IsSubcla(cla1.GenCla(idx))).Any() Then
                        Return True
                    End If
                End If
            Next
        End If

        Return False
    End Function

    ' このクラスが引数のクラスと同じかサブクラスならTrueを返す
    Public Function IsSubsetOf(cla1 As TClass) As Boolean
        Return cla1 Is Me OrElse AllSuperClassList.Contains(cla1)
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
    <_Weak()> Public RetDlg As TClass
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
    <_Weak()> Public ClaFld As TClass
    <_Weak()> Public OrgFld As TField

    Public TailCom As String

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
    Public Const ClassInitializerName As String = "__ClassInitializer"
    Public Const InstanceInitializerName As String = "__InstanceInitializer"
    Public Const ImplicitNewName As String = "__ImplicitNew"
    Public TypeFnc As EToken
    Public IsNew As Boolean = False
    Public IsTreeWalker As Boolean = False
    Public OpFnc As EToken = EToken.eUnknown
    <_Weak()> Public ClaFnc As TClass
    <_Weak()> Public RetType As TClass
    <_Weak()> Public InterfaceFnc As TClass
    <_Weak()> Public ImplFnc As TReference
    <_Weak()> Public ArgumentClassFnc As TList(Of TClass)
    Public ArgFnc As New TList(Of TVariable)
    Public ThisFnc As TVariable
    Public BlcFnc As TBlock
    <_Weak()> Public OrgFnc As TFunction
    <_Weak()> Public WithFnc As TClass

    <_Weak()> Public CallFrom As New TList(Of TFunction)
    <_Weak()> Public CallTo As New TList(Of TFunction)
    <_Weak()> Public CallToAll As New TList(Of TFunction)
    <_Weak()> Public OvrFnc As TFunction
    <_Weak()> Public OvredFnc As New TList(Of TFunction)
    <_Weak()> Public EqOvredFncAll As New TList(Of TFunction)
    Public Reachable As Boolean
    <_Weak()> Public RefFnc As New TList(Of TReference)
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

    Public Sub New(function_name As String, prj1 As TProject, cla1 As TClass)
        NameVar = function_name
        ModVar = New TModifier()
        TypeFnc = EToken.eSub
        ThisFnc = New TVariable(prj1.ParsePrj.ThisName, cla1)
        ClaFnc = cla1
        ClaFnc.FncCla.Add(Me)
        BlcFnc = New TBlock()
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

    Public Function IsInitializer() As Boolean
        Return NameFnc() = ClassInitializerName OrElse NameFnc() = InstanceInitializerName OrElse NameFnc() = ImplicitNewName
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
    <_Weak()> Public VarRef As TVariable
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
    Public IniApp As TArray

    <_Weak()> Public ClassApp As TClass
    <_Weak()> Public NewApp As TClass

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

    Public Shared Function MakeApp3Opr(trm1 As TTerm, trm2 As TTerm, trm3 As TTerm) As TApply
        Dim app1 As TApply

        app1 = New TApply()
        app1.TypeApp = EToken.Question

        app1.AddInArg(trm1)
        app1.AddInArg(trm2)
        app1.AddInArg(trm3)
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

        app1.TypeApp = EToken.eInstanceof
        app1.AddInArg(trm1)
        app1.AddInArg(New TReference(type2))

        Return app1
    End Function
End Class

' -------------------------------------------------------------------------------- TDot
Public Class TDot
    Inherits TReference
    Public TrmDot As TTerm
    <_Weak()> Public TypeDot As TClass

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

Public Class TQuery
    Inherits TTerm
    Public SeqQry As TTerm
    Public VarQry As TVariable
    Public CndQry As TTerm
End Class

' -------------------------------------------------------------------------------- TFrom
' From i In v1 Where i Mod 2 = 0 Select AA(i)
Public Class TFrom
    Inherits TQuery
    Public SelFrom As TTerm
    Public TakeFrom As TTerm
    Public InnerFrom As TFrom
End Class

' -------------------------------------------------------------------------------- TAggregate
' Aggregate x In v Into Sum(x.Value)
Public Class TAggregate
    Inherits TQuery
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
    <_Parent()> Public ParentStmt As Object
    <_Weak()> Public ComStmt As TList(Of TToken)
    <_Weak()> Public TailCom As String
    <_Weak()> Public vTknStmt As TList(Of TToken)
    Public StmtIdx As Integer
    Public TypeStmt As EToken
    <_Weak()> Public RefStmt As New TList(Of TReference)
    Public IsGenerated As Boolean
    Public UsedStmt As Boolean
    Public ValidStmt As Boolean = True
    Public BeforeSrc As String
    Public AfterSrc As String = ""

    <_Weak()> Public TokenListStmt As List(Of TToken)
    Public TabStmt As Integer
    <_Weak()> Public FunctionStmt As TFunction

    Public ClassifiedIf As Boolean

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
    <_Weak()> Public TypeDecl As TClass
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
    <_Weak()> Public VarBlc As New TList(Of TVariable)
    Public StmtBlc As New TList(Of TStatement)

    Public Sub AddStmtBlc(stmt1 As TStatement)
        StmtBlc.Add(stmt1)
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
    Public CndIf As TTerm
    Public WithIf As TTerm
    Public BlcIf As TBlock

    Public Sub New()
        TypeStmt = EToken.eIfBlock
    End Sub

    Public Sub New(cnd As TTerm, blc As TBlock)
        TypeStmt = EToken.eIfBlock
        CndIf = cnd
        Dim vidx = From i In TNaviUp.IndexList(blc.StmtBlc) Where TypeOf blc.StmtBlc(i) Is TWith
        If vidx.Any() Then
            Debug.Assert(vidx.Count() = 1)
            If blc.StmtBlc.Count <> 1 Then
                Debug.Assert((From x In blc.StmtBlc Where TypeOf x Is TComment).Count() = blc.StmtBlc.Count - 1)
            End If

            Dim with1 As TWith = CType(blc.StmtBlc(vidx.First()), TWith)
            WithIf = with1.TermWith

            BlcIf = with1.BlcWith
        Else
            BlcIf = blc
        End If
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
        TypeStmt = EToken.eSwitch
    End Sub
End Class

' -------------------------------------------------------------------------------- TTry
Public Class TTry
    Inherits TStatement
    Public BlcTry As TBlock
    Public VarCatch As TList(Of TVariable)
    Public BlcCatch As TBlock
End Class

' -------------------------------------------------------------------------------- TFor
Public Class TFor
    Inherits TStatement
    Public IdxVarFor As TVariable
    Public InTrmFor As TTerm
    Public InVarFor As TVariable
    Public IdxFor As TReference
    Public FromFor As TTerm
    Public ToFor As TTerm
    Public StepFor As TTerm
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

    <_Weak()> Public TermWith As TTerm
    <_Weak()> Public BlcWith As TBlock

    Public Sub New()
        TypeStmt = EToken.eWith
    End Sub
End Class

' -------------------------------------------------------------------------------- TWith
Public Class TWithStmt
    Inherits TStatement

    <_Weak()> Public TermWith As TTerm
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
    Public IsSystem As Boolean = False
    <_Weak()> Public LibSrc As TLibrary
    Public vUsing As New TList(Of String)
    <_Weak()> Public ClaSrc As New TList(Of TClass)
    Public vTextSrc As String()
    Public FileDir As String
    Public FileSrc As String
    <_Weak()> Public LineTkn As TList(Of TList(Of TToken))
    <_Weak()> Public InputTokenList As TList(Of TToken)
    <_Weak()> Public StmtSrc As TList(Of TStatement)
    <_Weak()> Public TokenListSrc As List(Of TToken)

    Public Sub New(lib1 As TLibrary, path1 As String)
        LibSrc = lib1
        FileSrc = path1
    End Sub

    Public Sub New(path1 As String, vtext As String())
        FileSrc = path1
        vTextSrc = vtext
    End Sub
End Class
