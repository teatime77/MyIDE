Imports System.Diagnostics

Public Enum EClass
    eClassCla
    eEnumCla
    eStructCla
    eInterfaceCla
    eDelegateCla
End Enum

Public Enum EApp
    eUnknownApp
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

Public Enum ETkn
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

Public Class TTkn
    Public SpcTkn As Integer
    Public TypeTkn As ETkn
    Public StrTkn As String
    Public PosTkn As Integer

    Public tknSym As Integer
    Public ObjTkn As Object

    Public Sub New()
    End Sub

    Public Sub New(type1 As ETkn, str1 As String, pos1 As Integer)
        TypeTkn = type1
        StrTkn = str1
        PosTkn = pos1
    End Sub

    Public Sub New(type1 As ETkn, str1 As String)
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

    Public Sub New(type1 As ETkn, obj As Object)
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
End Class

' -------------------------------------------------------------------------------- TTerm
Public Class TTerm
    Public UpTrm As Object
    Public TokenList As List(Of TTkn)

    Public Sub New()
    End Sub

    Public Function IsApp() As Boolean
        If TypeOf Me Is TApp Then
            Select Case CType(Me, TApp).TypeApp
                Case ETkn.eCast, ETkn.eAppCall, ETkn.eADD, ETkn.eBaseNew, ETkn.eNew, ETkn.eMns, ETkn.eMUL, ETkn.eBaseCall, ETkn.eDIV, ETkn.eMOD, ETkn.eAddressOf, ETkn.eGetType
                    Return True
            End Select
        End If

        Return False
    End Function

    Public Function IsRel() As Boolean
        If TypeOf Me Is TApp Then
            Select Case CType(Me, TApp).TypeApp
                Case ETkn.eEq, ETkn.eNE, ETkn.eASN, ETkn.eLT, ETkn.eGT, ETkn.eADDEQ, ETkn.eSUBEQ, ETkn.eMULEQ, ETkn.eDIVEQ, ETkn.eMODEQ, ETkn.eLE, ETkn.eGE, ETkn.eIsNot, ETkn.eTypeof, ETkn.eIs
                    Return True
            End Select
        End If

        Return False
    End Function

    Public Function IsOpr() As Boolean
        Dim app1 As TApp

        If TypeOf Me Is TApp Then
            app1 = CType(Me, TApp)
            Select Case app1.TypeApp
                Case ETkn.eAnd, ETkn.eOR, ETkn.eNot, ETkn.eAnp
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

' -------------------------------------------------------------------------------- TVar
Public Class TVar
    Public Shared VarCnt As Integer
    Public UpVar As Object
    Public IdxVar As Integer
    Public ModVar As TModifier
    Public NameVar As String
    Public TypeVar As TCls
    Public InitVar As TTerm
    Public RefVar As New TList(Of TRef)
    Public ByRefVar As Boolean = False
    Public ParamArrayVar As Boolean = False
    Public ComVar As TComment
    Public NoType As Boolean = False
    Public UsedVar As Boolean = False
    ' public List<TRef> UseVar = new List<TRef>();
    ' public List<TRef> DefVar = new List<TRef>();

    Public TokenListVar As List(Of TTkn)

    Public Sub CopyVarMem(var1 As TVar)
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

    Public Sub New(mod1 As TModifier, id1 As TTkn, type1 As TCls)
        ModVar = mod1
        IniVar(id1.StrTkn)
        TypeVar = type1
    End Sub

    Public Sub New(id1 As TTkn, type1 As TCls)
        IniVar(id1.StrTkn)
        TypeVar = type1
    End Sub

    Public Sub New(name1 As String, type1 As TCls)
        IniVar(name1)
        TypeVar = type1
    End Sub

    Public Function GetClassVar() As TCls
        If TypeOf Me Is TFld Then
            Return CType(Me, TFld).ClaFld
        ElseIf TypeOf Me Is TFnc Then
            Return CType(Me, TFnc).ClaFnc
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Overrides Function ToString() As String
        Return NameVar
    End Function
End Class

' -------------------------------------------------------------------------------- TCls
Public Class TCls
    Inherits TVar
    Public KndCla As EClass = EClass.eClassCla
    Public OrgCla As TCls
    Public GenCla As TList(Of TCls)
    Public SuperCla As New TList(Of TCls)
    Public SubClasses As New TList(Of TCls)
    Public AllSuperCla As TList(Of TCls)
    Public InterfacesCls As New TList(Of TCls)
    Public FldCla As New TList(Of TFld)
    Public FncCla As New TList(Of TFnc)
    Public DimCla As Integer = 0
    Public SrcCla As TSrc
    Public IsParamCla As Boolean = False

    Public TokenListCls As List(Of TTkn)

    Public Sub New()
        Debug.WriteLine("@a")
    End Sub

    Public Sub New(name1 As String)
        MyBase.New(name1, CType(Nothing, TCls))
    End Sub

    Public Sub New(mod1 As TModifier, name1 As String)
        MyBase.New(name1, CType(Nothing, TCls))
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

    Public Sub AddFld(fld As TFld)
        FldCla.Add(fld)
        fld.ClaFld = Me
    End Sub

    Public Shared Function MakeArr(cla1 As TCls, dim1 As Integer) As TCls
        Dim cla2 As TCls

        Debug.Assert(cla1 IsNot Nothing)
        cla2 = New TCls("Array")
        cla2.GenCla = New TList(Of TCls)()
        cla2.GenCla.Add(cla1)
        cla2.DimCla = dim1

        Return cla2
    End Function

    Public Sub AllSuperCla2(vcla As TList(Of TCls))
        vcla.AddRange(SuperCla)
        vcla.AddRange(InterfacesCls)
        ' for Each cla1 In SuperCla Call cla1
        For Each cla1 In SuperCla
            cla1.AllSuperCla2(vcla)
        Next

        ' for Each cla1 In InterfacesCls Call cla1
        For Each cla1 In InterfacesCls
            cla1.AllSuperCla2(vcla)
        Next
    End Sub

    Public Sub SetAllSuperCla()
        AllSuperCla = New TList(Of TCls)()
        AllSuperCla2(AllSuperCla)
    End Sub

    ' このクラスが引数のクラスのサブクラスならTrueを返す
    Public Function IsSubcla(cla1 As TCls) As Boolean
        Return AllSuperCla.Contains(cla1)
    End Function

    ' このクラスが引数のクラスと同じかサブクラスならTrueを返す
    Public Function IsSubsetOf(cla1 As TCls) As Boolean
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
    Inherits TCls
    Public RetDlg As TCls
    Public ArgDlg As New TList(Of TVar)

    Public Sub New(name1 As String)
        MyBase.New(name1)
    End Sub

    Public Sub New(fnc1 As TFnc)
        MyBase.New("Delegate@" + fnc1.NameVar)
        RetDlg = fnc1.RetType
        ArgDlg = New TList(Of TVar)(fnc1.ArgFnc)
    End Sub

End Class

' -------------------------------------------------------------------------------- TFld
Public Class TFld
    Inherits TVar
    Public ClaFld As TCls
    Public TailCom As String
    Public OrgFld As TFld

    Public Sub New()
    End Sub

    Public Sub New(mod1 As TModifier, id1 As TTkn, type1 As TCls)
        MyBase.New(mod1, id1, type1)
    End Sub

    Public Sub New(name1 As String, type1 As TCls)
        MyBase.New(name1, type1)
    End Sub

    Public Sub New(name1 As String, type1 As TCls, init As TTerm)
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

' -------------------------------------------------------------------------------- TFnc
Public Class TFnc
    Inherits TVar
    Public TypeFnc As ETkn
    Public IsNew As Boolean = False
    Public OpFnc As ETkn = ETkn.eUnknown
    Public ClaFnc As TCls
    Public RetType As TCls
    Public InterfaceFnc As TCls
    Public ImplFnc As TRef
    Public ArgFnc As New TList(Of TVar)
    Public ThisFnc As TVar
    Public BlcFnc As TBlc
    Public OrgFnc As TFnc

    Public CallFrom As New TList(Of TFnc)
    Public CallTo As New TList(Of TFnc)
    Public CallToAll As New TList(Of TFnc)
    Public OvrFnc As TFnc
    Public OvredFnc As New TList(Of TFnc)
    Public EqOvredFncAll As New TList(Of TFnc)
    Public Reachable As Boolean
    Public RefFnc As New TList(Of TRef)

    Public Sub CopyFncMem(fnc1 As TFnc)
        CopyVarMem(fnc1)
        fnc1.IsNew = IsNew
    End Sub

    Public Sub New()
    End Sub

    Public Sub New(id1 As TTkn, type1 As TCls)
        MyBase.New(id1, Nothing)
        RetType = type1
    End Sub

    Public Sub New(name1 As String, ret_type As TCls)
        MyBase.New(name1, CType(Nothing, TCls))
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

' -------------------------------------------------------------------------------- TAtm
Public Class TAtm
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

' -------------------------------------------------------------------------------- TCns
Public Class TCns
    Inherits TAtm
    Public TypeAtm As ETkn

    Public Sub New()
    End Sub

    Public Sub New(type1 As ETkn, name As String)
        MyBase.New(name)
        TypeAtm = type1
    End Sub
End Class

' -------------------------------------------------------------------------------- TRef
Public Class TRef
    Inherits TAtm
    Public VarRef As TVar
    Public FncRef As TFnc
    Public DefRef As Boolean = False


    Public Sub New()
        MyBase.New()
    End Sub

    Public Sub New(name1 As String)
        MyBase.New(name1)
    End Sub

    Public Sub New(tkn1 As TTkn)
        MyBase.New(tkn1.StrTkn)
    End Sub

    Public Sub New(var1 As TVar)
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

' -------------------------------------------------------------------------------- TPar
Public Class TPar
    Inherits TTerm
    Public TrmPar As TTerm

    Public Sub New(trm1 As TTerm)
        TrmPar = trm1
    End Sub
End Class

' -------------------------------------------------------------------------------- TApp
Public Class TApp
    Inherits TTerm
    Public TypeApp As ETkn = ETkn.eUnknown
    Public Negation As Boolean = False
    Public ArgApp As New TList(Of TTerm)
    Public KndApp As EApp = EApp.eUnknownApp
    Public FncApp As TTerm
    Public ClassApp As TCls
    Public NewApp As TCls
    Public IniApp As TArray

    Public Sub AddInArg(trm1 As TTerm)
        ArgApp.Add(trm1)
    End Sub

    Public Shared Function MakeAppNew(type1 As TCls) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        app1.TypeApp = ETkn.eNew
        app1.NewApp = type1
        app1.FncApp = New TRef("New@" + type1.NameCla())

        Return app1
    End Function

    Public Shared Function MakeAppBase(tkn1 As TTkn) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        If tkn1.TypeTkn = ETkn.eNew Then
            app1.TypeApp = ETkn.eBaseNew
        Else
            app1.TypeApp = ETkn.eBaseCall
            app1.FncApp = New TRef(tkn1)
        End If

        Return app1
    End Function

    Public Shared Function MakeAppCall(fnc1 As TTerm) As TApp
        Dim app1 As TApp

        Debug.Assert(fnc1 IsNot Nothing)
        If TypeOf fnc1 Is TDot OrElse TypeOf fnc1 Is TRef OrElse fnc1.IsApp() Then
        Else
            Debug.Assert(False, "make app call:new type")
        End If
        app1 = New TApp()
        app1.TypeApp = ETkn.eAppCall
        app1.FncApp = fnc1

        Return app1
    End Function

    Public Shared Function MakeApp1Opr(tkn1 As TTkn, trm1 As TTerm) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        app1.TypeApp = tkn1.TypeTkn

        app1.FncApp = New TRef(tkn1)

        app1.AddInArg(trm1)

        Return app1
    End Function

    Public Shared Function MakeApp2Opr(tkn1 As TTkn, trm1 As TTerm, trm2 As TTerm) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        app1.TypeApp = tkn1.TypeTkn

        app1.FncApp = New TRef(tkn1)

        app1.AddInArg(trm1)
        app1.AddInArg(trm2)

        Return app1
    End Function

    Public Shared Function MakeApp3Opr(tkn1 As TTkn, trm1 As TTerm, trm2 As TTerm, trm3 As TTerm) As TApp
        Dim app1 As TApp

        Debug.Assert(tkn1.TypeTkn = ETkn.eQUE)
        app1 = New TApp()
        app1.TypeApp = tkn1.TypeTkn

        app1.FncApp = New TRef(tkn1)

        app1.AddInArg(trm1)
        app1.AddInArg(trm2)
        app1.AddInArg(trm3)
        Return app1
    End Function

    Public Shared Function MakeAppCastClass(type1 As TCls) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        app1.TypeApp = ETkn.eCast
        app1.ClassApp = type1

        Return app1
    End Function

    Public Shared Function MakeAppGetType(type1 As TCls) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        app1.TypeApp = ETkn.eGetType
        app1.ClassApp = type1

        Return app1
    End Function

    Public Shared Function MakeAppAddressOf(trm1 As TTerm) As TApp
        Dim app1 As TApp

        app1 = New TApp()
        app1.TypeApp = ETkn.eAddressOf
        app1.AddInArg(trm1)
        Debug.Assert(TypeOf trm1 Is TRef)

        Return app1
    End Function

    Public Shared Function NewOpr(type1 As ETkn) As TApp
        Dim app1 As New TApp

        app1.TypeApp = type1

        Return app1
    End Function

    Public Shared Function NewOpr2(type1 As ETkn, trm1 As TTerm, trm2 As TTerm) As TApp
        Dim app1 As New TApp

        app1.TypeApp = type1
        app1.AddInArg(trm1)
        app1.AddInArg(trm2)

        Return app1
    End Function

    Public Shared Function NewTypeOf(trm1 As TTerm, type2 As TCls) As TApp
        Dim app1 As New TApp

        app1.TypeApp = ETkn.eTypeof
        app1.AddInArg(trm1)
        app1.AddInArg(New TRef(type2))

        Return app1
    End Function

    Public Sub TrmSrcSet(mk As TCodeGenerator, trm1 As TTerm)
        Select Case KndApp

            Case EApp.eListApp, EApp.eDictionaryApp
                Debug.Assert(ArgApp.Count = 1)

                mk.TrmSrc(FncApp)
                mk.WordAdd(".", EFigType.eSymFig, Me)
                If KndApp = EApp.eListApp Then
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
    Inherits TRef
    Public TrmDot As TTerm
    Public TypeDot As TCls

    Public Sub New(trm1 As TTerm, name1 As String)
        MyBase.New(name1)
        '        Debug.Assert(trm1 IsNot Nothing)
        TrmDot = trm1
    End Sub

    Public Sub New(trm1 As TTerm, var1 As TVar)
        MyBase.New(var1)
        TrmDot = trm1
    End Sub

    Public Function IsEnumDot() As Boolean
        Dim fld As TFld, fnc As TFnc

        If TypeOf VarRef Is TFld Then
            fld = CType(VarRef, TFld)
            If fld.ClaFld IsNot Nothing AndAlso fld.ClaFld.KndCla = EClass.eEnumCla Then
                ' ジェネリック型の場合ClaFldはNothing
                Return True
            End If
        ElseIf TypeOf VarRef Is TFnc Then
            fnc = CType(VarRef, TFnc)
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
    Public VarFrom As TVar
    Public SeqFrom As TTerm
    Public CndFrom As TTerm
    Public SelFrom As TTerm
    Public TakeFrom As TTerm
End Class

' -------------------------------------------------------------------------------- TAggregate
' Aggregate x In v Into Sum(x.Value)
Public Class TAggregate
    Inherits TTerm
    Public VarAggr As TVar
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

' -------------------------------------------------------------------------------- TStmt
Public Class TStmt
    Shared StmtCnt As Integer
    Public ParentStmt As Object
    Public ComStmt As TList(Of TTkn)
    Public TailCom As String
    Public vTknStmt As TList(Of TTkn)
    Public StmtIdx As Integer
    Public TypeStmt As ETkn
    Public BlcStmt As TBlc
    Public RefStmt As New TList(Of TRef)
    Public IsGenerated As Boolean
    Public UsedStmt As Boolean
    Public ValidStmt As Boolean = True
    Public BeforeSrc As String
    Public AfterSrc As String = ""

    Public TokenList As List(Of TTokenLine)
    Public TabStmt As Integer

    Public Sub New()
        StmtIdx = StmtCnt
        StmtCnt = StmtCnt + 1
    End Sub

    Public Overridable Function JumpEnd() As Boolean
        Select Case TypeStmt
            Case ETkn.eReturn, ETkn.eExitDo, ETkn.eExitFor, ETkn.eExitSub
                ' ジャンプする場合

                Return True
        End Select

        Return False
    End Function
End Class

' -------------------------------------------------------------------------------- TVarDecl
Public Class TVarDecl
    Inherits TStmt
    Public ModDecl As TModifier
    Public TypeDecl As TCls
    Public VarDecl As New TList(Of TVar)

    Public Sub New()
    End Sub

    Public Sub New(vvar1 As TList(Of TVar))
        VarDecl = vvar1
    End Sub

End Class

' -------------------------------------------------------------------------------- TBlc
Public Class TBlc
    Inherits TStmt
    Public VarBlc As New TList(Of TVar)
    Public StmtBlc As New TList(Of TStmt)

    Public Sub AddStmtBlc(stmt1 As TStmt)
        StmtBlc.Add(stmt1)
        stmt1.BlcStmt = Me
    End Sub

    Public Overrides Function JumpEnd() As Boolean
        Dim stmt1 As TStmt, i1 As Integer

        i1 = StmtBlc.Count - 1
        Do While 0 <= i1
            stmt1 = StmtBlc(i1)

            Select Case stmt1.TypeStmt
                Case ETkn.eComment
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
    Inherits TStmt
    Public IfBlc As New TList(Of TIfBlc)

    Public Sub New()
        TypeStmt = ETkn.eIf
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

' -------------------------------------------------------------------------------- TIfBlc
Public Class TIfBlc
    Inherits TStmt
    Public BlcIf As TBlc
    Public CndIf As TTerm

    Public Sub New()
        TypeStmt = ETkn.eIfBlock
    End Sub

    Public Sub New(cnd As TTerm, blc As TBlc)
        TypeStmt = ETkn.eIfBlock
        CndIf = cnd
        BlcIf = blc
    End Sub
End Class

' -------------------------------------------------------------------------------- TCase
Public Class TCase
    Inherits TStmt
    Public TrmCase As New TList(Of TTerm)
    Public DefaultCase As Boolean
    Public BlcCase As TBlc

    Public Sub New()
        TypeStmt = ETkn.eCase
    End Sub

    Public Sub AddTrmCase(trm As TTerm)
        TrmCase.Add(trm)
    End Sub
End Class

' -------------------------------------------------------------------------------- TSelect
Public Class TSelect
    Inherits TStmt
    Public TrmSel As TTerm
    Public CaseSel As New TList(Of TCase)

    Public Sub New()
        TypeStmt = ETkn.eSelect
    End Sub
End Class

' -------------------------------------------------------------------------------- TTry
Public Class TTry
    Inherits TStmt
    Public BlcTry As TBlc
    Public BlcCatch As TBlc
    Public VarCatch As TList(Of TVar)
End Class

' -------------------------------------------------------------------------------- TFor
Public Class TFor
    Inherits TStmt
    Public IdxVarFor As TVar
    Public InVarFor As TVar
    Public IdxFor As TRef
    Public FromFor As TTerm
    Public ToFor As TTerm
    Public StepFor As TTerm
    Public InTrmFor As TTerm
    Public IniFor As TStmt
    Public CndFor As TTerm
    Public StepStmtFor As TStmt
    Public BlcFor As TBlc
    Public IsDo As Boolean = False
    Public LabelFor As Integer

    Public Sub New()
        TypeStmt = ETkn.eFor
    End Sub

    Public Sub New(prj1 As TPrj, idx_name As String, from1 As TTerm, to1 As TTerm)
        TypeStmt = ETkn.eFor

        IdxVarFor = New TVar(idx_name, prj1.IntType)
        IdxFor = New TRef(IdxVarFor)
        FromFor = from1
        ToFor = to1
    End Sub
End Class

' -------------------------------------------------------------------------------- TWith
Public Class TWith
    Inherits TStmt

    Public TermWith As TTerm
    Public BlcWith As TBlc

    Public Sub New()
        TypeStmt = ETkn.eWith
    End Sub
End Class

' -------------------------------------------------------------------------------- TWith
Public Class TWithStmt
    Inherits TStmt

    Public TermWith As TTerm
End Class

' -------------------------------------------------------------------------------- TAsn
Public Class TAsn
    Inherits TStmt
    Public RelAsn As TApp

    Public Sub New(rel1 As TApp)
        TypeStmt = ETkn.eASN
        RelAsn = rel1
    End Sub

    Public Sub New(ref1 As TRef, trm1 As TTerm)
        TypeStmt = ETkn.eASN
        RelAsn = TApp.NewOpr2(ETkn.eASN, ref1, trm1)
    End Sub
End Class

' -------------------------------------------------------------------------------- TCall
Public Class TCall
    Inherits TStmt
    Public AppCall As TApp

    Public Sub New(app1 As TApp)
        TypeStmt = ETkn.eCall
        AppCall = app1
    End Sub
End Class

' -------------------------------------------------------------------------------- TRet
Public Class TRet
    Inherits TStmt
    Public TrmRet As TTerm
    Public YieldRet As Boolean

    Public Sub New(trm1 As TTerm, yield_ret As Boolean)
        TypeStmt = ETkn.eReturn
        TrmRet = trm1
        YieldRet = yield_ret
    End Sub
End Class

' -------------------------------------------------------------------------------- TSrc
Public Class TSrc
    Public vUsing As New TList(Of String)
    Public ClaSrc As New TList(Of TCls)
    Public vTextSrc As String()
    Public FileDir As String
    Public FileSrc As String
    Public LineTkn As TList(Of TList(Of TTkn))
    Public StmtSrc As TList(Of TStmt)
    Public FigSrc As TBasicCodeGenerator

    Public Sub New(path1 As String)
        FileSrc = path1
    End Sub

    Public Sub New(path1 As String, vtext As String())
        FileSrc = path1
        vTextSrc = vtext
    End Sub
End Class
