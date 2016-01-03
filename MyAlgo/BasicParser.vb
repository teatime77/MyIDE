Imports System.Diagnostics

Public MustInherit Class TSourceParser
    Public LanguageSP As ELanguage
    Public PrjParse As TProject
    Public vTknName As Dictionary(Of EToken, String)
    Public ThisName As String
    Public TranslationTable As New Dictionary(Of String, String)

    Public MustOverride Function Lex(src_text As String) As TList(Of TToken)
    Public Overridable Sub ReadAllStatement(src1 As TSourceFile)
    End Sub
    Public MustOverride Sub Parse(src1 As TSourceFile)
    Public MustOverride Sub ClearParse()
    Public MustOverride Sub RegAllClass(prj1 As TProject, src1 As TSourceFile)
    Public MustOverride Function NullName() As String


    Public Function TranslageReferenceName(ref1 As TReference) As String
        If TypeOf ref1.VarRef Is TField OrElse TypeOf ref1.VarRef Is TFunction Then

            Dim long_name As String = ref1.VarRef.GetClassVar().NameVar + "." + ref1.NameRef

            If TranslationTable.ContainsKey(long_name) Then
                Return TranslationTable(long_name)
            End If
        End If

        Return ref1.NameRef
    End Function
End Class

'-------------------------------------------------------------------------------- TBasicParser
' Basicの構文解析
Public Class TBasicParser
    Inherits TSourceParser


    Public vTkn As New Dictionary(Of String, EToken)
    Public CurBlc As TBlock
    Public CurPos As Integer
    Public CurTkn As TToken
    Public NxtTkn As TToken
    Dim EOTTkn As TToken
    Public CurVTkn As TList(Of TToken)
    Dim CurStmt As TStatement
    Public CurLineIdx As Integer
    Dim CurLineStr As String
    Dim ArgClassTable As Dictionary(Of String, TClass)

    Public Sub New(prj1 As TProject)
        LanguageSP = ELanguage.Basic
        ThisName = "Me"
        PrjParse = prj1
        RegTkn()
    End Sub

    Public Overrides Sub ClearParse()
        CurBlc = Nothing
        CurPos = 0
        CurTkn = Nothing
        NxtTkn = Nothing
        CurVTkn = Nothing
        CurStmt = Nothing
        CurLineIdx = 0
        CurLineStr = ""
    End Sub

    Public Overrides Function NullName() As String
        Return "Nothing"
    End Function

    Public Function GetTkn(type1 As EToken) As TToken
        Dim tkn1 As TToken

        If type1 = CurTkn.TypeTkn OrElse type1 = EToken.eUnknown Then
            tkn1 = CurTkn
            CurPos = CurPos + 1
            If CurPos < CurVTkn.Count Then
                If CurVTkn(CurPos).TypeTkn = EToken.eLowLine Then

                    CurLineIdx += 1
                    CurLineStr = PrjParse.CurSrc.vTextSrc(CurLineIdx)
                    CurVTkn = PrjParse.CurSrc.LineTkn(CurLineIdx)

                    CurPos = 0
                End If
                CurTkn = CurVTkn(CurPos)
                If CurPos + 1 < CurVTkn.Count Then
                    NxtTkn = CurVTkn(CurPos + 1)
                Else
                    NxtTkn = EOTTkn
                End If
            Else
                CurTkn = EOTTkn
                NxtTkn = EOTTkn
            End If

            Return tkn1
        Else
            Chk(False, CurLineStr)
            Return Nothing
        End If
    End Function

    Function ReadImports() As TStatement
        Dim stmt1 As New TImports
        Dim id1 As TToken
        Dim tkn1 As TToken
        Dim sb1 As TStringWriter

        stmt1.TypeStmt = EToken.eImports
        GetTkn(EToken.eImports)

        sb1 = New TStringWriter()
        Do While True
            id1 = GetTkn(EToken.eId)
            sb1.Append(id1.StrTkn)

            Select Case CurTkn.TypeTkn
                Case EToken.eEOT
                    Exit Do
                Case EToken.eDot
                    tkn1 = GetTkn(EToken.eDot)
                    sb1.Append(tkn1.StrTkn)
                Case Else
                    Chk(False)
            End Select
        Loop

        PrjParse.CurSrc.vUsing.Add(sb1.ToString())

        Return stmt1
    End Function

    Function ReadModule() As TStatement
        Dim stmt1 As New TModule
        Dim id1 As TToken

        stmt1.TypeStmt = EToken.eModule
        GetTkn(EToken.eModule)
        id1 = GetTkn(EToken.eId)
        stmt1.NameMod = id1.StrTkn

        Return stmt1
    End Function

    Function ReadEnum() As TStatement
        Dim stmt1 As New TEnumStatement
        Dim id1 As TToken

        stmt1.TypeStmt = EToken.eEnum
        GetTkn(EToken.eEnum)
        id1 = GetTkn(EToken.eId)
        stmt1.NameEnumStmt = id1.StrTkn
        Return stmt1
    End Function

    Function ReadClass(mod1 As TModifier) As TStatement
        Dim stmt1 As New TClassStatement, cla1 As TClass
        Dim id1 As TToken

        PrjParse.dicGenCla.Clear()

        stmt1.TypeStmt = EToken.eClass
        Select Case CurTkn.TypeTkn
            Case EToken.eClass
                stmt1.KndClaStmt = EClass.eClassCla
            Case EToken.eStruct
                stmt1.KndClaStmt = EClass.eStructCla
            Case EToken.eInterface
                stmt1.KndClaStmt = EClass.eInterfaceCla
        End Select
        GetTkn(EToken.eUnknown)
        id1 = GetTkn(EToken.eId)
        cla1 = PrjParse.GetCla(id1.StrTkn)
        Debug.Assert(cla1 IsNot Nothing)
        cla1.ModVar = mod1
        stmt1.ClaClaStmt = cla1

        If CurTkn.TypeTkn = EToken.eLP Then
            ' ジェネリック クラスの場合

            For Each cla2 In cla1.GenCla
                cla2.IsParamCla = True
                PrjParse.dicGenCla.Add(cla2.NameCla(), cla2)
            Next

            GetTkn(EToken.eLP)
            GetTkn(EToken.eOf)

            Do While True
                GetTkn(EToken.eId)
                If CurTkn.TypeTkn = EToken.eRP Then
                    GetTkn(EToken.eRP)
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If

        Return stmt1
    End Function

    Function ReadInherits() As TStatement
        Dim stmt1 As New TInheritsStatement, id1 As TToken, id2 As TToken

        stmt1.TypeStmt = EToken.eExtends
        GetTkn(EToken.eExtends)
        id1 = GetTkn(EToken.eId)
        stmt1.ClassNameInheritsStmt = id1.StrTkn

        If CurTkn.TypeTkn = EToken.eLP Then

            GetTkn(EToken.eLP)
            GetTkn(EToken.eOf)

            stmt1.ParamName = New TList(Of String)()
            Do While True
                id2 = GetTkn(EToken.eId)
                stmt1.ParamName.Add(id2.StrTkn)

                If CurTkn.TypeTkn <> EToken.eComma Then

                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
            GetTkn(EToken.eRP)

        End If
        Return stmt1
    End Function

    Function ReadImplements() As TStatement
        Dim stmt1 As New TImplementsStatement
        Dim cla1 As TClass

        stmt1.TypeStmt = EToken.eImplements
        GetTkn(EToken.eImplements)
        Do While True
            cla1 = ReadType(False)
            stmt1.ClassImplementsStmt.Add(cla1)

            If CurTkn.TypeTkn <> EToken.eComma Then
                Exit Do
            End If
            GetTkn(EToken.eComma)
        Loop
        Return stmt1
    End Function

    Function ReadSubFunction(mod1 As TModifier, is_delegate As Boolean) As TStatement
        Dim stmt1 As New TFunctionStatement
        Dim id1 As TToken, id2 As TToken, id3 As TToken
        Dim var1 As TVariable
        Dim by_ref As Boolean, param_array As Boolean
        Dim cla1 As TDelegate

        If is_delegate Then
            PrjParse.dicGenCla.Clear()
        End If

        stmt1.TypeStmt = CurTkn.TypeTkn
        stmt1.ModifierFncStmt = mod1
        stmt1.IsDelegateFncStmt = is_delegate
        GetTkn(EToken.eUnknown)
        If CurTkn.TypeTkn = EToken.eNew Then
            GetTkn(EToken.eNew)
            stmt1.TypeStmt = EToken.eNew
        Else
            If stmt1.TypeStmt = EToken.eOperator Then
                stmt1.OpFncStmt = CurTkn.TypeTkn
            Else
                Debug.Assert(CurTkn.TypeTkn = EToken.eId)
            End If
            id1 = GetTkn(EToken.eUnknown)
            stmt1.NameFncStmt = id1.StrTkn
            If is_delegate Then
                cla1 = PrjParse.GetDelegate(stmt1.NameFncStmt)
                If CurTkn.TypeTkn = EToken.eLP AndAlso NxtTkn.TypeTkn = EToken.eOf Then

                    For Each cla_f In cla1.GenCla
                        PrjParse.dicGenCla.Add(cla_f.NameCla(), cla_f)
                    Next

                    GetTkn(EToken.eLP)
                    GetTkn(EToken.eOf)
                    Do While True
                        GetTkn(EToken.eId)
                        If CurTkn.TypeTkn = EToken.eRP Then
                            Exit Do
                        End If
                        GetTkn(EToken.eComma)
                    Loop
                    GetTkn(EToken.eRP)
                End If
            End If
        End If

        If NxtTkn.TypeTkn = EToken.eOf Then

            ArgClassTable = New Dictionary(Of String, TClass)()
            stmt1.ArgumentClassFncStmt = New TList(Of TClass)()

            GetTkn(EToken.eLP)
            GetTkn(EToken.eOf)
            Do While True
                Dim class_name As TToken = GetTkn(EToken.eId)

                Dim arg_class = New TClass(PrjParse, class_name.StrTkn)
                arg_class.IsParamCla = True
                arg_class.GenericType = EGeneric.ArgumentClass

                ArgClassTable.Add(arg_class.NameVar, arg_class)
                stmt1.ArgumentClassFncStmt.Add(arg_class)

                If CurTkn.TypeTkn = EToken.eRP Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
            GetTkn(EToken.eRP)
        End If

        GetTkn(EToken.eLP)
        If CurTkn.TypeTkn <> EToken.eRP Then
            Do While True
                by_ref = False
                param_array = False
                Select Case CurTkn.TypeTkn
                    Case EToken.eRef
                        by_ref = True
                        GetTkn(EToken.eRef)
                    Case EToken.eParamArray
                        param_array = True
                        GetTkn(EToken.eParamArray)
                End Select
                var1 = ReadVariable(stmt1)
                var1.ByRefVar = by_ref
                var1.ParamArrayVar = param_array
                stmt1.ArgumentFncStmt.Add(var1)
                If CurTkn.TypeTkn <> EToken.eComma Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If
        GetTkn(EToken.eRP)

        If stmt1.TypeStmt = EToken.eFunction OrElse stmt1.TypeStmt = EToken.eOperator Then
            GetTkn(EToken.eAs)
            stmt1.RetType = ReadType(False)
        End If

        If CurTkn.TypeTkn = EToken.eImplements Then
            GetTkn(EToken.eImplements)

            id2 = GetTkn(EToken.eId)
            GetTkn(EToken.eDot)
            id3 = GetTkn(EToken.eId)

            stmt1.InterfaceFncStmt = PrjParse.GetCla(id2.StrTkn)
            Debug.Assert(stmt1.InterfaceFncStmt IsNot Nothing)
            stmt1.InterfaceFncName = id3.StrTkn
        End If

        If is_delegate Then
            PrjParse.dicGenCla.Clear()
        End If

        ArgClassTable = Nothing

        Return stmt1
    End Function

    ' ジェネリック型の構文解析
    Function ReadGenType(id1 As TToken) As TClass
        Dim tp1 As TClass, tp2 As TClass
        Dim vtp As TList(Of TClass)
        Dim is_param As Boolean = False

        GetTkn(EToken.eLP)
        GetTkn(EToken.eOf)

        vtp = New TList(Of TClass)()
        Do While True
            tp2 = ReadType(False)
            If tp2.GenericType = EGeneric.ArgumentClass Then
                is_param = True
            End If
            vtp.Add(tp2)

            If CurTkn.TypeTkn <> EToken.eComma Then

                Exit Do
            End If
            GetTkn(EToken.eComma)
        Loop
        GetTkn(EToken.eRP)

        ' ジェネリック型のクラスを得る。
        tp1 = PrjParse.GetAddSpecializedClass(id1.StrTkn, vtp)
        tp1.ContainsArgumentClass = is_param

        Return tp1
    End Function

    Function ReadType(is_new As Boolean) As TClass
        Dim tp1 As TClass
        Dim id1 As TToken, dim_cnt As Integer

        id1 = GetTkn(EToken.eId)
        If CurTkn.TypeTkn = EToken.eLP AndAlso NxtTkn.TypeTkn = EToken.eOf Then
            ' ジェネリック型の場合

            ' ジェネリック型の構文解析
            tp1 = ReadGenType(id1)
        Else

            tp1 = PrjParse.GetCla(id1.StrTkn)
            If tp1 Is Nothing Then
                If ArgClassTable IsNot Nothing Then
                    If ArgClassTable.ContainsKey(id1.StrTkn) Then
                        tp1 = ArgClassTable(id1.StrTkn)
                    End If
                End If
                If tp1 Is Nothing Then
                    Throw New TError(String.Format("不明なクラス {0}", id1.StrTkn))
                End If
            End If
        End If
        If CurTkn.TypeTkn = EToken.eLP AndAlso (NxtTkn.TypeTkn = EToken.eRP OrElse NxtTkn.TypeTkn = EToken.eComma) Then
            GetTkn(EToken.eLP)
            dim_cnt = 1
            Do While CurTkn.TypeTkn = EToken.eComma
                GetTkn(EToken.eComma)
                dim_cnt += 1
            Loop
            GetTkn(EToken.eRP)
            If Not is_new Then
                tp1 = PrjParse.GetArrCla(tp1, dim_cnt)
            End If
        End If

        Return tp1
    End Function

    Function ReadVariable(up1 As Object) As TVariable
        Dim var1 As New TVariable
        Dim id1 As TToken
        Dim app1 As TApply

        id1 = GetTkn(EToken.eId)
        var1.NameVar = id1.StrTkn

        If CurTkn.TypeTkn = EToken.eAs Then

            GetTkn(EToken.eAs)

            If CurTkn.TypeTkn = EToken.eNew Then
                app1 = NewExpression()
                var1.TypeVar = app1.NewApp
                var1.InitVar = app1

                Return var1
            End If

            var1.TypeVar = ReadType(False)
        End If

        If CurTkn.TypeTkn = EToken.eEq Then
            GetTkn(EToken.eEq)

            var1.InitVar = AdditiveExpression()
        End If

        Return var1
    End Function

    Function ReadTailCom() As String
        Dim tkn1 As TToken

        If CurTkn Is EOTTkn Then
            Return ""
        Else
            tkn1 = GetTkn(EToken.eLineComment)
            Return tkn1.StrTkn
        End If
    End Function

    Function ReadDim(mod1 As TModifier) As TStatement
        Dim stmt1 As New TVariableDeclaration
        Dim var1 As TVariable

        stmt1.TypeStmt = EToken.eVarDecl
        stmt1.ModDecl = mod1
        Do While True
            var1 = ReadVariable(stmt1)
            stmt1.VarDecl.Add(var1)
            If CurTkn.TypeTkn <> EToken.eComma Then
                Exit Do
            End If
            GetTkn(EToken.eComma)
        Loop

        stmt1.TailCom = ReadTailCom()

        Return stmt1
    End Function

    Function ReadReturn(type_tkn As EToken) As TReturn
        GetTkn(type_tkn)
        If CurTkn Is EOTTkn Then

            Return New TReturn(Nothing, type_tkn = EToken.eYield)
        End If

        Return New TReturn(TermExpression(), type_tkn = EToken.eYield)
    End Function

    Function ReadEnd() As TStatement
        Dim stmt1 As New TStatement

        GetTkn(EToken.eEnd)
        Select Case CurTkn.TypeTkn
            Case EToken.eIf
                stmt1.TypeStmt = EToken.eEndIf
            Case EToken.eSub
                stmt1.TypeStmt = EToken.eEndSub
            Case EToken.eFunction
                stmt1.TypeStmt = EToken.eEndFunction
            Case EToken.eOperator
                stmt1.TypeStmt = EToken.eEndOperator
            Case EToken.eClass
                PrjParse.dicGenCla.Clear()
                stmt1.TypeStmt = EToken.eEndClass
            Case EToken.eStruct
                stmt1.TypeStmt = EToken.eEndStruct
            Case EToken.eInterface
                stmt1.TypeStmt = EToken.eEndInterface
            Case EToken.eEnum
                stmt1.TypeStmt = EToken.eEndEnum
            Case EToken.eModule
                stmt1.TypeStmt = EToken.eEndModule
            Case EToken.eSelect
                stmt1.TypeStmt = EToken.eEndSelect
            Case EToken.eTry
                stmt1.TypeStmt = EToken.eEndTry
            Case EToken.eWith
                stmt1.TypeStmt = EToken.eEndWith
            Case Else
                Chk(False)
        End Select
        GetTkn(EToken.eUnknown)

        Return stmt1
    End Function

    Function ReadIf() As TStatement
        Dim stmt1 As New TIfStatement

        stmt1.TypeStmt = EToken.eIf
        GetTkn(EToken.eIf)
        stmt1.CndIfStmt = CType(TermExpression(), TTerm)
        GetTkn(EToken.eThen)
        Return stmt1
    End Function

    Function ReadElseIf() As TStatement
        Dim stmt1 As New TElseIf

        stmt1.TypeStmt = EToken.eElseIf
        GetTkn(EToken.eElseIf)
        stmt1.CndElseIf = CType(TermExpression(), TTerm)
        GetTkn(EToken.eThen)
        Return stmt1
    End Function

    Function ReadElse() As TStatement
        Dim stmt1 As New TStatement

        stmt1.TypeStmt = EToken.eElse
        GetTkn(EToken.eElse)
        Return stmt1
    End Function

    Function ReadDo() As TStatement
        Dim stmt1 As New TDoStmt

        stmt1.TypeStmt = EToken.eDo
        GetTkn(EToken.eDo)
        GetTkn(EToken.eWhile)
        stmt1.CndDo = CType(TermExpression(), TTerm)
        Return stmt1
    End Function

    Function ReadLoop() As TStatement
        Dim stmt1 As New TStatement

        stmt1.TypeStmt = EToken.eLoop
        GetTkn(EToken.eLoop)

        Return stmt1
    End Function

    Function ReadSelect() As TStatement
        Dim stmt1 As New TSelectStatement

        stmt1.TypeStmt = EToken.eSwitch
        GetTkn(EToken.eSelect)
        GetTkn(EToken.eCase)
        stmt1.TermSelectStatement = CType(TermExpression(), TTerm)
        Return stmt1
    End Function

    Function ReadCase() As TStatement
        Dim stmt1 As New TCaseStatement
        Dim trm1 As TTerm

        stmt1.TypeStmt = EToken.eCase
        GetTkn(EToken.eCase)

        If CurTkn.TypeTkn = EToken.eElse Then
            GetTkn(EToken.eElse)
            stmt1.IsCaseElse = True
        Else
            Do While True
                trm1 = CType(TermExpression(), TTerm)
                stmt1.TermCaseStmt.Add(trm1)
                If CurTkn.TypeTkn <> EToken.eComma Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If

        Return stmt1
    End Function

    Function ReadFor() As TStatement
        Dim stmt1 As New TForStatement, id1 As TToken

        stmt1.TypeStmt = EToken.eFor
        GetTkn(EToken.eFor)

        If CurTkn.TypeTkn = EToken.eId Then

            id1 = GetTkn(EToken.eId)
            stmt1.IdxForStmt = New TReference(id1)
            GetTkn(EToken.eEq)
            stmt1.FromForStmt = CType(TermExpression(), TTerm)
            GetTkn(EToken.eTo)
            stmt1.ToForStmt = CType(TermExpression(), TTerm)

            If CurTkn.TypeTkn = EToken.eStep Then
                GetTkn(EToken.eStep)
                stmt1.StepForStmt = CType(TermExpression(), TTerm)
            End If
        Else

            GetTkn(EToken.eEach)

            If CurTkn.TypeTkn = EToken.eId Then

                id1 = GetTkn(EToken.eId)
                stmt1.InVarForStmt = New TVariable(id1.StrTkn, Nothing)
            End If

            If CurTkn.TypeTkn = EToken.eAt Then

                GetTkn(EToken.eAt)

                Do While True
                    GetTkn(EToken.eId)
                    If CurTkn.TypeTkn <> EToken.eComma Then
                        Exit Do
                    End If
                    GetTkn(EToken.eComma)
                Loop
            End If

            GetTkn(EToken.eIn)

            stmt1.InTrmForStmt = CType(TermExpression(), TTerm)
        End If

        Return stmt1
    End Function

    Function ReadNext() As TStatement
        Dim stmt1 As New TStatement

        stmt1.TypeStmt = EToken.eNext
        GetTkn(EToken.eNext)

        Return stmt1
    End Function

    Function ReadExit() As TStatement
        Dim stmt1 As New TExit

        GetTkn(EToken.eExit)
        Select Case CurTkn.TypeTkn
            Case EToken.eDo
                stmt1.TypeStmt = EToken.eExitDo
            Case EToken.eFor
                stmt1.TypeStmt = EToken.eExitFor
            Case EToken.eSub
                stmt1.TypeStmt = EToken.eExitSub
            Case Else
                Chk(False)
        End Select
        GetTkn(EToken.eUnknown)

        Return stmt1
    End Function

    Function ReadTry() As TStatement
        Dim stmt1 As New TStatement

        stmt1.TypeStmt = EToken.eTry
        GetTkn(EToken.eTry)
        Return stmt1
    End Function

    Function ReadCatch() As TStatement
        Dim stmt1 As New TCatchStatement

        stmt1.TypeStmt = EToken.eCatch
        GetTkn(EToken.eCatch)
        stmt1.VariableCatchStmt = ReadVariable(stmt1)
        Return stmt1
    End Function

    Function ReadWith() As TStatement
        Dim stmt1 As New TWithStmt

        stmt1.TypeStmt = EToken.eWith
        GetTkn(EToken.eWith)

        stmt1.TermWith = DotExpression()

        Return stmt1
    End Function

    Function ReadThrow() As TStatement
        Dim stmt1 As TThrow

        GetTkn(EToken.eThrow)
        stmt1 = New TThrow(CType(TermExpression(), TTerm))

        Return stmt1
    End Function

    Function ReadReDim() As TStatement
        Dim stmt1 As TReDim, trm1 As TTerm, app1 As TApply

        GetTkn(EToken.eReDim)

        trm1 = TermExpression()
        Debug.Assert(trm1.IsApp())
        app1 = CType(trm1, TApply)
        Debug.Assert(app1.FncApp IsNot Nothing AndAlso app1.ArgApp.Count <> 0)
        stmt1 = New TReDim(app1.FncApp, app1.ArgApp)

        Return stmt1
    End Function

    Function ReadLineComment() As TStatement
        Dim stmt1 As New TStatement

        stmt1.TypeStmt = EToken.eLineComment
        GetTkn(EToken.eLineComment)
        Return stmt1
    End Function

    Sub Chk(b1 As Boolean, msg As String)
        If Not b1 Then
            Debug.WriteLine("コンパイラ　エラー {0} at {1}", CurLineStr, CurTkn.StrTkn)
            Debug.Assert(False)
        End If
    End Sub

    Sub Chk(b1 As Boolean)
        Chk(b1, "")
    End Sub

    Sub MakeModule(src1 As TSourceFile)
        Dim fnc1 As TFunction, is_module As Boolean = False, cla1 As TClassStatement, cla2 As TClass, com1 As TComment = Nothing

        CurLineIdx = 0
        Do While True
            CurStmt = GetNextStatement()
            If CurStmt Is Nothing OrElse CurStmt.TypeStmt <> EToken.eImports Then
                Exit Do
            End If
        Loop

        is_module = (CurStmt.TypeStmt = EToken.eModule)
        If is_module Then
            GetStatement(EToken.eModule)
        End If
        Do While CurStmt IsNot Nothing AndAlso CurStmt.TypeStmt <> EToken.eEndModule
            Select Case CurStmt.TypeStmt
                Case EToken.eClass
                    cla1 = CType(CurStmt, TClassStatement)
                    cla2 = MakeClass()
                    cla2.ComVar = com1
                    cla2.SrcCla = PrjParse.CurSrc
                    com1 = Nothing
                Case EToken.eEnum
                    cla2 = MakeEnum()
                    cla2.ComVar = com1
                    cla2.SrcCla = PrjParse.CurSrc
                    com1 = Nothing
                Case EToken.eSub, EToken.eFunction
                    If CType(CurStmt, TFunctionStatement).IsDelegateFncStmt Then
                        cla2 = MakeDelegate()
                        cla2.ComVar = com1
                        cla2.SrcCla = PrjParse.CurSrc
                    Else
                        fnc1 = MakeSubFnc(Nothing)
                        fnc1.ComVar = com1
                    End If
                    com1 = Nothing
                Case EToken.eComment
                    com1 = CType(CurStmt, TComment)
                    GetStatement(EToken.eComment)
                Case Else
                    Chk(False)
            End Select
        Loop

        If is_module Then
            GetStatement(EToken.eEndModule)
        End If
    End Sub

    Public Function MakeEnum() As TClass
        Dim cla1 As TClass
        Dim fld1 As TField
        Dim enum1 As TEnumStatement
        Dim ele1 As TEnumElement
        Dim type1 As TClass

        enum1 = CType(GetStatement(EToken.eEnum), TEnumStatement)
        cla1 = PrjParse.GetCla(enum1.NameEnumStmt)
        Debug.Assert(cla1 IsNot Nothing)
        PrjParse.CurSrc.ClaSrc.Add(cla1)

        cla1.KndCla = EClass.eEnumCla
        cla1.SuperClassList.Add(PrjParse.ObjectType)
        type1 = cla1

        Do While CurStmt.TypeStmt <> EToken.eEndEnum
            ele1 = CType(GetStatement(EToken.eId), TEnumElement)
            fld1 = New TField(ele1.NameEnumEle, type1)
            cla1.AddFld(fld1)
        Loop
        GetStatement(EToken.eEndEnum)
        cla1.Parsed = True

        Return cla1
    End Function

    Public Function MakeClass() As TClass
        Dim cla1 As TClass, spr_cla As TClass, vtp As TList(Of TClass)
        Dim fld1 As TField
        Dim fnc1 As TFunction
        Dim cla_stmt As TClassStatement
        Dim var_decl As TVariableDeclaration
        Dim instmt As TInheritsStatement, implstmt As TImplementsStatement, com1 As TComment = Nothing

        cla_stmt = CType(CurStmt, TClassStatement)
        cla1 = cla_stmt.ClaClaStmt

        cla1.KndCla = cla_stmt.KndClaStmt
        PrjParse.CurSrc.ClaSrc.Add(cla1)

        If cla1.GenCla IsNot Nothing Then
            ' ジェネリック クラスの場合

            ' for Add
            For Each cla_f In cla1.GenCla
                cla_f.IsParamCla = True
                PrjParse.dicGenCla.Add(cla_f.NameCla(), cla_f)
            Next
        End If

        GetStatement(EToken.eClass)

        If CurStmt.TypeStmt = EToken.eExtends Then
            instmt = CType(GetStatement(EToken.eExtends), TInheritsStatement)
            If instmt.ParamName Is Nothing Then
                spr_cla = PrjParse.GetCla(instmt.ClassNameInheritsStmt)
            Else
                vtp = New TList(Of TClass)()
                For Each s In instmt.ParamName
                    vtp.Add(PrjParse.GetCla(s))
                Next
                spr_cla = PrjParse.GetAddSpecializedClass(instmt.ClassNameInheritsStmt, vtp)
            End If
            cla1.SuperClassList.Add(spr_cla)

        End If

        If CurStmt.TypeStmt = EToken.eImplements Then
            implstmt = CType(GetStatement(EToken.eImplements), TImplementsStatement)

            cla1.InterfaceList = implstmt.ClassImplementsStmt
        End If

        If PrjParse.ObjectType Is Nothing Then
            Debug.Assert(cla1.NameCla() = "Object")
            PrjParse.ObjectType = cla1
        End If
        Select Case cla1.NameCla()
            Case "System"
                PrjParse.SystemType = cla1
            Case "String"
                PrjParse.StringType = cla1
            Case "Char"
                PrjParse.CharType = cla1
            Case "Integer"
                PrjParse.IntType = cla1
            Case "Double"
                PrjParse.DoubleType = cla1
            Case "Type"
                PrjParse.TypeType = cla1
            Case "Boolean"
                PrjParse.BoolType = cla1
            Case "WaitHandle"
                PrjParse.WaitHandleType = cla1
        End Select

        If PrjParse.ObjectType IsNot cla1 AndAlso cla1.SuperClassList.Count = 0 Then
            cla1.SuperClassList.Add(PrjParse.ObjectType)
        End If

        Do While CurStmt.TypeStmt <> EToken.eEndClass AndAlso CurStmt.TypeStmt <> EToken.eEndStruct AndAlso CurStmt.TypeStmt <> EToken.eEndInterface
            If CurStmt.TypeStmt = EToken.eVarDecl Then
                var_decl = CType(GetStatement(EToken.eVarDecl), TVariableDeclaration)
                ' for Add
                For Each var_f In var_decl.VarDecl
                    fld1 = New TField(var_f.NameVar, var_f.TypeVar, var_f.InitVar)
                    fld1.ComVar = com1
                    com1 = Nothing
                    fld1.ModVar = var_decl.ModDecl
                    fld1.TailCom = var_decl.TailCom
                    cla1.AddFld(fld1)
                Next
            ElseIf CurStmt.TypeStmt = EToken.eComment Then
                com1 = CType(GetStatement(EToken.eComment), TComment)
            Else
                fnc1 = MakeSubFnc(cla1)
                fnc1.ComVar = com1
                com1 = Nothing
                fnc1.ClaFnc = cla1
                cla1.FncCla.Add(fnc1)
            End If
        Loop
        GetStatement(EToken.eUnknown)

        PrjParse.dicGenCla.Clear()
        cla1.Parsed = True

        Return cla1
    End Function

    '  ブロックの構文解析をする
    Function BlcParse(up1 As Object) As TBlock
        Dim blc1 As TBlock
        Dim blc_sv As TBlock
        Dim var_decl As TVariableDeclaration
        Dim if1 As TIfStatement
        Dim if2 As TIf
        Dim for1 As TForStatement
        Dim for2 As TFor
        Dim do1 As TDoStmt
        Dim sel1 As TSelectStatement
        Dim sel2 As TSelect
        Dim case1 As TCaseStatement
        Dim case2 As TCase
        Dim eif1 As TElseIf
        Dim try1 As TTry
        Dim catch1 As TCatchStatement
        Dim with1 As TWithStmt, with2 As TWith
        Dim if_blc As TIfBlock

        blc1 = New TBlock()

        blc_sv = CurBlc
        CurBlc = blc1

        Do While True
            Select Case CurStmt.TypeStmt
                Case EToken.eASN, EToken.eCall, EToken.eReturn, EToken.eThrow, EToken.eExitDo, EToken.eExitFor, EToken.eExitSub, EToken.eGoto, EToken.eLabel, EToken.eComment, EToken.eReDim
                    CurBlc.AddStmtBlc(CurStmt)
                    GetStatement(EToken.eUnknown)
                Case EToken.eIf
                    if1 = CType(GetStatement(EToken.eIf), TIfStatement)
                    if2 = New TIf()
                    CurBlc.AddStmtBlc(if2)
                    if_blc = New TIfBlock(if1.CndIfStmt, BlcParse(if2))
                    if2.IfBlc.Add(if_blc)
                    Do While True
                        Select Case CurStmt.TypeStmt
                            Case EToken.eElseIf
                                eif1 = CType(GetStatement(EToken.eElseIf), TElseIf)
                                if_blc = New TIfBlock(eif1.CndElseIf, BlcParse(if2))
                                if2.IfBlc.Add(if_blc)
                            Case EToken.eElse
                                GetStatement(EToken.eElse)
                                if_blc = New TIfBlock(Nothing, BlcParse(if2))
                                if2.IfBlc.Add(if_blc)
                                GetStatement(EToken.eEndIf)
                                Exit Do
                            Case Else
                                GetStatement(EToken.eEndIf)
                                Exit Do
                        End Select
                    Loop

                Case EToken.eFor
                    for1 = CType(GetStatement(EToken.eFor), TForStatement)
                    for2 = New TFor()

                    for2.IdxFor = for1.IdxForStmt
                    for2.FromFor = for1.FromForStmt
                    for2.ToFor = for1.ToForStmt
                    for2.StepFor = for1.StepForStmt
                    for2.InVarFor = for1.InVarForStmt
                    for2.InTrmFor = for1.InTrmForStmt

                    for2.BlcFor = BlcParse(for2)
                    CurBlc.AddStmtBlc(for2)
                    GetStatement(EToken.eNext)

                Case EToken.eDo
                    do1 = CType(GetStatement(EToken.eDo), TDoStmt)
                    for2 = New TFor()
                    for2.IsDo = True
                    CurBlc.AddStmtBlc(for2)
                    for2.CndFor = do1.CndDo
                    for2.BlcFor = BlcParse(for2)
                    GetStatement(EToken.eLoop)

                Case EToken.eSwitch
                    sel1 = CType(GetStatement(EToken.eSwitch), TSelectStatement)
                    sel2 = New TSelect()
                    sel2.TrmSel = sel1.TermSelectStatement
                    CurBlc.AddStmtBlc(sel2)
                    Do While CurStmt.TypeStmt <> EToken.eEndSelect
                        case1 = CType(GetStatement(EToken.eCase), TCaseStatement)
                        case2 = New TCase()
                        case2.TrmCase = case1.TermCaseStmt
                        case2.DefaultCase = case1.IsCaseElse
                        sel2.CaseSel.Add(case2)
                        case2.BlcCase = BlcParse(sel1)
                    Loop
                    GetStatement(EToken.eEndSelect)

                Case EToken.eTry
                    GetStatement(EToken.eTry)
                    try1 = New TTry()
                    CurBlc.AddStmtBlc(try1)
                    try1.BlcTry = BlcParse(try1)
                    catch1 = CType(GetStatement(EToken.eCatch), TCatchStatement)
                    try1.VarCatch = New TList(Of TVariable)()
                    try1.VarCatch.Add(catch1.VariableCatchStmt)
                    try1.BlcCatch = BlcParse(try1)
                    GetStatement(EToken.eEndTry)

                Case EToken.eWith
                    with1 = CType(GetStatement(EToken.eWith), TWithStmt)
                    with2 = New TWith()
                    with2.TermWith = with1.TermWith
                    CurBlc.AddStmtBlc(with2)
                    with2.BlcWith = BlcParse(with2)
                    GetStatement(EToken.eEndWith)

                Case EToken.eVarDecl
                    var_decl = CType(GetStatement(EToken.eVarDecl), TVariableDeclaration)
                    CurBlc.AddStmtBlc(var_decl)
                    ' for Add
                    For Each var1 In var_decl.VarDecl
                        CurBlc.VarBlc.Add(var1)
                    Next

                Case EToken.eExit
                    Chk(False)
                Case Else
                    Exit Do
            End Select
        Loop

        CurBlc = blc_sv

        Return blc1
    End Function

    Public Function MakeDelegate() As TClass
        Dim stmt1 As TFunctionStatement
        Dim dlg1 As TDelegate
        Dim fnc1 As TFunction

        stmt1 = CType(GetStatement(EToken.eUnknown), TFunctionStatement)
        dlg1 = PrjParse.GetDelegate(stmt1.NameFncStmt)
        dlg1.Parsed = True

        dlg1.KndCla = EClass.eDelegateCla
        dlg1.RetDlg = stmt1.RetType
        dlg1.ArgDlg = stmt1.ArgumentFncStmt
        fnc1 = New TFunction("Invoke", stmt1.RetType)
        fnc1.SetModFnc(stmt1.ModifierFncStmt)
        fnc1.ArgFnc = stmt1.ArgumentFncStmt
        fnc1.ThisFnc = New TVariable(ThisName, dlg1)
        fnc1.ClaFnc = dlg1
        dlg1.FncCla.Add(fnc1)

        PrjParse.CurSrc.ClaSrc.Add(dlg1)

        Return dlg1
    End Function

    Public Function MakeSubFnc(cla1 As TClass) As TFunction
        Dim fnc1 As TFunctionStatement
        Dim fnc2 As TFunction, fnc_name As String

        Chk(CurStmt.TypeStmt = EToken.eSub OrElse CurStmt.TypeStmt = EToken.eFunction OrElse CurStmt.TypeStmt = EToken.eNew OrElse CurStmt.TypeStmt = EToken.eOperator)
        fnc1 = CType(GetStatement(EToken.eUnknown), TFunctionStatement)
        If fnc1.TypeStmt = EToken.eNew Then
            fnc_name = "New@" + cla1.NameCla()
        Else
            fnc_name = fnc1.NameFncStmt
        End If
        fnc2 = New TFunction(fnc_name, fnc1.RetType)
        fnc2.SetModFnc(fnc1.ModifierFncStmt)
        fnc2.TypeFnc = fnc1.TypeStmt
        fnc2.OpFnc = fnc1.OpFncStmt
        fnc2.ArgumentClassFnc = fnc1.ArgumentClassFncStmt
        fnc2.ArgFnc.AddRange(fnc1.ArgumentFncStmt)
        fnc2.ThisFnc = New TVariable(ThisName, cla1)
        fnc2.InterfaceFnc = fnc1.InterfaceFncStmt
        fnc2.ImplFnc = New TReference(fnc1.InterfaceFncName)
        fnc2.IsNew = (fnc1.TypeStmt = EToken.eNew)

        If fnc2.ModFnc().isMustOverride Then
            Return fnc2
        End If

        If cla1 Is Nothing OrElse cla1.KndCla <> EClass.eInterfaceCla Then
            ' インターフェイスでない場合

            Dim blc1 As TBlock = BlcParse(fnc2)
            If blc1.StmtBlc.Count = 1 AndAlso TypeOf blc1.StmtBlc(0) Is TWith Then
                Dim with1 As TWith = CType(blc1.StmtBlc(0), TWith)
                Dim ref1 As TReference = CType(with1.TermWith, TReference)
                Debug.Assert(ref1.NameRef = fnc2.ArgFnc(0).NameVar)
                If ref1.CastType IsNot Nothing Then
                    fnc2.WithFnc = ref1.CastType
                Else
                    fnc2.WithFnc = fnc2.ArgFnc(0).TypeVar
                End If

                fnc2.BlcFnc = with1.BlcWith
            Else
                fnc2.BlcFnc = blc1
            End If
            Chk(CurStmt.TypeStmt = EToken.eEndSub OrElse CurStmt.TypeStmt = EToken.eEndFunction OrElse CurStmt.TypeStmt = EToken.eEndOperator)
            GetStatement(EToken.eUnknown)
        End If

        Return fnc2
    End Function

    Public Overrides Sub ReadAllStatement(src1 As TSourceFile)
        Dim i1 As Integer, is_err As Boolean = False
        Dim com1 As TComment, stmt1 As TStatement, cla1 As TClass

        ' 文の配列を初期化する
        src1.StmtSrc = New TList(Of TStatement)()
        ' for Add
        For i1 = 0 To src1.vTextSrc.Length - 1
            src1.StmtSrc.Add(Nothing)
        Next

        com1 = New TComment()
        ' for ???
        i1 = 0
        Do While i1 < src1.vTextSrc.Length
            CurLineIdx = i1

            CurLineStr = src1.vTextSrc(i1)
            CurVTkn = src1.LineTkn(i1)

            If CurVTkn.Count = 0 Then
                '  空行の場合

                com1.LineCom.Add("")
            ElseIf CurVTkn(0).TypeTkn = EToken.eLineComment Then
                '  コメントの場合

                com1.LineCom.Add(CurVTkn(0).StrTkn)
            Else
                '  空行やコメントでない場合

                If com1.LineCom.Count <> 0 Then
                    ' コメント・空行がある場合

                    ' 前の行にコメント文を入れる
                    src1.StmtSrc(i1 - 1) = com1

                    com1 = New TComment()
                End If

                Try
                    stmt1 = ReadStatement()

                    src1.StmtSrc(i1) = stmt1

                    ' 継続行の場合
                    Dim i2 As Integer = i1
                    Do While i2 < CurLineIdx
                        i2 = i2 + 1
                        src1.StmtSrc(i2) = Nothing
                    Loop

                    If TypeOf stmt1 Is TClassStatement Then
                        ' クラス定義の始まりの場合

                        cla1 = CType(stmt1, TClassStatement).ClaClaStmt
                        PrjParse.dicGenCla.Clear()
                        If cla1.GenCla IsNot Nothing Then
                            ' ジェネリック クラスの場合

                            For Each cla_f In cla1.GenCla
                                cla_f.IsParamCla = True
                                PrjParse.dicGenCla.Add(cla_f.NameCla(), cla_f)
                            Next
                        End If

                    ElseIf stmt1.TypeStmt = EToken.eEndClass Then
                        ' クラス定義の終わりの場合

                        PrjParse.dicGenCla.Clear()
                    End If
                Catch ex As TError
                    is_err = True
                End Try
            End If

            i1 = CurLineIdx + 1
        Loop

        Debug.Assert(Not is_err)
    End Sub

    Public Function ReadModifier() As TModifier
        Dim mod1 As TModifier

        mod1 = New TModifier()
        mod1.ValidMod = False
        Do While True
            Select Case CurTkn.TypeTkn
                Case EToken.ePartial
                    mod1.isPartial = True
                Case EToken.ePublic
                    mod1.isPublic = True
                Case EToken.eShared
                    mod1.isShared = True
                Case EToken.eConst
                    mod1.isConst = True
                Case EToken.eAbstract
                    mod1.isAbstract = True
                Case EToken.eVirtual
                    mod1.isVirtual = True
                Case EToken.eMustOverride
                    mod1.isMustOverride = True
                Case EToken.eOverride
                    mod1.isOverride = True
                Case EToken.eIterator
                    mod1.isIterator = True
                Case EToken.eProtected, EToken.eFriend, EToken.ePrivate

                Case EToken.eLT
                    GetTkn(EToken.eLT)
                    Do While True
                        Dim id1 As TToken

                        id1 = GetTkn(EToken.eId)
                        If id1.StrTkn = "XmlIgnoreAttribute" Then
                            mod1.isXmlIgnore = True
                        ElseIf id1.StrTkn = "_Weak" Then
                            mod1.isWeak = True
                        ElseIf id1.StrTkn = "_Parent" Then
                            mod1.isParent = True
                        ElseIf id1.StrTkn = "_Invariant" Then
                            mod1.isInvariant = True
                        Else
                            Debug.Assert(False)
                        End If
                        GetTkn(EToken.eLP)
                        GetTkn(EToken.eRP)

                        If CurTkn.TypeTkn <> EToken.eComma Then
                            Exit Do
                        End If
                        GetTkn(EToken.eComma)

                    Loop
                    Debug.Assert(CurTkn.TypeTkn = EToken.eGT)

                Case Else
                    Exit Do
            End Select
            GetTkn(EToken.eUnknown)
            mod1.ValidMod = True
        Loop

        Return mod1
    End Function

    Function ReadStatement() As TStatement
        Dim mod1 As TModifier, stmt1 As TStatement


        CurPos = 0
        CurTkn = CurVTkn(0)
        If 1 < CurVTkn.Count Then
            NxtTkn = CurVTkn(1)
        Else
            NxtTkn = EOTTkn
        End If
        stmt1 = Nothing

        '  修飾子を調べる

        mod1 = ReadModifier()

        If mod1.ValidMod AndAlso CurTkn.TypeTkn = EToken.eId Then
            '  変数宣言の場合

            stmt1 = ReadDim(mod1)
        Else

            Select Case CurTkn.TypeTkn
                Case EToken.eImports
                    stmt1 = ReadImports()

                Case EToken.eModule
                    stmt1 = ReadModule()

                Case EToken.eDelegate
                    GetTkn(EToken.eDelegate)
                    Debug.Assert(CurTkn.TypeTkn = EToken.eFunction OrElse CurTkn.TypeTkn = EToken.eSub)
                    stmt1 = ReadSubFunction(mod1, True)

                Case EToken.eSub, EToken.eFunction, EToken.eOperator
                    stmt1 = ReadSubFunction(mod1, False)

                Case EToken.eEnd
                    stmt1 = ReadEnd()

                Case EToken.eVar
                    GetTkn(EToken.eVar)
                    stmt1 = ReadDim(mod1)

                Case EToken.eIf
                    stmt1 = ReadIf()

                Case EToken.eElse
                    stmt1 = ReadElse()

                Case EToken.eReturn, EToken.eYield
                    stmt1 = ReadReturn(CurTkn.TypeTkn)

                Case EToken.eDo
                    stmt1 = ReadDo()

                Case EToken.eLoop
                    stmt1 = ReadLoop()

                Case EToken.eSelect
                    stmt1 = ReadSelect()

                Case EToken.eCase
                    stmt1 = ReadCase()

                Case EToken.eFor
                    stmt1 = ReadFor()

                Case EToken.eNext
                    stmt1 = ReadNext()

                Case EToken.eElseIf
                    stmt1 = ReadElseIf()

                Case EToken.eEnum
                    stmt1 = ReadEnum()

                Case EToken.eClass, EToken.eStruct, EToken.eInterface
                    stmt1 = ReadClass(mod1)

                Case EToken.eExtends
                    stmt1 = ReadInherits()

                Case EToken.eImplements
                    stmt1 = ReadImplements()

                Case EToken.eExit
                    stmt1 = ReadExit()

                Case EToken.eId, EToken.eBase, EToken.eCType, EToken.eDot
                    stmt1 = AssignmentExpression()

                Case EToken.eTry
                    stmt1 = ReadTry()

                Case EToken.eCatch
                    stmt1 = ReadCatch()

                Case EToken.eWith
                    stmt1 = ReadWith()

                Case EToken.eThrow
                    stmt1 = ReadThrow()

                Case EToken.eReDim
                    stmt1 = ReadReDim()

                Case EToken.eLineComment
                    stmt1 = ReadLineComment()

                Case EToken.eEOT
                Case Else
                    Chk(False)
            End Select
        End If

        Chk(CurTkn Is EOTTkn)

        If stmt1 IsNot Nothing Then
            stmt1.vTknStmt = CurVTkn
        End If

        Return stmt1
    End Function

    Function GetNextStatement() As TStatement
        Dim stmt1 As TStatement

        Do While CurLineIdx < PrjParse.CurSrc.vTextSrc.Length
            stmt1 = PrjParse.CurSrc.StmtSrc(CurLineIdx)
            If stmt1 IsNot Nothing Then

                CurLineStr = PrjParse.CurSrc.vTextSrc(CurLineIdx)
                CurVTkn = PrjParse.CurSrc.LineTkn(CurLineIdx)
                CurLineIdx = CurLineIdx + 1
                Return stmt1
            End If
            CurLineIdx = CurLineIdx + 1
        Loop

        Return Nothing
    End Function

    Function GetStatement(type1 As EToken) As TStatement
        Dim stmt1 As TStatement

        Chk(type1 = EToken.eUnknown OrElse CurStmt IsNot Nothing)

        Do While type1 <> EToken.eUnknown AndAlso type1 <> EToken.eComment AndAlso CurStmt.TypeStmt = EToken.eComment
            CurStmt = GetNextStatement()
            Debug.Assert(CurStmt IsNot Nothing)
        Loop

        If type1 = EToken.eUnknown OrElse type1 = CurStmt.TypeStmt Then

            stmt1 = CurStmt
            Do While True
                CurStmt = GetNextStatement()
                If CurStmt Is Nothing OrElse CurStmt.TypeStmt <> EToken.eImports Then
                    Exit Do
                End If
            Loop

            Return stmt1
        Else
            Chk(False)
            Return Nothing
        End If
    End Function

    Public Overrides Sub Parse(src1 As TSourceFile)
        MakeModule(src1)
    End Sub

    Public Sub RegTkn()
        Dim dic1 As New Dictionary(Of String, EToken)

        EOTTkn = NewToken(EToken.eEOT, "", 0)

        dic1.Add("Imports", EToken.eImports)
        dic1.Add("Module", EToken.eModule)
        dic1.Add("OrElse", EToken.eOR)
        dic1.Add("AndAlso", EToken.eAnd)
        dic1.Add("Not", EToken.eNot)
        dic1.Add("<>", EToken.eNE)
        dic1.Add("MustInherit", EToken.eAbstract)
        dic1.Add("MustOverride", EToken.eMustOverride)
        dic1.Add("AddressOf", EToken.eAddressOf)
        dic1.Add("Aggregate", EToken.eAggregate)
        dic1.Add("As", EToken.eAs)
        dic1.Add("At", EToken.eAt)
        dic1.Add("MyBase", EToken.eBase)
        dic1.Add("Break", EToken.eBreak)
        dic1.Add("Byval", EToken.eByVal)
        dic1.Add("Call", EToken.eCall)
        dic1.Add("Case", EToken.eCase)
        dic1.Add("Catch", EToken.eCatch)
        dic1.Add("Class", EToken.eClass)
        dic1.Add("Const", EToken.eConst)
        dic1.Add("CType", EToken.eCType)
        dic1.Add("Default", EToken.eDefault)
        dic1.Add("Delegate", EToken.eDelegate)
        dic1.Add("Dim", EToken.eVar)
        dic1.Add("Do", EToken.eDo)
        dic1.Add("Each", EToken.eEach)
        dic1.Add("Else", EToken.eElse)
        dic1.Add("ElseIf", EToken.eElseIf)
        dic1.Add("End", EToken.eEnd)
        dic1.Add("Enum", EToken.eEnum)
        dic1.Add("Exit", EToken.eExit)
        dic1.Add("Inherits", EToken.eExtends)
        dic1.Add("For", EToken.eFor)
        dic1.Add("Foreach", EToken.eForeach)
        dic1.Add("From", EToken.eFrom)
        dic1.Add("Function", EToken.eFunction)
        dic1.Add("Get", EToken.eGet)
        dic1.Add("GetType", EToken.eGetType)
        dic1.Add("GoTo", EToken.eGoto)
        dic1.Add("Handles", EToken.eHandles)
        dic1.Add("If", EToken.eIf)
        dic1.Add("Implements", EToken.eImplements)
        dic1.Add("In", EToken.eIn)
        dic1.Add("Interface", EToken.eInterface)
        dic1.Add("Into", EToken.eInto)
        dic1.Add("Is", EToken.eIs)
        dic1.Add("IsNot", EToken.eIsNot)
        dic1.Add("Iterator", EToken.eIterator)
        dic1.Add("Loop", EToken.eLoop)
        dic1.Add("Namespace", EToken.eNamespace)
        dic1.Add("New", EToken.eNew)
        dic1.Add("Next", EToken.eNext)
        dic1.Add("Of", EToken.eOf)
        dic1.Add("Operator", EToken.eOperator)
        dic1.Add("Out", EToken.eOut)
        dic1.Add("Overrides", EToken.eOverride)
        dic1.Add("ParamArray", EToken.eParamArray)
        dic1.Add("Partial", EToken.ePartial)
        dic1.Add("Public", EToken.ePublic)
        dic1.Add("Protected", EToken.eProtected)
        dic1.Add("Friend", EToken.eFriend)
        dic1.Add("Private", EToken.ePrivate)
        dic1.Add("ByRef", EToken.eRef)
        dic1.Add("ReDim", EToken.eReDim)
        dic1.Add("Return", EToken.eReturn)
        dic1.Add("Set", EToken.eSet)
        dic1.Add("Select", EToken.eSelect)
        dic1.Add("Shared", EToken.eShared)
        dic1.Add("Step", EToken.eStep)
        dic1.Add("Structure", EToken.eStruct)
        dic1.Add("Sub", EToken.eSub)
        dic1.Add("Take", EToken.eTake)
        dic1.Add("Then", EToken.eThen)
        dic1.Add("Throw", EToken.eThrow)
        dic1.Add("To", EToken.eTo)
        dic1.Add("Try", EToken.eTry)
        dic1.Add("TypeOf", EToken.eInstanceof)
        dic1.Add("Overridable", EToken.eVirtual)
        dic1.Add("Where", EToken.eWhere)
        dic1.Add("While", EToken.eWhile)
        dic1.Add("With", EToken.eWith)
        dic1.Add("Yield", EToken.eYield)
        dic1.Add("@id", EToken.eId)
        dic1.Add("@int", EToken.eInt)
        dic1.Add("@hex", EToken.eHex)
        dic1.Add("/*", EToken.eBlockComment)
        dic1.Add("'", EToken.eLineComment)
        dic1.Add("=", EToken.eEq)
        dic1.Add("+=", EToken.eADDEQ)
        dic1.Add("-=", EToken.eSUBEQ)
        dic1.Add("*=", EToken.eMULEQ)
        dic1.Add("/=", EToken.eDIVEQ)
        dic1.Add("%=", EToken.eMODEQ)
        dic1.Add("+", EToken.eADD)
        dic1.Add("-", EToken.eMns)
        dic1.Add("Mod", EToken.eMOD)
        dic1.Add("And", EToken.eAnp)
        dic1.Add("(", EToken.eLP)
        dic1.Add(")", EToken.eRP)
        dic1.Add("*", EToken.eMUL)
        dic1.Add(",", EToken.eComma)
        dic1.Add(".", EToken.eDot)
        dic1.Add("/", EToken.eDIV)
        dic1.Add(":", EToken.eMMB)
        dic1.Add("", EToken.eSM)
        dic1.Add("[", EToken.eLB)
        dic1.Add("]", EToken.eRB)
        dic1.Add("_", EToken.eLowLine)
        dic1.Add("^", EToken.eHAT)
        dic1.Add("{", EToken.eLC)
        dic1.Add("|", EToken.eBitOR)
        dic1.Add("}", EToken.eRC)
        dic1.Add("~", EToken.eTilde)
        dic1.Add("<", EToken.eLT)
        dic1.Add(">", EToken.eGT)
        dic1.Add("<=", EToken.eLE)
        dic1.Add(">=", EToken.eGE)

        ' for Add
        For Each key1 In dic1.Keys
            vTkn.Add(key1.ToLower(), dic1(key1))
        Next

        If vTknName Is Nothing Then
            vTknName = New Dictionary(Of EToken, String)()
            ' for Add
            For Each key1 In dic1.Keys
                vTknName.Add(dic1(key1), key1)
            Next
            vTknName.Add(EToken.eASN, "=")
            vTknName.Add(EToken.eINC, "++")
            vTknName.Add(EToken.eDEC, "--")


            vTknName.Add(EToken.eExitFor, "Exit For")
            vTknName.Add(EToken.eExitDo, "Exit Do")
            vTknName.Add(EToken.eExitSub, "Exit Sub")

            vTknName.Add(EToken.eEndIf, "End If")
            vTknName.Add(EToken.eEndSub, "End Sub")
            vTknName.Add(EToken.eEndFunction, "End Function")
            vTknName.Add(EToken.eEndOperator, "End Operator")
            vTknName.Add(EToken.eEndClass, "End Class")
            vTknName.Add(EToken.eEndStruct, "End Structure")
            vTknName.Add(EToken.eEndInterface, "End Interface")
            vTknName.Add(EToken.eEndEnum, "End Enum")
            vTknName.Add(EToken.eEndModule, "End Module")
            vTknName.Add(EToken.eEndSelect, "End Select")
            vTknName.Add(EToken.eEndTry, "End Try")
            vTknName.Add(EToken.eEndWith, "End With")
        End If
    End Sub

    Function NewToken(type1 As EToken, str1 As String, pos1 As Integer) As TToken
        Dim tkn1 As New TToken

        tkn1.TypeTkn = type1
        tkn1.StrTkn = str1
        tkn1.PosTkn = pos1

        Return tkn1
    End Function

    Public Overrides Function Lex(src_text As String) As TList(Of TToken)
        Dim v1 As TList(Of TToken)
        Dim cur1 As Integer, spc As Integer
        Dim src_len As Integer
        Dim k1 As Integer
        Dim ch1 As Char
        Dim ch2 As Char
        Dim str1 As String = Nothing
        Dim type1 As EToken
        Dim prv_type As EToken
        Dim tkn1 As TToken
        Dim sb1 As TStringWriter
        Dim ok As Boolean

        src_len = src_text.Length
        v1 = New TList(Of TToken)()

        cur1 = 0
        prv_type = EToken.eUnknown

        Do While True
            tkn1 = Nothing

            spc = 0
            Do While cur1 < src_len
                ch1 = src_text(cur1)
                Select Case ch1
                    Case " "c
                        spc += 1
                    Case vbTab
                        spc += 4
                    Case Else
                        Exit Do
                End Select
                cur1 += 1
            Loop
            If src_len <= cur1 Then
                Exit Do
            End If

            ch1 = src_text(cur1)
            If cur1 + 1 < src_text.Length Then
                ch2 = src_text(cur1 + 1)
            Else
                ch2 = ChrW(0)
            End If

            Select Case ch1
                Case """"c
                    '  引用符の場合

                    sb1 = New TStringWriter()
                    k1 = cur1 + 1
                    Do While k1 < src_text.Length
                        ch2 = src_text(k1)
                        If ch2 = """"c Then
                            If k1 + 1 < src_text.Length AndAlso src_text(k1 + 1) = """"c Then
                                '  引用符のエスケープの場合

                                sb1.Append(""""c)
                                k1 = k1 + 2
                            Else
                                If k1 + 1 < src_text.Length AndAlso src_text(k1 + 1) = "c"c Then
                                    '  文字の場合

                                    tkn1 = New TToken(EToken.eChar, sb1.ToString(), cur1)
                                    cur1 = k1 + 2
                                Else
                                    '  文字列の場合

                                    tkn1 = NewToken(EToken.eString, sb1.ToString(), cur1)
                                    cur1 = k1 + 1
                                End If
                                Exit Do
                            End If
                        Else
                            sb1.Append(ch2)
                            k1 = k1 + 1
                        End If
                    Loop

                Case "'"c
                    '  コメントの場合
                    tkn1 = NewToken(EToken.eLineComment, src_text.Substring(cur1 + 1), cur1)
                    cur1 = src_text.Length

                Case Else
                    If Char.IsDigit(ch1) Then
                        '  数字の場合

                        ' for Find
                        For k1 = cur1 + 1 To src_text.Length - 1
                            ch2 = src_text(k1)
                            If Not Char.IsDigit(ch2) AndAlso ch2 <> "."c Then
                                Exit For
                            End If
                        Next
                        If k1 < src_text.Length AndAlso src_text(k1) = "F"c Then
                            k1 = k1 + 1
                        End If

                        str1 = TSys.Substring(src_text, cur1, k1)
                        tkn1 = NewToken(EToken.eInt, str1, cur1)

                        cur1 = k1
                    ElseIf ch1 = "&"c AndAlso ch2 = "H"c Then
                        ' 16進数の場合

                        ' for Find
                        For k1 = cur1 + 2 To src_text.Length - 1
                            ch2 = src_text(k1)
                            If Not (Char.IsDigit(ch2) OrElse "A"c <= ch2 AndAlso ch2 <= "F"c) Then
                                Exit For
                            End If
                        Next

                        str1 = TSys.Substring(src_text, cur1, k1)
                        tkn1 = NewToken(EToken.eHex, str1, cur1)

                        cur1 = k1
                    ElseIf 256 <= AscW(ch1) OrElse Char.IsLetter(ch1) OrElse ch1 = "_"c Then
                        '  英字の場合

                        ' for Find
                        For k1 = cur1 + 1 To src_text.Length - 1

                            ch2 = src_text(k1)
                            If AscW(ch2) < 256 AndAlso Not Char.IsLetterOrDigit(ch2) AndAlso ch2 <> "_"c Then
                                '  半角で英数字や"_"でない場合

                                Exit For
                            End If
                        Next
                        str1 = TSys.Substring(src_text, cur1, k1)
                        If vTkn.ContainsKey(str1.ToLower()) Then
                            '  予約語の場合

                            type1 = vTkn(str1.ToLower())
                            If type1 = EToken.eGetType AndAlso (prv_type = EToken.eDot OrElse prv_type = EToken.eFunction) Then
                                type1 = EToken.eId
                            ElseIf type1 = EToken.eSelect AndAlso prv_type = EToken.eDot Then
                                type1 = EToken.eId
                            End If
                        Else
                            '  識別子の場合

                            type1 = EToken.eId
                        End If
                        tkn1 = NewToken(type1, str1, cur1)

                        cur1 = k1
                    Else
                        '  記号の場合

                        ok = False
                        type1 = EToken.eUnknown
                        If cur1 + 1 < src_len Then
                            '  2文字の記号を調べる

                            str1 = TSys.Substring(src_text, cur1, cur1 + 2)
                            ok = vTkn.ContainsKey(str1)
                            If ok Then
                                type1 = vTkn(str1)
                            End If
                        End If
                        If Not ok Then
                            '  1文字の記号を調べる

                            str1 = TSys.Substring(src_text, cur1, cur1 + 1)
                            If vTkn.ContainsKey(str1) Then
                                type1 = vTkn(str1)
                            Else
                                '  ない場合

                                Debug.WriteLine("lex str err")
                                Chk(False)
                            End If
                        End If

                        tkn1 = NewToken(type1, str1, cur1)
                        cur1 = cur1 + str1.Length
                    End If
            End Select

            ' Debug.WriteLine("token:{0} {1}", tkn1.StrTkn, tkn1.TypeTkn)
            tkn1.SpcTkn = spc
            v1.Add(tkn1)
            prv_type = tkn1.TypeTkn
        Loop

        Return v1
    End Function

    Public Overrides Sub RegAllClass(prj1 As TProject, src1 As TSourceFile)
        Dim id1 As TToken, k1 As Integer, cla1 As TClass, cla2 As TClass, id2 As TToken, is_delegate As Boolean

        For Each v In src1.LineTkn

            If 3 <= v.Count AndAlso v(0).TypeTkn = EToken.ePublic Then

                is_delegate = False
                Select Case v(1).TypeTkn
                    Case EToken.eDelegate
                        Debug.Assert(v(2).TypeTkn = EToken.eSub OrElse v(2).TypeTkn = EToken.eFunction)
                        is_delegate = True
                        k1 = 3

                    Case EToken.eClass, EToken.eStruct, EToken.eInterface, EToken.eEnum
                        k1 = 2
                    Case EToken.eAbstract
                        Select Case v(2).TypeTkn
                            Case EToken.eClass, EToken.eStruct, EToken.eInterface
                                k1 = 3
                            Case Else
                                Debug.Assert(False)
                        End Select
                    Case Else
                        k1 = -1
                End Select

                If k1 <> -1 Then
                    id1 = v(k1)

                    If is_delegate Then
                        Debug.Assert(prj1.GetCla(id1.StrTkn) Is Nothing)

                        cla1 = New TDelegate(prj1, id1.StrTkn)
                        prj1.SimpleParameterizedClassList.Add(cla1)
                        prj1.SimpleParameterizedClassTable.Add(cla1.NameCla(), cla1)
                    Else
                        cla1 = prj1.RegCla(id1.StrTkn)
                    End If

                    If k1 + 2 < v.Count AndAlso v(k1 + 1).TypeTkn = EToken.eLP AndAlso v(k1 + 2).TypeTkn = EToken.eOf Then
                        cla1.GenericType = EGeneric.ParameterizedClass

                        cla1.GenCla = New TList(Of TClass)()

                        k1 += 3
                        Do While k1 < v.Count
                            id2 = v(k1)

                            cla2 = New TClass(prj1, id2.StrTkn)
                            cla2.IsParamCla = True
                            cla2.GenericType = EGeneric.ArgumentClass
                            cla1.GenCla.Add(cla2)

                            If v(k1 + 1).TypeTkn = EToken.eRP Then
                                Debug.Assert(is_delegate OrElse k1 + 2 = v.Count)
                                Exit Do
                            End If

                            Debug.Assert(v(k1 + 1).TypeTkn = EToken.eComma)
                            k1 += 2
                        Loop

                        prj1.dicCmpCla.Add(cla1, New TList(Of TClass)())
                    Else
                        cla1.GenericType = EGeneric.SimpleClass
                    End If
                End If
            End If
        Next
    End Sub

    Function ArgumentExpressionList(app1 As TApply) As TApply
        Dim trm1 As TTerm

        ' 			bool	b_of;
        '             b_of = false;
        GetTkn(EToken.eLP)
        If CurTkn.TypeTkn = EToken.eOf Then
            GetTkn(EToken.eOf)
        End If
        '                 b_of = true;
        If CurTkn.TypeTkn <> EToken.eRP Then
            Do While True
                trm1 = TermExpression()
                app1.AddInArg(trm1)

                If CurTkn.TypeTkn <> EToken.eComma Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If
        GetTkn(EToken.eRP)

        Return app1
    End Function

    Function CallExpression(trm1 As TTerm) As TTerm
        Dim app1 As TApply

        Do While CurTkn.TypeTkn = EToken.eLP
            app1 = TApply.MakeAppCall(trm1)
            ArgumentExpressionList(app1)
            trm1 = app1
        Loop

        Return trm1
    End Function

    '   配列の構文解析
    Function ArrayExpression() As TArray
        Dim arr1 As TArray
        Dim trm1 As TTerm

        arr1 = New TArray()
        GetTkn(EToken.eLC)
        If CurTkn.TypeTkn <> EToken.eRC Then
            Do While True
                trm1 = TermExpression()
                arr1.TrmArr.Add(trm1)
                If CurTkn.TypeTkn = EToken.eRC Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If
        GetTkn(EToken.eRC)

        Return arr1
    End Function


    Function NewExpression() As TApply
        Dim tkn1 As TToken
        Dim type1 As TClass
        Dim app1 As TApply

        tkn1 = GetTkn(EToken.eNew)
        type1 = ReadType(True)
        app1 = TApply.MakeAppNew(type1)
        If CurTkn.TypeTkn = EToken.eLP Then
            ArgumentExpressionList(app1)
        End If
        If CurTkn.TypeTkn = EToken.eLC Then
            ' 配列の場合
            app1.IniApp = ArrayExpression()

            ' 配列型に変える
            app1.NewApp = PrjParse.GetArrCla(app1.NewApp, 1)
        End If
        If CurTkn.TypeTkn = EToken.eFrom Then
            GetTkn(EToken.eFrom)

            Debug.Assert(CurTkn.TypeTkn = EToken.eLC)
            app1.IniApp = ArrayExpression()
        End If

        Return app1
    End Function

    ' From i In v1 Where i Mod 2 = 0 Select AA(i)
    Function FromExpression() As TFrom
        Dim from1 As New TFrom

        GetTkn(EToken.eFrom)

        Dim id1 As TToken = GetTkn(EToken.eId)
        from1.VarQry = New TVariable(id1, Nothing)

        GetTkn(EToken.eIn)
        from1.SeqQry = TermExpression()

        If CurTkn.TypeTkn = EToken.eWhere Then

            GetTkn(EToken.eWhere)
            from1.CndQry = TermExpression()
        End If

        If CurTkn.TypeTkn = EToken.eSelect Then

            GetTkn(EToken.eSelect)
            from1.SelFrom = TermExpression()
        End If

        If CurTkn.TypeTkn = EToken.eTake Then

            GetTkn(EToken.eTake)
            from1.TakeFrom = TermExpression()
        End If


        If CurTkn.TypeTkn = EToken.eFrom Then
            from1.InnerFrom = FromExpression()
        End If

        Return from1
    End Function

    ' Aggregate x In v Into Sum(x.Value)
    Function AggregateExpression() As TAggregate
        Dim aggr1 As New TAggregate, id1 As TToken, id2 As TToken

        GetTkn(EToken.eAggregate)
        id1 = GetTkn(EToken.eId)
        aggr1.VarQry = New TVariable(id1, Nothing)

        GetTkn(EToken.eIn)
        aggr1.SeqQry = TermExpression()

        If CurTkn.TypeTkn = EToken.eWhere Then

            GetTkn(EToken.eWhere)
            aggr1.CndQry = TermExpression()
        End If

        GetTkn(EToken.eInto)

        id2 = GetTkn(EToken.eId)
        Select Case id2.StrTkn
            Case "Sum"
                aggr1.FunctionAggr = EAggregateFunction.eSum
            Case "Max"
                aggr1.FunctionAggr = EAggregateFunction.eMax
            Case "Min"
                aggr1.FunctionAggr = EAggregateFunction.eMin
            Case Else
                Debug.Assert(False)
        End Select


        GetTkn(EToken.eLP)

        aggr1.IntoAggr = TermExpression()

        GetTkn(EToken.eRP)

        Return aggr1
    End Function

    Function PrimaryExpression() As TTerm
        Dim ref1 As TReference
        Dim trm1 As TTerm, trm2 As TTerm
        Dim ret1 As TTerm
        Dim id1 As TToken, tkn1 As TToken
        Dim type1 As TClass
        Dim app1 As TApply

        Select Case CurTkn.TypeTkn
            Case EToken.eId
                id1 = GetTkn(EToken.eId)
                If CurTkn.TypeTkn = EToken.eLP AndAlso NxtTkn.TypeTkn = EToken.eOf Then
                    ' ジェネリック型の場合

                    ' ジェネリック型の構文解析
                    type1 = ReadGenType(id1)
                    ref1 = New TReference(type1)
                    Return ref1
                End If

                ref1 = New TReference(id1)
                Return CallExpression(ref1)

            Case EToken.eDot
                trm1 = Nothing
                Do While CurTkn.TypeTkn = EToken.eDot
                    GetTkn(EToken.eDot)
                    id1 = GetTkn(EToken.eId)
                    trm2 = New TDot(trm1, id1.StrTkn)
                    trm1 = CallExpression(trm2)
                Loop

                Return trm1

            Case EToken.eBase
                GetTkn(EToken.eBase)
                GetTkn(EToken.eDot)
                Debug.Assert(CurTkn.TypeTkn = EToken.eNew OrElse CurTkn.TypeTkn = EToken.eId)
                tkn1 = GetTkn(EToken.eUnknown)
                app1 = TApply.MakeAppBase(tkn1)
                ArgumentExpressionList(app1)
                Return app1

            Case EToken.eLP
                GetTkn(EToken.eLP)
                trm1 = New TParenthesis(TermExpression())
                GetTkn(EToken.eRP)
                ret1 = CallExpression(trm1)

            Case EToken.eLC
                Return ArrayExpression()

            Case EToken.eString, EToken.eChar, EToken.eInt, EToken.eHex
                tkn1 = GetTkn(EToken.eUnknown)
                ret1 = New TConstant(tkn1.TypeTkn, tkn1.StrTkn)

            Case EToken.eNew
                Return NewExpression()

            Case EToken.eInstanceof
                GetTkn(EToken.eInstanceof)
                trm1 = AdditiveExpression()
                GetTkn(EToken.eIs)
                type1 = ReadType(False)
                Return TApply.NewTypeOf(trm1, type1)

            Case EToken.eGetType
                GetTkn(EToken.eGetType)
                GetTkn(EToken.eLP)
                type1 = ReadType(False)
                GetTkn(EToken.eRP)
                Return TApply.MakeAppGetType(type1)

            Case EToken.eCType
                GetTkn(EToken.eCType)
                GetTkn(EToken.eLP)
                trm1 = AdditiveExpression()
                GetTkn(EToken.eComma)
                trm1.CastType = ReadType(False)
                GetTkn(EToken.eRP)

                Return trm1

            Case EToken.eAddressOf
                GetTkn(EToken.eAddressOf)
                trm1 = TermExpression()
                Debug.Assert(TypeOf trm1 Is TReference)
                CType(trm1, TReference).IsAddressOf = True
                Return trm1

            Case EToken.eFrom
                Return FromExpression()

            Case EToken.eAggregate
                Return AggregateExpression()

            Case EToken.eIf
                GetTkn(EToken.eIf)
                GetTkn(EToken.eLP)
                Dim cnd1 As TTerm = TermExpression()
                GetTkn(EToken.eComma)
                trm1 = TermExpression()
                GetTkn(EToken.eComma)
                trm2 = TermExpression()
                GetTkn(EToken.eRP)

                app1 = TApply.MakeApp3Opr(cnd1, trm1, trm2)
                Return app1

            Case Else
                Chk(False)
                Return Nothing
        End Select

        Return ret1
    End Function

    Function DotExpression() As TTerm
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim id1 As TToken

        trm1 = PrimaryExpression()

        Do While CurTkn.TypeTkn = EToken.eDot
            GetTkn(EToken.eDot)
            id1 = GetTkn(EToken.eId)
            trm2 = New TDot(trm1, id1.StrTkn)
            trm1 = CallExpression(trm2)
        Loop

        Return trm1
    End Function

    Function UnaryExpression() As TTerm
        Dim tkn1 As TToken
        Dim trm1 As TTerm

        If CurTkn.TypeTkn = EToken.eMns Then
            tkn1 = GetTkn(EToken.eMns)
            trm1 = DotExpression()

            Return TApply.MakeApp1Opr(tkn1, trm1)
        End If

        Return DotExpression()
    End Function

    Function MultiplicativeExpression() As TTerm
        Dim trm1 As TTerm
        Dim tkn1 As TToken
        Dim trm2 As TTerm

        trm1 = UnaryExpression()
        If CurTkn.TypeTkn = EToken.eMUL OrElse CurTkn.TypeTkn = EToken.eDIV OrElse CurTkn.TypeTkn = EToken.eMOD Then
            tkn1 = GetTkn(EToken.eUnknown)
            trm2 = MultiplicativeExpression()

            Return TApply.MakeApp2Opr(tkn1, trm1, trm2)
        End If

        Return trm1
    End Function

    Public Function AdditiveExpression() As TTerm
        Dim trm1 As TTerm
        Dim tkn1 As TToken
        Dim trm2 As TTerm

        trm1 = MultiplicativeExpression()
        If CurTkn.TypeTkn = EToken.eADD OrElse CurTkn.TypeTkn = EToken.eMns Then
            tkn1 = GetTkn(EToken.eUnknown)
            trm2 = AdditiveExpression()

            Return TApply.MakeApp2Opr(tkn1, trm1, trm2)
        End If
        Return trm1
    End Function

    Public Function RelationalExpression() As TTerm
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim type1 As EToken
        '      Dim type2 As TClass
        'Dim par1 As Boolean

        trm1 = AdditiveExpression()
        Select Case CurTkn.TypeTkn
            Case EToken.eEq, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eNE, EToken.eLT, EToken.eGT, EToken.eLE, EToken.eGE, EToken.eIs, EToken.eIsNot
                type1 = CurTkn.TypeTkn
                GetTkn(EToken.eUnknown)
                trm2 = AdditiveExpression()
                Return TApply.NewOpr2(type1, trm1, trm2)

            Case Else
                Return trm1
        End Select
    End Function

    Function NotExpression() As TTerm
        Dim trm1 As TTerm
        Dim type1 As EToken
        Dim app1 As TApply

        If CurTkn.TypeTkn = EToken.eNot Then
            type1 = CurTkn.TypeTkn
            GetTkn(type1)
            trm1 = NotExpression()
            Debug.Assert(TypeOf trm1 Is TApply OrElse TypeOf trm1 Is TReference OrElse TypeOf trm1 Is TParenthesis)
            If TypeOf trm1 Is TApply Then
                app1 = CType(trm1, TApply)
                app1.Negation = Not app1.Negation
                Return app1
            Else
                Dim opr1 As TApply = TApply.NewOpr(type1)

                opr1.AddInArg(trm1)
                Return opr1
            End If
        End If

        Return RelationalExpression()
    End Function

    Function AndExpression() As TTerm
        Dim trm1 As TTerm
        Dim opr1 As TApply
        Dim type1 As EToken

        trm1 = NotExpression()
        If CurTkn.TypeTkn = EToken.eAnd OrElse CurTkn.TypeTkn = EToken.eAnp Then

            type1 = CurTkn.TypeTkn
            opr1 = TApply.NewOpr(type1)
            opr1.AddInArg(trm1)
            Do While CurTkn.TypeTkn = type1
                GetTkn(type1)
                opr1.AddInArg(NotExpression())
            Loop

            Return opr1
        Else

            Return trm1
        End If
    End Function

    Function OrExpression() As TTerm
        Dim trm1 As TTerm
        Dim opr1 As TApply
        Dim type1 As EToken

        trm1 = AndExpression()
        If CurTkn.TypeTkn = EToken.eOR Then

            type1 = CurTkn.TypeTkn
            opr1 = TApply.NewOpr(type1)
            opr1.AddInArg(trm1)
            Do While CurTkn.TypeTkn = type1
                GetTkn(type1)
                opr1.AddInArg(AndExpression())
            Loop

            Return opr1
        Else

            Return trm1
        End If
    End Function

    Public Function TermExpression() As TTerm
        Return OrExpression()
    End Function

    Public Function AssignmentExpression() As TStatement
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim rel1 As TApply
        Dim asn1 As TAssignment
        Dim eq1 As TToken

        trm1 = CType(AdditiveExpression(), TTerm)

        Select Case CurTkn.TypeTkn
            Case EToken.eEq, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ
                eq1 = GetTkn(EToken.eUnknown)
                trm2 = CType(TermExpression(), TTerm)
                rel1 = TApply.NewOpr2(eq1.TypeTkn, trm1, trm2)
                asn1 = New TAssignment(rel1)
                Return asn1
        End Select

        If TypeOf trm1 Is TReference Then
            Return New TEnumElement(CType(trm1, TReference))
        End If
        Return New TCall(CType(trm1, TApply))
    End Function

End Class

Public Class TClassStatement
    Inherits TStatement
    Public KndClaStmt As EClass = EClass.eClassCla
    Public ClaClaStmt As TClass
End Class

Public Class TInheritsStatement
    Inherits TStatement
    Public ClassNameInheritsStmt As String
    Public ParamName As TList(Of String)
End Class

Public Class TImplementsStatement
    Inherits TStatement
    Public ClassImplementsStmt As New TList(Of TClass)
End Class

Public Class TEnumStatement
    Inherits TStatement
    Public NameEnumStmt As String
End Class

Public Class TFunctionStatement
    Inherits TStatement
    Public ModifierFncStmt As TModifier
    Public OpFncStmt As EToken = EToken.eUnknown
    Public NameFncStmt As String
    Public ArgumentClassFncStmt As TList(Of TClass)
    Public ArgumentFncStmt As New TList(Of TVariable)
    Public RetType As TClass
    Public InterfaceFncStmt As TClass
    Public InterfaceFncName As String
    Public IsDelegateFncStmt As Boolean
End Class

Public Class TIfStatement
    Inherits TStatement
    Public CndIfStmt As TTerm
End Class

Public Class TSelectStatement
    Inherits TStatement
    Public TermSelectStatement As TTerm
End Class

Public Class TCaseStatement
    Inherits TStatement
    Public IsCaseElse As Boolean
    Public TermCaseStmt As New TList(Of TTerm)
End Class

Public Class TCatchStatement
    Inherits TStatement
    Public VariableCatchStmt As TVariable
End Class

Public Class TForStatement
    Inherits TStatement
    Public IdxForStmt As TReference
    Public FromForStmt As TTerm
    Public ToForStmt As TTerm
    Public StepForStmt As TTerm
    Public InVarForStmt As TVariable
    Public InTrmForStmt As TTerm
End Class

Public Class TExit
    Inherits TStatement
    Public LabelExit As Integer

    Public Sub New()
    End Sub
End Class

Public Class TThrow
    Inherits TStatement
    Public TrmThrow As TTerm

    Public Sub New(trm1 As TTerm)
        TypeStmt = EToken.eThrow
        TrmThrow = trm1
    End Sub

End Class

Public Class TReDim
    Inherits TStatement
    Public TrmReDim As TTerm
    Public DimReDim As TList(Of TTerm)

    Public Sub New(trm1 As TTerm, vtrm1 As TList(Of TTerm))
        TypeStmt = EToken.eReDim
        TrmReDim = trm1
        DimReDim = vtrm1
    End Sub
End Class

Public Class TImports
    Inherits TStatement
End Class

Public Class TModule
    Inherits TStatement
    Public NameMod As String
End Class

Public Class TElseIf
    Inherits TStatement
    Public CndElseIf As TTerm
End Class

Public Class TDoStmt
    Inherits TStatement

    Public CndDo As TTerm
End Class

Public Class TEnumElement
    Inherits TStatement

    Public NameEnumEle As String

    Public Sub New(ref1 As TReference)
        TypeStmt = EToken.eId
        NameEnumEle = ref1.NameRef
    End Sub

End Class

Public Class TComment
    Inherits TStatement

    Public LineCom As New TList(Of String)

    Public Sub New()
        TypeStmt = EToken.eComment
    End Sub

    Public Function GetFirstLine() As String
        For Each s In LineCom
            If s <> "" Then
                Return s
            End If
        Next

        Return ""
    End Function
End Class

Public Class TError
    Inherits Exception

    Public MsgErr As String

    Public Sub New(msg As String)
        Debug.WriteLine("err:" + msg)
        MsgErr = msg
    End Sub
End Class
