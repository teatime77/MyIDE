Imports System.Diagnostics

'-------------------------------------------------------------------------------- TParseBas
' Basicの構文解析
Public Class TParseBas
    Public PrjParse As TPrj
    Public vTknName As Dictionary(Of ETkn, String)

    Public vTkn As New Dictionary(Of String, ETkn)
    Public CurBlc As TBlc
    Public CurPos As Integer
    Public CurTkn As TTkn
    Public NxtTkn As TTkn
    Dim EOTTkn As TTkn
    Public CurVTkn As TList(Of TTkn)
    Dim CurStmt As TStmt
    Public CurLineIdx As Integer
    Dim CurLineStr As String

    Public Sub New(prj1 As TPrj)
        PrjParse = prj1
        RegTkn()
    End Sub

    Public Sub ClearParse()
        CurBlc = Nothing
        CurPos = 0
        CurTkn = Nothing
        NxtTkn = Nothing
        CurVTkn = Nothing
        CurStmt = Nothing
        CurLineIdx = 0
        CurLineStr = ""
    End Sub

    Public Function GetTkn(type1 As ETkn) As TTkn
        Dim tkn1 As TTkn

        If type1 = CurTkn.TypeTkn OrElse type1 = ETkn.eUnknown Then
            tkn1 = CurTkn
            CurPos = CurPos + 1
            If CurPos < CurVTkn.Count Then
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

    Function ReadImports() As TStmt
        Dim stmt1 As New TImports
        Dim id1 As TTkn
        Dim tkn1 As TTkn
        Dim sb1 As TStringWriter

        stmt1.TypeStmt = ETkn.eImports
        GetTkn(ETkn.eImports)

        sb1 = New TStringWriter()
        Do While True
            id1 = GetTkn(ETkn.eId)
            sb1.Append(id1.StrTkn)

            Select Case CurTkn.TypeTkn
                Case ETkn.eEOT
                    Exit Do
                Case ETkn.eDot
                    tkn1 = GetTkn(ETkn.eDot)
                    sb1.Append(tkn1.StrTkn)
                Case Else
                    Chk(False)
            End Select
        Loop

        PrjParse.CurSrc.vUsing.Add(sb1.ToString())

        Return stmt1
    End Function

    Function ReadModule() As TStmt
        Dim stmt1 As New TModule
        Dim id1 As TTkn

        stmt1.TypeStmt = ETkn.eModule
        GetTkn(ETkn.eModule)
        id1 = GetTkn(ETkn.eId)
        stmt1.NameMod = id1.StrTkn

        Return stmt1
    End Function

    Function ReadEnum() As TStmt
        Dim stmt1 As New TEnumStatement
        Dim id1 As TTkn

        stmt1.TypeStmt = ETkn.eEnum
        GetTkn(ETkn.eEnum)
        id1 = GetTkn(ETkn.eId)
        stmt1.NameEnumStmt = id1.StrTkn
        Return stmt1
    End Function

    ' クラスやデリケートの宣言でジェネリックのパラメータを読む
    Sub ReadGenCla(cla1 As TCls, is_delegate As Boolean)
        Dim cla2 As TCls, id2 As TTkn

        Debug.Assert(cla1.GenCla Is Nothing)
        cla1.GenCla = New TList(Of TCls)()

        GetTkn(ETkn.eLP)
        GetTkn(ETkn.eOf)
        Do While True
            id2 = GetTkn(ETkn.eId)

            cla2 = New TCls(id2.StrTkn)
            cla1.GenCla.Add(cla2)

            If Not is_delegate Then
                cla2.IsParamCla = True
                PrjParse.dicGenCla.Add(cla2.NameCla(), cla2)
            End If

            If CurTkn.TypeTkn = ETkn.eRP Then
                Exit Do
            End If
            GetTkn(ETkn.eComma)
        Loop
        GetTkn(ETkn.eRP)
    End Sub

    Function ReadClass(mod1 As TModifier) As TStmt
        Dim stmt1 As New TClassStatement, cla1 As TCls
        Dim id1 As TTkn

        PrjParse.dicGenCla.Clear()

        stmt1.TypeStmt = ETkn.eClass
        Select Case CurTkn.TypeTkn
            Case ETkn.eClass
                stmt1.KndClaStmt = EClass.eClassCla
            Case ETkn.eStruct
                stmt1.KndClaStmt = EClass.eStructCla
            Case ETkn.eInterface
                stmt1.KndClaStmt = EClass.eInterfaceCla
        End Select
        GetTkn(ETkn.eUnknown)
        id1 = GetTkn(ETkn.eId)
        cla1 = PrjParse.GetCla(id1.StrTkn)
        Debug.Assert(cla1 IsNot Nothing)
        cla1.ModVar = mod1
        stmt1.ClaClaStmt = cla1

        If CurTkn.TypeTkn = ETkn.eLP Then
            ' ジェネリック クラスの場合

            '            ReadGenCla(cla1, False)
            For Each cla2 In cla1.GenCla
                cla2.IsParamCla = True
                PrjParse.dicGenCla.Add(cla2.NameCla(), cla2)
            Next

            GetTkn(ETkn.eLP)
            GetTkn(ETkn.eOf)

            Do While True
                GetTkn(ETkn.eId)
                If CurTkn.TypeTkn = ETkn.eRP Then
                    GetTkn(ETkn.eRP)
                    Exit Do
                End If
                GetTkn(ETkn.eComma)
            Loop
        End If

        Return stmt1
    End Function

    Function ReadInherits() As TStmt
        Dim stmt1 As New TInheritsStatement, id1 As TTkn, id2 As TTkn

        stmt1.TypeStmt = ETkn.eInherits
        GetTkn(ETkn.eInherits)
        id1 = GetTkn(ETkn.eId)
        stmt1.ClassNameInheritsStmt = id1.StrTkn

        If CurTkn.TypeTkn = ETkn.eLP Then

            GetTkn(ETkn.eLP)
            GetTkn(ETkn.eOf)

            stmt1.ParamName = New TList(Of String)()
            Do While True
                id2 = GetTkn(ETkn.eId)
                stmt1.ParamName.Add(id2.StrTkn)

                If CurTkn.TypeTkn <> ETkn.eComma Then

                    Exit Do
                End If
                GetTkn(ETkn.eComma)
            Loop
            GetTkn(ETkn.eRP)

        End If
        Return stmt1
    End Function

    Function ReadImplements() As TStmt
        Dim stmt1 As New TImplementsStatement
        Dim cla1 As TCls

        stmt1.TypeStmt = ETkn.eImplements
        GetTkn(ETkn.eImplements)
        Do While True
            cla1 = ReadType(False)
            stmt1.ClassImplementsStmt.Add(cla1)

            If CurTkn.TypeTkn <> ETkn.eComma Then
                Exit Do
            End If
            GetTkn(ETkn.eComma)
        Loop
        Return stmt1
    End Function

    Function ReadSubFunction(mod1 As TModifier, is_delegate As Boolean) As TStmt
        Dim stmt1 As New TFunctionStatement
        Dim id1 As TTkn, id2 As TTkn, id3 As TTkn
        Dim var1 As TVar
        Dim by_ref As Boolean, param_array As Boolean
        Dim cla1 As TDelegate

        If is_delegate Then
            PrjParse.dicGenCla.Clear()
        End If

        stmt1.TypeStmt = CurTkn.TypeTkn
        stmt1.ModifierFncStmt = mod1
        stmt1.IsDelegateFncStmt = is_delegate
        GetTkn(ETkn.eUnknown)
        If CurTkn.TypeTkn = ETkn.eNew Then
            GetTkn(ETkn.eNew)
            stmt1.TypeStmt = ETkn.eNew
        Else
            If stmt1.TypeStmt = ETkn.eOperator Then
                stmt1.OpFncStmt = CurTkn.TypeTkn
            Else
                Debug.Assert(CurTkn.TypeTkn = ETkn.eId)
            End If
            id1 = GetTkn(ETkn.eUnknown)
            stmt1.NameFncStmt = id1.StrTkn
            If is_delegate Then
                cla1 = PrjParse.GetDelegate(stmt1.NameFncStmt)
                If CurTkn.TypeTkn = ETkn.eLP AndAlso NxtTkn.TypeTkn = ETkn.eOf Then

                    ReadGenCla(cla1, True)

                    For Each cla_f In cla1.GenCla
                        cla_f.IsParamCla = True
                        PrjParse.dicGenCla.Add(cla_f.NameCla(), cla_f)
                    Next
                End If
            End If
        End If

        GetTkn(ETkn.eLP)
        If CurTkn.TypeTkn <> ETkn.eRP Then
            Do While True
                by_ref = False
                param_array = False
                Select Case CurTkn.TypeTkn
                    Case ETkn.eRef
                        by_ref = True
                        GetTkn(ETkn.eRef)
                    Case ETkn.eParamArray
                        param_array = True
                        GetTkn(ETkn.eParamArray)
                End Select
                var1 = ReadVariable(stmt1)
                var1.ByRefVar = by_ref
                var1.ParamArrayVar = param_array
                stmt1.ArgumentFncStmt.Add(var1)
                If CurTkn.TypeTkn <> ETkn.eComma Then
                    Exit Do
                End If
                GetTkn(ETkn.eComma)
            Loop
        End If
        GetTkn(ETkn.eRP)

        If stmt1.TypeStmt = ETkn.eFunction OrElse stmt1.TypeStmt = ETkn.eOperator Then
            GetTkn(ETkn.eAs)
            stmt1.RetType = ReadType(False)
        End If

        If CurTkn.TypeTkn = ETkn.eImplements Then
            GetTkn(ETkn.eImplements)

            id2 = GetTkn(ETkn.eId)
            GetTkn(ETkn.eDot)
            id3 = GetTkn(ETkn.eId)

            stmt1.InterfaceFncStmt = PrjParse.GetCla(id2.StrTkn)
            Debug.Assert(stmt1.InterfaceFncStmt IsNot Nothing)
            stmt1.InterfaceFncName = id3.StrTkn
        End If

        If is_delegate Then
            PrjParse.dicGenCla.Clear()
        End If

        Return stmt1
    End Function

    ' ジェネリック型の構文解析
    Function ReadGenType(id1 As TTkn) As TCls
        Dim tp1 As TCls, tp2 As TCls
        Dim vtp As TList(Of TCls)

        GetTkn(ETkn.eLP)
        GetTkn(ETkn.eOf)

        vtp = New TList(Of TCls)()
        Do While True
            tp2 = ReadType(False)
            vtp.Add(tp2)
            If CurTkn.TypeTkn <> ETkn.eComma Then

                Exit Do
            End If
            GetTkn(ETkn.eComma)
        Loop
        GetTkn(ETkn.eRP)

        ' ジェネリック型のクラスを得る。
        tp1 = PrjParse.GetRegGenCla(id1.StrTkn, vtp)

        Return tp1
    End Function

    Function ReadType(is_new As Boolean) As TCls
        Dim tp1 As TCls
        Dim id1 As TTkn, dim_cnt As Integer

        id1 = GetTkn(ETkn.eId)
        If CurTkn.TypeTkn = ETkn.eLP AndAlso NxtTkn.TypeTkn = ETkn.eOf Then
            ' ジェネリック型の場合

            ' ジェネリック型の構文解析
            tp1 = ReadGenType(id1)
        Else

            tp1 = PrjParse.GetCla(id1.StrTkn)
            If tp1 Is Nothing Then
                Throw New TErr(String.Format("不明なクラス {0}", id1.StrTkn))
            End If
        End If
        If CurTkn.TypeTkn = ETkn.eLP AndAlso (NxtTkn.TypeTkn = ETkn.eRP OrElse NxtTkn.TypeTkn = ETkn.eComma) Then
            GetTkn(ETkn.eLP)
            dim_cnt = 1
            Do While CurTkn.TypeTkn = ETkn.eComma
                GetTkn(ETkn.eComma)
                dim_cnt += 1
            Loop
            GetTkn(ETkn.eRP)
            If Not is_new Then
                tp1 = PrjParse.GetArrCla(tp1, dim_cnt)
            End If
        End If

        Return tp1
    End Function

    Function ReadVariable(up1 As Object) As TVar
        Dim var1 As New TVar
        Dim id1 As TTkn
        Dim app1 As TApp

        id1 = GetTkn(ETkn.eId)
        var1.NameVar = id1.StrTkn

        If CurTkn.TypeTkn = ETkn.eAs Then

            GetTkn(ETkn.eAs)

            If CurTkn.TypeTkn = ETkn.eNew Then
                app1 = NewExpression()
                var1.TypeVar = app1.NewApp
                var1.InitVar = app1

                Return var1
            End If

            var1.TypeVar = ReadType(False)
        End If

        If CurTkn.TypeTkn = ETkn.eEq Then
            GetTkn(ETkn.eEq)

            var1.InitVar = AdditiveExpression()
        End If

        Return var1
    End Function

    Function ReadTailCom() As String
        Dim tkn1 As TTkn

        If CurTkn Is EOTTkn Then
            Return ""
        Else
            tkn1 = GetTkn(ETkn.eLineComment)
            Return tkn1.StrTkn
        End If
    End Function

    Function ReadDim(mod1 As TModifier) As TStmt
        Dim stmt1 As New TVarDecl
        Dim var1 As TVar

        stmt1.TypeStmt = ETkn.eVarDecl
        stmt1.ModDecl = mod1
        Do While True
            var1 = ReadVariable(stmt1)
            stmt1.VarDecl.Add(var1)
            If CurTkn.TypeTkn <> ETkn.eComma Then
                Exit Do
            End If
            GetTkn(ETkn.eComma)
        Loop

        stmt1.TailCom = ReadTailCom()

        Return stmt1
    End Function

    Function ReadReturn(type_tkn As ETkn) As TRet
        GetTkn(type_tkn)
        If CurTkn Is EOTTkn Then

            Return New TRet(Nothing, type_tkn = ETkn.eYield)
        End If

        Return New TRet(TermExpression(), type_tkn = ETkn.eYield)
    End Function

    Function ReadEnd() As TStmt
        Dim stmt1 As New TStmt

        GetTkn(ETkn.eEnd)
        Select Case CurTkn.TypeTkn
            Case ETkn.eIf
                stmt1.TypeStmt = ETkn.eEndIf
            Case ETkn.eSub
                stmt1.TypeStmt = ETkn.eEndSub
            Case ETkn.eFunction
                stmt1.TypeStmt = ETkn.eEndFunction
            Case ETkn.eOperator
                stmt1.TypeStmt = ETkn.eEndOperator
            Case ETkn.eClass
                PrjParse.dicGenCla.Clear()
                stmt1.TypeStmt = ETkn.eEndClass
            Case ETkn.eStruct
                stmt1.TypeStmt = ETkn.eEndStruct
            Case ETkn.eInterface
                stmt1.TypeStmt = ETkn.eEndInterface
            Case ETkn.eEnum
                stmt1.TypeStmt = ETkn.eEndEnum
            Case ETkn.eModule
                stmt1.TypeStmt = ETkn.eEndModule
            Case ETkn.eSelect
                stmt1.TypeStmt = ETkn.eEndSelect
            Case ETkn.eTry
                stmt1.TypeStmt = ETkn.eEndTry
            Case ETkn.eWith
                stmt1.TypeStmt = ETkn.eEndWith
            Case Else
                Chk(False)
        End Select
        GetTkn(ETkn.eUnknown)

        Return stmt1
    End Function

    Function ReadIf() As TStmt
        Dim stmt1 As New TIfStatement

        stmt1.TypeStmt = ETkn.eIf
        GetTkn(ETkn.eIf)
        stmt1.CndIfStmt = CType(TermExpression(), TTerm)
        GetTkn(ETkn.eThen)
        Return stmt1
    End Function

    Function ReadElseIf() As TStmt
        Dim stmt1 As New TElseIf

        stmt1.TypeStmt = ETkn.eElseIf
        GetTkn(ETkn.eElseIf)
        stmt1.CndElseIf = CType(TermExpression(), TTerm)
        GetTkn(ETkn.eThen)
        Return stmt1
    End Function

    Function ReadElse() As TStmt
        Dim stmt1 As New TStmt

        stmt1.TypeStmt = ETkn.eElse
        GetTkn(ETkn.eElse)
        Return stmt1
    End Function

    Function ReadDo() As TStmt
        Dim stmt1 As New TDoStmt

        stmt1.TypeStmt = ETkn.eDo
        GetTkn(ETkn.eDo)
        GetTkn(ETkn.eWhile)
        stmt1.CndDo = CType(TermExpression(), TTerm)
        Return stmt1
    End Function

    Function ReadLoop() As TStmt
        Dim stmt1 As New TStmt

        stmt1.TypeStmt = ETkn.eLoop
        GetTkn(ETkn.eLoop)

        Return stmt1
    End Function

    Function ReadSelect() As TStmt
        Dim stmt1 As New TSelectStatement

        stmt1.TypeStmt = ETkn.eSelect
        GetTkn(ETkn.eSelect)
        GetTkn(ETkn.eCase)
        stmt1.TermSelectStatement = CType(TermExpression(), TTerm)
        Return stmt1
    End Function

    Function ReadCase() As TStmt
        Dim stmt1 As New TCaseStatement
        Dim trm1 As TTerm

        stmt1.TypeStmt = ETkn.eCase
        GetTkn(ETkn.eCase)

        If CurTkn.TypeTkn = ETkn.eElse Then
            GetTkn(ETkn.eElse)
            stmt1.IsCaseElse = True
        Else
            Do While True
                trm1 = CType(TermExpression(), TTerm)
                stmt1.TermCaseStmt.Add(trm1)
                If CurTkn.TypeTkn <> ETkn.eComma Then
                    Exit Do
                End If
                GetTkn(ETkn.eComma)
            Loop
        End If

        Return stmt1
    End Function

    Function ReadFor() As TStmt
        Dim stmt1 As New TForStatement, id1 As TTkn

        stmt1.TypeStmt = ETkn.eFor
        GetTkn(ETkn.eFor)

        If CurTkn.TypeTkn = ETkn.eId Then

            id1 = GetTkn(ETkn.eId)
            stmt1.IdxForStmt = New TRef(id1)
            GetTkn(ETkn.eEq)
            stmt1.FromForStmt = CType(TermExpression(), TTerm)
            GetTkn(ETkn.eTo)
            stmt1.ToForStmt = CType(TermExpression(), TTerm)

            If CurTkn.TypeTkn = ETkn.eStep Then
                GetTkn(ETkn.eStep)
                stmt1.StepForStmt = CType(TermExpression(), TTerm)
            End If
        Else

            GetTkn(ETkn.eEach)

            If CurTkn.TypeTkn = ETkn.eId Then

                id1 = GetTkn(ETkn.eId)
                stmt1.InVarForStmt = New TVar(id1.StrTkn, Nothing)
            End If

            If CurTkn.TypeTkn = ETkn.eAt Then

                GetTkn(ETkn.eAt)

                Do While True
                    GetTkn(ETkn.eId)
                    If CurTkn.TypeTkn <> ETkn.eComma Then
                        Exit Do
                    End If
                    GetTkn(ETkn.eComma)
                Loop
            End If

            GetTkn(ETkn.eIn)

            stmt1.InTrmForStmt = CType(TermExpression(), TTerm)
        End If

        Return stmt1
    End Function

    Function ReadNext() As TStmt
        Dim stmt1 As New TStmt

        stmt1.TypeStmt = ETkn.eNext
        GetTkn(ETkn.eNext)

        Return stmt1
    End Function

    Function ReadExit() As TStmt
        Dim stmt1 As New TExit

        GetTkn(ETkn.eExit)
        Select Case CurTkn.TypeTkn
            Case ETkn.eDo
                stmt1.TypeStmt = ETkn.eExitDo
            Case ETkn.eFor
                stmt1.TypeStmt = ETkn.eExitFor
            Case ETkn.eSub
                stmt1.TypeStmt = ETkn.eExitSub
            Case Else
                Chk(False)
        End Select
        GetTkn(ETkn.eUnknown)

        Return stmt1
    End Function

    Function ReadTry() As TStmt
        Dim stmt1 As New TStmt

        stmt1.TypeStmt = ETkn.eTry
        GetTkn(ETkn.eTry)
        Return stmt1
    End Function

    Function ReadCatch() As TStmt
        Dim stmt1 As New TCatchStatement

        stmt1.TypeStmt = ETkn.eCatch
        GetTkn(ETkn.eCatch)
        stmt1.VariableCatchStmt = ReadVariable(stmt1)
        Return stmt1
    End Function

    Function ReadWith() As TStmt
        Dim stmt1 As New TWithStmt

        stmt1.TypeStmt = ETkn.eWith
        GetTkn(ETkn.eWith)
        stmt1.TermWith = TermExpression()

        Return stmt1
    End Function

    Function ReadThrow() As TStmt
        Dim stmt1 As TThrow

        GetTkn(ETkn.eThrow)
        stmt1 = New TThrow(CType(TermExpression(), TTerm))

        Return stmt1
    End Function

    Function ReadReDim() As TStmt
        Dim stmt1 As TReDim, trm1 As TTerm, app1 As TApp

        GetTkn(ETkn.eReDim)

        trm1 = TermExpression()
        Debug.Assert(trm1.IsApp())
        app1 = CType(trm1, TApp)
        Debug.Assert(app1.FncApp IsNot Nothing AndAlso app1.ArgApp.Count <> 0)
        stmt1 = New TReDim(app1.FncApp, app1.ArgApp)

        Return stmt1
    End Function

    Function ReadLineComment() As TStmt
        Dim stmt1 As New TStmt

        stmt1.TypeStmt = ETkn.eLineComment
        GetTkn(ETkn.eLineComment)
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

    Sub MakeModule(src1 As TSrc)
        Dim fnc1 As TFnc, is_module As Boolean = False, cla1 As TClassStatement, cla2 As TCls, com1 As TComment = Nothing

        CurLineIdx = 0
        Do While True
            CurStmt = GetNextStatement()
            If CurStmt Is Nothing OrElse CurStmt.TypeStmt <> ETkn.eImports Then
                Exit Do
            End If
        Loop

        is_module = (CurStmt.TypeStmt = ETkn.eModule)
        If is_module Then
            GetStatement(ETkn.eModule)
        End If
        Do While CurStmt IsNot Nothing AndAlso CurStmt.TypeStmt <> ETkn.eEndModule
            Select Case CurStmt.TypeStmt
                Case ETkn.eClass
                    cla1 = CType(CurStmt, TClassStatement)
                    cla2 = MakeClass()
                    cla2.ComVar = com1
                    cla2.SrcCla = PrjParse.CurSrc
                    com1 = Nothing
                Case ETkn.eEnum
                    cla2 = MakeEnum()
                    cla2.ComVar = com1
                    cla2.SrcCla = PrjParse.CurSrc
                    com1 = Nothing
                Case ETkn.eSub, ETkn.eFunction
                    If CType(CurStmt, TFunctionStatement).IsDelegateFncStmt Then
                        cla2 = MakeDelegate()
                        cla2.ComVar = com1
                        cla2.SrcCla = PrjParse.CurSrc
                    Else
                        fnc1 = MakeSubFnc(Nothing)
                        fnc1.ComVar = com1
                    End If
                    com1 = Nothing
                Case ETkn.eComment
                    com1 = CType(CurStmt, TComment)
                    GetStatement(ETkn.eComment)
                Case Else
                    Chk(False)
            End Select
        Loop

        If is_module Then
            GetStatement(ETkn.eEndModule)
        End If
    End Sub

    Public Function MakeEnum() As TCls
        Dim cla1 As TCls
        Dim fld1 As TFld
        Dim enum1 As TEnumStatement
        Dim ele1 As TEnumEle
        Dim type1 As TCls

        enum1 = CType(GetStatement(ETkn.eEnum), TEnumStatement)
        cla1 = PrjParse.GetCla(enum1.NameEnumStmt)
        Debug.Assert(cla1 IsNot Nothing)
        PrjParse.CurSrc.ClaSrc.Add(cla1)

        cla1.KndCla = EClass.eEnumCla
        cla1.SuperCla.Add(PrjParse.ObjectType)
        type1 = cla1

        Do While CurStmt.TypeStmt <> ETkn.eEndEnum
            ele1 = CType(GetStatement(ETkn.eId), TEnumEle)
            fld1 = New TFld(ele1.NameEnumEle, type1)
            cla1.AddFld(fld1)
        Loop
        GetStatement(ETkn.eEndEnum)

        Return cla1
    End Function

    Public Function MakeClass() As TCls
        Dim cla1 As TCls, spr_cla As TCls, vtp As TList(Of TCls)
        Dim fld1 As TFld
        Dim fnc1 As TFnc
        Dim cla_stmt As TClassStatement
        Dim var_decl As TVarDecl
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

        GetStatement(ETkn.eClass)

        If CurStmt.TypeStmt = ETkn.eInherits Then
            instmt = CType(GetStatement(ETkn.eInherits), TInheritsStatement)
            If instmt.ParamName Is Nothing Then
                spr_cla = PrjParse.GetCla(instmt.ClassNameInheritsStmt)
            Else
                vtp = New TList(Of TCls)()
                For Each s In instmt.ParamName
                    vtp.Add(PrjParse.GetCla(s))
                Next
                spr_cla = PrjParse.GetRegGenCla(instmt.ClassNameInheritsStmt, vtp)
            End If
            cla1.SuperCla.Add(spr_cla)

        End If

        If CurStmt.TypeStmt = ETkn.eImplements Then
            implstmt = CType(GetStatement(ETkn.eImplements), TImplementsStatement)

            cla1.InterfacesCls = implstmt.ClassImplementsStmt
        End If

        If PrjParse.ObjectType Is Nothing Then
            Debug.Assert(cla1.NameCla() = "Object")
            PrjParse.ObjectType = cla1
        End If
        Select Case cla1.NameCla()
            Case "System"
                PrjParse.SystemType = cla1
            Case "Array"
                PrjParse.ArrayType = cla1
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

        If PrjParse.ObjectType IsNot cla1 AndAlso cla1.SuperCla.Count = 0 Then
            cla1.SuperCla.Add(PrjParse.ObjectType)
        End If

        Do While CurStmt.TypeStmt <> ETkn.eEndClass AndAlso CurStmt.TypeStmt <> ETkn.eEndStruct AndAlso CurStmt.TypeStmt <> ETkn.eEndInterface
            If CurStmt.TypeStmt = ETkn.eVarDecl Then
                var_decl = CType(GetStatement(ETkn.eVarDecl), TVarDecl)
                ' for Add
                For Each var_f In var_decl.VarDecl
                    fld1 = New TFld(var_f.NameVar, var_f.TypeVar, var_f.InitVar)
                    fld1.ComVar = com1
                    com1 = Nothing
                    fld1.ModVar = var_decl.ModDecl
                    fld1.TailCom = var_decl.TailCom
                    cla1.AddFld(fld1)
                Next
            ElseIf CurStmt.TypeStmt = ETkn.eComment Then
                com1 = CType(GetStatement(ETkn.eComment), TComment)
            Else
                fnc1 = MakeSubFnc(cla1)
                fnc1.ComVar = com1
                com1 = Nothing
                fnc1.ClaFnc = cla1
                cla1.FncCla.Add(fnc1)
            End If
        Loop
        GetStatement(ETkn.eUnknown)

        PrjParse.dicGenCla.Clear()

        Return cla1
    End Function

    '  ブロックの構文解析をする
    Function BlcParse(up1 As Object) As TBlc
        Dim blc1 As TBlc
        Dim blc_sv As TBlc
        Dim var_decl As TVarDecl
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
        Dim if_blc As TIfBlc

        blc1 = New TBlc()

        blc_sv = CurBlc
        CurBlc = blc1

        Do While True
            Select Case CurStmt.TypeStmt
                Case ETkn.eASN, ETkn.eCall, ETkn.eReturn, ETkn.eThrow, ETkn.eExitDo, ETkn.eExitFor, ETkn.eExitSub, ETkn.eGoto, ETkn.eLabel, ETkn.eComment, ETkn.eReDim
                    CurBlc.AddStmtBlc(CurStmt)
                    GetStatement(ETkn.eUnknown)
                Case ETkn.eIf
                    if1 = CType(GetStatement(ETkn.eIf), TIfStatement)
                    if2 = New TIf()
                    CurBlc.AddStmtBlc(if2)
                    if_blc = New TIfBlc(if1.CndIfStmt, BlcParse(if2))
                    if2.IfBlc.Add(if_blc)
                    Do While True
                        Select Case CurStmt.TypeStmt
                            Case ETkn.eElseIf
                                eif1 = CType(GetStatement(ETkn.eElseIf), TElseIf)
                                if_blc = New TIfBlc(eif1.CndElseIf, BlcParse(if2))
                                if2.IfBlc.Add(if_blc)
                            Case ETkn.eElse
                                GetStatement(ETkn.eElse)
                                if_blc = New TIfBlc(Nothing, BlcParse(if2))
                                if2.IfBlc.Add(if_blc)
                                GetStatement(ETkn.eEndIf)
                                Exit Do
                            Case Else
                                GetStatement(ETkn.eEndIf)
                                Exit Do
                        End Select
                    Loop

                Case ETkn.eFor
                    for1 = CType(GetStatement(ETkn.eFor), TForStatement)
                    for2 = New TFor()

                    for2.IdxFor = for1.IdxForStmt
                    for2.FromFor = for1.FromForStmt
                    for2.ToFor = for1.ToForStmt
                    for2.StepFor = for1.StepForStmt
                    for2.InVarFor = for1.InVarForStmt
                    for2.InTrmFor = for1.InTrmForStmt

                    for2.BlcFor = BlcParse(for2)
                    CurBlc.AddStmtBlc(for2)
                    GetStatement(ETkn.eNext)

                Case ETkn.eDo
                    do1 = CType(GetStatement(ETkn.eDo), TDoStmt)
                    for2 = New TFor()
                    for2.IsDo = True
                    CurBlc.AddStmtBlc(for2)
                    for2.CndFor = do1.CndDo
                    for2.BlcFor = BlcParse(for2)
                    GetStatement(ETkn.eLoop)

                Case ETkn.eSelect
                    sel1 = CType(GetStatement(ETkn.eSelect), TSelectStatement)
                    sel2 = New TSelect()
                    sel2.TrmSel = sel1.TermSelectStatement
                    CurBlc.AddStmtBlc(sel2)
                    Do While CurStmt.TypeStmt <> ETkn.eEndSelect
                        case1 = CType(GetStatement(ETkn.eCase), TCaseStatement)
                        case2 = New TCase()
                        case2.TrmCase = case1.TermCaseStmt
                        case2.DefaultCase = case1.IsCaseElse
                        sel2.CaseSel.Add(case2)
                        case2.BlcCase = BlcParse(sel1)
                    Loop
                    GetStatement(ETkn.eEndSelect)

                Case ETkn.eTry
                    GetStatement(ETkn.eTry)
                    try1 = New TTry()
                    CurBlc.AddStmtBlc(try1)
                    try1.BlcTry = BlcParse(try1)
                    catch1 = CType(GetStatement(ETkn.eCatch), TCatchStatement)
                    try1.VarCatch = New TList(Of TVar)()
                    try1.VarCatch.Add(catch1.VariableCatchStmt)
                    try1.BlcCatch = BlcParse(try1)
                    GetStatement(ETkn.eEndTry)

                Case ETkn.eWith
                    with1 = CType(GetStatement(ETkn.eWith), TWithStmt)
                    with2 = New TWith()
                    with2.TermWith = with1.TermWith
                    CurBlc.AddStmtBlc(with2)
                    with2.BlcWith = BlcParse(with2)
                    GetStatement(ETkn.eEndWith)

                Case ETkn.eVarDecl
                    var_decl = CType(GetStatement(ETkn.eVarDecl), TVarDecl)
                    CurBlc.AddStmtBlc(var_decl)
                    ' for Add
                    For Each var1 In var_decl.VarDecl
                        CurBlc.VarBlc.Add(var1)
                    Next

                Case ETkn.eExit
                    Chk(False)
                Case Else
                    Exit Do
            End Select
        Loop

        CurBlc = blc_sv

        Return blc1
    End Function

    Public Function MakeDelegate() As TCls
        Dim stmt1 As TFunctionStatement
        Dim dlg1 As TDelegate
        Dim fnc1 As TFnc

        stmt1 = CType(GetStatement(ETkn.eUnknown), TFunctionStatement)
        dlg1 = PrjParse.GetDelegate(stmt1.NameFncStmt)

        dlg1.KndCla = EClass.eDelegateCla
        dlg1.RetDlg = stmt1.RetType
        dlg1.ArgDlg = stmt1.ArgumentFncStmt
        fnc1 = New TFnc("Invoke", stmt1.RetType)
        fnc1.SetModFnc(stmt1.ModifierFncStmt)
        fnc1.ArgFnc = stmt1.ArgumentFncStmt
        fnc1.ThisFnc = New TVar("Me", dlg1)
        fnc1.ClaFnc = dlg1
        dlg1.FncCla.Add(fnc1)

        PrjParse.CurSrc.ClaSrc.Add(dlg1)

        Return dlg1
    End Function

    Public Function MakeSubFnc(cla1 As TCls) As TFnc
        Dim fnc1 As TFunctionStatement
        Dim fnc2 As TFnc, fnc_name As String

        Chk(CurStmt.TypeStmt = ETkn.eSub OrElse CurStmt.TypeStmt = ETkn.eFunction OrElse CurStmt.TypeStmt = ETkn.eNew OrElse CurStmt.TypeStmt = ETkn.eOperator)
        fnc1 = CType(GetStatement(ETkn.eUnknown), TFunctionStatement)
        If fnc1.TypeStmt = ETkn.eNew Then
            fnc_name = "New@" + cla1.NameCla()
        Else
            fnc_name = fnc1.NameFncStmt
        End If
        fnc2 = New TFnc(fnc_name, fnc1.RetType)
        fnc2.SetModFnc(fnc1.ModifierFncStmt)
        fnc2.TypeFnc = fnc1.TypeStmt
        fnc2.OpFnc = fnc1.OpFncStmt
        fnc2.ArgFnc.AddRange(fnc1.ArgumentFncStmt)
        fnc2.ThisFnc = New TVar("Me", cla1)
        fnc2.InterfaceFnc = fnc1.InterfaceFncStmt
        fnc2.ImplFnc = New TRef(fnc1.InterfaceFncName)
        fnc2.IsNew = (fnc1.TypeStmt = ETkn.eNew)

        If fnc2.ModFnc().isMustOverride Then
            Return fnc2
        End If

        If cla1 Is Nothing OrElse cla1.KndCla <> EClass.eInterfaceCla Then
            ' インターフェイスでない場合

            fnc2.BlcFnc = BlcParse(fnc2)
            Chk(CurStmt.TypeStmt = ETkn.eEndSub OrElse CurStmt.TypeStmt = ETkn.eEndFunction OrElse CurStmt.TypeStmt = ETkn.eEndOperator)
            GetStatement(ETkn.eUnknown)
        End If

        Return fnc2
    End Function

    Public Sub ReadAllStatement()
        Dim i1 As Integer, is_err As Boolean = False
        Dim com1 As TComment, stmt1 As TStmt, cla1 As TCls

        ' 文の配列を初期化する
        PrjParse.CurSrc.StmtSrc = New TList(Of TStmt)()
        ' for Add
        For i1 = 0 To PrjParse.CurSrc.vTextSrc.Length - 1
            PrjParse.CurSrc.StmtSrc.Add(Nothing)
        Next

        com1 = New TComment()
        ' for ???
        For i1 = 0 To PrjParse.CurSrc.vTextSrc.Length - 1
            CurLineStr = PrjParse.CurSrc.vTextSrc(i1)
            CurVTkn = PrjParse.CurSrc.LineTkn(i1)

            If CurVTkn.Count = 0 Then
                '  空行の場合

                com1.LineCom.Add("")
            ElseIf CurVTkn(0).TypeTkn = ETkn.eLineComment Then
                '  コメントの場合

                com1.LineCom.Add(CurVTkn(0).StrTkn)
            Else
                '  空行やコメントでない場合

                If com1.LineCom.Count <> 0 Then
                    ' コメント・空行がある場合

                    ' 前の行にコメント文を入れる
                    PrjParse.CurSrc.StmtSrc(i1 - 1) = com1

                    com1 = New TComment()
                End If

                Try
                    stmt1 = ReadStatement()

                    PrjParse.CurSrc.StmtSrc(i1) = stmt1

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

                    ElseIf stmt1.TypeStmt = ETkn.eEndClass Then
                        ' クラス定義の終わりの場合

                        PrjParse.dicGenCla.Clear()
                    End If
                Catch ex As TErr
                    is_err = True
                End Try
            End If
        Next

        Debug.Assert(Not is_err)
    End Sub

    Public Function ReadModifier() As TModifier
        Dim mod1 As TModifier

        mod1 = New TModifier()
        mod1.ValidMod = False
        Do While True
            Select Case CurTkn.TypeTkn
                Case ETkn.ePartial
                    mod1.isPartial = True
                Case ETkn.ePublic
                    mod1.isPublic = True
                Case ETkn.eShared
                    mod1.isShared = True
                Case ETkn.eConst
                    mod1.isConst = True
                Case ETkn.eAbstract
                    mod1.isAbstract = True
                Case ETkn.eVirtual
                    mod1.isVirtual = True
                Case ETkn.eMustOverride
                    mod1.isMustOverride = True
                Case ETkn.eOverride
                    mod1.isOverride = True
                Case ETkn.eIterator
                    mod1.isIterator = True
                Case ETkn.eProtected, ETkn.eFriend, ETkn.ePrivate
                Case Else
                    Exit Do
            End Select
            GetTkn(ETkn.eUnknown)
            mod1.ValidMod = True
        Loop

        Return mod1
    End Function

    Function ReadStatement() As TStmt
        Dim mod1 As TModifier, stmt1 As TStmt


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

        If mod1.ValidMod AndAlso CurTkn.TypeTkn = ETkn.eId Then
            '  変数宣言の場合

            stmt1 = ReadDim(mod1)
        Else

            Select Case CurTkn.TypeTkn
                Case ETkn.eImports
                    stmt1 = ReadImports()

                Case ETkn.eModule
                    stmt1 = ReadModule()

                Case ETkn.eDelegate
                    GetTkn(ETkn.eDelegate)
                    Debug.Assert(CurTkn.TypeTkn = ETkn.eFunction OrElse CurTkn.TypeTkn = ETkn.eSub)
                    stmt1 = ReadSubFunction(mod1, True)

                Case ETkn.eSub, ETkn.eFunction, ETkn.eOperator
                    stmt1 = ReadSubFunction(mod1, False)

                Case ETkn.eEnd
                    stmt1 = ReadEnd()

                Case ETkn.eDim
                    GetTkn(ETkn.eDim)
                    stmt1 = ReadDim(mod1)

                Case ETkn.eIf
                    stmt1 = ReadIf()

                Case ETkn.eElse
                    stmt1 = ReadElse()

                Case ETkn.eReturn, ETkn.eYield
                    stmt1 = ReadReturn(CurTkn.TypeTkn)

                Case ETkn.eDo
                    stmt1 = ReadDo()

                Case ETkn.eLoop
                    stmt1 = ReadLoop()

                Case ETkn.eSelect
                    stmt1 = ReadSelect()

                Case ETkn.eCase
                    stmt1 = ReadCase()

                Case ETkn.eFor
                    stmt1 = ReadFor()

                Case ETkn.eNext
                    stmt1 = ReadNext()

                Case ETkn.eElseIf
                    stmt1 = ReadElseIf()

                Case ETkn.eEnum
                    stmt1 = ReadEnum()

                Case ETkn.eClass, ETkn.eStruct, ETkn.eInterface
                    stmt1 = ReadClass(mod1)

                Case ETkn.eInherits
                    stmt1 = ReadInherits()

                Case ETkn.eImplements
                    stmt1 = ReadImplements()

                Case ETkn.eExit
                    stmt1 = ReadExit()

                Case ETkn.eId, ETkn.eBase, ETkn.eCType, ETkn.eDot
                    stmt1 = AssignmentExpression()

                Case ETkn.eTry
                    stmt1 = ReadTry()

                Case ETkn.eCatch
                    stmt1 = ReadCatch()

                Case ETkn.eWith
                    stmt1 = ReadWith()

                Case ETkn.eThrow
                    stmt1 = ReadThrow()

                Case ETkn.eReDim
                    stmt1 = ReadReDim()

                Case ETkn.eLineComment
                    stmt1 = ReadLineComment()

                Case ETkn.eEOT
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

    Function GetNextStatement() As TStmt
        Dim stmt1 As TStmt

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

    Function GetStatement(type1 As ETkn) As TStmt
        Dim stmt1 As TStmt

        Chk(type1 = ETkn.eUnknown OrElse CurStmt IsNot Nothing)

        Do While type1 <> ETkn.eUnknown AndAlso type1 <> ETkn.eComment AndAlso CurStmt.TypeStmt = ETkn.eComment
            CurStmt = GetNextStatement()
            Debug.Assert(CurStmt IsNot Nothing)
        Loop

        If type1 = ETkn.eUnknown OrElse type1 = CurStmt.TypeStmt Then

            stmt1 = CurStmt
            Do While True
                CurStmt = GetNextStatement()
                If CurStmt Is Nothing OrElse CurStmt.TypeStmt <> ETkn.eImports Then
                    Exit Do
                End If
            Loop

            Return stmt1
        Else
            Chk(False)
            Return Nothing
        End If
    End Function

    Public Sub ParseAllLines(src1 As TSrc)
        If src1.vTextSrc Is Nothing Then
            src1.vTextSrc = TFile.ReadAllLines(PrjParse.SrcDir, src1.FileSrc)
        End If

        src1.LineTkn = New TList(Of TList(Of TTkn))()
        ' for Add
        For Each line1 In src1.vTextSrc
            src1.LineTkn.Add(Lex(line1))
        Next
    End Sub

    Public Sub RegAllClass(src1 As TSrc)
        Dim id1 As TTkn, k1 As Integer, cla1 As TCls, cla2 As TCls, id2 As TTkn

        For Each v In src1.LineTkn

            If 3 <= v.Count AndAlso v(0).TypeTkn = ETkn.ePublic Then

                If v(1).TypeTkn = ETkn.eDelegate Then
                    Debug.Assert(v(2).TypeTkn = ETkn.eSub OrElse v(2).TypeTkn = ETkn.eFunction)
                    id1 = v(3)
                    cla1 = PrjParse.RegDelegate(id1.StrTkn)
                Else
                    Select Case v(1).TypeTkn
                        Case ETkn.eClass, ETkn.eStruct, ETkn.eInterface, ETkn.eEnum
                            k1 = 2
                        Case ETkn.eAbstract
                            Select Case v(2).TypeTkn
                                Case ETkn.eClass, ETkn.eStruct, ETkn.eInterface
                                    k1 = 3
                                Case Else
                                    Debug.Assert(False)
                            End Select
                        Case Else
                            k1 = -1
                    End Select

                    If k1 <> -1 Then
                        id1 = v(k1)
                        cla1 = PrjParse.RegCla(id1.StrTkn)

                        If k1 + 1 < v.Count Then
                            Debug.Assert(v(k1 + 1).TypeTkn = ETkn.eLP)
                            Debug.Assert(v(k1 + 2).TypeTkn = ETkn.eOf)

                            cla1.GenCla = New TList(Of TCls)()

                            k1 += 3
                            Do While k1 < v.Count
                                id2 = v(k1)

                                cla2 = New TCls(id2.StrTkn)
                                cla2.IsParamCla = True
                                cla1.GenCla.Add(cla2)

                                If v(k1 + 1).TypeTkn = ETkn.eRP Then
                                    Debug.Assert(k1 + 2 = v.Count)
                                    Exit Do
                                End If

                                Debug.Assert(v(k1 + 1).TypeTkn = ETkn.eComma)
                                k1 += 2
                            Loop

                            PrjParse.RegGenCla(cla1.NameCla(), cla1.GenCla)
                        End If

                    End If
                End If
            End If
        Next
    End Sub

    Public Sub Parse(src1 As TSrc)
        MakeModule(src1)
    End Sub

    Public Sub RegTkn()
        Dim dic1 As New Dictionary(Of String, ETkn)

        EOTTkn = NewToken(ETkn.eEOT, "", 0)

        dic1.Add("Imports", ETkn.eImports)
        dic1.Add("Module", ETkn.eModule)
        dic1.Add("OrElse", ETkn.eOR)
        dic1.Add("AndAlso", ETkn.eAnd)
        dic1.Add("Not", ETkn.eNot)
        dic1.Add("<>", ETkn.eNE)
        dic1.Add("MustInherit", ETkn.eAbstract)
        dic1.Add("MustOverride", ETkn.eMustOverride)
        dic1.Add("AddressOf", ETkn.eAddressOf)
        dic1.Add("Aggregate", ETkn.eAggregate)
        dic1.Add("As", ETkn.eAs)
        dic1.Add("At", ETkn.eAt)
        dic1.Add("MyBase", ETkn.eBase)
        dic1.Add("Break", ETkn.eBreak)
        dic1.Add("Byval", ETkn.eByVal)
        dic1.Add("Call", ETkn.eCall)
        dic1.Add("Case", ETkn.eCase)
        dic1.Add("Catch", ETkn.eCatch)
        dic1.Add("Class", ETkn.eClass)
        dic1.Add("Const", ETkn.eConst)
        dic1.Add("CType", ETkn.eCType)
        dic1.Add("Default", ETkn.eDefault)
        dic1.Add("Delegate", ETkn.eDelegate)
        dic1.Add("Dim", ETkn.eDim)
        dic1.Add("Do", ETkn.eDo)
        dic1.Add("Each", ETkn.eEach)
        dic1.Add("Else", ETkn.eElse)
        dic1.Add("Elseif", ETkn.eElseIf)
        dic1.Add("End", ETkn.eEnd)
        dic1.Add("Endif", ETkn.eEndIf)
        dic1.Add("Enum", ETkn.eEnum)
        dic1.Add("Exit", ETkn.eExit)
        dic1.Add("Extends", ETkn.eExtends)
        dic1.Add("For", ETkn.eFor)
        dic1.Add("Foreach", ETkn.eForeach)
        dic1.Add("From", ETkn.eFrom)
        dic1.Add("Function", ETkn.eFunction)
        dic1.Add("Get", ETkn.eGet)
        dic1.Add("GetType", ETkn.eGetType)
        dic1.Add("GoTo", ETkn.eGoto)
        dic1.Add("Handles", ETkn.eHandles)
        dic1.Add("If", ETkn.eIf)
        '		dic1.Add("IIf", ETkn.eIIF)
        dic1.Add("Implements", ETkn.eImplements)
        dic1.Add("In", ETkn.eIn)
        dic1.Add("Inherits", ETkn.eInherits)
        dic1.Add("Interface", ETkn.eInterface)
        dic1.Add("Into", ETkn.eInto)
        dic1.Add("Is", ETkn.eIs)
        dic1.Add("IsNot", ETkn.eIsNot)
        dic1.Add("Iterator", ETkn.eIterator)
        dic1.Add("Loop", ETkn.eLoop)
        dic1.Add("Namespace", ETkn.eNamespace)
        dic1.Add("New", ETkn.eNew)
        dic1.Add("Next", ETkn.eNext)
        dic1.Add("Of", ETkn.eOf)
        dic1.Add("Operator", ETkn.eOperator)
        dic1.Add("Out", ETkn.eOut)
        dic1.Add("Overrides", ETkn.eOverride)
        dic1.Add("ParamArray", ETkn.eParamArray)
        dic1.Add("Partial", ETkn.ePartial)
        dic1.Add("Public", ETkn.ePublic)
        dic1.Add("Protected", ETkn.eProtected)
        dic1.Add("Friend", ETkn.eFriend)
        dic1.Add("Private", ETkn.ePrivate)
        dic1.Add("ByRef", ETkn.eRef)
        dic1.Add("ReDim", ETkn.eReDim)
        dic1.Add("Return", ETkn.eReturn)
        dic1.Add("Set", ETkn.eSet)
        dic1.Add("Select", ETkn.eSelect)
        dic1.Add("Shared", ETkn.eShared)
        dic1.Add("Step", ETkn.eStep)
        dic1.Add("Structure", ETkn.eStruct)
        dic1.Add("Sub", ETkn.eSub)
        dic1.Add("Take", ETkn.eTake)
        dic1.Add("Then", ETkn.eThen)
        dic1.Add("Throw", ETkn.eThrow)
        dic1.Add("To", ETkn.eTo)
        dic1.Add("Try", ETkn.eTry)
        dic1.Add("TypeOf", ETkn.eTypeof)
        dic1.Add("Var", ETkn.eVar)
        dic1.Add("Overridable", ETkn.eVirtual)
        dic1.Add("Where", ETkn.eWhere)
        dic1.Add("While", ETkn.eWhile)
        dic1.Add("With", ETkn.eWith)
        dic1.Add("Yield", ETkn.eYield)
        dic1.Add("@id", ETkn.eId)
        dic1.Add("@int", ETkn.eInt)
        dic1.Add("@hex", ETkn.eHex)
        dic1.Add("/*", ETkn.eBlockComment)
        dic1.Add("'", ETkn.eLineComment)
        dic1.Add("=", ETkn.eEq)
        dic1.Add("+=", ETkn.eADDEQ)
        dic1.Add("-=", ETkn.eSUBEQ)
        dic1.Add("*=", ETkn.eMULEQ)
        dic1.Add("/=", ETkn.eDIVEQ)
        dic1.Add("%=", ETkn.eMODEQ)
        dic1.Add("+", ETkn.eADD)
        dic1.Add("-", ETkn.eMns)
        dic1.Add("Mod", ETkn.eMOD)
        dic1.Add("And", ETkn.eAnp)
        dic1.Add("(", ETkn.eLP)
        dic1.Add(")", ETkn.eRP)
        dic1.Add("*", ETkn.eMUL)
        dic1.Add(",", ETkn.eComma)
        dic1.Add(".", ETkn.eDot)
        dic1.Add("/", ETkn.eDIV)
        dic1.Add(":", ETkn.eMMB)
        dic1.Add("", ETkn.eSM)
        dic1.Add("?", ETkn.eQUE)
        dic1.Add("[", ETkn.eLB)
        dic1.Add("]", ETkn.eRB)
        dic1.Add("^", ETkn.eHAT)
        dic1.Add("{", ETkn.eLC)
        dic1.Add("|", ETkn.eVLine)
        dic1.Add("}", ETkn.eRC)
        dic1.Add("~", ETkn.eTilde)
        dic1.Add("<", ETkn.eLT)
        dic1.Add(">", ETkn.eGT)
        dic1.Add("<=", ETkn.eLE)
        dic1.Add(">=", ETkn.eGE)

        ' for Add
        For Each key1 In dic1.Keys
            vTkn.Add(key1.ToLower(), dic1(key1))
        Next

        If vTknName Is Nothing Then
            vTknName = New Dictionary(Of ETkn, String)()
            ' for Add
            For Each key1 In dic1.Keys
                vTknName.Add(dic1(key1), key1)
            Next
            vTknName.Add(ETkn.eASN, "=")
            vTknName.Add(ETkn.eExitFor, "Exit For")
            vTknName.Add(ETkn.eExitDo, "Exit Do")
        End If
        PrjParse.vTknNamePrj = vTknName
    End Sub

    Function NewToken(type1 As ETkn, str1 As String, pos1 As Integer) As TTkn
        Dim tkn1 As New TTkn

        tkn1.TypeTkn = type1
        tkn1.StrTkn = str1
        tkn1.PosTkn = pos1

        Return tkn1
    End Function

    Public Function Lex(src_text As String) As TList(Of TTkn)
        Dim v1 As TList(Of TTkn)
        Dim cur1 As Integer, spc As Integer
        Dim src_len As Integer
        Dim k1 As Integer
        Dim ch1 As Char
        Dim ch2 As Char
        Dim str1 As String = Nothing
        Dim type1 As ETkn
        Dim prv_type As ETkn
        Dim tkn1 As TTkn
        Dim sb1 As TStringWriter
        Dim ok As Boolean

        src_len = src_text.Length
        v1 = New TList(Of TTkn)()

        cur1 = 0
        prv_type = ETkn.eUnknown

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

                                    tkn1 = New TTkn(ETkn.eChar, sb1.ToString(), cur1)
                                    cur1 = k1 + 2
                                Else
                                    '  文字列の場合

                                    tkn1 = NewToken(ETkn.eString, sb1.ToString(), cur1)
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
                    tkn1 = NewToken(ETkn.eLineComment, src_text.Substring(cur1 + 1), cur1)
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
                        tkn1 = NewToken(ETkn.eInt, str1, cur1)

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
                        tkn1 = NewToken(ETkn.eHex, str1, cur1)

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
                            If type1 = ETkn.eGetType AndAlso (prv_type = ETkn.eDot OrElse prv_type = ETkn.eFunction) Then
                                type1 = ETkn.eId
                            ElseIf type1 = ETkn.eSelect AndAlso prv_type = ETkn.eDot Then
                                type1 = ETkn.eId
                            End If
                        Else
                            '  識別子の場合

                            type1 = ETkn.eId
                        End If
                        tkn1 = NewToken(type1, str1, cur1)

                        cur1 = k1
                    Else
                        '  記号の場合

                        ok = False
                        type1 = ETkn.eUnknown
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

    Function ArgumentExpressionList(app1 As TApp) As TApp
        Dim trm1 As TTerm

        ' 			bool	b_of;
        '             b_of = false;
        GetTkn(ETkn.eLP)
        If CurTkn.TypeTkn = ETkn.eOf Then
            GetTkn(ETkn.eOf)
        End If
        '                 b_of = true;
        If CurTkn.TypeTkn <> ETkn.eRP Then
            Do While True
                trm1 = TermExpression()
                app1.AddInArg(trm1)

                If CurTkn.TypeTkn <> ETkn.eComma Then
                    Exit Do
                End If
                GetTkn(ETkn.eComma)
            Loop
        End If
        GetTkn(ETkn.eRP)

        Return app1
    End Function

    Function CallExpression(trm1 As TTerm) As TTerm
        Dim app1 As TApp

        Do While CurTkn.TypeTkn = ETkn.eLP
            app1 = TApp.MakeAppCall(trm1)
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
        GetTkn(ETkn.eLC)
        If CurTkn.TypeTkn <> ETkn.eRC Then
            Do While True
                trm1 = TermExpression()
                arr1.TrmArr.Add(trm1)
                If CurTkn.TypeTkn = ETkn.eRC Then
                    Exit Do
                End If
                GetTkn(ETkn.eComma)
            Loop
        End If
        GetTkn(ETkn.eRC)

        Return arr1
    End Function


    Function NewExpression() As TApp
        Dim tkn1 As TTkn
        Dim type1 As TCls
        Dim app1 As TApp

        tkn1 = GetTkn(ETkn.eNew)
        type1 = ReadType(True)
        app1 = TApp.MakeAppNew(type1)
        If CurTkn.TypeTkn = ETkn.eLP Then
            ArgumentExpressionList(app1)
        End If
        If CurTkn.TypeTkn = ETkn.eLC Then
            ' 配列の場合
            app1.IniApp = ArrayExpression()

            ' 配列型に変える
            app1.NewApp = PrjParse.GetArrCla(app1.NewApp, 1)
        End If
        If CurTkn.TypeTkn = ETkn.eFrom Then
            GetTkn(ETkn.eFrom)

            Debug.Assert(CurTkn.TypeTkn = ETkn.eLC)
            app1.IniApp = ArrayExpression()
        End If

        Return app1
    End Function

    ' From i In v1 Where i Mod 2 = 0 Select AA(i)
    Function FromExpression() As TFrom
        Dim stmt1 As New TFrom
        Dim id1 As TTkn, seq1 As TTerm, cnd1 As TTerm = Nothing, sel1 As TTerm = Nothing, take1 As TTerm = Nothing

        GetTkn(ETkn.eFrom)
        id1 = GetTkn(ETkn.eId)

        GetTkn(ETkn.eIn)
        seq1 = TermExpression()

        If CurTkn.TypeTkn = ETkn.eWhere Then

            GetTkn(ETkn.eWhere)
            cnd1 = TermExpression()
        End If

        If CurTkn.TypeTkn = ETkn.eSelect Then

            GetTkn(ETkn.eSelect)
            sel1 = TermExpression()
        End If

        If CurTkn.TypeTkn = ETkn.eTake Then

            GetTkn(ETkn.eTake)
            take1 = TermExpression()
        End If

        stmt1.VarFrom = New TVar(id1, Nothing)
        stmt1.SeqFrom = seq1
        stmt1.CndFrom = cnd1
        stmt1.SelFrom = sel1
        stmt1.TakeFrom = take1

        Return stmt1
    End Function

    ' Aggregate x In v Into Sum(x.Value)
    Function AggregateExpression() As TAggregate
        Dim aggr1 As New TAggregate, id1 As TTkn, id2 As TTkn

        GetTkn(ETkn.eAggregate)
        id1 = GetTkn(ETkn.eId)
        aggr1.VarAggr = New TVar(id1, Nothing)

        GetTkn(ETkn.eIn)
        aggr1.SeqAggr = TermExpression()

        GetTkn(ETkn.eInto)

        id2 = GetTkn(ETkn.eId)
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


        GetTkn(ETkn.eLP)

        aggr1.IntoAggr = TermExpression()

        GetTkn(ETkn.eRP)

        Return aggr1
    End Function

    Function PrimaryExpression() As TTerm
        Dim ref1 As TRef
        Dim trm1 As TTerm, trm2 As TTerm
        Dim ret1 As TTerm
        Dim id1 As TTkn, tkn1 As TTkn
        Dim type1 As TCls
        Dim app1 As TApp

        Select Case CurTkn.TypeTkn
            Case ETkn.eId
                id1 = GetTkn(ETkn.eId)
                If CurTkn.TypeTkn = ETkn.eLP AndAlso NxtTkn.TypeTkn = ETkn.eOf Then
                    ' ジェネリック型の場合

                    ' ジェネリック型の構文解析
                    type1 = ReadGenType(id1)
                    ref1 = New TRef(type1)
                    Return ref1
                End If

                ref1 = New TRef(id1)
                Return CallExpression(ref1)

            Case ETkn.eDot
                trm1 = Nothing
                Do While CurTkn.TypeTkn = ETkn.eDot
                    GetTkn(ETkn.eDot)
                    id1 = GetTkn(ETkn.eId)
                    trm2 = New TDot(trm1, id1.StrTkn)
                    trm1 = CallExpression(trm2)
                Loop

                Return trm1

            Case ETkn.eBase
                GetTkn(ETkn.eBase)
                GetTkn(ETkn.eDot)
                Debug.Assert(CurTkn.TypeTkn = ETkn.eNew OrElse CurTkn.TypeTkn = ETkn.eId)
                tkn1 = GetTkn(ETkn.eUnknown)
                app1 = TApp.MakeAppBase(tkn1)
                ArgumentExpressionList(app1)
                Return app1

            Case ETkn.eLP
                GetTkn(ETkn.eLP)
                trm1 = New TPar(TermExpression())
                GetTkn(ETkn.eRP)
                ret1 = CallExpression(trm1)

            Case ETkn.eLC
                Return ArrayExpression()

            Case ETkn.eString, ETkn.eChar, ETkn.eInt, ETkn.eHex
                tkn1 = GetTkn(ETkn.eUnknown)
                ret1 = New TCns(tkn1.TypeTkn, tkn1.StrTkn)

            Case ETkn.eNew
                Return NewExpression()

            Case ETkn.eTypeof
                GetTkn(ETkn.eTypeof)
                trm1 = AdditiveExpression()
                GetTkn(ETkn.eIs)
                type1 = ReadType(False)
                Return TApp.NewTypeOf(trm1, type1)

            Case ETkn.eGetType
                GetTkn(ETkn.eGetType)
                GetTkn(ETkn.eLP)
                type1 = ReadType(False)
                GetTkn(ETkn.eRP)
                Return TApp.MakeAppGetType(type1)

            Case ETkn.eCType
                GetTkn(ETkn.eCType)
                GetTkn(ETkn.eLP)
                trm1 = AdditiveExpression()
                GetTkn(ETkn.eComma)
                type1 = ReadType(False)
                GetTkn(ETkn.eRP)

                app1 = TApp.MakeAppCastClass(type1)
                app1.AddInArg(trm1)
                Return app1

            Case ETkn.eAddressOf
                GetTkn(ETkn.eAddressOf)
                trm1 = TermExpression()
                app1 = TApp.MakeAppAddressOf(trm1)
                Return app1

            Case ETkn.eFrom
                Return FromExpression()

            Case ETkn.eAggregate
                Return AggregateExpression()

            Case Else
                Chk(False)
                Return Nothing
        End Select

        Return ret1
    End Function

    Function DotExpression() As TTerm
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim id1 As TTkn

        trm1 = PrimaryExpression()

        Do While CurTkn.TypeTkn = ETkn.eDot
            GetTkn(ETkn.eDot)
            id1 = GetTkn(ETkn.eId)
            trm2 = New TDot(trm1, id1.StrTkn)
            trm1 = CallExpression(trm2)
        Loop

        Return trm1
    End Function

    Function UnaryExpression() As TTerm
        Dim tkn1 As TTkn
        Dim trm1 As TTerm

        If CurTkn.TypeTkn = ETkn.eMns Then
            tkn1 = GetTkn(ETkn.eMns)
            trm1 = DotExpression()

            Return TApp.MakeApp1Opr(tkn1, trm1)
        End If

        Return DotExpression()
    End Function

    Function MultiplicativeExpression() As TTerm
        Dim trm1 As TTerm
        Dim tkn1 As TTkn
        Dim trm2 As TTerm

        trm1 = UnaryExpression()
        If CurTkn.TypeTkn = ETkn.eMUL OrElse CurTkn.TypeTkn = ETkn.eDIV OrElse CurTkn.TypeTkn = ETkn.eMOD Then
            tkn1 = GetTkn(ETkn.eUnknown)
            trm2 = MultiplicativeExpression()

            Return TApp.MakeApp2Opr(tkn1, trm1, trm2)
        End If

        Return trm1
    End Function

    Public Function AdditiveExpression() As TTerm
        Dim trm1 As TTerm
        Dim tkn1 As TTkn
        Dim trm2 As TTerm

        trm1 = MultiplicativeExpression()
        If CurTkn.TypeTkn = ETkn.eADD OrElse CurTkn.TypeTkn = ETkn.eMns Then
            tkn1 = GetTkn(ETkn.eUnknown)
            trm2 = AdditiveExpression()

            Return TApp.MakeApp2Opr(tkn1, trm1, trm2)
        End If
        Return trm1
    End Function

    Public Function RelationalExpression() As TTerm
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim type1 As ETkn
        '      Dim type2 As TCls
        'Dim par1 As Boolean

        trm1 = AdditiveExpression()
        Select Case CurTkn.TypeTkn
            Case ETkn.eEq, ETkn.eADDEQ, ETkn.eSUBEQ, ETkn.eMULEQ, ETkn.eDIVEQ, ETkn.eMODEQ, ETkn.eNE, ETkn.eLT, ETkn.eGT, ETkn.eLE, ETkn.eGE, ETkn.eIs, ETkn.eIsNot
                type1 = CurTkn.TypeTkn
                GetTkn(ETkn.eUnknown)
                trm2 = AdditiveExpression()
                Return TApp.NewOpr2(type1, trm1, trm2)

            Case Else
                Return trm1
        End Select
    End Function

    Function NotExpression() As TTerm
        Dim trm1 As TTerm
        Dim type1 As ETkn
        Dim opr1 As TApp
        Dim app1 As TApp

        If CurTkn.TypeTkn = ETkn.eNot Then
            type1 = CurTkn.TypeTkn
            GetTkn(type1)
            opr1 = TApp.NewOpr(type1)
            trm1 = NotExpression()
            Debug.Assert(TypeOf trm1 Is TApp OrElse TypeOf trm1 Is TRef OrElse TypeOf trm1 Is TPar)
            If TypeOf trm1 Is TApp Then
                app1 = CType(trm1, TApp)
                app1.Negation = Not app1.Negation
                Return app1
            Else
                opr1.AddInArg(trm1)
                Return opr1
            End If
        End If

        Return RelationalExpression()
    End Function

    Function AndExpression() As TTerm
        Dim trm1 As TTerm
        Dim opr1 As TApp
        Dim type1 As ETkn

        trm1 = NotExpression()
        If CurTkn.TypeTkn = ETkn.eAnd OrElse CurTkn.TypeTkn = ETkn.eAnp Then

            type1 = CurTkn.TypeTkn
            opr1 = TApp.NewOpr(type1)
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
        Dim opr1 As TApp
        Dim type1 As ETkn

        trm1 = AndExpression()
        If CurTkn.TypeTkn = ETkn.eOR Then

            type1 = CurTkn.TypeTkn
            opr1 = TApp.NewOpr(type1)
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

    Public Function AssignmentExpression() As TStmt
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim rel1 As TApp
        Dim asn1 As TAsn
        Dim eq1 As TTkn

        trm1 = CType(AdditiveExpression(), TTerm)

        Select Case CurTkn.TypeTkn
            Case ETkn.eEq, ETkn.eADDEQ, ETkn.eSUBEQ, ETkn.eMULEQ, ETkn.eDIVEQ, ETkn.eMODEQ
                eq1 = GetTkn(ETkn.eUnknown)
                trm2 = CType(TermExpression(), TTerm)
                rel1 = TApp.NewOpr2(eq1.TypeTkn, trm1, trm2)
                asn1 = New TAsn(rel1)
                Return asn1
        End Select

        If TypeOf trm1 Is TRef Then
            Return New TEnumEle(CType(trm1, TRef))
        End If
        Return New TCall(CType(trm1, TApp))
    End Function

End Class

Public Class TClassStatement
    Inherits TStmt
    Public KndClaStmt As EClass = EClass.eClassCla
    Public ClaClaStmt As TCls
End Class

Public Class TInheritsStatement
    Inherits TStmt
    Public ClassNameInheritsStmt As String
    Public ParamName As TList(Of String)
End Class

Public Class TImplementsStatement
    Inherits TStmt
    Public ClassImplementsStmt As New TList(Of TCls)
End Class

Public Class TEnumStatement
    Inherits TStmt
    Public NameEnumStmt As String
End Class

Public Class TFunctionStatement
    Inherits TStmt
    Public ModifierFncStmt As TModifier
    Public OpFncStmt As ETkn = ETkn.eUnknown
    Public NameFncStmt As String
    Public ArgumentFncStmt As New TList(Of TVar)
    Public RetType As TCls
    Public InterfaceFncStmt As TCls
    Public InterfaceFncName As String
    Public IsDelegateFncStmt As Boolean
End Class

Public Class TIfStatement
    Inherits TStmt
    Public CndIfStmt As TTerm
End Class

Public Class TSelectStatement
    Inherits TStmt
    Public TermSelectStatement As TTerm
End Class

Public Class TCaseStatement
    Inherits TStmt
    Public IsCaseElse As Boolean
    Public TermCaseStmt As New TList(Of TTerm)
End Class

Public Class TCatchStatement
    Inherits TStmt
    Public VariableCatchStmt As TVar
End Class

Public Class TForStatement
    Inherits TStmt
    Public IdxForStmt As TRef
    Public FromForStmt As TTerm
    Public ToForStmt As TTerm
    Public StepForStmt As TTerm
    Public InVarForStmt As TVar
    Public InTrmForStmt As TTerm
End Class

Public Class TExit
    Inherits TStmt
    Public LabelExit As Integer

    Public Sub New()
    End Sub
End Class

Public Class TThrow
    Inherits TStmt
    Public TrmThrow As TTerm
    Public Sub New(trm1 As TTerm)
        TypeStmt = ETkn.eThrow
        TrmThrow = trm1
    End Sub

End Class

Public Class TReDim
    Inherits TStmt
    Public TrmReDim As TTerm
    Public DimReDim As TList(Of TTerm)

    Public Sub New(trm1 As TTerm, vtrm1 As TList(Of TTerm))
        TypeStmt = ETkn.eReDim
        TrmReDim = trm1
        DimReDim = vtrm1
    End Sub
End Class

Public Class TImports
    Inherits TStmt
End Class

Public Class TModule
    Inherits TStmt
    Public NameMod As String
End Class

Public Class TElseIf
    Inherits TStmt
    Public CndElseIf As TTerm
End Class

Public Class TDoStmt
    Inherits TStmt

    Public CndDo As TTerm
End Class

Public Class TEnumEle
    Inherits TStmt

    Public NameEnumEle As String

    Public Sub New(ref1 As TRef)
        TypeStmt = ETkn.eId
        NameEnumEle = ref1.NameRef
    End Sub

End Class

Public Class TComment
    Inherits TStmt

    Public LineCom As New TList(Of String)

	Public Sub New()
		TypeStmt = ETkn.eComment
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

Public Class TErr
    Inherits Exception

    Public MsgErr As String

    Public Sub New(msg As String)
        Debug.WriteLine("err:" + msg)
        MsgErr = msg
    End Sub
End Class
