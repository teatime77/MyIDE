Imports System.Diagnostics

' -------------------------------------------------------------------------------- TNavi
Public Class TDeclarative
    Public RefCnt As Integer
    Public ErrNav As Boolean = False

    Public Sub IncRefCnt(ref1 As TReference)
        RefCnt += 1
    End Sub

    Public Overridable Sub StartCondition(self As Object)
        If TypeOf self Is TFunction Then
            Dim fnc1 As TFunction = CType(self, TFunction)
            With fnc1
            End With

        End If
    End Sub

    Public Overridable Sub EndCondition(self As Object)
        If TypeOf self Is TDot Then
            Dim dot1 As TDot = CType(self, TDot)
            With dot1
            End With

        ElseIf TypeOf self Is TReference Then
            Dim ref1 As TReference = CType(self, TReference)
            With ref1
            End With

        ElseIf TypeOf self Is TApply Then
            Dim app1 As TApply = CType(self, TApply)
            With app1
            End With

        ElseIf TypeOf self Is TVariable Then
            Dim var1 As TVariable = CType(self, TVariable)
            With var1
            End With

        End If
    End Sub

    Public Overridable Sub NaviConstant(cns1 As TConstant)
        EndCondition(cns1)
    End Sub

    Public Overridable Sub NaviDot(dot1 As TDot)
        StartCondition(dot1)

        NaviTerm(dot1.TrmDot)

        EndCondition(dot1)
    End Sub

    Public Overridable Sub NaviReference(ref1 As TReference)
        EndCondition(ref1)
    End Sub

    Public Overridable Sub NaviTerm(trm1 As TTerm)
        If trm1 IsNot Nothing Then

            StartCondition(trm1)

            Try
                If TypeOf trm1 Is TConstant Then
                    NaviConstant(CType(trm1, TConstant))
                ElseIf TypeOf trm1 Is TArray Then
                    NaviArray(CType(trm1, TArray))
                ElseIf TypeOf trm1 Is TDot Then
                    NaviDot(CType(trm1, TDot))
                ElseIf TypeOf trm1 Is TReference Then
                    NaviReference(CType(trm1, TReference))
                ElseIf trm1.IsApp() Then
                    NaviApply(CType(trm1, TApply))
                ElseIf trm1.IsLog() Then
                    NaviLog(CType(trm1, TApply))
                ElseIf TypeOf trm1 Is TParenthesis Then
                    NaviTerm(CType(trm1, TParenthesis).TrmPar)
                    EndCondition(trm1)
                ElseIf TypeOf trm1 Is TFrom Then
                    NaviFrom(CType(trm1, TFrom))
                    EndCondition(trm1)
                ElseIf TypeOf trm1 Is TAggregate Then
                    NaviAggregate(CType(trm1, TAggregate))
                    EndCondition(trm1)
                Else
                    Debug.Assert(False)
                End If
            Catch ex As TError
            End Try
        End If
    End Sub

    Public Overridable Sub NaviTermList(list1 As TList(Of TTerm))
        StartCondition(list1)

        For Each trm1 In list1
            NaviTerm(trm1)
        Next
    End Sub

    Public Overridable Sub NaviCaseList(list1 As TList(Of TCase))
        StartCondition(list1)

        For Each cas1 In list1
            NaviStatement(cas1)
        Next
    End Sub

    Public Overridable Sub NaviStatementList(list1 As TList(Of TStatement))
        StartCondition(list1)

        For Each stmt1 In list1
            NaviStatement(stmt1)
        Next
    End Sub

    Public Overridable Sub NaviBlockList(list1 As TList(Of TBlock))
        StartCondition(list1)

        For Each blc_f In list1
            NaviStatement(blc_f)
        Next
    End Sub

    Public Overridable Sub NaviIfBlockList(list1 As TList(Of TIfBlock))
        StartCondition(list1)

        For Each blc_f In list1
            NaviStatement(blc_f)
        Next
    End Sub

    Public Overridable Sub NaviLocalVariableList(list1 As TList(Of TVariable))
        StartCondition(list1)

        For Each var1 In list1
            NaviLocalVariable(var1)
        Next
    End Sub

    Public Overridable Sub NaviLocalVariable(var1 As TVariable)
        If var1 IsNot Nothing Then

            StartCondition(var1)

            NaviTerm(var1.InitVar)

            EndCondition(var1)
        End If
    End Sub

    Public Overridable Sub NaviArray(arr1 As TArray)
        If arr1 IsNot Nothing Then
            NaviTermList(arr1.TrmArr)
        End If
    End Sub

    Public Overridable Sub NaviApply(app1 As TApply)
        If app1 IsNot Nothing Then
            NaviTermList(app1.ArgApp)

            If app1.FncApp IsNot Nothing Then
                NaviTerm(app1.FncApp)
            End If

            If app1.IniApp IsNot Nothing Then
                NaviTerm(app1.IniApp)
            End If

            EndCondition(app1)
        End If
    End Sub

    Public Overridable Sub NaviLog(opr1 As TApply)
        If opr1 IsNot Nothing Then
            NaviTermList(opr1.ArgApp)
            EndCondition(opr1)
        End If
    End Sub

    Public Overridable Sub NaviFrom(frm1 As TFrom)
        NaviTerm(frm1.SeqFrom)
        NaviLocalVariable(frm1.VarFrom)
        NaviTerm(frm1.CndFrom)
        NaviTerm(frm1.SelFrom)
        NaviTerm(frm1.TakeFrom)
    End Sub

    Public Overridable Sub NaviAggregate(aggr1 As TAggregate)
        NaviTerm(aggr1.SeqAggr)
        NaviLocalVariable(aggr1.VarAggr)
        NaviTerm(aggr1.IntoAggr)
    End Sub

    Public Overridable Sub NaviFor(for1 As TFor)
        With for1
            NaviTerm(.IdxFor)
            NaviTerm(.InTrmFor)
            NaviLocalVariable(.InVarFor)
            NaviTerm(.FromFor)
            NaviTerm(.ToFor)
            NaviTerm(.StepFor)
            NaviStatement(.IniFor)
            NaviTerm(.CndFor)
            NaviStatement(.StepStmtFor)
            NaviStatement(.BlcFor)
        End With
    End Sub

    Public Overridable Sub NaviCase(cas1 As TCase)
        StartCondition(cas1)

        NaviTermList(cas1.TrmCase)
        NaviStatement(cas1.BlcCase)
    End Sub

    Public Overridable Sub NaviSelect(swt1 As TSelect)
        NaviTerm(swt1.TrmSel)
        NaviCaseList(swt1.CaseSel)
    End Sub

    Public Overridable Sub NaviAssignment(asn1 As TAssignment)
        StartCondition(asn1)
        NaviTerm(asn1.RelAsn)
    End Sub

    Public Overridable Sub NaviCall(call1 As TCall)
        NaviTerm(call1.AppCall)
    End Sub

    Public Overridable Sub NaviReDim(red1 As TReDim)
        NaviTerm(red1.TrmReDim)
        NaviTermList(red1.DimReDim)
    End Sub

    Public Overridable Sub NaviIf(if1 As TIf)
        StartCondition(if1)
        NaviIfBlockList(if1.IfBlc)
    End Sub

    Public Overridable Sub NaviIfBlock(if_blc As TIfBlock)
        NaviTerm(if_blc.CndIf)
        NaviStatement(if_blc.BlcIf)
    End Sub

    Public Overridable Sub NaviTry(try1 As TTry)
        NaviStatement(try1.BlcTry)
        NaviStatement(try1.BlcCatch)
    End Sub

    Public Overridable Sub NaviWith(with1 As TWith)
        NaviTerm(with1.TermWith)
        NaviStatement(with1.BlcWith)
    End Sub

    Public Overridable Sub NaviVariableDeclaration(dcl1 As TVariableDeclaration)
        NaviLocalVariableList(dcl1.VarDecl)
    End Sub

    Public Overridable Sub NaviReturn(ret1 As TReturn)
        NaviTerm(ret1.TrmRet)
    End Sub

    Public Overridable Sub NaviThrow(throw1 As TThrow)
        NaviTerm(throw1.TrmThrow)
    End Sub

    Public Overridable Sub NaviBlock(blc1 As TBlock)
        NaviStatementList(blc1.StmtBlc)
    End Sub

    Public Overridable Sub NaviStatement(stmt1 As TStatement)

        StartCondition(stmt1)

        If stmt1 IsNot Nothing Then

            If TypeOf stmt1 Is TAssignment Then
                NaviAssignment(CType(stmt1, TAssignment))
            ElseIf TypeOf stmt1 Is TCall Then
                NaviCall(CType(stmt1, TCall))
            ElseIf TypeOf stmt1 Is TVariableDeclaration Then
                NaviVariableDeclaration(CType(stmt1, TVariableDeclaration))
            ElseIf TypeOf stmt1 Is TReDim Then
                NaviReDim(CType(stmt1, TReDim))
            ElseIf TypeOf stmt1 Is TIf Then
                NaviIf(CType(stmt1, TIf))
            ElseIf TypeOf stmt1 Is TIfBlock Then
                NaviIfBlock(CType(stmt1, TIfBlock))
            ElseIf TypeOf stmt1 Is TSelect Then
                NaviSelect(CType(stmt1, TSelect))
            ElseIf TypeOf stmt1 Is TCase Then
                NaviCase(CType(stmt1, TCase))
            ElseIf TypeOf stmt1 Is TTry Then
                NaviTry(CType(stmt1, TTry))
            ElseIf TypeOf stmt1 Is TWith Then
                NaviWith(CType(stmt1, TWith))
            ElseIf TypeOf stmt1 Is TFor Then
                NaviFor(CType(stmt1, TFor))
            ElseIf TypeOf stmt1 Is TBlock Then
                NaviBlock(CType(stmt1, TBlock))
            ElseIf TypeOf stmt1 Is TReturn Then
                NaviReturn(CType(stmt1, TReturn))
            ElseIf TypeOf stmt1 Is TThrow Then
                NaviThrow(CType(stmt1, TThrow))
            ElseIf TypeOf stmt1 Is TComment Then
            Else
                Select Case stmt1.TypeStmt
                    Case EToken.eExitDo

                    Case EToken.eExitFor

                    Case EToken.eExitSub

                    Case Else
                        Debug.Assert(False)
                End Select
            End If
        End If

        EndCondition(stmt1)
    End Sub

    Public Overridable Sub NaviFunction(fnc1 As TFunction)
        With fnc1
            StartCondition(fnc1)

            If fnc1.BlcFnc IsNot Nothing Then
                NaviStatement(.BlcFnc)
            End If

        End With
    End Sub

    Public Overridable Sub NaviClass(cla1 As TClass)
        If (cla1.FldCla.Count <> 0 OrElse cla1.FncCla.Count <> 0) AndAlso Not (cla1.GenCla IsNot Nothing AndAlso cla1.OrgCla Is Nothing) Then

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla
                NaviFunction(fnc1)
            Next
        End If
    End Sub

    Public Overridable Sub NaviProject(prj As TProject)
        '  すべてのクラスに対し
        For Each cla1 In prj.SimpleParameterizedClassList
            NaviClass(cla1)
        Next
    End Sub
End Class

' -------------------------------------------------------------------------------- TSetRefDeclarative
Public Class TSetRefDeclarative
    Inherits TDeclarative

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TFunction Then
            Dim fnc1 As TFunction = CType(self, TFunction)
            With fnc1
                If .InterfaceFnc IsNot Nothing Then
                    .ImplFnc.VarRef = TProject.FindFieldFunction(.InterfaceFnc, .ImplFnc.NameRef, Nothing)
                    Debug.Assert(.ImplFnc IsNot Nothing)
                End If

                Debug.Assert(.ThisFnc IsNot Nothing)
            End With

        End If
    End Sub

    Public Sub SetTypeTrm(trm1 As TTerm)
        With trm1
            If TypeOf trm1 Is TConstant Then
                With CType(trm1, TConstant)
                    Select Case .TypeAtm
                        Case EToken.eString
                            .TypeTrm = .ProjectTrm.StringType
                        Case EToken.eInt, EToken.eHex
                            .TypeTrm = .ProjectTrm.IntType
                        Case EToken.eChar
                            .TypeTrm = .ProjectTrm.CharType
                        Case Else
                            Debug.WriteLine("@h")
                            .TypeTrm = Nothing
                    End Select
                End With

            ElseIf TypeOf trm1 Is TArray Then

            ElseIf TypeOf trm1 Is TReference Then
                With CType(trm1, TReference)

                    If .VarRef IsNot Nothing Then
                        If TypeOf .VarRef Is TFunction Then
                            If .IsAddressOf Then
                                .TypeTrm = New TDelegate(.ProjectTrm, CType(.VarRef, TFunction))
                            Else
                                .TypeTrm = CType(.VarRef, TFunction).RetType
                                If .TypeTrm Is Nothing Then
                                    'Debug.Print("void型 {0}", .VarRef.NameVar)
                                End If
                            End If
                        ElseIf TypeOf .VarRef Is TClass Then
                            .TypeTrm = CType(.VarRef, TClass)
                        Else
                            .TypeTrm = .VarRef.TypeVar
                            Debug.Assert(.TypeTrm IsNot Nothing)
                        End If
                    End If
                End With

            ElseIf TypeOf trm1 Is TApply Then
                With CType(trm1, TApply)
                    Select Case .TypeApp
                        Case EToken.eAnd, EToken.eOR, EToken.eNot, EToken.eAnp
                            .TypeTrm = .ProjectTrm.BoolType

                        Case EToken.eEq, EToken.eNE, EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE, EToken.eIsNot, EToken.eTypeof, EToken.eIs
                            .TypeTrm = .ProjectTrm.BoolType

                        Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD
                            .TypeTrm = .ArgApp(0).TypeTrm

                        Case EToken.eAppCall
                            If TypeOf .FncApp Is TReference Then

                                Dim ref1 As TReference = CType(.FncApp, TReference)
                                If TypeOf ref1.VarRef Is TFunction Then
                                    Dim fnc1 As TFunction = CType(ref1.VarRef, TFunction)
                                    .TypeTrm = fnc1.RetType
                                Else
                                    Select Case .KndApp
                                        Case EApply.eCallApp
                                            Debug.Assert(TypeOf .FncApp.TypeTrm Is TDelegate)
                                            .TypeTrm = CType(.FncApp.TypeTrm, TDelegate).RetDlg
                                        Case EApply.eArrayApp
                                            Debug.Assert(ref1.VarRef.TypeVar.GenCla IsNot Nothing AndAlso ref1.VarRef.TypeVar.GenCla.Count = 1)
                                            .TypeTrm = ref1.VarRef.TypeVar.GenCla(0)
                                        Case EApply.eStringApp
                                            .TypeTrm = .ProjectTrm.CharType
                                        Case EApply.eListApp
                                            Debug.Assert(ref1.VarRef.TypeVar.GenCla IsNot Nothing AndAlso ref1.VarRef.TypeVar.GenCla.Count = 1)
                                            .TypeTrm = ref1.VarRef.TypeVar.GenCla(0)
                                        Case EApply.eDictionaryApp
                                            Debug.Assert(ref1.VarRef.TypeVar.GenCla IsNot Nothing AndAlso ref1.VarRef.TypeVar.GenCla.Count = 2)
                                            .TypeTrm = ref1.VarRef.TypeVar.GenCla(1)
                                        Case Else
                                            Debug.Assert(False)
                                    End Select
                                End If
                            Else
                                Dim cla1 As TClass

                                cla1 = .FncApp.TypeTrm
                                If cla1 Is .ProjectTrm.StringType Then
                                    .TypeTrm = .ProjectTrm.CharType
                                Else
                                    .TypeTrm = cla1
                                End If
                            End If

                        Case EToken.eBaseCall
                            .TypeTrm = Nothing

                        Case EToken.eBaseNew
                            .TypeTrm = Nothing

                        Case EToken.eAs, EToken.eCast
                            .TypeTrm = .ClassApp

                        Case EToken.eQUE
                            .TypeTrm = .ArgApp(1).TypeTrm

                        Case EToken.eTypeof
                            .TypeTrm = .ProjectTrm.BoolType

                        Case EToken.eNew
                            .TypeTrm = .NewApp

                        Case EToken.eGetType
                            .TypeTrm = .ProjectTrm.TypeType

                        Case Else
                            Debug.WriteLine("Err Trm Src2:{0}", .TypeApp)
                            Debug.Assert(False)
                    End Select
                End With

            ElseIf TypeOf trm1 Is TParenthesis Then
                With CType(trm1, TParenthesis)
                    .TypeTrm = .TrmPar.TypeTrm
                End With

            ElseIf TypeOf trm1 Is TFrom Then
                With CType(trm1, TFrom)
                    Dim cla1 As TClass, cla2 As TClass

                    If .SelFrom Is Nothing Then
                        cla1 = .SeqFrom.TypeTrm
                        Debug.Assert(cla1 IsNot Nothing)
                        cla2 = .ProjectTrm.ElementType(cla1)
                        .TypeTrm = .ProjectTrm.GetIEnumerableClass(cla2)
                    Else
                        cla1 = .SelFrom.TypeTrm
                        Debug.Assert(cla1 IsNot Nothing)
                        .TypeTrm = .ProjectTrm.GetIEnumerableClass(cla1)
                    End If
                End With

            ElseIf TypeOf trm1 Is TAggregate Then
                With CType(trm1, TAggregate)

                    .TypeTrm = .IntoAggr.TypeTrm
                End With

            Else
                Debug.Assert(False)
            End If
        End With
    End Sub

    Public Overrides Sub EndCondition(self As Object)
        If TypeOf self Is TTerm Then
            Dim trm1 As TTerm = CType(self, TTerm)

            If TypeOf self Is TConstant Then
                SetTypeTrm(trm1)

            ElseIf TypeOf self Is TDot Then
                Dim dot1 As TDot = CType(self, TDot)
                With dot1
                    IncRefCnt(dot1)

                    If .TrmDot Is Nothing Then
                        Dim with1 As TWith = CType((From o In TNaviUp.AncestorList(self) Where TypeOf (o) Is TWith).First(), TWith)

                        .TypeDot = with1.TermWith.TypeTrm
                    Else

                        .TypeDot = .TrmDot.TypeTrm
                    End If

                    If .TypeDot Is Nothing Then
                        Throw New TError(String.Format("ドットの左の項の型が不明 {0}", .NameRef))
                    End If
                    If TypeOf .TypeDot Is TDelegate Then
                        Debug.Assert(.NameRef = "Invoke")
                    End If

                    Dim obj As Object = TNaviUp.UpObj(dot1)
                    Dim dot_is_fncapp As Boolean = (TypeOf obj Is TApply AndAlso dot1 Is CType(obj, TApply).FncApp)
                    If dot_is_fncapp Then
                        Dim app1 As TApply = CType(obj, TApply)

                        Debug.Assert(dot1 Is app1.FncApp)
                        Debug.Assert(app1.TypeApp = EToken.eAppCall)

                        If .IsAddressOf Then
                            .VarRef = TProject.FindFieldFunction(.TypeDot, .NameRef, Nothing)
                        Else
                            If .TypeDot.IsArray() Then
                                Debug.Assert(.ProjectTrm.ArrayType IsNot Nothing)
                                .VarRef = TProject.FindFieldFunction(.ProjectTrm.ArrayType, .NameRef, app1.ArgApp)
                            Else
                                .VarRef = TProject.FindFieldFunction(.TypeDot, .NameRef, app1.ArgApp)
                            End If
                        End If

                        If .VarRef Is Nothing Then
                            Throw New TError(String.Format("不明なメンバー {0} {1}", .TypeDot.LongName(), .NameRef))
                        Else
                            Debug.Assert(TypeOf .VarRef Is TFunction OrElse TypeOf .VarRef.TypeVar Is TDelegate OrElse .VarRef.TypeVar.HasIndex())
                        End If
                    Else

                        If .TypeDot.IsArray() Then
                            Debug.Assert(.ProjectTrm.ArrayType IsNot Nothing)
                            .VarRef = TProject.FindFieldFunction(.ProjectTrm.ArrayType, .NameRef, Nothing)
                        Else
                            .VarRef = TProject.FindFieldFunction(.TypeDot, .NameRef, Nothing)
                        End If
                    End If


                    If .VarRef Is Nothing Then
                        Throw New TError(String.Format("不明なメンバー {0} {1}", .TypeDot.LongName(), .NameRef))
                    Else
                        If Not dot_is_fncapp AndAlso TypeOf .VarRef Is TFunction Then
                            Debug.WriteLine("メソッドを値として参照 {0}", .VarRef.NameVar)
                        End If
                    End If

                    SetTypeTrm(trm1)
                End With

            ElseIf TypeOf self Is TReference Then
                Dim ref1 As TReference = CType(self, TReference)
                With ref1
                    IncRefCnt(ref1)
                    Dim obj As Object = TNaviUp.UpObj(ref1)
                    If TypeOf obj Is TApply Then
                        Dim app1 As TApply = CType(obj, TApply)

                        If ref1 Is app1.FncApp Then
                            Select Case app1.TypeApp
                                Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD

                                    If .VarRef Is Nothing Then

                                        ' 演算子オーバーロード関数を得る
                                        Dim fnc1 As TFunction = app1.ProjectTrm.GetOperatorFunction(app1.TypeApp, app1.ArgApp(0))
                                        If fnc1 IsNot Nothing Then
                                            ' 演算子オーバーロード関数を得られた場合

                                            .VarRef = fnc1
                                        Else
                                            ' 演算子オーバーロード関数を得られない場合

                                            Dim name1 As String = ""
                                            Select Case app1.TypeApp
                                                Case EToken.eADD
                                                    name1 = "__Add"
                                                Case EToken.eMns
                                                    name1 = "__Mns"
                                                Case EToken.eMUL
                                                    name1 = "__Mul"
                                                Case EToken.eDIV
                                                    name1 = "__Div"
                                                Case EToken.eMOD
                                                    name1 = "__Mod"
                                            End Select

                                            Dim vvar = (From fnc In app1.ProjectTrm.SystemType.FncCla Where fnc.NameVar = name1).ToList()
                                            If vvar.Count <> 1 Then

                                                Debug.WriteLine("演算子未定義:{0}", .NameRef)
                                                Debug.Assert(False)
                                            Else
                                                .VarRef = vvar(0)
                                            End If
                                        End If

                                        If .VarRef Is Nothing Then
                                            Debug.WriteLine("演算子未定義:{0}", .NameRef)
                                        End If
                                    End If

                                Case EToken.eAppCall
                                    .VarRef = TProject.FindFieldFunction(app1.FunctionTrm.ClaFnc, .NameRef, app1.ArgApp)
                                    If .VarRef Is Nothing Then

                                        .VarRef = .ProjectTrm.FindVariable(ref1, .NameRef)
                                        If .VarRef Is Nothing Then

                                            Debug.WriteLine("関数呼び出し 未定義:{0}", .NameRef)
                                        End If
                                    End If

                                Case EToken.eNew
                                    If app1.NewApp.DimCla = 0 Then
                                        .VarRef = TProject.FindNew(app1.NewApp, app1.ArgApp)
                                        'AndAlso app1.ArgApp.Count <> 0 AndAlso app1.NewApp.DimCla = 0
                                        If .VarRef Is Nothing Then
                                            Debug.WriteLine("New 未定義 {0} {1}", .NameRef, app1.ArgApp.Count)
                                        End If
                                    Else
                                        .VarRef = .ProjectTrm.ArrayMaker
                                    End If
                                    Return

                                Case EToken.eBaseCall
                                    .VarRef = TProject.FindFieldFunction(.FunctionTrm.ClaFnc.SuperClassList(0), .NameRef, app1.ArgApp)
                                    Debug.Assert(.VarRef IsNot Nothing AndAlso TypeOf .VarRef Is TFunction)
                                    Return

                                Case EToken.eBaseNew
                                    .VarRef = TProject.FindNew(.FunctionTrm.ClaFnc.SuperClassList(0), app1.ArgApp)
                                    If .VarRef Is Nothing Then
                                        If app1.ArgApp.Count <> 0 Then

                                            Debug.WriteLine("New 未定義 {0} {1}", .NameRef, app1.ArgApp.Count)
                                        End If
                                    End If

                            End Select
                        End If
                    End If

                    If .VarRef Is Nothing Then
                        .VarRef = TProject.FindFieldFunction(.FunctionTrm.ClaFnc, .NameRef, Nothing)
                        If .VarRef Is Nothing Then

                            .VarRef = .ProjectTrm.FindVariable(ref1, .NameRef)
                            If .VarRef Is Nothing Then
                                .VarRef = TProject.FindFieldFunction(.FunctionTrm.ClaFnc, .NameRef, Nothing)
                                Debug.WriteLine("変数未定義:{0}", .NameRef)
                            End If
                        End If
                    End If

                    SetTypeTrm(trm1)
                End With

            ElseIf TypeOf self Is TApply Then
                With CType(self, TApply)
                    Select Case .TypeApp
                        Case EToken.eAppCall
                            If .FncApp IsNot Nothing Then

                                If TypeOf .FncApp Is TReference AndAlso TypeOf CType(.FncApp, TReference).VarRef Is TFunction Then
                                    ' 関数呼び出しの場合

                                    .KndApp = EApply.eCallApp

                                ElseIf TypeOf .FncApp Is TReference AndAlso TypeOf (CType(.FncApp, TReference).VarRef.TypeVar) Is TDelegate Then
                                    .KndApp = EApply.eCallApp
                                Else
                                    ' 関数呼び出しでない場合

                                    Dim cla1 As TClass = .FncApp.TypeTrm
                                    Debug.Assert(cla1 IsNot Nothing)

                                    If cla1.NameCla() = "String" Then
                                        .KndApp = EApply.eStringApp
                                    ElseIf cla1.IsArray() Then
                                        .KndApp = EApply.eArrayApp
                                    ElseIf cla1.IsList() Then
                                        .KndApp = EApply.eListApp
                                    ElseIf cla1.IsDictionary() Then
                                        .KndApp = EApply.eDictionaryApp
                                    Else
                                        Debug.Print("想定外 1")
                                    End If
                                End If

                            Else
                                Debug.Print("想定外 2")
                            End If

                        Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD
                        Case EToken.eNew, EToken.eCast, EToken.eGetType, EToken.eBaseNew, EToken.eBaseCall
                        Case Else
                            If .IsLog() Then
                            Else
                                Debug.Print("想定外 3")
                            End If

                    End Select

                    SetTypeTrm(trm1)
                End With

            ElseIf TypeOf trm1 Is TParenthesis Then
                SetTypeTrm(trm1)

            ElseIf TypeOf trm1 Is TFrom Then
                SetTypeTrm(trm1)

            ElseIf TypeOf trm1 Is TAggregate Then
                SetTypeTrm(trm1)

            End If

        ElseIf TypeOf self Is TVariable Then
            With CType(self, TVariable)
                Dim obj As Object = TNaviUp.UpObj(self)

                If TypeOf obj Is TFrom Then
                    Dim frm1 As TFrom = CType(obj, TFrom)
                    Debug.Assert(self Is frm1.VarFrom)

                    Dim type1 As TClass = frm1.SeqFrom.TypeTrm
                    .TypeVar = frm1.ProjectTrm.ElementType(type1)
                    Debug.Assert(.TypeVar IsNot Nothing)

                ElseIf TypeOf obj Is TAggregate Then
                    Dim aggr1 As TAggregate = CType(obj, TAggregate)
                    Debug.Assert(self Is aggr1.VarAggr)

                    Dim type1 As TClass = aggr1.SeqAggr.TypeTrm
                    .TypeVar = aggr1.ProjectTrm.ElementType(type1)
                    Debug.Assert(.TypeVar IsNot Nothing)

                ElseIf TypeOf obj Is TFor Then
                    Dim for1 As TFor = CType(obj, TFor)
                    Debug.Assert(self Is for1.InVarFor)

                    Dim type1 As TClass = for1.InTrmFor.TypeTrm
                    .TypeVar = for1.ProjectStmt().ElementType(type1)
                    Debug.Assert(.TypeVar IsNot Nothing)

                Else

                    Dim stmt As TStatement = TNaviUp.UpToStmt(self)
                    If TypeOf stmt Is TVariableDeclaration Then
                        If .InitVar IsNot Nothing Then
                            If .TypeVar Is Nothing Then
                                .NoType = True
                                .TypeVar = .InitVar.TypeTrm
                            End If
                        End If
                    End If
                End If
            End With
        End If
    End Sub
End Class


' -------------------------------------------------------------------------------- TNaviSetDefRef
Public Class TNaviSetDefRef2
    Inherits TDeclarative

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TReference Then

            With CType(self, TReference)

                Dim up_stmt As TStatement = TNaviUp.UpToStmt(self)
                If TypeOf up_stmt Is TAssignment Then
                    Dim asn1 As TAssignment = CType(up_stmt, TAssignment)

                    If TypeOf asn1.RelAsn.ArgApp(0) Is TReference Then
                        ' 左辺が変数参照の場合

                        If asn1.RelAsn.ArgApp(0) Is self Then
                            .DefRef = True
                        End If
                    Else
                        ' 左辺が関数呼び出しの場合

                        Debug.Assert(asn1.RelAsn.ArgApp(0).IsApp())
                        Dim app1 As TApply = CType(asn1.RelAsn.ArgApp(0), TApply)

                        Debug.Assert(app1.KndApp = EApply.eArrayApp OrElse app1.KndApp = EApply.eListApp)

                        If app1.FncApp Is self Then

                            .DefRef = True
                        End If
                    End If
                End If
            End With
        End If
    End Sub
End Class


' -------------------------------------------------------------------------------- TNaviSetProjectTrm
Public Class TNaviSetProjectTrm
    Inherits TDeclarative

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TTerm Then
            With CType(self, TTerm)
                Dim prj = From obj In TNaviUp.AncestorList(self) Where TypeOf obj Is TProject
                .ProjectTrm = CType(prj.First(), TProject)
            End With
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviSetLabel
Public Class TNaviSetLabel
    Inherits TDeclarative

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TExit Then
            With CType(self, TExit)

                If .TypeStmt = EToken.eExitDo OrElse .TypeStmt = EToken.eExitFor Then
                    Dim for_do As TFor = Nothing

                    Dim for_select As Object = (From obj In TNaviUp.AncestorList(self) Where TypeOf (obj) Is TFor OrElse TypeOf obj Is TSelect).First()
                    Select Case .TypeStmt
                        Case EToken.eExitDo
                            If Not (TypeOf for_select Is TFor AndAlso CType(for_select, TFor).IsDo) Then
                                ' 直近のSelectまたはループがDoでない場合

                                ' 直近のDoを探す。
                                for_do = CType((From obj In TNaviUp.AncestorList(self) Where TypeOf (obj) Is TFor AndAlso CType(obj, TFor).IsDo).First(), TFor)
                            End If

                        Case EToken.eExitFor
                            If Not (TypeOf for_select Is TFor AndAlso Not CType(for_select, TFor).IsDo) Then
                                ' 直近のSelectまたはループがForでない場合

                                ' 直近のForを探す。
                                for_do = CType((From obj In TNaviUp.AncestorList(self) Where TypeOf (obj) Is TFor AndAlso Not CType(obj, TFor).IsDo).First(), TFor)
                            End If
                    End Select

                    If for_do IsNot Nothing Then
                        ' 直近のSelectまたはループがこのExitに対応しない場合

                        If for_do.LabelFor = 0 Then
                            ' ForまたはDoにラベル番号をつけていない場合

                            ' 関数ごとのラベル番号をカウントアップする。
                            .FunctionStmt.LabelCount = .FunctionStmt.LabelCount + 1

                            ' ForまたはDoにラベル番号をつける。
                            for_do.LabelFor = .FunctionStmt.LabelCount
                        End If

                        ' Exit文にラベル番号を指定する。
                        .LabelExit = for_do.LabelFor
                    End If
                End If
            End With
        End If
    End Sub
End Class
