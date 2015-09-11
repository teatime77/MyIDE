﻿Imports System.Diagnostics

' -------------------------------------------------------------------------------- TNavi
Public Class TNavi
    Public RefCnt As Integer
    Public ErrNav As Boolean = False

    Public Sub IncRefCnt(ref1 As TReference)
        RefCnt += 1
    End Sub


    Public Overridable Function StartReference(ref1 As TReference, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartDot(dot1 As TDot, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartLocalVariable(var1 As TVariable, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartTerm(trm1 As TTerm, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartCase(case1 As TCase, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartAssignment(asn1 As TAssignment, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartIf(if1 As TIf, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartFunction(fnc1 As TFunction, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartLocalVariableList(list1 As TList(Of TVariable), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartTermList(list1 As TList(Of TTerm), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartCaseList(list1 As TList(Of TCase), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartStatementList(list1 As TList(Of TStatement), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartBlockList(list1 As TList(Of TBlock), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartIfBlockList(list1 As TList(Of TIfBlock), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Sub EndStatement(stmt1 As TStatement, arg1 As Object)
    End Sub

    Public Overridable Sub NaviDot(dot1 As TDot, arg1 As Object)
        arg1 = StartDot(dot1, arg1)

        If dot1.TrmDot Is Nothing Then
        Else
            NaviTerm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overridable Sub NaviReference(ref1 As TReference, arg1 As Object)
        StartReference(ref1, arg1)
    End Sub

    Public Overridable Sub NaviTerm(trm1 As TTerm, arg1 As Object)
        arg1 = StartTerm(trm1, arg1)

        If trm1 IsNot Nothing Then
            Try
                If TypeOf trm1 Is TConstant Then
                ElseIf TypeOf trm1 Is TArray Then
                    NaviArray(CType(trm1, TArray), arg1)
                ElseIf TypeOf trm1 Is TDot Then
                    NaviDot(CType(trm1, TDot), arg1)
                ElseIf TypeOf trm1 Is TReference Then
                    NaviReference(CType(trm1, TReference), arg1)
                ElseIf trm1.IsApp() Then
                    NaviApply(CType(trm1, TApply), arg1)
                ElseIf trm1.IsLog() Then
                    NaviLog(CType(trm1, TApply), arg1)
                ElseIf TypeOf trm1 Is TParenthesis Then
                    NaviTerm(CType(trm1, TParenthesis).TrmPar, arg1)
                ElseIf TypeOf trm1 Is TFrom Then
                    NaviFrom(CType(trm1, TFrom), arg1)
                ElseIf TypeOf trm1 Is TAggregate Then
                    NaviAggregate(CType(trm1, TAggregate), arg1)
                Else
                    Debug.Assert(False)
                End If
            Catch ex As TError
                ErrNav = True
            End Try
        End If
    End Sub

    Public Overridable Sub NaviTermList(list1 As TList(Of TTerm), arg1 As Object)
        arg1 = StartTermList(list1, arg1)

        For Each trm1 In list1
            NaviTerm(trm1, arg1)
        Next
    End Sub

    Public Overridable Sub NaviCaseList(list1 As TList(Of TCase), arg1 As Object)
        arg1 = StartCaseList(list1, arg1)

        For Each cas1 In list1
            NaviStatement(cas1, arg1)
        Next
    End Sub

    Public Overridable Sub NaviStatementList(list1 As TList(Of TStatement), arg1 As Object)
        arg1 = StartStatementList(list1, arg1)

        For Each stmt1 In list1
            NaviStatement(stmt1, arg1)
        Next
    End Sub

    Public Overridable Sub NaviBlockList(list1 As TList(Of TBlock), arg1 As Object)
        arg1 = StartBlockList(list1, arg1)

        For Each blc_f In list1
            NaviStatement(blc_f, arg1)
        Next
    End Sub

    Public Overridable Sub NaviIfBlockList(list1 As TList(Of TIfBlock), arg1 As Object)
        arg1 = StartIfBlockList(list1, arg1)

        For Each blc_f In list1
            NaviStatement(blc_f, arg1)
        Next
    End Sub

    Public Overridable Sub NaviLocalVariableList(list1 As TList(Of TVariable), arg1 As Object)
        arg1 = StartLocalVariableList(list1, arg1)

        For Each var1 In list1
            NaviLocalVariable(var1, arg1)
        Next
    End Sub

    Public Overridable Sub NaviLocalVariable(var1 As TVariable, arg1 As Object)
        arg1 = StartLocalVariable(var1, arg1)

        NaviTerm(var1.InitVar, arg1)
    End Sub

    Public Overridable Sub NaviArray(arr1 As TArray, arg1 As Object)
        If arr1 IsNot Nothing Then
            NaviTermList(arr1.TrmArr, arg1)
        End If
    End Sub

    Public Overridable Sub NaviApply(app1 As TApply, arg1 As Object)
        Dim ref1 As TReference, dot1 As TDot

        If app1 IsNot Nothing Then
            If app1.TypeApp = EToken.eAddressOf Then
                If TypeOf app1.ArgApp(0) Is TDot Then
                    dot1 = CType(app1.ArgApp(0), TDot)
                    NaviTerm(dot1, arg1)
                ElseIf TypeOf app1.ArgApp(0) Is TReference Then
                    ref1 = CType(app1.ArgApp(0), TReference)
                    NaviTerm(ref1, arg1)
                Else
                    Debug.Assert(False)
                End If
            Else
                NaviTermList(app1.ArgApp, arg1)
            End If

            If app1.FncApp IsNot Nothing Then
                NaviTerm(app1.FncApp, arg1)
            End If

            If app1.IniApp IsNot Nothing Then
                NaviTerm(app1.IniApp, arg1)
            End If
        End If
    End Sub

    Public Overridable Sub NaviLog(opr1 As TApply, arg1 As Object)
        If opr1 IsNot Nothing Then
            NaviTermList(opr1.ArgApp, arg1)
        End If
    End Sub

    Public Overridable Sub NaviFrom(frm1 As TFrom, arg1 As Object)
        NaviTerm(frm1.SeqFrom, arg1)
        NaviTerm(frm1.CndFrom, arg1)
        NaviTerm(frm1.SelFrom, arg1)
        NaviTerm(frm1.TakeFrom, arg1)
    End Sub

    Public Overridable Sub NaviAggregate(aggr1 As TAggregate, arg1 As Object)
        NaviTerm(aggr1.SeqAggr, arg1)
        NaviTerm(aggr1.IntoAggr, arg1)
    End Sub

    Public Overridable Sub NaviFor(for1 As TFor, arg1 As Object)
        NaviTerm(for1.IdxFor, arg1)
        NaviTerm(for1.InTrmFor, arg1)
        NaviTerm(for1.FromFor, arg1)
        NaviTerm(for1.ToFor, arg1)
        NaviTerm(for1.StepFor, arg1)
        NaviStatement(for1.IniFor, arg1)
        NaviTerm(for1.CndFor, arg1)
        NaviStatement(for1.StepStmtFor, arg1)
        NaviStatement(for1.BlcFor, arg1)
    End Sub

    Public Overridable Sub NaviCase(cas1 As TCase, arg1 As Object)
        arg1 = StartCase(cas1, arg1)

        NaviTermList(cas1.TrmCase, arg1)
        NaviStatement(cas1.BlcCase, arg1)
    End Sub

    Public Overridable Sub NaviSelect(swt1 As TSelect, arg1 As Object)
        NaviTerm(swt1.TrmSel, arg1)
        NaviCaseList(swt1.CaseSel, arg1)
    End Sub

    Public Overridable Sub NaviAssignment(asn1 As TAssignment, arg1 As Object)
        arg1 = StartAssignment(asn1, arg1)
        NaviTerm(asn1.RelAsn, arg1)
    End Sub

    Public Overridable Sub NaviCall(call1 As TCall, arg1 As Object)
        NaviTerm(call1.AppCall, arg1)
    End Sub

    Public Overridable Sub NaviReDim(red1 As TReDim, arg1 As Object)
        NaviTerm(red1.TrmReDim, arg1)
        NaviTermList(red1.DimReDim, arg1)
    End Sub

    Public Overridable Sub NaviIf(if1 As TIf, arg1 As Object)
        arg1 = StartIf(if1, arg1)
        NaviIfBlockList(if1.IfBlc, arg1)
    End Sub

    Public Overridable Sub NaviIfBlock(if_blc As TIfBlock, arg1 As Object)
        NaviTerm(if_blc.CndIf, arg1)
        NaviStatement(if_blc.BlcIf, arg1)
    End Sub

    Public Overridable Sub NaviTry(try1 As TTry, arg1 As Object)
        NaviStatement(try1.BlcTry, arg1)
        NaviStatement(try1.BlcCatch, arg1)
    End Sub

    Public Overridable Sub NaviWith(with1 As TWith, arg1 As Object)
        NaviTerm(with1.TermWith, arg1)
        NaviStatement(with1.BlcWith, arg1)
    End Sub

    Public Overridable Sub NaviVariableDeclaration(dcl1 As TVariableDeclaration, arg1 As Object)
        NaviLocalVariableList(dcl1.VarDecl, arg1)
    End Sub

    Public Overridable Sub NaviReturn(ret1 As TReturn, arg1 As Object)
        NaviTerm(ret1.TrmRet, arg1)
    End Sub

    Public Overridable Sub NaviThrow(throw1 As TThrow, arg1 As Object)
        NaviTerm(throw1.TrmThrow, arg1)
    End Sub

    Public Overridable Sub NaviBlock(blc1 As TBlock, arg1 As Object)
        NaviStatementList(blc1.StmtBlc, arg1)
    End Sub

    Public Overridable Sub NaviStatement(stmt1 As TStatement, arg1 As Object)

        arg1 = StartStatement(stmt1, arg1)

        If stmt1 IsNot Nothing Then

            If TypeOf stmt1 Is TAssignment Then
                NaviAssignment(CType(stmt1, TAssignment), arg1)
            ElseIf TypeOf stmt1 Is TCall Then
                NaviCall(CType(stmt1, TCall), arg1)
            ElseIf TypeOf stmt1 Is TVariableDeclaration Then
                NaviVariableDeclaration(CType(stmt1, TVariableDeclaration), arg1)
            ElseIf TypeOf stmt1 Is TReDim Then
                NaviReDim(CType(stmt1, TReDim), arg1)
            ElseIf TypeOf stmt1 Is TIf Then
                NaviIf(CType(stmt1, TIf), arg1)
            ElseIf TypeOf stmt1 Is TIfBlock Then
                NaviIfBlock(CType(stmt1, TIfBlock), arg1)
            ElseIf TypeOf stmt1 Is TSelect Then
                NaviSelect(CType(stmt1, TSelect), arg1)
            ElseIf TypeOf stmt1 Is TCase Then
                NaviCase(CType(stmt1, TCase), arg1)
            ElseIf TypeOf stmt1 Is TTry Then
                NaviTry(CType(stmt1, TTry), arg1)
            ElseIf TypeOf stmt1 Is TWith Then
                NaviWith(CType(stmt1, TWith), arg1)
            ElseIf TypeOf stmt1 Is TFor Then
                NaviFor(CType(stmt1, TFor), arg1)
            ElseIf TypeOf stmt1 Is TBlock Then
                NaviBlock(CType(stmt1, TBlock), arg1)
            ElseIf TypeOf stmt1 Is TReturn Then
                NaviReturn(CType(stmt1, TReturn), arg1)
            ElseIf TypeOf stmt1 Is TThrow Then
                NaviThrow(CType(stmt1, TThrow), arg1)
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

        EndStatement(stmt1, arg1)
    End Sub

    Public Overridable Sub NaviFunction(fnc1 As TFunction, arg1 As Object)
        arg1 = StartFunction(fnc1, arg1)

        If fnc1.BlcFnc IsNot Nothing Then
            NaviStatement(fnc1.BlcFnc, arg1)
        End If
    End Sub

    Public Overridable Sub NaviClass(cla1 As TClass, arg1 As Object)
        If (cla1.FldCla.Count <> 0 OrElse cla1.FncCla.Count <> 0) AndAlso Not (cla1.GenCla IsNot Nothing AndAlso cla1.OrgCla Is Nothing) Then

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla
                NaviFunction(fnc1, arg1)
            Next
        End If
    End Sub

    Public Overridable Sub NaviProject(prj As TProject, arg1 As Object)
        '  すべてのクラスに対し
        For Each cla1 In prj.SimpleParameterizedClassList
            NaviClass(cla1, arg1)
        Next
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviTest
Public Class TNaviTest
    Inherits TNavi

    Public Overrides Sub NaviDot(dot1 As TDot, arg1 As Object)
        IncRefCnt(dot1)
        If dot1.TrmDot Is Nothing Then
        Else
            NaviTerm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overrides Sub NaviReference(ref1 As TReference, arg1 As Object)
        IncRefCnt(ref1)
        If arg1 IsNot Nothing Then

            CType(arg1, TStatement).RefStmt.Add(ref1)
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviSetLabel
Public Class TNaviSetLabel
    Inherits TNavi
    Public CurLoop As TFor
    Public InSelect As Boolean
    Public LabelCnt As Integer

    Public Overrides Sub NaviStatement(stmt1 As TStatement, arg1 As Object)
        Dim loop_sv As TFor, in_switch_sv As Boolean

        If TypeOf stmt1 Is TSelect Then

            in_switch_sv = InSelect
            InSelect = True
            NaviSelect(CType(stmt1, TSelect), arg1)

            InSelect = in_switch_sv

        ElseIf TypeOf stmt1 Is TFor Then
            loop_sv = CurLoop
            CurLoop = CType(stmt1, TFor)
            in_switch_sv = InSelect
            InSelect = False

            NaviFor(CType(stmt1, TFor), arg1)

            CurLoop = loop_sv
            InSelect = in_switch_sv

        Else
            Dim exit_label As Boolean

            exit_label = False
            Select Case stmt1.TypeStmt
                Case EToken.eExitDo
                    Debug.Assert(CurLoop IsNot Nothing)
                    If InSelect OrElse Not CurLoop.IsDo Then
                        exit_label = True
                    End If

                Case EToken.eExitFor
                    Debug.Assert(CurLoop IsNot Nothing)
                    If InSelect OrElse CurLoop.IsDo Then
                        exit_label = True
                    End If
            End Select

            If exit_label Then
                If CurLoop.LabelFor = 0 Then

                    LabelCnt = LabelCnt + 1
                    CurLoop.LabelFor = LabelCnt
                End If
                CType(stmt1, TExit).LabelExit = CurLoop.LabelFor
            End If

        End If
    End Sub

    Public Overrides Sub NaviClass(cla1 As TClass, arg1 As Object)
        If (cla1.FldCla.Count <> 0 OrElse cla1.FncCla.Count <> 0) AndAlso Not (cla1.GenCla IsNot Nothing AndAlso cla1.OrgCla Is Nothing) Then
            '  フィールド/メソッドの定義がある場合

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla
                CurLoop = Nothing
                InSelect = False
                LabelCnt = 0
                NaviFunction(fnc1, arg1)
            Next
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviSetRef
Public Class TNaviSetRef
    Inherits TNavi

    Public Sub NavDotLeft(dot1 As TDot, arg1 As Object)
        IncRefCnt(dot1)

        If dot1.TrmDot Is Nothing Then
            Dim with1 As TWith = Nothing

            Dim up_obj As Object = TNaviUp.UpObj(dot1)
            Do While up_obj IsNot Nothing
                If TypeOf up_obj Is TWith Then
                    with1 = CType(up_obj, TWith)
                    Exit Do
                End If

                up_obj = TNaviUp.UpObj(up_obj)
            Loop
            Debug.Assert(with1 IsNot Nothing)

            dot1.TypeDot = dot1.ProjectTrm().GetTermType(with1.TermWith)
        Else

            NaviTerm(dot1.TrmDot, arg1)

            dot1.TypeDot = dot1.ProjectTrm().GetTermType(dot1.TrmDot)
        End If

        If dot1.TypeDot Is Nothing Then
            'NavDotLeft(dot1, arg1)
            Throw New TError(String.Format("ドットの左の項の型が不明 {0}", dot1.NameRef))
        End If
        If TypeOf dot1.TypeDot Is TDelegate Then
            Debug.Assert(dot1.NameRef = "Invoke")
        End If

    End Sub

    Public Overrides Sub NaviDot(dot1 As TDot, arg1 As Object)
        NavDotLeft(dot1, arg1)

        If dot1.TypeDot.IsArray() Then
            Debug.Assert(dot1.ProjectTrm().ArrayType IsNot Nothing)
            dot1.VarRef = TProject.FindFieldFunction(dot1.ProjectTrm().ArrayType, dot1.NameRef, Nothing)
        Else
            dot1.VarRef = TProject.FindFieldFunction(dot1.TypeDot, dot1.NameRef, Nothing)
        End If

        If dot1.VarRef Is Nothing Then
            'NaviDot(dot1, arg1)
            Throw New TError(String.Format("不明なメンバー {0} {1}", dot1.TypeDot.LongName(), dot1.NameRef))
        Else
            If TypeOf dot1.VarRef Is TFunction Then
                Debug.WriteLine("メソッドを値として参照 {0}", dot1.VarRef.NameVar)
            End If
        End If
    End Sub


    Public Sub NavDotMethod(dot1 As TDot, arg1 As Object, varg As TList(Of TTerm))
        NavDotLeft(dot1, arg1)

        If dot1.TypeDot.IsArray() Then
            Debug.Assert(dot1.ProjectTrm().ArrayType IsNot Nothing)
            dot1.VarRef = TProject.FindFieldFunction(dot1.ProjectTrm().ArrayType, dot1.NameRef, varg)
        Else
            dot1.VarRef = TProject.FindFieldFunction(dot1.TypeDot, dot1.NameRef, varg)
        End If

        If dot1.VarRef Is Nothing Then
            'NavDotMethod(dot1, arg1, varg)
            Throw New TError(String.Format("不明なメンバー {0} {1}", dot1.TypeDot.LongName(), dot1.NameRef))
        Else
            If Not TypeOf dot1.VarRef Is TFunction Then
                If dot1.VarRef.TypeVar.HasIndex() Then
                ElseIf TypeOf dot1.VarRef.TypeVar Is TDelegate Then
                Else
                    Debug.Assert(False)
                End If
            End If
        End If
    End Sub

    Public Overrides Sub NaviReference(ref1 As TReference, arg1 As Object)
        IncRefCnt(ref1)
        If ref1.VarRef Is Nothing Then
            ref1.VarRef = TProject.FindFieldFunction(ref1.FunctionTrm.ClaFnc, ref1.NameRef, Nothing)
            If ref1.VarRef Is Nothing Then

                ref1.VarRef = ref1.ProjectTrm().FindVariable(ref1, ref1.NameRef)
                If ref1.VarRef Is Nothing Then
                    Debug.WriteLine("変数未定義:{0}", ref1.NameRef)
                End If
            End If
        End If
    End Sub

    Public Overrides Sub NaviApply(app1 As TApply, arg1 As Object)
        Dim ref1 As TReference, name1 As String = "", dot1 As TDot, cla1 As TClass, fnc1 As TFunction, spr_cla As TClass, base_new As TReference

        If app1 IsNot Nothing Then
            If app1.TypeApp = EToken.eAddressOf Then
                If TypeOf app1.ArgApp(0) Is TDot Then
                    dot1 = CType(app1.ArgApp(0), TDot)
                    NaviTerm(dot1, arg1)
                ElseIf TypeOf app1.ArgApp(0) Is TReference Then
                    ref1 = CType(app1.ArgApp(0), TReference)

                    IncRefCnt(ref1)
                    ref1.VarRef = TProject.FindFieldFunction(app1.FunctionTrm.ClaFnc, ref1.NameRef, Nothing)
                    If ref1.VarRef Is Nothing Then
                        Debug.Print("Address Of 不明のメソッド {0}", ref1.NameRef)
                        Debug.Assert(False)
                    End If
                Else
                    Debug.Assert(False)
                End If
            Else
                ' for Call
                For Each trm1 In app1.ArgApp
                    NaviTerm(trm1, arg1)
                Next
            End If

            If app1.FncApp IsNot Nothing Then

                If TypeOf app1.FncApp Is TDot Then
                    Debug.Assert(app1.TypeApp = EToken.eAppCall)
                ElseIf TypeOf app1.FncApp Is TReference Then
                ElseIf app1.FncApp.IsApp() Then
                Else
                    Debug.Assert(False)
                End If

                Select Case app1.TypeApp
                    Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD
                        Debug.Assert(TypeOf app1.FncApp Is TReference)
                        ref1 = CType(app1.FncApp, TReference)
                        IncRefCnt(ref1)

                        If ref1.VarRef Is Nothing Then

                            ' 演算子オーバーロード関数を得る
                            fnc1 = app1.ProjectTrm().GetOperatorFunction(app1.TypeApp, app1.ArgApp(0))
                            If fnc1 IsNot Nothing Then
                                ' 演算子オーバーロード関数を得られた場合

                                ref1.VarRef = fnc1
                            Else
                                ' 演算子オーバーロード関数を得られない場合

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

                                Dim vvar = (From fnc In app1.ProjectTrm().SystemType.FncCla Where fnc.NameVar = name1).ToList()
                                If vvar.Count <> 1 Then

                                    Debug.WriteLine("演算子未定義:{0}", ref1.NameRef)
                                    Debug.Assert(False)
                                Else
                                    ref1.VarRef = vvar(0)
                                End If
                            End If

                            If ref1.VarRef Is Nothing Then
                                Debug.WriteLine("演算子未定義:{0}", ref1.NameRef)
                            End If
                        End If

                    Case EToken.eAppCall
                        If TypeOf app1.FncApp Is TDot Then
                            NavDotMethod(CType(app1.FncApp, TDot), arg1, app1.ArgApp)
                        ElseIf TypeOf app1.FncApp Is TReference Then
                            ref1 = CType(app1.FncApp, TReference)
                            ref1.VarRef = TProject.FindFieldFunction(app1.FunctionTrm.ClaFnc, ref1.NameRef, app1.ArgApp)
                            If ref1.VarRef IsNot Nothing Then
                                IncRefCnt(ref1)
                            Else
                                NaviReference(ref1, arg1)
                            End If
                        ElseIf TypeOf app1.FncApp Is TApply Then
                            NaviTerm(app1.FncApp, arg1)
                        Else
                            Debug.Assert(False)
                        End If

                        If Not (TypeOf app1.FncApp Is TReference AndAlso TypeOf CType(app1.FncApp, TReference).VarRef Is TFunction) Then
                            ' 関数呼び出しでない場合

                            cla1 = app1.ProjectTrm().GetTermType(app1.FncApp)
                            Debug.Assert(cla1 IsNot Nothing)

                            If cla1.NameCla() = "String" Then
                                app1.KndApp = EApply.eStringApp
                            ElseIf cla1.IsArray() Then
                                app1.KndApp = EApply.eArrayApp
                            ElseIf cla1.IsList() Then
                                app1.KndApp = EApply.eListApp
                            ElseIf cla1.IsDictionary() Then
                                app1.KndApp = EApply.eDictionaryApp
                            End If
                        End If

                    Case EToken.eNew
                        Dim new_ref As TReference = CType(app1.FncApp, TReference)

                        IncRefCnt(new_ref)
                        If app1.NewApp.DimCla = 0 Then
                            new_ref.VarRef = TProject.FindNew(app1.NewApp, app1.ArgApp)
                            'AndAlso app1.ArgApp.Count <> 0 AndAlso app1.NewApp.DimCla = 0
                            If new_ref.VarRef Is Nothing Then
                                Debug.WriteLine("New 未定義 {0} {1}", new_ref.NameRef, app1.ArgApp.Count)
                            End If
                        Else
                            new_ref.VarRef = app1.ProjectTrm().ArrayMaker
                        End If

                    Case EToken.eBaseCall
                        ref1 = CType(app1.FncApp, TReference)
                        ref1.VarRef = TProject.FindFieldFunction(app1.FunctionTrm.ClaFnc.SuperCla(0), ref1.NameRef, app1.ArgApp)
                        Debug.Assert(ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TFunction)
                        IncRefCnt(ref1)

                    Case Else
                        Debug.Assert(False)
                End Select
            Else
                Select Case app1.TypeApp
                    Case EToken.eBaseNew

                        spr_cla = app1.FunctionTrm.ClaFnc.SuperCla(0)
                        base_new = New TReference("New@" + spr_cla.NameCla())
                        base_new.VarRef = TProject.FindNew(spr_cla, app1.ArgApp)
                        If base_new.VarRef Is Nothing Then
                            If app1.ArgApp.Count <> 0 Then

                                Debug.WriteLine("New 未定義 {0} {1}", base_new.NameRef, app1.ArgApp.Count)
                            End If
                        End If
                        app1.FncApp = base_new
                        IncRefCnt(base_new)

                    Case EToken.eCast, EToken.eAddressOf, EToken.eGetType
                    Case Else
                        Debug.Assert(False)
                End Select
            End If

            If app1.IniApp IsNot Nothing Then
                NaviArray(app1.IniApp, arg1)
            End If
        End If
    End Sub

    Public Overrides Sub NaviFrom(frm1 As TFrom, arg1 As Object)
        Dim type1 As TClass

        NaviTerm(frm1.SeqFrom, arg1)
        type1 = frm1.ProjectTrm().GetTermType(frm1.SeqFrom)
        frm1.VarFrom.TypeVar = frm1.ProjectTrm().ElementType(type1)
        Debug.Assert(frm1.VarFrom.TypeVar IsNot Nothing)

        If frm1.CndFrom IsNot Nothing Then

            NaviTerm(frm1.CndFrom, arg1)
        End If

        If frm1.SelFrom IsNot Nothing Then

            NaviTerm(frm1.SelFrom, arg1)
        End If

        If frm1.TakeFrom IsNot Nothing Then

            NaviTerm(frm1.TakeFrom, arg1)
        End If
    End Sub

    Public Overrides Sub NaviAggregate(aggr1 As TAggregate, arg1 As Object)
        Dim type1 As TClass

        NaviTerm(aggr1.SeqAggr, arg1)
        type1 = aggr1.ProjectTrm().GetTermType(aggr1.SeqAggr)
        aggr1.VarAggr.TypeVar = aggr1.ProjectTrm().ElementType(type1)
        Debug.Assert(aggr1.VarAggr.TypeVar IsNot Nothing)

        NaviTerm(aggr1.IntoAggr, arg1)
    End Sub

    Public Overrides Sub NaviFor(for1 As TFor, arg1 As Object)
        Dim type1 As TClass

        If for1.IdxFor IsNot Nothing Then
            NaviTerm(for1.IdxFor, arg1)
        End If

        If for1.InVarFor IsNot Nothing Then
            NaviTerm(for1.InTrmFor, arg1)
            type1 = for1.ProjectStmt().GetTermType(for1.InTrmFor)
            for1.InVarFor.TypeVar = for1.ProjectStmt().ElementType(type1)
            Debug.Assert(for1.InVarFor.TypeVar IsNot Nothing)
        End If

        NaviTerm(for1.FromFor, arg1)
        NaviTerm(for1.ToFor, arg1)
        NaviTerm(for1.StepFor, arg1)

        NaviStatement(for1.IniFor, arg1)
        NaviTerm(for1.CndFor, arg1)
        NaviStatement(for1.StepStmtFor, arg1)
        NaviBlock(for1.BlcFor, arg1)
    End Sub

    Public Overrides Sub NaviStatement(stmt1 As TStatement, arg1 As Object)
        Dim try1 As TTry, with1 As TWith

        Dim if1 As TIf
        Dim red1 As TReDim
        Dim dcl1 As TVariableDeclaration

        If stmt1 IsNot Nothing Then
            If TypeOf stmt1 Is TAssignment Then
                NaviAssignment(CType(stmt1, TAssignment), arg1)
            ElseIf TypeOf stmt1 Is TCall Then
                NaviTerm(CType(stmt1, TCall).AppCall, arg1)
            ElseIf TypeOf stmt1 Is TVariableDeclaration Then
                dcl1 = CType(stmt1, TVariableDeclaration)
                ' for Call
                For Each var1 In dcl1.VarDecl
                    If var1.InitVar IsNot Nothing Then
                        NaviTerm(var1.InitVar, arg1)

                        If var1.TypeVar Is Nothing Then
                            var1.NoType = True
                            var1.TypeVar = TProject.Prj.GetTermType(var1.InitVar)
                        End If
                    End If
                Next
            ElseIf TypeOf stmt1 Is TReDim Then
                red1 = CType(stmt1, TReDim)
                NaviTerm(red1.TrmReDim, arg1)
                ' for Call
                For Each trm_f In red1.DimReDim
                    NaviTerm(trm_f, arg1)
                Next
            ElseIf TypeOf stmt1 Is TIf Then
                if1 = CType(stmt1, TIf)
                ' for Call
                For Each if_blc In if1.IfBlc
                    NaviTerm(if_blc.CndIf, arg1)
                    NaviBlock(if_blc.BlcIf, arg1)
                Next
            ElseIf TypeOf stmt1 Is TBlock Then
                NaviBlock(CType(stmt1, TBlock), arg1)
            ElseIf TypeOf stmt1 Is TSelect Then

                NaviSelect(CType(stmt1, TSelect), arg1)


            ElseIf TypeOf stmt1 Is TCase Then
                NaviCase(CType(stmt1, TCase), arg1)

            ElseIf TypeOf stmt1 Is TTry Then
                try1 = CType(stmt1, TTry)
                NaviBlock(try1.BlcTry, arg1)
                NaviBlock(try1.BlcCatch, arg1)

            ElseIf TypeOf stmt1 Is TWith Then
                with1 = CType(stmt1, TWith)
                NaviTerm(with1.TermWith, arg1)

                NaviBlock(with1.BlcWith, arg1)
            ElseIf TypeOf stmt1 Is TFor Then

                NaviFor(CType(stmt1, TFor), arg1)

            ElseIf TypeOf stmt1 Is TReturn Then
                NaviTerm(CType(stmt1, TReturn).TrmRet, arg1)

            ElseIf TypeOf stmt1 Is TThrow Then
                NaviTerm(CType(stmt1, TThrow).TrmThrow, arg1)

            ElseIf TypeOf stmt1 Is TComment Then
            Else
            End If
        End If
    End Sub

    Public Overrides Function StartFunction(fnc1 As TFunction, arg1 As Object) As Object
        If fnc1.InterfaceFnc IsNot Nothing Then
            fnc1.ImplFnc.VarRef = TProject.FindFieldFunction(fnc1.InterfaceFnc, fnc1.ImplFnc.NameRef, Nothing)
            Debug.Assert(fnc1.ImplFnc IsNot Nothing)
        End If

        Debug.Assert(fnc1.ThisFnc IsNot Nothing)
        Return arg1
    End Function
End Class


' -------------------------------------------------------------------------------- TNaviSetDefRef
Public Class TNaviSetDefRef
    Inherits TNavi

    Public Overrides Function StartAssignment(asn1 As TAssignment, arg1 As Object) As Object
        If TypeOf asn1.RelAsn.ArgApp(0) Is TReference Then
            ' 左辺が変数参照の場合

            CType(asn1.RelAsn.ArgApp(0), TReference).DefRef = True
        Else
            ' 左辺が関数呼び出しの場合

            Debug.Assert(asn1.RelAsn.ArgApp(0).IsApp())
            Dim app1 As TApply = CType(asn1.RelAsn.ArgApp(0), TApply)

            Debug.Assert(app1.KndApp = EApply.eArrayApp OrElse app1.KndApp = EApply.eListApp)
            Debug.Assert(TypeOf app1.FncApp Is TReference)

            CType(app1.FncApp, TReference).DefRef = True
        End If

        Return arg1
    End Function
End Class


' -------------------------------------------------------------------------------- TNaviSetVarRef
Public Class TNaviSetVarRef
    Inherits TNavi

    Public Overrides Sub NaviDot(dot1 As TDot, arg1 As Object)
        IncRefCnt(dot1)

        Debug.Assert(dot1.VarRef IsNot Nothing)
        Debug.Assert(Not dot1.VarRef.RefVar.Contains(dot1))

        dot1.VarRef.RefVar.Add(dot1)

        NaviTerm(dot1.TrmDot, arg1)
    End Sub

    Public Overrides Sub NaviReference(ref1 As TReference, arg1 As Object)
        IncRefCnt(ref1)

        Debug.Assert(ref1.VarRef IsNot Nothing)
        Debug.Assert(Not ref1.VarRef.RefVar.Contains(ref1))

        ref1.VarRef.RefVar.Add(ref1)
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviSetRefFnc
Public Class TNaviSetRefFnc
    Inherits TNavi

    Public Overrides Sub NaviDot(dot1 As TDot, arg1 As Object)
        Dim cur_fnc As TFunction

        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFunction)
        cur_fnc = CType(arg1, TFunction)

        IncRefCnt(dot1)

        dot1.FncRef = cur_fnc
        cur_fnc.RefFnc.Add(dot1)

        If dot1.TrmDot Is Nothing Then
        Else
            NaviTerm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overrides Sub NaviReference(ref1 As TReference, arg1 As Object)
        Dim cur_fnc As TFunction

        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFunction)
        cur_fnc = CType(arg1, TFunction)

        IncRefCnt(ref1)

        ref1.FncRef = cur_fnc
        cur_fnc.RefFnc.Add(ref1)
    End Sub

    Public Overrides Sub NaviFunction(fnc1 As TFunction, arg1 As Object)
        If fnc1.BlcFnc IsNot Nothing Then
            NaviBlock(fnc1.BlcFnc, fnc1)
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviSetCall
Public Class TNaviSetCall
    Inherits TNavi

    Public Sub SetCall(fnc1 As TFunction, ref1 As TReference)
        Dim fnc2 As TFunction

        fnc2 = CType(ref1.VarRef, TFunction)

        If Not fnc1.CallTo.Contains(fnc2) Then
            fnc1.CallTo.Add(fnc2)
        End If
        If Not fnc2.CallFrom.Contains(fnc1) Then
            fnc2.CallFrom.Add(fnc1)
        End If
    End Sub

    Public Overrides Sub NaviDot(dot1 As TDot, arg1 As Object)
        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFunction)

        IncRefCnt(dot1)

        If dot1.VarRef IsNot Nothing AndAlso TypeOf dot1.VarRef Is TFunction Then

            SetCall(CType(arg1, TFunction), dot1)
        End If

        If dot1.TrmDot Is Nothing Then
        Else
            NaviTerm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overrides Sub NaviReference(ref1 As TReference, arg1 As Object)
        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFunction)

        IncRefCnt(ref1)
        If ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TFunction Then

            SetCall(CType(arg1, TFunction), ref1)
        End If
    End Sub


    Public Overrides Sub NaviFunction(fnc1 As TFunction, arg1 As Object)
        Debug.Assert(arg1 Is Nothing)
        If fnc1.BlcFnc IsNot Nothing Then
            NaviBlock(fnc1.BlcFnc, fnc1)
        End If
    End Sub

End Class

' -------------------------------------------------------------------------------- TNaviSetFunction
Public Class TNaviSetFunction
    Inherits TNavi
    Public FunctionNavi As TFunction

    Public Overrides Function StartTerm(trm1 As TTerm, arg1 As Object) As Object
        If trm1 IsNot Nothing Then
            trm1.FunctionTrm = FunctionNavi
        End If

        Return arg1
    End Function

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then
            stmt1.FunctionStmt = FunctionNavi
        End If

        Return arg1
    End Function

    Public Overrides Function StartFunction(fnc1 As TFunction, arg1 As Object) As Object
        FunctionNavi = fnc1

        Return arg1
    End Function
End Class

' -------------------------------------------------------------------------------- TNaviSetParentStmt
Public Class TNaviSetParentStmt
    Inherits TNavi

    Public Overrides Function StartLocalVariable(var1 As TVariable, arg1 As Object) As Object
        If var1 IsNot Nothing Then
            var1.UpVar = arg1
        End If

        Return var1
    End Function

    Public Overrides Function StartTerm(trm1 As TTerm, arg1 As Object) As Object
        If trm1 IsNot Nothing Then
            trm1.UpTrm = arg1
        End If

        Return trm1
    End Function

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then
            stmt1.ParentStmt = arg1
        End If

        Return stmt1
    End Function

    Public Overrides Function StartIf(if1 As TIf, arg1 As Object) As Object
        If if1 IsNot Nothing Then
            if1.IfBlc.UpList = if1
        End If

        Return if1
    End Function

    Public Overrides Function StartFunction(fnc1 As TFunction, arg1 As Object) As Object
        Return fnc1
    End Function

    Public Overrides Function StartLocalVariableList(list1 As TList(Of TVariable), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function

    Public Overrides Function StartTermList(list1 As TList(Of TTerm), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function

    Public Overrides Function StartCaseList(list1 As TList(Of TCase), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function

    Public Overrides Function StartStatementList(list1 As TList(Of TStatement), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function

    Public Overrides Function StartBlockList(list1 As TList(Of TBlock), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function
End Class


' -------------------------------------------------------------------------------- TNaviSetUpTrm
Public Class TNaviSetUpTrm
    Inherits TNavi
    Public NavUp As New TNaviUp

    Public Sub Test(ref1 As TReference)
        Dim stmt1 As TStatement

        NavUp.UpToFnc(ref1)

        stmt1 = NavUp.UpToStmt(ref1)
    End Sub

    Public Overrides Function StartReference(ref1 As TReference, arg1 As Object) As Object
        Test(ref1)
        Return arg1
    End Function

    Public Overrides Function StartDot(dot1 As TDot, arg1 As Object) As Object
        Test(dot1)
        Return arg1
    End Function

End Class


' -------------------------------------------------------------------------------- TNaviUp
Public Class TNaviUp
    Public Shared Function UpObj(obj As Object) As Object
        If TypeOf obj Is TVariable Then
            Return CType(obj, TVariable).UpVar
        ElseIf TypeOf obj Is TTerm Then
            Return CType(obj, TTerm).UpTrm
        ElseIf TypeOf obj Is TStatement Then
            Return CType(obj, TStatement).ParentStmt
        ElseIf TypeOf obj Is TList(Of TVariable) Then
            Return CType(obj, TList(Of TVariable)).UpList
        ElseIf TypeOf obj Is TList(Of TTerm) Then
            Return CType(obj, TList(Of TTerm)).UpList
        ElseIf TypeOf obj Is TList(Of TCase) Then
            Return CType(obj, TList(Of TCase)).UpList
        ElseIf TypeOf obj Is TList(Of TStatement) Then
            Return CType(obj, TList(Of TStatement)).UpList
        ElseIf TypeOf obj Is TList(Of TBlock) Then
            Return CType(obj, TList(Of TBlock)).UpList
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Function UpToFnc(obj1 As Object) As TFunction
        Dim obj2 As Object

        obj2 = obj1
        Do While obj2 IsNot Nothing AndAlso Not TypeOf obj2 Is TFunction
            obj2 = UpObj(obj2)
        Loop

        Debug.Assert(obj2 IsNot Nothing AndAlso TypeOf obj2 Is TFunction)

        Return CType(obj2, TFunction)
    End Function

    Public Function UpToStmt(obj1 As Object) As TStatement
        Dim obj2 As Object

        obj2 = obj1
        Do While obj2 IsNot Nothing AndAlso Not TypeOf obj2 Is TStatement
            obj2 = UpObj(obj2)
        Loop

        Debug.Assert(obj2 IsNot Nothing AndAlso TypeOf obj2 Is TStatement)

        Return CType(obj2, TStatement)
    End Function
End Class

' -------------------------------------------------------------------------------- TNaviClearUsedStmt
Public Class TNaviClearUsedStmt
    Inherits TNavi

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        stmt1.UsedStmt = False
        Return arg1
    End Function

End Class

' -------------------------------------------------------------------------------- TNaviAllStmt
Public Class TNaviAllStmt
    Inherits TNavi
    Public AllStmts As New TList(Of TStatement)

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then
            AllStmts.Add(stmt1)
        End If

        Return arg1
    End Function
End Class

' -------------------------------------------------------------------------------- TNaviSetValidStmt
Public Class TNaviSetValidStmt
    Inherits TNavi

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then

            Dim up_stmt As TStatement = TDataflow.UpStmtProper(stmt1.ParentStmt)

            If up_stmt IsNot Nothing AndAlso Not up_stmt.ValidStmt Then
                ' 親の文が無効の場合

                ' この文も無効とする。
                stmt1.ValidStmt = False
            End If
        End If

        Return arg1
    End Function
End Class

' -------------------------------------------------------------------------------- TNaviSetValidStmt
Public Class TNaviMakeVirtualMethod
    Inherits TNavi
    Public InnermostClass As TClass
    Public VirtualSeparable As Boolean
    Public CurrentVar As TVariable
    Public StmtToInnermostClass As New Dictionary(Of TStatement, TClass)

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then
            StmtToInnermostClass.Add(stmt1, InnermostClass)
        End If

        Return stmt1
    End Function

    Public Overrides Sub NaviIfBlock(if_blc As TIfBlock, arg1 As Object)
        Dim innermost_class As TClass = InnermostClass

        If VirtualSeparable Then
            ' 仮想関数に分離可能の場合

            If TypeOf (if_blc.CndIf) Is TApply Then
                Dim app1 As TApply = CType(if_blc.CndIf, TApply)

                If app1.TypeApp = EToken.eTypeof AndAlso TypeOf app1.ArgApp(0) Is TReference Then
                    Dim ref1 As TReference = CType(app1.ArgApp(0), TReference)

                    If ref1.VarRef Is CurrentVar Then
                        InnermostClass = CType(CType(app1.ArgApp(1), TReference).VarRef, TClass)
                    End If

                End If
            End If
        End If
        NaviTerm(if_blc.CndIf, arg1)
        NaviStatement(if_blc.BlcIf, arg1)

        InnermostClass = innermost_class
    End Sub
End Class
