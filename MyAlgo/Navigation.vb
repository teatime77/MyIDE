Imports System.Diagnostics

' -------------------------------------------------------------------------------- TNavPrj
Public Class TNavPrj
    Public RefCnt As Integer
    Public ErrNav As Boolean = False
    Public CurrentWith As TWith

    Public Sub IncRefCnt(ref1 As TRef)
        RefCnt += 1
    End Sub


    Public Overridable Function StartRef(ref1 As TRef, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartDot(dot1 As TDot, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartLocalVar(var1 As TVar, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartTerm(trm1 As TTerm, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartCase(case1 As TCase, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartStmt(stmt1 As TStmt, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartIf(if1 As TIf, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartFnc(fnc1 As TFnc, arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartLocalVarList(list1 As TList(Of TVar), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartTermList(list1 As TList(Of TTerm), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartCaseList(list1 As TList(Of TCase), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartStmtList(list1 As TList(Of TStmt), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartBlcList(list1 As TList(Of TBlc), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Function StartIfBlcList(list1 As TList(Of TIfBlc), arg1 As Object) As Object
        Return arg1
    End Function

    Public Overridable Sub EndStmt(stmt1 As TStmt, arg1 As Object)
    End Sub

    Public Overridable Sub NavDot(dot1 As TDot, arg1 As Object)
        arg1 = StartDot(dot1, arg1)

        If dot1.TrmDot Is Nothing Then
            '            Debug.Assert(CurrentWith IsNot Nothing)
        Else
            NavTrm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overridable Sub NavRef(ref1 As TRef, arg1 As Object)
        StartRef(ref1, arg1)
    End Sub

    Public Overridable Sub NavTrm(trm1 As TTerm, arg1 As Object)
        arg1 = StartTerm(trm1, arg1)

        If trm1 IsNot Nothing Then
            Try
                If TypeOf trm1 Is TCns Then
                ElseIf TypeOf trm1 Is TArray Then
                    NavArr(CType(trm1, TArray), arg1)
                ElseIf TypeOf trm1 Is TDot Then
                    NavDot(CType(trm1, TDot), arg1)
                ElseIf TypeOf trm1 Is TRef Then
                    NavRef(CType(trm1, TRef), arg1)
                ElseIf trm1.IsApp() Then
                    NavApp(CType(trm1, TApp), arg1)
                ElseIf trm1.IsLog() Then
                    NavLog(CType(trm1, TApp), arg1)
                ElseIf TypeOf trm1 Is TPar Then
                    NavTrm(CType(trm1, TPar).TrmPar, arg1)
                ElseIf TypeOf trm1 Is TFrom Then
                    NavFrom(CType(trm1, TFrom), arg1)
                ElseIf TypeOf trm1 Is TAggregate Then
                    NavAggregate(CType(trm1, TAggregate), arg1)
                Else
                    Debug.Assert(False)
                End If
            Catch ex As TErr
                ErrNav = True
            End Try
        End If
    End Sub

    Public Overridable Sub NavTermList(list1 As TList(Of TTerm), arg1 As Object)
        arg1 = StartTermList(list1, arg1)

        For Each trm1 In list1
            NavTrm(trm1, arg1)
        Next
    End Sub

    Public Overridable Sub NavCaseList(list1 As TList(Of TCase), arg1 As Object)
        arg1 = StartCaseList(list1, arg1)

        For Each cas1 In list1
            NavStmt(cas1, arg1)
        Next
    End Sub

    Public Overridable Sub NavStmtList(list1 As TList(Of TStmt), arg1 As Object)
        arg1 = StartStmtList(list1, arg1)

        For Each stmt1 In list1
            NavStmt(stmt1, arg1)
        Next
    End Sub

    Public Overridable Sub NavBlcList(list1 As TList(Of TBlc), arg1 As Object)
        arg1 = StartBlcList(list1, arg1)

        For Each blc_f In list1
            NavStmt(blc_f, arg1)
        Next
    End Sub

    Public Overridable Sub NavIfBlcList(list1 As TList(Of TIfBlc), arg1 As Object)
        arg1 = StartIfBlcList(list1, arg1)

        For Each blc_f In list1
            NavStmt(blc_f, arg1)
        Next
    End Sub

    Public Overridable Sub NavLocalVarList(list1 As TList(Of TVar), arg1 As Object)
        arg1 = StartLocalVarList(list1, arg1)

        For Each var1 In list1
            NavLocalVar(var1, arg1)
        Next
    End Sub

    Public Overridable Sub NavLocalVar(var1 As TVar, arg1 As Object)
        arg1 = StartLocalVar(var1, arg1)

        NavTrm(var1.InitVar, arg1)
    End Sub

    Public Overridable Sub NavArr(arr1 As TArray, arg1 As Object)
        If arr1 IsNot Nothing Then
            NavTermList(arr1.TrmArr, arg1)
        End If
    End Sub

    Public Overridable Sub NavApp(app1 As TApp, arg1 As Object)
        Dim ref1 As TRef, dot1 As TDot

        If app1 IsNot Nothing Then
            If app1.TypeApp = ETkn.eAddressOf Then
                If TypeOf app1.ArgApp(0) Is TDot Then
                    dot1 = CType(app1.ArgApp(0), TDot)
                    NavTrm(dot1, arg1)
                ElseIf TypeOf app1.ArgApp(0) Is TRef Then
                    ref1 = CType(app1.ArgApp(0), TRef)
                    NavTrm(ref1, arg1)
                Else
                    Debug.Assert(False)
                End If
            Else
                NavTermList(app1.ArgApp, arg1)
            End If

            If app1.FncApp IsNot Nothing Then
                NavTrm(app1.FncApp, arg1)
            End If

            If app1.IniApp IsNot Nothing Then
                NavTrm(app1.IniApp, arg1)
            End If
        End If
    End Sub

    Public Overridable Sub NavLog(opr1 As TApp, arg1 As Object)
        If opr1 IsNot Nothing Then
            NavTermList(opr1.ArgApp, arg1)
        End If
    End Sub

    Public Overridable Sub NavFrom(frm1 As TFrom, arg1 As Object)
        NavTrm(frm1.SeqFrom, arg1)
        NavTrm(frm1.CndFrom, arg1)
        NavTrm(frm1.SelFrom, arg1)
        NavTrm(frm1.TakeFrom, arg1)
    End Sub

    Public Overridable Sub NavAggregate(aggr1 As TAggregate, arg1 As Object)
        NavTrm(aggr1.SeqAggr, arg1)
        NavTrm(aggr1.IntoAggr, arg1)
    End Sub

    Public Overridable Sub NavFor(for1 As TFor, arg1 As Object)
        NavTrm(for1.IdxFor, arg1)
        NavTrm(for1.InTrmFor, arg1)
        NavTrm(for1.FromFor, arg1)
        NavTrm(for1.ToFor, arg1)
        NavTrm(for1.StepFor, arg1)
        NavStmt(for1.IniFor, arg1)
        NavTrm(for1.CndFor, arg1)
        NavStmt(for1.StepStmtFor, arg1)
        NavStmt(for1.BlcFor, arg1)
    End Sub

    Public Overridable Sub NavCase(cas1 As TCase, arg1 As Object)
        arg1 = StartCase(cas1, arg1)

        NavTermList(cas1.TrmCase, arg1)
        NavStmt(cas1.BlcCase, arg1)
    End Sub

    Public Overridable Sub NavSelect(swt1 As TSelect, arg1 As Object)
        NavTrm(swt1.TrmSel, arg1)
        NavCaseList(swt1.CaseSel, arg1)
    End Sub

    Public Overridable Sub NavAsn(asn1 As TAsn, arg1 As Object)
        NavTrm(asn1.RelAsn, arg1)
        If TypeOf asn1.RelAsn.ArgApp(0) Is TRef Then
            CType(asn1.RelAsn.ArgApp(0), TRef).DefRef = True
        End If
    End Sub

    Public Overridable Sub NavCall(call1 As TCall, arg1 As Object)
        NavTrm(call1.AppCall, arg1)
    End Sub

    Public Overridable Sub NavReDim(red1 As TReDim, arg1 As Object)
        NavTrm(red1.TrmReDim, arg1)
        NavTermList(red1.DimReDim, arg1)
    End Sub

    Public Overridable Sub NavIf(if1 As TIf, arg1 As Object)
        arg1 = StartIf(if1, arg1)
        NavIfBlcList(if1.IfBlc, arg1)
    End Sub

    Public Overridable Sub NavIfBlc(if_blc As TIfBlc, arg1 As Object)
        NavTrm(if_blc.CndIf, arg1)
        NavStmt(if_blc.BlcIf, arg1)
    End Sub

    Public Overridable Sub NavTry(try1 As TTry, arg1 As Object)
        NavStmt(try1.BlcTry, arg1)
        NavStmt(try1.BlcCatch, arg1)
    End Sub

    Public Overridable Sub NavWith(with1 As TWith, arg1 As Object)
        Dim with_sv As TWith

        with_sv = CurrentWith
        CurrentWith = with1

        NavTrm(with1.TermWith, arg1)
        NavStmt(with1.BlcWith, arg1)

        CurrentWith = with_sv
    End Sub

    Public Overridable Sub NavVarDecl(dcl1 As TVarDecl, arg1 As Object)
        NavLocalVarList(dcl1.VarDecl, arg1)
    End Sub

    Public Overridable Sub NavRet(ret1 As TRet, arg1 As Object)
        NavTrm(ret1.TrmRet, arg1)
    End Sub

    Public Overridable Sub NavThrow(throw1 As TThrow, arg1 As Object)
        NavTrm(throw1.TrmThrow, arg1)
    End Sub

    Public Overridable Sub NavBlc(blc1 As TBlc, arg1 As Object)
        NavStmtList(blc1.StmtBlc, arg1)
    End Sub

    Public Overridable Sub NavStmt(stmt1 As TStmt, arg1 As Object)

        arg1 = StartStmt(stmt1, arg1)

        If stmt1 IsNot Nothing Then

            If TypeOf stmt1 Is TAsn Then
                NavAsn(CType(stmt1, TAsn), arg1)
            ElseIf TypeOf stmt1 Is TCall Then
                NavCall(CType(stmt1, TCall), arg1)
            ElseIf TypeOf stmt1 Is TVarDecl Then
                NavVarDecl(CType(stmt1, TVarDecl), arg1)
            ElseIf TypeOf stmt1 Is TReDim Then
                NavReDim(CType(stmt1, TReDim), arg1)
            ElseIf TypeOf stmt1 Is TIf Then
                NavIf(CType(stmt1, TIf), arg1)
            ElseIf TypeOf stmt1 Is TIfBlc Then
                NavIfBlc(CType(stmt1, TIfBlc), arg1)
            ElseIf TypeOf stmt1 Is TSelect Then
                NavSelect(CType(stmt1, TSelect), arg1)
            ElseIf TypeOf stmt1 Is TCase Then
                NavCase(CType(stmt1, TCase), arg1)
            ElseIf TypeOf stmt1 Is TTry Then
                NavTry(CType(stmt1, TTry), arg1)
            ElseIf TypeOf stmt1 Is TWith Then
                NavWith(CType(stmt1, TWith), arg1)
            ElseIf TypeOf stmt1 Is TFor Then
                NavFor(CType(stmt1, TFor), arg1)
            ElseIf TypeOf stmt1 Is TBlc Then
                NavBlc(CType(stmt1, TBlc), arg1)
            ElseIf TypeOf stmt1 Is TRet Then
                NavRet(CType(stmt1, TRet), arg1)
            ElseIf TypeOf stmt1 Is TThrow Then
                NavThrow(CType(stmt1, TThrow), arg1)
            ElseIf TypeOf stmt1 Is TComment Then
            Else
                Select Case stmt1.TypeStmt
                    Case ETkn.eExitDo

                    Case ETkn.eExitFor

                    Case ETkn.eExitSub

                    Case Else
                        Debug.Assert(False)
                End Select
            End If
        End If

        EndStmt(stmt1, arg1)
    End Sub

    Public Overridable Sub NavFnc(fnc1 As TFnc, arg1 As Object)
        arg1 = StartFnc(fnc1, arg1)

        If fnc1.BlcFnc IsNot Nothing Then
            NavStmt(fnc1.BlcFnc, arg1)
        End If
    End Sub

    Public Overridable Sub NavCla(cla1 As TCls, arg1 As Object)
        If (cla1.FldCla.Count <> 0 OrElse cla1.FncCla.Count <> 0) AndAlso Not (cla1.GenCla IsNot Nothing AndAlso cla1.OrgCla Is Nothing) Then

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla
                NavFnc(fnc1, arg1)
            Next
        End If
    End Sub

    Public Overridable Sub NavPrj(prj As TPrj, arg1 As Object)
        '  すべてのクラスに対し
        For Each cla1 In prj.vCla
            NavCla(cla1, arg1)
        Next
    End Sub
End Class

' -------------------------------------------------------------------------------- TNavTest
Public Class TNavTest
    Inherits TNavPrj

    Public Overrides Sub NavDot(dot1 As TDot, arg1 As Object)
        IncRefCnt(dot1)
        If dot1.TrmDot Is Nothing Then
            Debug.Assert(CurrentWith IsNot Nothing)
        Else
            NavTrm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overrides Sub NavRef(ref1 As TRef, arg1 As Object)
        IncRefCnt(ref1)
        If arg1 IsNot Nothing Then

            CType(arg1, TStmt).RefStmt.Add(ref1)
        End If
    End Sub
End Class



' -------------------------------------------------------------------------------- TNavSetRef
Public Class TNavSetRef
    Inherits TNavPrj
    Public PrjSetRef As TPrj
    Public CurFncPrj As TFnc
    Public CurLoop As TFor
    Public InSelect As Boolean
    Public LabelCnt As Integer

    Public Sub NavDotLeft(dot1 As TDot, vvvar As TList(Of TList(Of TVar)))
        IncRefCnt(dot1)

        If dot1.TrmDot Is Nothing Then
            Debug.Assert(CurrentWith IsNot Nothing)

            dot1.TypeDot = PrjSetRef.GetTermType(CurrentWith.TermWith)
        Else

            NavTrm(dot1.TrmDot, vvvar)

            dot1.TypeDot = PrjSetRef.GetTermType(dot1.TrmDot)
        End If

        If dot1.TypeDot Is Nothing Then
            Throw New TErr(String.Format("ドットの左の項の型が不明 {0}", dot1.NameRef))
        End If
        If TypeOf dot1.TypeDot Is TDelegate Then
            Debug.Assert(dot1.NameRef = "Invoke")
        End If

    End Sub

    Public Overrides Sub NavDot(dot1 As TDot, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        NavDotLeft(dot1, vvvar)

        If dot1.TypeDot.IsArray() Then
            If PrjSetRef.ArrayType Is Nothing Then
                Debug.Assert(False)
            Else
                dot1.VarRef = TPrj.FindFieldFunction(PrjSetRef.ArrayType, dot1.NameRef, Nothing)
            End If
        Else
            dot1.VarRef = TPrj.FindFieldFunction(dot1.TypeDot, dot1.NameRef, Nothing)
        End If

        If dot1.VarRef Is Nothing Then
            'NavDot(dot1, vvvar)
            Throw New TErr(String.Format("不明なメンバー {0} {1}", dot1.TypeDot.LongName(), dot1.NameRef))
        Else
            If TypeOf dot1.VarRef Is TFnc Then
                Debug.WriteLine("メソッドを値として参照 {0}", dot1.VarRef.NameVar)
            End If
        End If
    End Sub


    Public Sub NavDot2(dot1 As TDot, arg1 As Object, varg As TList(Of TTerm))
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        NavDotLeft(dot1, vvvar)

        If dot1.TypeDot.IsArray() Then
            If PrjSetRef.ArrayType Is Nothing Then
                Debug.Assert(False)
            Else
                dot1.VarRef = TPrj.FindFieldFunction(PrjSetRef.ArrayType, dot1.NameRef, varg)
            End If
        Else
            dot1.VarRef = TPrj.FindFieldFunction(dot1.TypeDot, dot1.NameRef, varg)
        End If

        If dot1.VarRef Is Nothing Then
            NavDot2(dot1, vvvar, varg)
            Throw New TErr(String.Format("不明なメンバー {0} {1}", dot1.TypeDot.LongName(), dot1.NameRef))
        Else
            If Not TypeOf dot1.VarRef Is TFnc Then
                If dot1.VarRef.TypeVar.HasIndex() Then
                ElseIf TypeOf dot1.VarRef.TypeVar Is TDelegate Then
                Else
                    Debug.Assert(False)
                End If
            End If
        End If
    End Sub

    Public Overrides Sub NavRef(ref1 As TRef, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        IncRefCnt(ref1)
        If ref1.VarRef Is Nothing Then
            ref1.VarRef = PrjSetRef.FindVariable(ref1.NameRef, vvvar)
            If ref1.VarRef Is Nothing Then
                Debug.WriteLine("変数未定義:{0}", ref1.NameRef)
            End If
        End If
    End Sub

    Public Overrides Sub NavApp(app1 As TApp, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar)), new_ref As TRef

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        Dim ref1 As TRef, name1 As String = "", dot1 As TDot, cla1 As TCls, fnc1 As TFnc, spr_cla As TCls, base_new As TRef

        If app1 IsNot Nothing Then
            If app1.TypeApp = ETkn.eAddressOf Then
                If TypeOf app1.ArgApp(0) Is TDot Then
                    dot1 = CType(app1.ArgApp(0), TDot)
                    NavTrm(dot1, vvvar)
                ElseIf TypeOf app1.ArgApp(0) Is TRef Then
                    ref1 = CType(app1.ArgApp(0), TRef)
                    NavTrm(ref1, vvvar)
                Else
                    Debug.Assert(False)
                End If
            Else
                ' for Call
                For Each trm1 In app1.ArgApp
                    NavTrm(trm1, vvvar)
                Next
            End If

            If app1.FncApp IsNot Nothing Then

                If TypeOf app1.FncApp Is TDot Then
                    Debug.Assert(app1.TypeApp = ETkn.eAppCall)
                ElseIf TypeOf app1.FncApp Is TRef Then
                ElseIf app1.FncApp.IsApp() Then
                Else
                    Debug.Assert(False)
                End If

                Select Case app1.TypeApp
                    Case ETkn.eADD, ETkn.eMns, ETkn.eMUL, ETkn.eDIV, ETkn.eMOD
                        Debug.Assert(TypeOf app1.FncApp Is TRef)
                        ref1 = CType(app1.FncApp, TRef)
                        IncRefCnt(ref1)

                        If ref1.VarRef Is Nothing Then

                            ' 演算子オーバーロード関数を得る
                            fnc1 = PrjSetRef.GetOperatorFunction(app1.TypeApp, app1.ArgApp(0))
                            If fnc1 IsNot Nothing Then
                                ' 演算子オーバーロード関数を得られた場合

                                ref1.VarRef = fnc1
                            Else
                                ' 演算子オーバーロード関数を得られない場合

                                Select Case app1.TypeApp
                                    Case ETkn.eADD
                                        name1 = "__Add"
                                    Case ETkn.eMns
                                        name1 = "__Mns"
                                    Case ETkn.eMUL
                                        name1 = "__Mul"
                                    Case ETkn.eDIV
                                        name1 = "__Div"
                                    Case ETkn.eMOD
                                        name1 = "__Mod"
                                End Select

                                ref1.VarRef = PrjSetRef.FindVariable(name1, vvvar)
                            End If

                            If ref1.VarRef Is Nothing Then
                                Debug.WriteLine("演算子未定義:{0}", ref1.NameRef)
                            End If
                        End If

                    Case ETkn.eAppCall
                        If TypeOf app1.FncApp Is TDot Then
                            NavDot2(app1.FncApp, vvvar, app1.ArgApp)
                        ElseIf TypeOf app1.FncApp Is TRef Then
                            ref1 = CType(app1.FncApp, TRef)
                            ref1.VarRef = TPrj.FindFieldFunction(CurFncPrj.ClaFnc, ref1.NameRef, app1.ArgApp)
                            If ref1.VarRef IsNot Nothing Then
                                IncRefCnt(ref1)
                            Else
                                NavRef(ref1, vvvar)
                            End If
                        ElseIf TypeOf app1.FncApp Is TApp Then
                            NavTrm(app1.FncApp, vvvar)
                        Else
                            Debug.Assert(False)
                        End If

                        If Not (TypeOf app1.FncApp Is TRef AndAlso TypeOf CType(app1.FncApp, TRef).VarRef Is TFnc) Then
                            ' 関数呼び出しでない場合

                            cla1 = PrjSetRef.GetTermType(app1.FncApp)
                            Debug.Assert(cla1 IsNot Nothing)

                            If cla1.NameCla() = "String" Then
                                app1.KndApp = EApp.eStringApp
                            ElseIf cla1.IsArray() Then
                                app1.KndApp = EApp.eArrayApp
                            ElseIf cla1.IsList() Then
                                app1.KndApp = EApp.eListApp
                            ElseIf cla1.IsDictionary() Then
                                app1.KndApp = EApp.eDictionaryApp
                            End If
                        End If

                    Case ETkn.eNew
                        new_ref = CType(app1.FncApp, TRef)
                        IncRefCnt(new_ref)
                        If app1.NewApp.DimCla = 0 Then
                            new_ref.VarRef = TPrj.FindNew(app1.NewApp, app1.ArgApp)
                            'AndAlso app1.ArgApp.Count <> 0 AndAlso app1.NewApp.DimCla = 0
                            If new_ref.VarRef Is Nothing Then
                                Debug.WriteLine("New 未定義 {0} {1}", new_ref.NameRef, app1.ArgApp.Count)
                            End If
                        Else
                            new_ref.VarRef = PrjSetRef.ArrayMaker
                        End If

                    Case ETkn.eBaseCall
                        ref1 = CType(app1.FncApp, TRef)
                        ref1.VarRef = TPrj.FindFieldFunction(CurFncPrj.ClaFnc.SuperCla(0), ref1.NameRef, app1.ArgApp)
                        Debug.Assert(ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TFnc)
                        IncRefCnt(ref1)

                    Case Else
                        Debug.Assert(False)
                End Select
            Else
                Select Case app1.TypeApp
                    Case ETkn.eBaseNew

                        spr_cla = CurFncPrj.ClaFnc.SuperCla(0)
                        base_new = New TRef("New@" + spr_cla.NameCla())
                        base_new.VarRef = TPrj.FindNew(spr_cla, app1.ArgApp)
                        If base_new.VarRef Is Nothing Then
                            If app1.ArgApp.Count <> 0 Then

                                Debug.WriteLine("New 未定義 {0} {1}", base_new.NameRef, app1.ArgApp.Count)
                            End If
                        End If
                        app1.FncApp = base_new
                        IncRefCnt(base_new)

                    Case ETkn.eCast, ETkn.eAddressOf, ETkn.eGetType
                    Case Else
                        Debug.Assert(False)
                End Select
            End If

            If app1.IniApp IsNot Nothing Then
                NavArr(app1.IniApp, vvvar)
            End If
        End If
    End Sub

    Public Overrides Sub NavFrom(frm1 As TFrom, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))
        Dim vvar As New TList(Of TVar), type1 As TCls

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        NavTrm(frm1.SeqFrom, vvvar)
        type1 = PrjSetRef.GetTermType(frm1.SeqFrom)
        frm1.VarFrom.TypeVar = PrjSetRef.ElementType(type1)
        Debug.Assert(frm1.VarFrom.TypeVar IsNot Nothing)

        vvar.Add(frm1.VarFrom)
        vvvar.Insert(0, vvar)

        If frm1.CndFrom IsNot Nothing Then

            NavTrm(frm1.CndFrom, vvvar)
        End If

        If frm1.SelFrom IsNot Nothing Then

            NavTrm(frm1.SelFrom, vvvar)
        End If

        If frm1.TakeFrom IsNot Nothing Then

            NavTrm(frm1.TakeFrom, vvvar)
        End If

        vvvar.RemoveAt(0)
    End Sub

    Public Overrides Sub NavAggregate(aggr1 As TAggregate, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))
        Dim vvar As New TList(Of TVar), type1 As TCls

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        NavTrm(aggr1.SeqAggr, vvvar)
        type1 = PrjSetRef.GetTermType(aggr1.SeqAggr)
        aggr1.VarAggr.TypeVar = PrjSetRef.ElementType(type1)
        Debug.Assert(aggr1.VarAggr.TypeVar IsNot Nothing)

        vvar.Add(aggr1.VarAggr)
        vvvar.Insert(0, vvar)

        NavTrm(aggr1.IntoAggr, vvvar)

        vvvar.RemoveAt(0)
    End Sub

    Public Overrides Sub NavFor(for1 As TFor, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        Dim vvar As New TList(Of TVar), type1 As TCls

        If for1.IdxFor IsNot Nothing Then
            NavTrm(for1.IdxFor, vvvar)
        End If

        If for1.InVarFor IsNot Nothing Then
            NavTrm(for1.InTrmFor, vvvar)
            type1 = PrjSetRef.GetTermType(for1.InTrmFor)
            for1.InVarFor.TypeVar = PrjSetRef.ElementType(type1)
            Debug.Assert(for1.InVarFor.TypeVar IsNot Nothing)
            vvar.Add(for1.InVarFor)
        End If

        vvvar.Insert(0, vvar)
        NavTrm(for1.FromFor, vvvar)
        NavTrm(for1.ToFor, vvvar)
        NavTrm(for1.StepFor, vvvar)

        NavStmt(for1.IniFor, vvvar)
        NavTrm(for1.CndFor, vvvar)
        NavStmt(for1.StepStmtFor, vvvar)
        NavBlc(for1.BlcFor, vvvar)
        vvvar.RemoveAt(0)
    End Sub

    Public Overrides Sub NavAsn(asn1 As TAsn, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        Dim app1 As TApp

        NavLog(asn1.RelAsn, vvvar)

        If TypeOf asn1.RelAsn.ArgApp(0) Is TRef Then
            ' 左辺が変数参照の場合

            CType(asn1.RelAsn.ArgApp(0), TRef).DefRef = True
        Else
            ' 左辺が関数呼び出しの場合

            Debug.Assert(asn1.RelAsn.ArgApp(0).IsApp())
            app1 = CType(asn1.RelAsn.ArgApp(0), TApp)

            Debug.Assert(app1.KndApp = EApp.eArrayApp OrElse app1.KndApp = EApp.eListApp)
            Debug.Assert(TypeOf app1.FncApp Is TRef)

            CType(app1.FncApp, TRef).DefRef = True
        End If
    End Sub

    Public Overrides Sub NavStmt(stmt1 As TStmt, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))
        Dim try1 As TTry, with1 As TWith, with_sv As TWith

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        Dim if1 As TIf
        Dim red1 As TReDim
        Dim loop_sv As TFor, in_switch_sv As Boolean, exit_label As Boolean
        Dim dcl1 As TVarDecl

        If stmt1 IsNot Nothing Then
            If TypeOf stmt1 Is TAsn Then
                NavAsn(CType(stmt1, TAsn), vvvar)
            ElseIf TypeOf stmt1 Is TCall Then
                NavTrm(CType(stmt1, TCall).AppCall, vvvar)
            ElseIf TypeOf stmt1 Is TVarDecl Then
                dcl1 = CType(stmt1, TVarDecl)
                ' for Call
                For Each var1 In dcl1.VarDecl
                    If var1.InitVar IsNot Nothing Then
                        NavTrm(var1.InitVar, vvvar)

                        If var1.TypeVar Is Nothing Then
                            var1.NoType = True
                            var1.TypeVar = TPrj.Prj.GetTermType(var1.InitVar)
                        End If
                    End If
                Next
            ElseIf TypeOf stmt1 Is TReDim Then
                red1 = CType(stmt1, TReDim)
                NavTrm(red1.TrmReDim, vvvar)
                ' for Call
                For Each trm_f In red1.DimReDim
                    NavTrm(trm_f, vvvar)
                Next
            ElseIf TypeOf stmt1 Is TIf Then
                if1 = CType(stmt1, TIf)
                ' for Call
                For Each if_blc In if1.IfBlc
                    NavTrm(if_blc.CndIf, vvvar)
                    NavBlc(if_blc.BlcIf, vvvar)
                Next
            ElseIf TypeOf stmt1 Is TBlc Then
                NavBlc(CType(stmt1, TBlc), vvvar)
            ElseIf TypeOf stmt1 Is TSelect Then
                in_switch_sv = InSelect
                InSelect = True

                NavSelect(CType(stmt1, TSelect), vvvar)

                InSelect = in_switch_sv

            ElseIf TypeOf stmt1 Is TCase Then
                NavCase(CType(stmt1, TCase), arg1)

            ElseIf TypeOf stmt1 Is TTry Then
                try1 = CType(stmt1, TTry)
                NavBlc(try1.BlcTry, vvvar)
                vvvar.Insert(0, try1.VarCatch)
                NavBlc(try1.BlcCatch, vvvar)
                vvvar.RemoveAt(0)

            ElseIf TypeOf stmt1 Is TWith Then
                with1 = CType(stmt1, TWith)
                NavTrm(with1.TermWith, vvvar)

                with_sv = CurrentWith
                CurrentWith = with1

                NavBlc(with1.BlcWith, vvvar)

                CurrentWith = with_sv

            ElseIf TypeOf stmt1 Is TFor Then
                loop_sv = CurLoop
                CurLoop = CType(stmt1, TFor)
                in_switch_sv = InSelect
                InSelect = False

                NavFor(CType(stmt1, TFor), vvvar)

                CurLoop = loop_sv
                InSelect = in_switch_sv
            ElseIf TypeOf stmt1 Is TRet Then
                NavTrm(CType(stmt1, TRet).TrmRet, vvvar)

            ElseIf TypeOf stmt1 Is TThrow Then
                NavTrm(CType(stmt1, TThrow).TrmThrow, vvvar)

            ElseIf TypeOf stmt1 Is TComment Then
            Else
                exit_label = False
                Select Case stmt1.TypeStmt
                    Case ETkn.eExitDo
                        Debug.Assert(CurLoop IsNot Nothing)
                        If InSelect OrElse Not CurLoop.IsDo Then
                            exit_label = True
                        End If

                    Case ETkn.eExitFor
                        Debug.Assert(CurLoop IsNot Nothing)
                        If InSelect OrElse CurLoop.IsDo Then
                            exit_label = True
                        End If

                    Case ETkn.eExitSub

                    Case Else
                        Debug.WriteLine("Set Ref Stmt err:{0}", stmt1)
                        Debug.Assert(False)
                End Select
                If exit_label Then
                    If CurLoop.LabelFor = 0 Then

                        LabelCnt = LabelCnt + 1
                        CurLoop.LabelFor = LabelCnt
                    End If
                    CType(stmt1, TExit).LabelExit = CurLoop.LabelFor
                End If
            End If
        End If
    End Sub

    Public Overrides Sub NavBlc(blc1 As TBlc, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        If blc1 IsNot Nothing Then
            vvvar.Insert(0, blc1.VarBlc)
            ' for Call
            For Each stmt1 In blc1.StmtBlc
                NavStmt(stmt1, vvvar)
            Next

            vvvar.RemoveAt(0)
        End If
    End Sub

    Public Overrides Sub NavFnc(fnc1 As TFnc, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))

        vvvar = CType(arg1, TList(Of TList(Of TVar)))

        Dim vvar As TList(Of TVar)

        If fnc1 IsNot Nothing Then

            If fnc1.InterfaceFnc IsNot Nothing Then
                fnc1.ImplFnc.VarRef = TPrj.FindFieldFunction(fnc1.InterfaceFnc, fnc1.ImplFnc.NameRef, Nothing)
                Debug.Assert(fnc1.ImplFnc IsNot Nothing)
            End If

            vvar = New TList(Of TVar)()
            vvar.AddRange(fnc1.ArgFnc)
            Debug.Assert(fnc1.ThisFnc IsNot Nothing)
            vvar.Add(fnc1.ThisFnc)
            vvvar.Add(vvar)
            NavBlc(fnc1.BlcFnc, vvvar)
            vvvar.RemoveAt(vvvar.Count - 1)
        End If
    End Sub

    Public Overrides Sub NavCla(cla1 As TCls, arg1 As Object)
        Dim vvvar As TList(Of TList(Of TVar))
        Dim vsuper_cla As TList(Of TCls)

        If (cla1.FldCla.Count <> 0 OrElse cla1.FncCla.Count <> 0) AndAlso Not (cla1.GenCla IsNot Nothing AndAlso cla1.OrgCla Is Nothing) Then
            '  フィールド/メソッドの定義がある場合

            vvvar = New TList(Of TList(Of TVar))()

            vvvar.Add(New TList(Of TVar)(From x In PrjSetRef.SystemType.FldCla Select CType(x, TVar)))
            vvvar.Add(New TList(Of TVar)(From x In PrjSetRef.SystemType.FncCla Select CType(x, TVar)))

            vsuper_cla = New TList(Of TCls)()
            vsuper_cla.AddRange(cla1.AllSuperCla)
            vsuper_cla.Add(cla1)

            ' for Add Add
            For Each cla2 In vsuper_cla
                vvvar.Add(New TList(Of TVar)(From x In cla2.FldCla Select CType(x, TVar)))
                vvvar.Add(New TList(Of TVar)(From x In cla2.FncCla Select CType(x, TVar)))
            Next

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla
                CurFncPrj = fnc1
                CurLoop = Nothing
                InSelect = False
                LabelCnt = 0
                NavFnc(fnc1, vvvar)
                CurFncPrj = Nothing
            Next
        End If
    End Sub
End Class


' -------------------------------------------------------------------------------- TNavSetVarRef
Public Class TNavSetVarRef
    Inherits TNavPrj

    Public Overrides Sub NavDot(dot1 As TDot, arg1 As Object)
        IncRefCnt(dot1)

        Debug.Assert(dot1.VarRef IsNot Nothing)
        Debug.Assert(Not dot1.VarRef.RefVar.Contains(dot1))

        dot1.VarRef.RefVar.Add(dot1)

        NavTrm(dot1.TrmDot, arg1)
    End Sub

    Public Overrides Sub NavRef(ref1 As TRef, arg1 As Object)
        IncRefCnt(ref1)

        Debug.Assert(ref1.VarRef IsNot Nothing)
        Debug.Assert(Not ref1.VarRef.RefVar.Contains(ref1))

        ref1.VarRef.RefVar.Add(ref1)
    End Sub
End Class

' -------------------------------------------------------------------------------- TNavSetRefFnc
Public Class TNavSetRefFnc
    Inherits TNavPrj

    Public Overrides Sub NavDot(dot1 As TDot, arg1 As Object)
        Dim cur_fnc As TFnc

        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFnc)
        cur_fnc = CType(arg1, TFnc)

        IncRefCnt(dot1)

        dot1.FncRef = cur_fnc
        cur_fnc.RefFnc.Add(dot1)

        If dot1.TrmDot Is Nothing Then
            Debug.Assert(CurrentWith IsNot Nothing)
        Else
            NavTrm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overrides Sub NavRef(ref1 As TRef, arg1 As Object)
        Dim cur_fnc As TFnc

        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFnc)
        cur_fnc = CType(arg1, TFnc)

        IncRefCnt(ref1)

        ref1.FncRef = cur_fnc
        cur_fnc.RefFnc.Add(ref1)
    End Sub

    Public Overrides Sub NavFnc(fnc1 As TFnc, arg1 As Object)
        If fnc1.BlcFnc IsNot Nothing Then
            NavBlc(fnc1.BlcFnc, fnc1)
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNavSetCall
Public Class TNavSetCall
    Inherits TNavPrj

    Public Sub SetCall(fnc1 As TFnc, ref1 As TRef)
        Dim fnc2 As TFnc

        fnc2 = CType(ref1.VarRef, TFnc)

        If Not fnc1.CallTo.Contains(fnc2) Then
            fnc1.CallTo.Add(fnc2)
        End If
        If Not fnc2.CallFrom.Contains(fnc1) Then
            fnc2.CallFrom.Add(fnc1)
        End If
    End Sub

    Public Overrides Sub NavDot(dot1 As TDot, arg1 As Object)
        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFnc)

        IncRefCnt(dot1)

        If dot1.VarRef IsNot Nothing AndAlso TypeOf dot1.VarRef Is TFnc Then

            SetCall(CType(arg1, TFnc), dot1)
        End If

        If dot1.TrmDot Is Nothing Then
            Debug.Assert(CurrentWith IsNot Nothing)
        Else
            NavTrm(dot1.TrmDot, arg1)
        End If
    End Sub

    Public Overrides Sub NavRef(ref1 As TRef, arg1 As Object)
        Debug.Assert(arg1 IsNot Nothing AndAlso TypeOf arg1 Is TFnc)

        IncRefCnt(ref1)
        If ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TFnc Then

            SetCall(CType(arg1, TFnc), ref1)
        End If
    End Sub


    Public Overrides Sub NavFnc(fnc1 As TFnc, arg1 As Object)
        Debug.Assert(arg1 Is Nothing)
        If fnc1.BlcFnc IsNot Nothing Then
            NavBlc(fnc1.BlcFnc, fnc1)
        End If
    End Sub

End Class

' -------------------------------------------------------------------------------- TNavSetParentStmt
Public Class TNavSetParentStmt
    Inherits TNavPrj

    Public Overrides Function StartLocalVar(var1 As TVar, arg1 As Object) As Object
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

    Public Overrides Function StartStmt(stmt1 As TStmt, arg1 As Object) As Object
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

    Public Overrides Function StartFnc(fnc1 As TFnc, arg1 As Object) As Object
        Return fnc1
    End Function

    Public Overrides Function StartLocalVarList(list1 As TList(Of TVar), arg1 As Object) As Object
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

    Public Overrides Function StartStmtList(list1 As TList(Of TStmt), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function

    Public Overrides Function StartBlcList(list1 As TList(Of TBlc), arg1 As Object) As Object
        If list1 IsNot Nothing Then
            list1.UpList = arg1
        End If

        Return list1
    End Function
End Class


' -------------------------------------------------------------------------------- TNavSetUpTrm
Public Class TNavSetUpTrm
    Inherits TNavPrj
    Public NavUp As New TNavUp

    Public Sub Test(ref1 As TRef)
        Dim stmt1 As TStmt

        NavUp.UpToFnc(ref1)

        stmt1 = NavUp.UpToStmt(ref1)
    End Sub

    Public Overrides Function StartRef(ref1 As TRef, arg1 As Object) As Object
        Test(ref1)
        Return arg1
    End Function

    Public Overrides Function StartDot(dot1 As TDot, arg1 As Object) As Object
        Test(dot1)
        Return arg1
    End Function

End Class


' -------------------------------------------------------------------------------- TNavUp
Public Class TNavUp
    Public Function UpObj(obj As Object) As Object
        If TypeOf obj Is TVar Then
            Return CType(obj, TVar).UpVar
        ElseIf TypeOf obj Is TTerm Then
            Return CType(obj, TTerm).UpTrm
        ElseIf TypeOf obj Is TStmt Then
            Return CType(obj, TStmt).ParentStmt
        ElseIf TypeOf obj Is TList(Of TVar) Then
            Return CType(obj, TList(Of TVar)).UpList
        ElseIf TypeOf obj Is TList(Of TTerm) Then
            Return CType(obj, TList(Of TTerm)).UpList
        ElseIf TypeOf obj Is TList(Of TCase) Then
            Return CType(obj, TList(Of TCase)).UpList
        ElseIf TypeOf obj Is TList(Of TStmt) Then
            Return CType(obj, TList(Of TStmt)).UpList
        ElseIf TypeOf obj Is TList(Of TBlc) Then
            Return CType(obj, TList(Of TBlc)).UpList
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Function UpToFnc(obj1 As Object) As TFnc
        Dim obj2 As Object

        obj2 = obj1
        Do While obj2 IsNot Nothing AndAlso Not TypeOf obj2 Is TFnc
            obj2 = UpObj(obj2)
        Loop

        Debug.Assert(obj2 IsNot Nothing AndAlso TypeOf obj2 Is TFnc)

        Return CType(obj2, TFnc)
    End Function

    Public Function UpToStmt(obj1 As Object) As TStmt
        Dim obj2 As Object

        obj2 = obj1
        Do While obj2 IsNot Nothing AndAlso Not TypeOf obj2 Is TStmt
            obj2 = UpObj(obj2)
        Loop

        Debug.Assert(obj2 IsNot Nothing AndAlso TypeOf obj2 Is TStmt)

        Return CType(obj2, TStmt)
    End Function
End Class

' -------------------------------------------------------------------------------- TNavClearUsedStmt
Public Class TNavClearUsedStmt
    Inherits TNavPrj

    Public Overrides Function StartStmt(stmt1 As TStmt, arg1 As Object) As Object
        stmt1.UsedStmt = False
        Return arg1
    End Function

End Class

' -------------------------------------------------------------------------------- TNavAllStmt
Public Class TNavAllStmt
    Inherits TNavPrj
    Public AllStmts As New TList(Of TStmt)

    Public Overrides Function StartStmt(stmt1 As TStmt, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then
            AllStmts.Add(stmt1)
        End If

        Return arg1
    End Function
End Class

' -------------------------------------------------------------------------------- TNavSetValidStmt
Public Class TNavSetValidStmt
    Inherits TNavPrj

    Public Overrides Function StartStmt(stmt1 As TStmt, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then

            Dim up_stmt As TStmt = TDataflow.UpStmtProper(stmt1.ParentStmt)

            If up_stmt IsNot Nothing AndAlso Not up_stmt.ValidStmt Then
                ' 親の文が無効の場合

                ' この文も無効とする。
                stmt1.ValidStmt = False
            End If
        End If

        Return arg1
    End Function
End Class
