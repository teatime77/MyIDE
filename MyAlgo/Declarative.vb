Imports System.Diagnostics

' -------------------------------------------------------------------------------- TNavi
Public Class TDeclarative
    Public RefCnt As Integer
    Public ErrNav As Boolean = False

    Public Sub IncRefCnt(ref1 As TReference)
        RefCnt += 1
    End Sub

    Public Overridable Sub StartCondition(self As Object)
    End Sub

    Public Overridable Sub EndCondition(self As Object)
    End Sub

    Public Overridable Sub NaviModifier(mod1 As TModifier)
        EndCondition(mod1)
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
            EndCondition(arr1)
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
        NaviTerm(frm1.SeqQry)
        NaviLocalVariable(frm1.VarQry)
        NaviTerm(frm1.CndQry)
        NaviTerm(frm1.SelFrom)
        NaviTerm(frm1.TakeFrom)
        NaviTerm(frm1.InnerFrom)
    End Sub

    Public Overridable Sub NaviAggregate(aggr1 As TAggregate)
        NaviTerm(aggr1.SeqQry)
        NaviLocalVariable(aggr1.VarQry)
        NaviTerm(aggr1.CndQry)
        NaviTerm(aggr1.IntoAggr)
    End Sub

    Public Overridable Sub NaviFor(for1 As TFor)
        NaviTerm(for1.IdxFor)
        NaviTerm(for1.InTrmFor)
        NaviLocalVariable(for1.InVarFor)
        NaviTerm(for1.FromFor)
        NaviTerm(for1.ToFor)
        NaviTerm(for1.StepFor)
        NaviStatement(for1.IniFor)
        NaviTerm(for1.CndFor)
        NaviStatement(for1.StepStmtFor)
        NaviStatement(for1.BlcFor)
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
        NaviIfBlockList(if1.IfBlc)
    End Sub

    Public Overridable Sub NaviIfBlock(if_blc As TIfBlock)
        NaviTerm(if_blc.CndIf)
        NaviTerm(if_blc.WithIf)
        NaviStatement(if_blc.BlcIf)
    End Sub

    Public Overridable Sub NaviTry(try1 As TTry)
        NaviStatement(try1.BlcTry)
        NaviLocalVariableList(try1.VarCatch)
        NaviStatement(try1.BlcCatch)
    End Sub

    Public Overridable Sub NaviVariableDeclaration(dcl1 As TVariableDeclaration)
        NaviModifier(dcl1.ModDecl)
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
        StartCondition(fnc1)

        NaviStatement(fnc1.ComVar)
        NaviModifier(fnc1.ModFnc)
        NaviLocalVariableList(fnc1.ArgFnc)

        If fnc1.BlcFnc IsNot Nothing Then
            NaviStatement(fnc1.BlcFnc)
        End If

        EndCondition(fnc1)
    End Sub

    Public Overridable Sub NaviField(fld1 As TField)
        NaviStatement(fld1.ComVar)
        NaviModifier(fld1.ModVar)
        'NaviTerm(fld1.InitVar)

        EndCondition(fld1)
    End Sub

    Public Overridable Sub NaviClass(cla1 As TClass)
        If (cla1.FldCla.Count <> 0 OrElse cla1.FncCla.Count <> 0) AndAlso Not (cla1.GenCla IsNot Nothing AndAlso cla1.OrgCla Is Nothing) Then

            For Each fld1 In cla1.FldCla
                NaviField(fld1)
            Next

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla
                NaviFunction(fnc1)
            Next

            EndCondition(cla1)
        End If
    End Sub

    Public Overridable Sub NaviSourceFile(src As TSourceFile)
        For Each cla1 In src.ClaSrc
            NaviClass(cla1)
        Next

        EndCondition(src)
    End Sub

    Public Overridable Sub NaviProject(prj As TProject)
        For Each src In prj.SrcPrj
            NaviSourceFile(src)
        Next
        '  すべてのクラスに対し
        'For Each cla1 In prj.SimpleParameterizedClassList
        '    NaviClass(cla1)
        'Next

        EndCondition(prj)
    End Sub
End Class

' -------------------------------------------------------------------------------- TSetRefDeclarative
Public Class TSetRefDeclarative
    Inherits TDeclarative

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TFunction Then
            With CType(self, TFunction)
                If .InterfaceFnc IsNot Nothing Then
                    .ImplFnc.VarRef = TProject.FindFieldFunction(.InterfaceFnc, .ImplFnc.NameRef, Nothing)
                    Debug.Assert(.ImplFnc IsNot Nothing)
                End If

                Debug.Assert(.ThisFnc IsNot Nothing)
            End With

        End If
    End Sub

    Function InstanceOfIfBlock(var1 As TVariable, o As Object) As Boolean
        If TypeOf (o) Is TIfBlock Then
            Dim cnd As TTerm = CType(o, TIfBlock).CndIf

            If TypeOf (cnd) Is TApply Then
                Dim app1 As TApply = CType(cnd, TApply)

                If app1.TypeApp = EToken.eInstanceof AndAlso TypeOf (app1.ArgApp(0)) Is TReference AndAlso CType(app1.ArgApp(0), TReference).VarRef Is var1 Then
                    Dim o2 As Object = app1.ArgApp(1)
                    Return True
                End If
            End If
        End If

        Return False
    End Function

    Public Sub SetTypeTrm(trm1 As TTerm)
        If trm1.CastType IsNot Nothing Then
            trm1.TypeTrm = trm1.CastType
        Else
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

                            Dim v = From o In TNaviUp.AncestorList(trm1) Where InstanceOfIfBlock(.VarRef, o)
                            If v.Any() Then
                                Dim if_blc As TIfBlock = v.First()
                                Dim tp1 As TClass = CType(CType(CType(if_blc.CndIf, TApply).ArgApp(1), TReference).VarRef, TClass)
                                If tp1 IsNot Nothing AndAlso tp1.IsSubcla(.VarRef.TypeVar) Then
                                    .TypeTrm = tp1
                                End If
                            End If
                        End If
                    End If
                End With

            ElseIf TypeOf trm1 Is TApply Then
                With CType(trm1, TApply)
                    Select Case .TypeApp
                        Case EToken.eAnd, EToken.eOR, EToken.eNot, EToken.eAnp
                            .TypeTrm = .ProjectTrm.BoolType

                        Case EToken.eEq, EToken.eNE, EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE, EToken.eIsNot, EToken.eInstanceof, EToken.eIs
                            .TypeTrm = .ProjectTrm.BoolType

                        Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD, EToken.eINC, EToken.eDEC, EToken.eBitOR
                            .TypeTrm = .ArgApp(0).TypeTrm

                        Case EToken.eAppCall
                            If TypeOf .FncApp Is TReference Then

                                Dim ref1 As TReference = CType(.FncApp, TReference)
                                If TypeOf ref1.VarRef Is TFunction Then
                                    Dim fnc1 As TFunction = CType(ref1.VarRef, TFunction)
                                    If fnc1.RetType IsNot Nothing AndAlso fnc1.RetType.ContainsArgumentClass Then

                                        Dim dic As New Dictionary(Of String, TClass)
                                        Dim vidx = From idx In TNaviUp.IndexList(fnc1.ArgFnc) Where fnc1.ArgFnc(idx).TypeVar.ContainsArgumentClass
                                        Debug.Assert(vidx.Any())
                                        Dim i1 As Integer = vidx.First()
                                        Dim tp1 As TClass = fnc1.ArgFnc(i1).TypeVar
                                        Dim tp2 As TClass = .ArgApp(i1).TypeTrm
                                        Debug.Assert(tp1.OrgCla Is tp2.OrgCla)
                                        Dim i2 As Integer
                                        For i2 = 0 To tp1.GenCla.Count - 1
                                            dic.Add(tp1.GenCla(i2).NameVar, tp2.GenCla(i2))
                                        Next

                                        .TypeTrm = .ProjectTrm.SubstituteArgumentClass(fnc1.RetType, dic)
                                    Else
                                        .TypeTrm = fnc1.RetType
                                    End If
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

                        Case EToken.eAs
                            .TypeTrm = .ClassApp

                        Case EToken.Question
                            .TypeTrm = .ArgApp(1).TypeTrm

                        Case EToken.eInstanceof
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

                    If .InnerFrom IsNot Nothing Then
                        .TypeTrm = .InnerFrom.TypeTrm
                    Else
                        If .SelFrom Is Nothing Then
                            cla1 = .SeqQry.TypeTrm
                            Debug.Assert(cla1 IsNot Nothing)
                            cla2 = .ProjectTrm.ElementType(cla1)
                            .TypeTrm = .ProjectTrm.GetIEnumerableClass(cla2)
                        Else
                            cla1 = .SelFrom.TypeTrm
                            Debug.Assert(cla1 IsNot Nothing)
                            .TypeTrm = .ProjectTrm.GetIEnumerableClass(cla1)
                        End If
                    End If
                End With

            ElseIf TypeOf trm1 Is TAggregate Then
                With CType(trm1, TAggregate)

                    .TypeTrm = .IntoAggr.TypeTrm
                End With

            Else
                Debug.Assert(False)
            End If
        End If
    End Sub

    Public Function GetWithClass(self As TDot) As TClass
        Dim if_blc = From o In TNaviUp.AncestorList(self) Where TypeOf o Is TIfBlock AndAlso CType(o, TIfBlock).WithIf IsNot Nothing

        If if_blc.Any() Then
            Return CType(if_blc.First(), TIfBlock).WithIf.TypeTrm
        End If

        Return self.FunctionTrm.WithFnc
    End Function

    Public Overrides Sub EndCondition(self As Object)
        If TypeOf self Is TTerm Then
            Dim trm1 As TTerm = CType(self, TTerm)

            If TypeOf self Is TConstant Then
                SetTypeTrm(trm1)

            ElseIf TypeOf self Is TDot Then
                With CType(self, TDot)
                    IncRefCnt(self)

                    If .TrmDot Is Nothing Then

                        .TypeDot = GetWithClass(CType(self, TDot))
                        Debug.Assert(.TypeDot IsNot Nothing)
                    Else

                        .TypeDot = .TrmDot.TypeTrm
                    End If

                    If .TypeDot Is Nothing Then
                        Throw New TError(String.Format("ドットの左の項の型が不明 {0}", .NameRef))
                    End If
                    If TypeOf .TypeDot Is TDelegate Then
                        Debug.Assert(.NameRef = "Invoke")
                    End If

                    Dim obj As Object = TNaviUp.UpObj(self)
                    Dim dot_is_fncapp As Boolean = (TypeOf obj Is TApply AndAlso self Is CType(obj, TApply).FncApp)
                    If dot_is_fncapp Then
                        Dim app1 As TApply = CType(obj, TApply)

                        Debug.Assert(self Is app1.FncApp)
                        Debug.Assert(app1.TypeApp = EToken.eAppCall)

                        If .IsAddressOf Then
                            .VarRef = TProject.FindFieldFunction(.TypeDot, .NameRef, Nothing)
                        Else
                            .VarRef = TProject.FindFieldFunction(.TypeDot, .NameRef, app1.ArgApp)
                        End If

                        If .VarRef Is Nothing Then
                            Throw New TError(String.Format("不明なメンバー {0} {1}", .TypeDot.LongName(), .NameRef))
                        Else
                            Debug.Assert(TypeOf .VarRef Is TFunction OrElse TypeOf .VarRef.TypeVar Is TDelegate OrElse .VarRef.TypeVar.HasIndex())
                        End If
                    Else

                        .VarRef = TProject.FindFieldFunction(.TypeDot, .NameRef, Nothing)
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
                With CType(self, TReference)
                    Dim ref1 As TReference = CType(self, TReference)
                    IncRefCnt(ref1)
                    Dim obj As Object = TNaviUp.UpObj(ref1)
                    If TypeOf obj Is TApply Then
                        Dim app1 As TApply = CType(obj, TApply)

                        If ref1 Is app1.FncApp Then
                            Select Case app1.TypeApp
                                Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD, EToken.eINC, EToken.eDEC, EToken.eBitOR

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
                                                Case EToken.eINC
                                                    name1 = "__Inc"
                                                Case EToken.eDEC
                                                    name1 = "__Dec"
                                                Case EToken.eBitOR
                                                    name1 = "__BitOr"
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
                                '.VarRef = .ProjectTrm.FindVariable(ref1, .NameRef)
                                '.VarRef = TProject.FindFieldFunction(.FunctionTrm.ClaFnc, .NameRef, Nothing)
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

                        Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD, EToken.eINC, EToken.eDEC, EToken.eBitOR, EToken.Question
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

        ElseIf TypeOf self Is TClass Then

        ElseIf TypeOf self Is TFunction Then

        ElseIf TypeOf self Is TVariable Then
            With CType(self, TVariable)
                Dim obj As Object = TNaviUp.UpObj(self)

                If TypeOf obj Is TFrom Then
                    Dim frm1 As TFrom = CType(obj, TFrom)
                    Debug.Assert(self Is frm1.VarQry)

                    Dim type1 As TClass = frm1.SeqQry.TypeTrm
                    .TypeVar = frm1.ProjectTrm.ElementType(type1)
                    Debug.Assert(.TypeVar IsNot Nothing)

                ElseIf TypeOf obj Is TAggregate Then
                    Dim aggr1 As TAggregate = CType(obj, TAggregate)
                    Debug.Assert(self Is aggr1.VarQry)

                    Dim type1 As TClass = aggr1.SeqQry.TypeTrm
                    .TypeVar = aggr1.ProjectTrm.ElementType(type1)
                    Debug.Assert(.TypeVar IsNot Nothing)

                ElseIf TypeOf obj Is TFor Then
                    Dim for1 As TFor = CType(obj, TFor)
                    Debug.Assert(self Is for1.InVarFor)

                    Dim type1 As TClass = for1.InTrmFor.TypeTrm
                    .TypeVar = for1.ProjectStmt().ElementType(type1)
                    Debug.Assert(.TypeVar IsNot Nothing)

                Else

                    Dim vstmt = From up_self In TNaviUp.AncestorList(self) Where TypeOf up_self Is TVariableDeclaration

                    If vstmt.Any() Then
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


' -------------------------------------------------------------------------------- TNaviSetVarRef
Public Class TNaviSetVarRefNEW
    Inherits TDeclarative

    Public Overrides Sub EndCondition(self As Object)
        If TypeOf self Is TDot Then
            With CType(self, TReference)
                Debug.Assert(.VarRef IsNot Nothing)
                Debug.Assert(Not .VarRef.RefVar.Contains(CType(self, TReference)))

                .VarRef.RefVar.Add(CType(self, TReference))
            End With

        ElseIf TypeOf self Is TReference Then
            With CType(self, TReference)
                Debug.Assert(.VarRef IsNot Nothing)
                Debug.Assert(Not .VarRef.RefVar.Contains(CType(self, TReference)))

                .VarRef.RefVar.Add(CType(self, TReference))
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

' -------------------------------------------------------------------------------- TNaviSetClassifiedIf
' クラスの場合分けのIf文を探す。
Public Class TNaviSetClassifiedIf
    Inherits TDeclarative

    Public Function IsClassifiedIfBlock(if_blc As TIfBlock) As Boolean
        If TypeOf if_blc.CndIf Is TApply Then
            Dim app1 As TApply = CType(if_blc.CndIf, TApply)

            If TypeOf app1.ArgApp(0) Is TReference AndAlso app1.TypeApp = EToken.eInstanceof Then
                Dim ref1 As TReference = CType(app1.ArgApp(0), TReference)

                If ref1.VarRef Is if_blc.FunctionStmt.ArgFnc(0) Then
                    Return True
                End If
            End If
        End If
        Return False
    End Function

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TIf Then
            With CType(self, TIf)
                Dim may_be_classified_if As Boolean = False

                Dim up_stmt As TStatement = TDataflow.UpStmtProper(.ParentStmt)

                If up_stmt Is Nothing Then
                    may_be_classified_if = True
                Else
                    If TypeOf up_stmt Is TIfBlock Then
                        may_be_classified_if = CType(CType(up_stmt, TIfBlock).ParentStmt, TIf).ClassifiedIf
                    Else
                        may_be_classified_if = False
                    End If
                End If

                If may_be_classified_if Then
                    Dim classified_if_block_list = From x In .IfBlc Where IsClassifiedIfBlock(x)

                    If classified_if_block_list.Count() = .IfBlc.Count Then
                        .ClassifiedIf = True
                    Else
                        .ClassifiedIf = False
                    End If
                Else
                    .ClassifiedIf = False
                End If
            End With
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviMakeNavigateFunction
' ナビゲート関数を作る。
Public Class TNaviMakeNavigateFunction
    Inherits TDeclarative
    Public Prj As TProject
    Public UseParentClassList As List(Of TClass)
    Public ClassifiedClassList As List(Of TClass)
    Public NaviFunctionList As New List(Of TFunction)

    Public Sub AddRuleCall(fnc1 As TFunction, cla1 As TClass)
        ' RuleのCall文を作る。
        Dim rule_fnc_list = From c In TNaviUp.ThisAncestorSuperClassList(cla1).Distinct() From f In c.FncCla Where f.ModVar.isInvariant Select f
        If rule_fnc_list.Any() Then

            Dim rule_fnc As TFunction = rule_fnc_list.First()
            Dim rule_app As TApply = TApply.MakeAppCall(New TDot(Nothing, rule_fnc))

            Dim self_var As TVariable = fnc1.ArgFnc(0)
            Dim app_var As TVariable = fnc1.ArgFnc(1)

            rule_app.AddInArg(New TReference(self_var))
            rule_app.AddInArg(New TReference(app_var))

            fnc1.BlcFnc.AddStmtBlc(New TCall(rule_app))
        End If
    End Sub

    Public Function InitNavigateFunction(function_name As String, cla1 As TClass) As TFunction
        Dim fnc1 As New TFunction(function_name, Prj, cla1)

        Debug.Print("Init Navigate Function {0}.{1}", cla1.NameVar, function_name)

        NaviFunctionList.Add(fnc1)

        Dim self_var As New TVariable("self", Prj.ObjectType)
        Dim app_var As New TVariable("app", Prj.MainClass)
        fnc1.ArgFnc.Add(self_var)
        fnc1.ArgFnc.Add(app_var)
        fnc1.WithFnc = cla1

        Return fnc1
    End Function

    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TFunction Then
            With CType(self, TFunction)
                Dim reachable_from_bottom_pending As New List(Of TField)
                Dim reachable_from_bottom_processed As New List(Of TField)

                Dim reachable_from_top_pending As New List(Of TField)
                Dim reachable_from_top_processed As New List(Of TField)

                For Each classified_class In ClassifiedClassList

                    ' classified_classとそのスーパークラスのリスト
                    Dim classified_super_class_list = Enumerable.Distinct(TNaviUp.ThisAncestorSuperClassList(classified_class))

                    ' 型がclassified_classかスーパークラスであるフィールドのリスト
                    Dim parent_field_list = From parent_field In Prj.SimpleFieldList Where parent_field.ModVar.isStrong() AndAlso classified_super_class_list.Contains(CType(If(parent_field.TypeVar.OrgCla Is Nothing, parent_field.TypeVar, Prj.ElementType(parent_field.TypeVar)), TClass))

                    ' reachable_from_bottom_pendingに入っていないparent_fieldを追加する。
                    reachable_from_bottom_pending.AddRange((From parent_field In parent_field_list Where Not reachable_from_bottom_pending.Contains(parent_field)).ToList())
                Next

                Dim parent_to_child_field_list_table As New Dictionary(Of TField, List(Of TField))

                Do While reachable_from_bottom_pending.Count <> 0
                    Dim current_field = reachable_from_bottom_pending(0)
                    reachable_from_bottom_pending.RemoveAt(0)
                    reachable_from_bottom_processed.Add(current_field)

                    If TNaviUp.ThisAncestorSuperClassList(Prj.MainClass).Contains(current_field.ClaFld) Then
                        ' current_fieldが属するクラスが、メインクラスかそのスーパークラスの場合

                        ' メインクラスからアクセス可能
                        reachable_from_top_pending.Add(current_field)
                    End If

                    ' current_fieldが属するクラスとそのスーパークラスのリスト
                    Dim current_field_super_class_list = Enumerable.Distinct(TNaviUp.ThisAncestorSuperClassList(current_field.ClaFld))

                    ' 型がcurrent_fieldが属するクラスかスーパークラスであるフィールドのリスト
                    Dim parent_field_list = From parent_field In Prj.SimpleFieldList Where parent_field.ModVar.isStrong() AndAlso current_field_super_class_list.Contains(Prj.FieldElementType(parent_field))

                    For Each parent_field In parent_field_list
                        Dim child_field_list As List(Of TField)

                        If parent_to_child_field_list_table.ContainsKey(parent_field) Then
                            child_field_list = parent_to_child_field_list_table(parent_field)
                        Else
                            child_field_list = New List(Of TField)()
                            parent_to_child_field_list_table.Add(parent_field, child_field_list)
                        End If

                        ' このリストにcurrent_fieldを追加する。
                        child_field_list.Add(current_field)
                        Debug.Print("parent field {0}", parent_field)
                    Next

                    ' 未処理のフィールド
                    Dim pending_parent_field_list = From parent_field In parent_field_list Where Not reachable_from_bottom_processed.Contains(parent_field) AndAlso Not reachable_from_bottom_pending.Contains(parent_field)

                    reachable_from_bottom_pending.AddRange(pending_parent_field_list)
                Loop

                Dim used_field_list_table As New Dictionary(Of TClass, List(Of TField))

                Do While reachable_from_top_pending.Count <> 0
                    ' reachable_from_top_pendingからcurrent_fieldを取り出し、reachable_from_top_processedに追加する。
                    Dim current_field = reachable_from_top_pending(0)
                    reachable_from_top_pending.RemoveAt(0)
                    reachable_from_top_processed.Add(current_field)

                    Dim used_field_list As List(Of TField)
                    If used_field_list_table.ContainsKey(current_field.ClaFld) Then
                        used_field_list = used_field_list_table(current_field.ClaFld)
                    Else
                        used_field_list = New List(Of TField)()
                        used_field_list_table.Add(current_field.ClaFld, used_field_list)
                    End If
                    used_field_list.Add(current_field)

                    If parent_to_child_field_list_table.ContainsKey(current_field) Then
                        Dim child_field_list As List(Of TField) = parent_to_child_field_list_table(current_field)

                        ' 未処理のフィールド
                        Dim pending_chile_field_list = From chile_field In child_field_list Where Not reachable_from_top_processed.Contains(chile_field) AndAlso Not reachable_from_top_pending.Contains(chile_field)

                        reachable_from_top_pending.AddRange(pending_chile_field_list)
                    End If
                Loop

                Dim function_name As String = "Navigate_" + .NameVar

                Dim dummy_function As New TFunction(function_name, Nothing)
                Dim navi_needed_class_list As New List(Of TClass)

                For Each cla1 In used_field_list_table.Keys
                    Dim used_field_list As List(Of TField) = used_field_list_table(cla1)
                    Dim fnc1 As TFunction = InitNavigateFunction(function_name, cla1)

                    If UseParentClassList.Contains(cla1) Then
                        ' 親のフィールドの値を参照している場合

                        AddRuleCall(fnc1, cla1)
                    End If

                    Dim self_var As TVariable = fnc1.ArgFnc(0)
                    Dim app_var As TVariable = fnc1.ArgFnc(1)

                    Debug.Print("Rule {0}", cla1.NameVar)

                    For Each used_field In used_field_list
                        If used_field.TypeVar.OrgCla IsNot Nothing Then
                            ' リストの場合

                            ' ループを作る。
                            Dim for1 As New TFor
                            for1.InVarFor = New TVariable("x", Nothing)
                            for1.InTrmFor = New TDot(Nothing, used_field)
                            for1.BlcFor = New TBlock()

                            ' リスト内の各要素に対しメソッドを呼ぶ。
                            Dim app1 As TApply = TApply.MakeAppCall(New TDot(New TReference(for1.InVarFor), dummy_function))
                            app1.ArgApp.Add(New TReference(for1.InVarFor))
                            app1.ArgApp.Add(New TReference(app_var))
                            for1.BlcFor.AddStmtBlc(New TCall(app1))

                            fnc1.BlcFnc.AddStmtBlc(for1)


                            Debug.Print("For Each x in .{0}" + vbCrLf + "x.{1}()" + vbCrLf + "Next", used_field.NameVar, function_name)
                        Else
                            ' リストでない場合

                            ' フィールドに対しメソッドを呼ぶ。
                            Dim app1 As TApply = TApply.MakeAppCall(New TDot(New TDot(Nothing, used_field), dummy_function))
                            app1.ArgApp.Add(New TDot(Nothing, used_field))
                            app1.ArgApp.Add(New TReference(app_var))
                            fnc1.BlcFnc.AddStmtBlc(New TCall(app1))

                            Debug.Print(".{0}()", function_name)
                        End If

                        Dim navi_needed_class As TClass = If(used_field.TypeVar.OrgCla IsNot Nothing, Prj.ElementType(used_field.TypeVar), used_field.TypeVar)
                        If Not navi_needed_class_list.Contains(navi_needed_class) Then
                            navi_needed_class_list.Add(navi_needed_class)
                        End If
                    Next


                    If Not UseParentClassList.Contains(cla1) Then
                        ' 親のフィールドの値を参照していない場合

                        AddRuleCall(fnc1, cla1)
                    End If

                Next

                For Each cla1 In used_field_list_table.Keys
                    If navi_needed_class_list.Contains(cla1) Then
                        navi_needed_class_list.Remove(cla1)
                    End If
                Next

                For Each cla1 In navi_needed_class_list
                    Dim fnc1 As TFunction = InitNavigateFunction(function_name, cla1)
                    AddRuleCall(fnc1, cla1)
                Next
            End With
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviSetRefPath
' 参照パスをセットする。
Public Class TNaviSetRefPath
    Inherits TDeclarative

    Public Overrides Sub EndCondition(self As Object)
        If TypeOf self Is TTerm Then
            With CType(self, TTerm)
                .RefPathTrm = New TRefPath()

                If TypeOf self Is TDot Then
                    With CType(self, TDot)
                        Debug.Assert(.VarRef IsNot Nothing)

                        If .TrmDot Is Nothing Then
                            .RefPathTrm.RefPathType = ERefPathType.SelfField
                        Else
                            Select Case .TrmDot.RefPathTrm.RefPathType
                                Case ERefPathType.Self
                                    .RefPathTrm.RefPathType = ERefPathType.SelfField

                                Case ERefPathType.Parent
                                    .RefPathTrm.RefPathType = ERefPathType.ParentField

                                Case ERefPathType.Prev
                                    .RefPathTrm.RefPathType = ERefPathType.PrevField

                                Case ERefPathType.App
                                    .RefPathTrm.RefPathType = ERefPathType.AppField

                            End Select
                        End If
                    End With

                ElseIf TypeOf self Is TReference Then
                    With CType(self, TReference)
                        Debug.Assert(.VarRef IsNot Nothing)

                        Select Case .VarRef.RefPathVar.RefPathType
                            Case ERefPathType.SelfField, ERefPathType.Parent, ERefPathType.Prev, ERefPathType.App
                                .RefPathTrm = .VarRef.RefPathVar

                            Case Else
                        End Select
                    End With

                ElseIf TypeOf self Is TQuery Then

                ElseIf TypeOf self Is TFrom Then
                    With CType(self, TFrom)

                    End With

                ElseIf TypeOf self Is TAggregate Then
                    With CType(self, TAggregate)

                    End With
                End If

            End With

        ElseIf TypeOf self Is TVariable Then
            With CType(self, TVariable)
                .RefPathVar = New TRefPath()

                If TypeOf self Is TField Then
                    With CType(self, TField)

                    End With
                Else
                    If TypeOf .UpVar Is TList(Of TVariable) Then
                        Dim up_obj As Object = TNaviUp.UpObj(.UpVar)

                        If TypeOf up_obj Is TFunction Then
                            Dim fnc1 As TFunction = CType(up_obj, TFunction)

                            Dim k As Integer = fnc1.ArgFnc.IndexOf(CType(self, TVariable))
                            Select Case k
                                Case 0
                                    .RefPathVar.RefPathType = ERefPathType.App

                                Case 1
                                    .RefPathVar.RefPathType = ERefPathType.Self

                                Case Else
                                    Debug.Assert(False)
                            End Select

                        ElseIf TypeOf up_obj Is TVariableDeclaration Then
                            If .InitVar IsNot Nothing Then
                                .RefPathVar = .InitVar.RefPathTrm
                            End If

                        ElseIf TypeOf up_obj Is TTry Then
                            Debug.Assert(False)
                        Else
                            Debug.Assert(False)
                            Debug.Print("----------------------------------------- UpVar List {0}", up_obj.GetType())
                        End If

                    ElseIf TypeOf .UpVar Is TQuery Then
                        Dim qry1 As TQuery = CType(.UpVar, TQuery)

                        Select Case qry1.SeqQry.RefPathTrm.RefPathType
                            Case ERefPathType.SelfField
                                qry1.VarQry.RefPathVar.RefPathType = ERefPathType.SelfFieldChild

                            Case ERefPathType.SelfFieldChildField
                                qry1.VarQry.RefPathVar.RefPathType = ERefPathType.SelfFieldChildFieldChild

                            Case Else
                                Debug.Assert(False)
                        End Select

                    ElseIf TypeOf .UpVar Is TAggregate Then
                        Dim aggr1 As TAggregate = CType(.UpVar, TAggregate)

                    ElseIf TypeOf .UpVar Is TFor Then
                        Debug.Assert(False)

                    Else
                        Debug.Assert(False)
                        Debug.Print("----------------------------------------- UpVar {0}", .UpVar.GetType())
                    End If

                End If

            End With
        End If
    End Sub
End Class
