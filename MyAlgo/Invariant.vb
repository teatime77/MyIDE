Imports System.Diagnostics

Public Class TTokenWriter
    Public ObjTlm As Object
    Public TokenListTW As New List(Of TToken)

    Public Sub New(obj As Object)
        ObjTlm = obj
    End Sub

    Sub AddToken(obj As Object)
        TokenListTW.Add(New TToken(obj))
    End Sub

    Public Sub AddToken(type1 As EToken, obj As Object)
        TokenListTW.Add(New TToken(type1, obj))
    End Sub

    Public Sub TAB(n As Integer)
        Dim tab1 As New TToken

        tab1.TypeTkn = EToken.eTab
        tab1.TabTkn = n
        TokenListTW.Add(tab1)
    End Sub

    Public Sub Fmt(ParamArray args As Object())
        For Each o1 In args

            If TypeOf o1 Is String Then
                TokenListTW.Add(New TToken(CType(o1, String), o1))

            ElseIf TypeOf o1 Is TDot Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TReference Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TClass Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TVariable Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TToken Then
                TokenListTW.Add(CType(o1, TToken))

            ElseIf TypeOf o1 Is EToken Then
                Dim type1 As EToken = CType(o1, EToken)

                If type1 = EToken.eNL Then
                    Dim new_line As New TToken

                    new_line.TypeTkn = EToken.eNL
                    TokenListTW.Add(new_line)
                Else
                    AddToken(type1, ObjTlm)
                End If

            ElseIf TypeOf o1 Is List(Of TToken) Then
                TokenListTW.AddRange(CType(o1, List(Of TToken)))

            Else
                Debug.Assert(False)
            End If
        Next

    End Sub

    Public Function GetTokenList() As List(Of TToken)
        Return TokenListTW
    End Function
End Class

Public Class TNaviMakeBasicSource
    Inherits TDeclarative
    Public PrjMK As TProject

    Public Sub New(prj1 As TProject)
        PrjMK = prj1
    End Sub


    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TFunction Then

        End If
    End Sub

    Public Overrides Sub EndCondition(self As Object)
        Dim tw As New TTokenWriter(self)

        If self Is Nothing Then
            tw.Fmt("null stmt", EToken.eNL)
            Exit Sub

        End If

        If TypeOf self Is TVariable Then
            With CType(self, TVariable)
                If TypeOf self Is TClass Then
                    With CType(self, TClass)

                        '!!!!!            ComSrc(CType(.ComCla(), TComment), 0, cla1)
                        Select Case .KndCla
                            Case EClass.eEnumCla
                                '  列挙型の場合

                                tw.Fmt(EToken.ePublic, EToken.eEnum, self, EToken.eNL)
                                '  すべてのフィールドに対し
                                For Each fld1 In .FldCla
                                    'If PrjMK.OutputNotUsed OrElse fld1.UsedVar Then
                                    'End If
                                    '''''TAB(1)
                                    tw.Fmt(fld1, EToken.eNL)
                                Next

                                tw.Fmt(EToken.eEnd, EToken.eEnum, EToken.eNL)

                            Case EClass.eDelegateCla
                                ' デリゲートの場合

                                Dim dlg1 As TDelegate = CType(self, TDelegate)

                                tw.Fmt(EToken.ePublic, EToken.eDelegate)
                                If dlg1.RetDlg Is Nothing Then
                                    tw.Fmt(EToken.eSub, EToken.eNL)
                                Else
                                    tw.Fmt(EToken.eFunction, EToken.eNL)
                                End If

                                GenericSrc(self)

                            '''''''''VarListSrc(dlg1.ArgDlg, dlg1)
                            '''''''''If dlg1.RetDlg IsNot Nothing Then
                            '''''''''    Fmt(EToken.eAs)
                            '''''''''    TypeSrc(dlg1.RetDlg)
                            '''''''''End If
                            '''''''''NL(dlg1)

                            Case Else

                                '  クラスの場合
                                If .ModCla().isPartial Then
                                    tw.Fmt(EToken.ePartial)
                                End If

                                tw.Fmt(EToken.ePublic)

                                If .ModCla().isAbstract Then
                                    tw.Fmt(EToken.eAbstract)
                                End If

                                Select Case .KndCla
                                    Case EClass.eClassCla
                                        tw.Fmt(EToken.eClass)
                                    Case EClass.eStructCla
                                        tw.Fmt(EToken.eStruct)
                                    Case EClass.eInterfaceCla
                                        tw.Fmt(EToken.eInterface)
                                End Select
                                tw.Fmt(self)

                                GenericSrc(self)

                                tw.Fmt(EToken.eNL)

                                If .SuperClassList.Count <> 0 AndAlso .SuperClassList(0) IsNot PrjMK.ObjectType Then
                                    tw.Fmt(EToken.eInherits, .SuperClassList(0).TokenListCls, EToken.eNL)
                                End If

                                If .InterfaceList.Count <> 0 AndAlso .InterfaceList(0) IsNot PrjMK.ObjectType Then
                                    tw.Fmt(EToken.eImplements)
                                    tw.Fmt(Laminate((From cls1 In .InterfaceList Select cls1.TokenListCls), New TToken(EToken.eComma, self)), EToken.eNL)
                                End If

                                '  すべてのフィールドに対し
                                For Each fld1 In .FldCla
                                    If PrjMK.OutputNotUsed OrElse fld1.UsedVar Then
                                        tw.Fmt(fld1.TokenListVar)
                                    End If
                                Next

                                '  すべてのメソッドに対し
                                For Each fnc1 In .FncCla
                                    If Not fnc1.IsGenerated() AndAlso (PrjMK.OutputNotUsed OrElse (fnc1.Reachable OrElse fnc1.ModFnc().isMustOverride) OrElse fnc1.NameVar = "New@TList") Then

                                        tw.Fmt(fnc1.TokenListVar)
                                    End If
                                Next

                                Select Case .KndCla
                                    Case EClass.eClassCla
                                        tw.Fmt(EToken.eEndClass, EToken.eNL)
                                    Case EClass.eStructCla
                                        tw.Fmt(EToken.eEndStruct, EToken.eNL)
                                    Case EClass.eInterfaceCla
                                        tw.Fmt(EToken.eEndInterface, EToken.eNL)
                                End Select
                        End Select
                    End With

                ElseIf TypeOf self Is TField Then
                    With CType(self, TField)

                        tw.Fmt(.ComVar.TokenListStmt)
                        tw.Fmt(.ModVar.TokenListMod)
                        If .ModVar Is Nothing OrElse Not .ModVar.isPublic AndAlso Not .ModVar.isShared Then
                            tw.Fmt(EToken.eDim)
                        End If
                        tw.Fmt(self)

                        If .TailCom <> "" Then
                            tw.Fmt(New TToken(EToken.eComment, .TailCom))
                        End If
                        tw.Fmt(EToken.eNL)
                    End With

                ElseIf TypeOf self Is TFunction Then
                    With CType(self, TFunction)

                        tw.Fmt(.ComVar.TokenListStmt)

                        tw.Fmt(.ModVar.TokenListMod)

                        Select Case .TypeFnc
                            Case EToken.eFunction
                                tw.Fmt(EToken.eFunction, self)
                            Case EToken.eSub
                                tw.Fmt(EToken.eSub, self)
                            Case EToken.eNew
                                tw.Fmt(EToken.eSub, EToken.eNew)

                            Case EToken.eOperator
                                tw.Fmt(EToken.eOperator, self)

                            Case Else
                                Debug.WriteLine("")
                        End Select

                        tw.Fmt(Laminate((From var1 In .ArgFnc Select var1.TokenListVar), New TToken(EToken.eComma, self)))

                        If .RetType IsNot Nothing Then
                            tw.Fmt(EToken.eAs, .RetType.TokenListCls)

                        End If

                        If .InterfaceFnc IsNot Nothing Then
                            tw.Fmt(EToken.eImplements, .InterfaceFnc.TokenListCls, EToken.eDot, .ImplFnc)
                        End If
                        tw.Fmt(EToken.eNL)


                        If .BlcFnc IsNot Nothing Then
                            tw.Fmt(.BlcFnc.TokenListStmt)
                            tw.Fmt(EToken.eEndFunction)
                        End If
                    End With

                Else

                    Dim as_new As Boolean, app1 As TApply

                    as_new = False
                    If .ByRefVar Then
                        tw.Fmt(EToken.eRef)
                    End If
                    If .ParamArrayVar Then
                        tw.Fmt(EToken.eParamArray)
                    End If

                    tw.Fmt(self)
                    If .TypeVar IsNot Nothing AndAlso Not .NoType Then
                        tw.Fmt(EToken.eAs)
                        If .InitVar IsNot Nothing AndAlso .InitVar.IsApp() AndAlso CType(.InitVar, TApply).TypeApp = EToken.eNew Then
                            as_new = True
                            app1 = CType(.InitVar, TApply)
                            If app1.ArgApp.Count = 0 Then
                                ' 引数がない場合

                                tw.Fmt(EToken.eNew)
                                tw.Fmt(app1.NewApp.ClassNameTokenList)
                            Else
                                ' 引数がある場合
                                tw.Fmt(app1.TokenList)
                            End If

                            If app1.IniApp IsNot Nothing Then

                                tw.Fmt(EToken.eFrom, app1.IniApp.TokenList)
                            End If
                        Else
                            PrjMK.SetClassNameList(.TypeVar)
                            tw.Fmt(.TypeVar.ClassNameTokenList)
                        End If
                    End If

                    If Not as_new AndAlso .InitVar IsNot Nothing Then
                        tw.Fmt(EToken.eASN, .InitVar.TokenList)
                    End If

                    .TokenListVar = tw.GetTokenList()
                End If

            End With
        End If

        If TypeOf self Is TTerm Then
            With CType(self, TTerm)

                If TypeOf self Is TConstant Then
                    With CType(self, TConstant)
                        Select Case .TypeAtm
                            Case EToken.eChar
                                tw.Fmt("""" + Escape(.NameRef) + """c")

                            Case EToken.eString
                                tw.Fmt("""" + Escape(.NameRef) + """")

                            Case EToken.eRegEx
                                tw.Fmt(Escape(.NameRef))

                            Case EToken.eInt
                                tw.Fmt(.NameRef)

                            Case EToken.eHex
                                tw.Fmt(.NameRef)
                                Debug.Assert(TSys.Substring(.NameRef, 0, 2) = "&H")

                            Case Else
                                Debug.Assert(False)
                        End Select

                    End With

                ElseIf TypeOf self Is TArray Then
                    With CType(self, TArray)
                        tw.Fmt(EToken.eLC, Laminate((From trm In .TrmArr Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eRC)
                    End With

                ElseIf TypeOf self Is TDot Then
                    With CType(self, TDot)
                        If .TrmDot IsNot Nothing Then
                            tw.Fmt(.TrmDot.TokenList)
                        End If
                        tw.Fmt(EToken.eDot, .NameRef)
                    End With

                ElseIf TypeOf self Is TReference Then
                    With CType(self, TReference)
                        tw.Fmt(self)
                    End With

                ElseIf TypeOf self Is TApply Then
                    With CType(self, TApply)

                        If .Negation Then

                            tw.Fmt(EToken.eNot)
                        End If

                        Select Case .TypeApp
                            Case EToken.eOR, EToken.eAnd, EToken.eAnp, EToken.eVLine
                                tw.Fmt(Laminate((From trm In .ArgApp Select trm.TokenList), New TToken(.TypeApp, self)))
                            Case EToken.eNot
                                tw.Fmt(EToken.eNot, .ArgApp(0).TokenList)

                            '--------------------------------------------------------------------------------------
                            Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD
                                If .ArgApp.Count = 1 AndAlso (.TypeApp = EToken.eADD OrElse .TypeApp = EToken.eMns) Then
                                    tw.Fmt(.TypeApp, .ArgApp(0).TokenList)
                                Else

                                    tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                                End If

                            Case EToken.eAppCall

                                tw.Fmt(.FncApp.TokenList, AppArgTokenList(self))

                            Case EToken.eBaseCall

                                tw.Fmt(EToken.eBase, EToken.eDot, .FncApp.TokenList, AppArgTokenList(self))

                            Case EToken.eBaseNew

                                tw.Fmt(EToken.eBase, EToken.eDot, EToken.eNew, AppArgTokenList(self))

                            Case EToken.eNew
                                Debug.Assert(.NewApp IsNot Nothing)

                                tw.Fmt(EToken.eNew)

                                If .IniApp Is Nothing Then
                                    ' 初期値がない場合

                                    tw.Fmt(.NewApp.TokenListCls, AppArgTokenList(self))
                                Else
                                    ' 初期値がある場合

                                    If .NewApp.IsArray() Then
                                        ' 配列の場合

                                        tw.Fmt(.NewApp.GenCla(0).TokenListCls, AppArgTokenList(self))
                                    Else
                                        ' 配列でない場合

                                        tw.Fmt(.NewApp.TokenListCls, AppArgTokenList(self), EToken.eFrom)
                                    End If

                                    tw.Fmt(.IniApp.TokenList)
                                End If

                            Case EToken.eAs, EToken.eCast

                                tw.Fmt(EToken.eCType, EToken.eLP, .ArgApp(0).TokenList, EToken.eComma, .ClassApp.TokenListCls, EToken.eRP)

                            Case EToken.eGetType
                                tw.Fmt(EToken.eGetType, EToken.eLP, .ClassApp.TokenListCls, EToken.eRP)

                            Case EToken.eQUE
                                tw.Fmt(EToken.eIIF, EToken.eLP, .ArgApp(0).TokenList, EToken.eComma, .ArgApp(1).TokenList, EToken.eComma, .ArgApp(2).TokenList, EToken.eRP)

                            Case EToken.eTypeof
                                tw.Fmt(EToken.eTypeof, EToken.eLP, .ArgApp(0).TokenList, EToken.eRP)

                            '--------------------------------------------------------------------------------------
                            Case EToken.eEq, EToken.eNE
                                Dim tp1 As TClass, tp2 As TClass

                                tw.Fmt(.ArgApp(0).TokenList)
                                tp1 = .ArgApp(0).TypeTrm
                                tp2 = .ArgApp(1).TypeTrm
                                If tp1 Is Nothing OrElse tp2 Is Nothing Then
                                    ' Debug.WriteLine("");
                                    ' tp1 = .ArgApp[0].TypeTrm;
                                    ' tp2 = .ArgApp[1].TypeTrm;
                                End If
                                If tp1 IsNot Nothing AndAlso (tp1.IsAtomType() OrElse tp1.KndCla = EClass.eStructCla) OrElse tp2 IsNot Nothing AndAlso (tp2.IsAtomType() OrElse tp2.KndCla = EClass.eStructCla) Then
                                    tw.Fmt(.TypeApp)
                                Else
                                    If .TypeApp = EToken.eNE Then
                                        tw.Fmt(EToken.eIsNot)
                                    Else
                                        tw.Fmt(EToken.eIs)
                                    End If
                                End If
                                tw.Fmt(.ArgApp(1).TokenList)
                            Case EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE
                                tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                            Case EToken.eIsNot
                                tw.Fmt(.ArgApp(0).TokenList, EToken.eIsNot, .ArgApp(1).TokenList)

                            Case EToken.eTypeof
                                tw.Fmt(EToken.eTypeof, .ArgApp(0).TokenList, EToken.eIs, .ArgApp(1).TokenList)

                            Case EToken.eIs
                                tw.Fmt(.ArgApp(0).TokenList, EToken.eIs, .ArgApp(1).TokenList)

                            Case Else
                                Debug.Assert(False)
                        End Select
                    End With

                ElseIf TypeOf self Is TParenthesis Then
                    With CType(self, TParenthesis)
                        If .TrmPar.IsApp() AndAlso CType(.TrmPar, TApply).TypeApp = EToken.eCast Then

                            tw.Fmt(.TrmPar.TokenList)
                        Else

                            tw.Fmt(EToken.eLP, .TrmPar.TokenList, EToken.eRP)
                        End If

                    End With
                ElseIf TypeOf self Is TFrom Then
                    With CType(self, TFrom)
                        tw.Fmt(EToken.eFrom, .VarFrom.TokenListVar, EToken.eIn, .SeqFrom.TokenList)

                        If .CndFrom IsNot Nothing Then

                            tw.Fmt(EToken.eWhere, .CndFrom.TokenList)
                        End If

                        If .SelFrom IsNot Nothing Then

                            tw.Fmt(EToken.eSelect, .SelFrom.TokenList)
                        End If

                        If .TakeFrom IsNot Nothing Then

                            tw.Fmt(EToken.eTake, .TakeFrom.TokenList)
                        End If

                    End With

                ElseIf TypeOf self Is TAggregate Then
                    With CType(self, TAggregate)
                        tw.Fmt(EToken.eAggregate, .VarAggr.TokenListVar, EToken.eIn, .SeqAggr.TokenList, EToken.eInto)

                        Select Case .FunctionAggr
                            Case EAggregateFunction.eSum
                                tw.Fmt("Sum")
                            Case EAggregateFunction.eMax
                                tw.Fmt("Max")
                            Case EAggregateFunction.eMin
                                tw.Fmt("Min")
                            Case Else
                                Debug.Assert(False)
                        End Select
                        tw.Fmt(EToken.eLP, .IntoAggr.TokenList, EToken.eRP)

                    End With
                Else
                    Debug.Assert(False)
                End If

                .TokenList = tw.GetTokenList()
            End With

        ElseIf TypeOf self Is TStatement Then
            With CType(self, TStatement)

                If .BeforeSrc IsNot Nothing Then
                    Dim v = .BeforeSrc.Replace(vbCr, "").Split(New Char() {vbLf(0)})
                    For Each s In v
                        tw.Fmt(s, EToken.eNL)
                    Next
                End If

                If .ComStmt IsNot Nothing Then
                    For Each tkn_f In .ComStmt
                        tw.TAB(.TabStmt)
                        tw.Fmt(tkn_f, EToken.eNL)
                    Next
                End If
                If TypeOf self Is TAssignment OrElse TypeOf self Is TCall OrElse TypeOf self Is TVariableDeclaration Then
                    tw.TAB(.TabStmt)
                    If TypeOf self Is TAssignment Then
                        With CType(self, TAssignment)
                            tw.Fmt(.RelAsn.ArgApp(0).TokenList, .RelAsn.TypeApp, .RelAsn.ArgApp(1).TokenList)
                        End With

                    ElseIf TypeOf self Is TCall Then
                        With CType(self, TCall)
                            tw.Fmt(.AppCall.TokenList)
                        End With

                    ElseIf TypeOf self Is TVariableDeclaration Then
                        With CType(self, TVariableDeclaration)

                            tw.Fmt(.ModDecl.TokenListMod)
                            If .ModDecl Is Nothing OrElse Not .ModDecl.isPublic AndAlso Not .ModDecl.isShared Then
                                tw.Fmt(EToken.eDim)
                            End If

                            tw.Fmt(Laminate((From var1 In .VarDecl Select var1.TokenListVar), New TToken(EToken.eComma, self)))

                        End With
                    End If

                    If .TailCom <> "" Then

                        tw.Fmt(New TToken(EToken.eComment, .TailCom))
                    End If
                    tw.Fmt(EToken.eNL)

                ElseIf TypeOf self Is TIfBlock Then
                    With CType(self, TIfBlock)
                        Dim if1 As TIf, i1 As Integer

                        if1 = CType(.ParentStmt, TIf)
                        i1 = if1.IfBlc.IndexOf(CType(self, TIfBlock))
                        Debug.Assert(i1 <> -1)

                        If i1 = 0 Then
                            tw.Fmt(EToken.eIf, .CndIf.TokenList, EToken.eThen, EToken.eNL)
                        Else
                            If .CndIf IsNot Nothing Then
                                tw.Fmt(EToken.eElseIf, .CndIf.TokenList, EToken.eThen, EToken.eNL)
                            Else
                                tw.Fmt(EToken.eElse, EToken.eNL)
                            End If
                        End If

                        tw.Fmt(.BlcIf.TokenListStmt)
                    End With

                ElseIf TypeOf self Is TIf Then
                    With CType(self, TIf)
                        For Each if_blc In .IfBlc
                            tw.Fmt(if_blc.TokenListStmt)
                        Next

                        tw.Fmt(EToken.eEndIf, EToken.eNL)
                    End With

                ElseIf TypeOf self Is TCase Then
                    With CType(self, TCase)
                        If Not .DefaultCase Then
                            tw.Fmt(EToken.eCase, Laminate((From trm In .TrmCase Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eNL)
                        Else
                            tw.Fmt(EToken.eCase, EToken.eElse, EToken.eNL)
                        End If
                        tw.Fmt(.BlcCase.TokenListStmt)
                    End With

                ElseIf TypeOf self Is TSelect Then
                    With CType(self, TSelect)
                        tw.Fmt(EToken.eSelect, EToken.eCase, .TrmSel.TokenList, EToken.eNL)

                        For Each cas1 In .CaseSel
                            tw.Fmt(cas1.TokenListStmt)
                        Next

                        tw.Fmt(EToken.eEndSelect, EToken.eNL)
                    End With

                ElseIf TypeOf self Is TTry Then
                    With CType(self, TTry)
                        tw.Fmt(EToken.eTry, EToken.eNL)
                        tw.Fmt(.BlcTry.TokenListStmt)
                        tw.Fmt(EToken.eCatch, Laminate((From var1 In .VarCatch Select var1.TokenListVar), New TToken(EToken.eComma, self)))
                        tw.Fmt(.BlcCatch.TokenListStmt)
                        tw.Fmt(EToken.eEndTry, EToken.eNL)
                    End With

                ElseIf TypeOf self Is TWith Then
                    With CType(self, TWith)
                        tw.Fmt(EToken.eWith, .TermWith.TokenList, EToken.eNL)
                        tw.Fmt(.BlcWith.TokenListStmt)
                        tw.Fmt(EToken.eEndWith, EToken.eNL)
                    End With

                ElseIf TypeOf self Is TFor Then
                    With CType(self, TFor)
                        If .IsDo Then
                            tw.Fmt(EToken.eDo, EToken.eWhile, .CndFor.TokenList, EToken.eNL)
                            tw.Fmt(.BlcFor.TokenListStmt)
                            tw.Fmt(EToken.eLoop, EToken.eNL)

                        ElseIf .InVarFor IsNot Nothing Then
                            tw.Fmt(EToken.eFor, EToken.eEach, .InVarFor.TokenListVar, EToken.eIn, .InTrmFor.TokenList, EToken.eNL)
                            tw.Fmt(.BlcFor.TokenListStmt)
                            tw.Fmt(EToken.eNext, EToken.eNL)

                        ElseIf .FromFor IsNot Nothing Then
                            tw.Fmt(EToken.eFor, .IdxFor.TokenList, EToken.eEq, .FromFor.TokenList, EToken.eTo, .ToFor.TokenList)

                            If .StepFor IsNot Nothing Then
                                tw.Fmt(EToken.eStep, .StepFor.TokenList)
                            End If
                            tw.Fmt(EToken.eNL)

                            tw.Fmt(.BlcFor.TokenListStmt)
                            tw.Fmt(EToken.eNext, EToken.eNL)
                        Else
                            Debug.Assert(False, "For Src Bas")
                        End If
                    End With

                ElseIf TypeOf self Is TReDim Then
                    With CType(self, TReDim)
                        tw.Fmt(EToken.eReDim, .TrmReDim.TokenList, EToken.eLP, Laminate((From trm In .DimReDim Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eNL)
                    End With

                ElseIf TypeOf self Is TBlock Then
                    With CType(self, TBlock)
                        For Each stmt In .StmtBlc
                            tw.Fmt(stmt.TokenListStmt)
                        Next
                    End With

                ElseIf TypeOf self Is TReturn Then
                    With CType(self, TReturn)
                        tw.Fmt(EToken.eReturn)
                        If .TrmRet IsNot Nothing Then
                            tw.Fmt(.TrmRet.TokenList)
                        End If
                        tw.Fmt(EToken.eNL)

                    End With

                ElseIf TypeOf self Is TThrow Then
                    With CType(self, TThrow)
                        tw.Fmt(EToken.eThrow, .TrmThrow.TokenList, EToken.eNL)
                    End With

                ElseIf TypeOf self Is TComment Then
                    With CType(self, TComment)
                        For Each s In .LineCom
                            tw.Fmt(New TToken(EToken.eComment, s), EToken.eNL)
                        Next
                    End With

                Else
                    Select Case .TypeStmt
                        Case EToken.eExitDo, EToken.eExitFor, EToken.eExitSub
                            Select Case .TypeStmt
                                Case EToken.eExitDo
                                    tw.Fmt(EToken.eExitDo, EToken.eNL)
                                Case EToken.eExitFor
                                    tw.Fmt(EToken.eExitFor, EToken.eNL)
                                Case EToken.eExitSub
                                    tw.Fmt(EToken.eExitSub, EToken.eNL)
                                Case Else
                                    Debug.Assert(False)
                            End Select

                        Case Else
                            Debug.WriteLine("Err Stmt Src:{0}", self)
                            Debug.Assert(False)
                    End Select
                End If

                If .AfterSrc <> "" Then
                    Dim v = .AfterSrc.Trim().Replace(vbCr, "").Split(New Char() {vbLf(0)})
                    For Each s In v
                        tw.Fmt(s, EToken.eNL)
                    Next
                End If

                .TokenListStmt = tw.TokenListTW
            End With

        ElseIf TypeOf self Is TModifier Then
            With CType(self, TModifier)
                If .isPublic Then
                    tw.Fmt(EToken.ePublic)
                End If
                If .isShared Then
                    tw.Fmt(EToken.eShared)
                End If
                If .isIterator Then
                    tw.Fmt(EToken.eIterator)
                End If
                If .isConst Then
                    tw.Fmt(EToken.eConst)
                End If
                If .isVirtual Then
                    tw.Fmt(EToken.eVirtual)
                End If
                If .isMustOverride Then
                    tw.Fmt(EToken.eMustOverride)
                End If
                If .isOverride Then
                    tw.Fmt(EToken.eOverride)
                End If

                .TokenListMod = tw.GetTokenList()
            End With

        ElseIf TypeOf self Is TSourceFile Then
            With CType(self, TSourceFile)

                For Each str_f In .vUsing
                    tw.Fmt(EToken.eImports, str_f, EToken.eNL)
                Next

                For Each cla1 In .ClaSrc
                    'If PrjMK.OutputNotUsed OrElse cla1.UsedVar OrElse cla1.KndCla = EClass.eDelegateCla Then

                    'End If
                Next
            End With

        End If

    End Sub

    '   エスケープ文字を作る
    Public Function Escape(str1 As String) As String
        Dim sb As New TStringWriter

        For Each ch1 In str1
            If ch1 = """"c Then
                sb.Append("""""")
            Else
                sb.Append(ch1)
            End If
        Next

        Return sb.ToString()
    End Function

    Public Function Laminate(vvtkn As IEnumerable(Of List(Of TToken)), sep As TToken) As List(Of TToken)
        Dim i As Integer = 0
        Dim vtkn As New List(Of TToken)

        For Each tkns In vvtkn
            If i <> 0 Then
                vtkn.Add(sep)
            End If

            vtkn.AddRange(tkns)
        Next

        Return vtkn
    End Function

    Public Function AppArgTokenList(self As Object) As List(Of TToken)
        With CType(self, TApply)
            Dim vtkn As New List(Of TToken)

            vtkn.Add(New TToken(EToken.eLP, self))
            vtkn.AddRange(Laminate((From trm In .ArgApp Select trm.TokenList), New TToken(EToken.eComma, self)))
            vtkn.Add(New TToken(EToken.eRP, self))

            Return vtkn
        End With
    End Function

    Public Sub ModifierSrc(mod1 As TModifier)
        Dim tw As New TTokenWriter(Nothing)

        If mod1 IsNot Nothing Then
            If mod1.isPublic Then
                tw.Fmt(EToken.ePublic)
            End If
            If mod1.isShared Then
                tw.Fmt(EToken.eShared)
            End If
            If mod1.isConst Then
                tw.Fmt(EToken.eConst)
            End If
            If mod1.isVirtual Then
                tw.Fmt(EToken.eVirtual)
            End If
            If mod1.isMustOverride Then
                tw.Fmt(EToken.eMustOverride)
            End If
            If mod1.isOverride Then
                tw.Fmt(EToken.eOverride)
            End If

            mod1.TokenListMod = tw.GetTokenList()
        End If
    End Sub

    Public Sub GenericSrc(self As Object)
        With CType(self, TClass)
            Dim tw As New TTokenWriter(self)

            If .GenCla IsNot Nothing Then
                ' ジェネリック型の場合

                tw.Fmt(EToken.eLP, EToken.eOf, Laminate((From cls1 In .GenCla Select cls1.TokenListCls), New TToken(EToken.eComma, self)), EToken.eRP)
            End If

        End With
    End Sub

End Class
