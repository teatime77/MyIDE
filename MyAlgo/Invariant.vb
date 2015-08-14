Imports System.Diagnostics

Public Class TTokenLine
    Public Tab As Integer
    Public TokenList As New List(Of TTkn)
End Class

Public Class TTokenWriter
    Public ObjTlm As Object
    Public TokenLine As TTokenLine
    Public TokenLineList As New List(Of TTokenLine)

    Public Sub New(obj As Object)
        ObjTlm = obj
    End Sub

    Sub AddToken(obj As Object)
        TokenLine.TokenList.Add(New TTkn(obj))
    End Sub

    Public Sub AddToken(type1 As ETkn, obj As Object)
        TokenLine.TokenList.Add(New TTkn(type1, obj))
    End Sub

    Public Sub TAB(n As Integer)
        If TokenLine Is Nothing Then
            TokenLine = New TTokenLine()
        End If

        TokenLine.Tab = n
    End Sub

    Public Sub Fmt(o1 As Object)
        If TokenLine Is Nothing Then
            TokenLine = New TTokenLine()
        End If

        If TypeOf o1 Is String Then
            TokenLine.TokenList.Add(New TTkn(CType(o1, String), o1))
        ElseIf TypeOf o1 Is TDot Then
            AddToken(o1)
        ElseIf TypeOf o1 Is TRef Then
            AddToken(o1)

        ElseIf TypeOf o1 Is TCls Then
            AddToken(o1)

        ElseIf TypeOf o1 Is TVar Then
            AddToken(o1)

        ElseIf TypeOf o1 Is TTkn Then
            TokenLine.TokenList.Add(CType(o1, TTkn))

        ElseIf TypeOf o1 Is ETkn Then
            Dim type1 As ETkn = CType(o1, ETkn)

            If type1 = ETkn.eNL Then
                TokenLineList.Add(TokenLine)
                TokenLine = Nothing
            Else
                AddToken(type1, ObjTlm)
            End If
        ElseIf TypeOf o1 Is List(Of TTkn) Then
            TokenLine.TokenList.AddRange(CType(o1, List(Of TTkn)))
        Else
            Debug.Assert(False)
        End If
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object)
        Fmt(o1)
        Fmt(o2)
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object, o3 As Object)
        Fmt(o1, o2)
        Fmt(o3)
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object, o3 As Object, o4 As Object)
        Fmt(o1, o2, o3)
        Fmt(o4)
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object, o3 As Object, o4 As Object, o5 As Object)
        Fmt(o1, o2, o3, o4)
        Fmt(o5)
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object, o3 As Object, o4 As Object, o5 As Object, o6 As Object)
        Fmt(o1, o2, o3, o4, o5)
        Fmt(o6)
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object, o3 As Object, o4 As Object, o5 As Object, o6 As Object, o7 As Object)
        Fmt(o1, o2, o3, o4, o5, o6)
        Fmt(o7)
    End Sub

    Public Sub Fmt(o1 As Object, o2 As Object, o3 As Object, o4 As Object, o5 As Object, o6 As Object, o7 As Object, o8 As Object)
        Fmt(o1, o2, o3, o4, o5, o6, o7)
        Fmt(o8)
    End Sub

    Public Function GetTokenList() As List(Of TTkn)
        Return TokenLine.TokenList
    End Function
End Class

Public Class TInvariant

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

    Public Function Laminate(vvtkn As IEnumerable(Of List(Of TTkn)), sep As TTkn) As List(Of TTkn)
        Dim i As Integer = 0
        Dim vtkn As New List(Of TTkn)

        For Each tkns In vvtkn
            If i <> 0 Then
                vtkn.Add(sep)
            End If

            vtkn.AddRange(tkns)
        Next

        Return vtkn
    End Function

    Public Function AppArgTokenList(self As Object) As List(Of TTkn)
        With CType(self, TApp)
            Dim vtkn As New List(Of TTkn)

            vtkn.Add(New TTkn(ETkn.eLP, self))
            vtkn.AddRange(Laminate((From trm In .ArgApp Select trm.TokenList), New TTkn(ETkn.eComma, self)))
            vtkn.Add(New TTkn(ETkn.eRP, self))

            Return vtkn
        End With
    End Function

    Public Sub MakeBasicTermCode(self As Object)
        If TypeOf self Is TTerm Then
            With CType(self, TTerm)
                Dim tw As New TTokenWriter(self)

                If TypeOf self Is TCns Then
                    With CType(self, TCns)
                        Select Case .TypeAtm
                            Case ETkn.eChar
                                tw.Fmt("""" + Escape(.NameRef) + """c")

                            Case ETkn.eString
                                tw.Fmt("""" + Escape(.NameRef) + """")

                            Case ETkn.eRegEx
                                tw.Fmt(Escape(.NameRef))

                            Case ETkn.eInt
                                tw.Fmt(.NameRef)

                            Case ETkn.eHex
                                tw.Fmt(.NameRef)
                                Debug.Assert(TSys.Substring(.NameRef, 0, 2) = "&H")

                            Case Else
                                Debug.Assert(False)
                        End Select

                    End With

                ElseIf TypeOf self Is TArray Then
                    With CType(self, TArray)
                        tw.Fmt(ETkn.eLC, Laminate((From trm In .TrmArr Select trm.TokenList), New TTkn(ETkn.eComma, self)), ETkn.eRC)
                    End With

                ElseIf TypeOf self Is TDot Then
                    With CType(self, TDot)
                        If .TrmDot IsNot Nothing Then
                            tw.Fmt(.TrmDot.TokenList)
                        End If
                        tw.Fmt(ETkn.eDot, .NameRef)
                    End With

                ElseIf TypeOf self Is TRef Then
                    With CType(self, TRef)
                        tw.Fmt(self)
                    End With
                ElseIf .IsOpr() Then
                    With CType(self, TApp)
                        If .Negation Then
                            ' ソース生成の前にNegationは除去すべき?
                            Debug.Assert(False)
                        End If
                        Select Case .TypeApp
                            Case ETkn.eOR, ETkn.eAnd, ETkn.eAnp, ETkn.eVLine
                                tw.Fmt(Laminate((From trm In .ArgApp Select trm.TokenList), New TTkn(.TypeApp, self)))
                            Case ETkn.eNot
                                tw.Fmt(ETkn.eNot, .ArgApp(0).TokenList)
                            Case Else
                                Debug.Assert(False)
                        End Select

                    End With
                ElseIf .IsApp() Then
                    With CType(self, TApp)
                        Select Case .TypeApp
                            Case ETkn.eADD, ETkn.eMns, ETkn.eMUL, ETkn.eDIV, ETkn.eMOD
                                If .ArgApp.Count = 1 AndAlso (.TypeApp = ETkn.eADD OrElse .TypeApp = ETkn.eMns) Then
                                    tw.Fmt(.TypeApp, .ArgApp(0).TokenList)
                                Else

                                    tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                                End If

                            Case ETkn.eAppCall

                                tw.Fmt(.FncApp.TokenList, AppArgTokenList(self))

                            Case ETkn.eBaseCall

                                tw.Fmt(ETkn.eBase, ETkn.eDot, .FncApp.TokenList, AppArgTokenList(self))

                            Case ETkn.eBaseNew

                                tw.Fmt(ETkn.eBase, ETkn.eDot, ETkn.eNew, AppArgTokenList(self))

                            Case ETkn.eNew
                                Debug.Assert(.NewApp IsNot Nothing)

                                tw.Fmt(ETkn.eNew)

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

                                        tw.Fmt(.NewApp.TokenListCls, AppArgTokenList(self), ETkn.eFrom)
                                    End If

                                    tw.Fmt(.IniApp.TokenList)
                                End If

                            Case ETkn.eAs, ETkn.eCast

                                tw.Fmt(ETkn.eCType, ETkn.eLP, .ArgApp(0).TokenList, ETkn.eComma, .ClassApp.TokenListCls, ETkn.eRP)

                            Case ETkn.eGetType
                                tw.Fmt(ETkn.eGetType, ETkn.eLP, .ClassApp.TokenListCls, ETkn.eRP)

                            Case ETkn.eQUE
                                tw.Fmt(ETkn.eIIF, ETkn.eLP, .ArgApp(0).TokenList, ETkn.eComma, .ArgApp(1).TokenList, ETkn.eComma, .ArgApp(2).TokenList, ETkn.eRP)

                            Case ETkn.eTypeof
                                tw.Fmt(ETkn.eTypeof, ETkn.eLP, .ArgApp(0).TokenList, ETkn.eRP)

                            Case ETkn.eAddressOf
                                tw.Fmt(ETkn.eAddressOf, .ArgApp(0).TokenList)

                            Case Else
                                Debug.WriteLine("Err Trm Src2:{0}", .TypeApp)
                                Debug.Assert(False)
                        End Select

                    End With
                ElseIf .IsRel() Then
                    With CType(self, TApp)
                        Dim tp1 As TCls, tp2 As TCls

                        Select Case .TypeApp
                            Case ETkn.eEq, ETkn.eNE
                                tw.Fmt(.ArgApp(0).TokenList)
                                tp1 = TPrj.Prj.GetTermType(.ArgApp(0))
                                tp2 = TPrj.Prj.GetTermType(.ArgApp(1))
                                If tp1 Is Nothing OrElse tp2 Is Nothing Then
                                    ' Debug.WriteLine("");
                                    ' tp1 = TPrj.Prj.GetTermType(.ArgApp[0]);
                                    ' tp2 = TPrj.Prj.GetTermType(.ArgApp[1]);
                                End If
                                If tp1 IsNot Nothing AndAlso (tp1.IsAtomType() OrElse tp1.KndCla = EClass.eStructCla) OrElse tp2 IsNot Nothing AndAlso (tp2.IsAtomType() OrElse tp2.KndCla = EClass.eStructCla) Then
                                    tw.Fmt(.TypeApp)
                                Else
                                    If .TypeApp = ETkn.eNE Then
                                        tw.Fmt(ETkn.eIsNot)
                                    Else
                                        tw.Fmt(ETkn.eIs)
                                    End If
                                End If
                                tw.Fmt(.ArgApp(1).TokenList)
                            Case ETkn.eASN, ETkn.eLT, ETkn.eGT, ETkn.eADDEQ, ETkn.eSUBEQ, ETkn.eMULEQ, ETkn.eDIVEQ, ETkn.eMODEQ, ETkn.eLE, ETkn.eGE
                                tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                            Case ETkn.eIsNot
                                tw.Fmt(.ArgApp(0).TokenList, ETkn.eIsNot, .ArgApp(1).TokenList)

                            Case ETkn.eTypeof
                                tw.Fmt(ETkn.eTypeof, .ArgApp(0).TokenList, ETkn.eIs, .ArgApp(1).TokenList)

                            Case ETkn.eIs
                                tw.Fmt(.ArgApp(0).TokenList, ETkn.eIs, .ArgApp(1).TokenList)
                            Case Else
                                Debug.Assert(False)
                        End Select

                    End With
                ElseIf TypeOf self Is TPar Then
                    With CType(self, TPar)
                        If .TrmPar.IsApp() AndAlso CType(.TrmPar, TApp).TypeApp = ETkn.eCast Then

                            tw.Fmt(.TrmPar.TokenList)
                        Else

                            tw.Fmt(ETkn.eLP, .TrmPar.TokenList, ETkn.eRP)
                        End If

                    End With
                ElseIf TypeOf self Is TFrom Then
                    With CType(self, TFrom)
                        tw.Fmt(ETkn.eFrom, .VarFrom.TokenListVar, ETkn.eIn, .SeqFrom.TokenList)

                        If .CndFrom IsNot Nothing Then

                            tw.Fmt(ETkn.eWhere, .CndFrom.TokenList)
                        End If

                        If .SelFrom IsNot Nothing Then

                            tw.Fmt(ETkn.eSelect, .SelFrom.TokenList)
                        End If

                        If .TakeFrom IsNot Nothing Then

                            tw.Fmt(ETkn.eTake, .TakeFrom.TokenList)
                        End If

                    End With

                ElseIf TypeOf self Is TAggregate Then
                    With CType(self, TAggregate)
                        tw.Fmt(ETkn.eAggregate, .VarAggr.TokenListVar, ETkn.eIn, .SeqAggr.TokenList, ETkn.eInto)

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
                        tw.Fmt(ETkn.eLP, .IntoAggr.TokenList, ETkn.eRP)

                    End With
                Else
                    Debug.Assert(False)
                End If

                .TokenList = tw.GetTokenList()
            End With
        End If

    End Sub

    Public Function ModifierSrc(mod1 As TModifier) As List(Of TTkn)
        Dim tw As New TTokenWriter(Nothing)

        If mod1 IsNot Nothing Then
            If mod1.isPublic Then
                tw.Fmt(ETkn.ePublic)
            End If
            If mod1.isShared Then
                tw.Fmt(ETkn.eShared)
            End If
            If mod1.isConst Then
                tw.Fmt(ETkn.eConst)
            End If
            If mod1.isVirtual Then
                tw.Fmt(ETkn.eVirtual)
            End If
            If mod1.isMustOverride Then
                tw.Fmt(ETkn.eMustOverride)
            End If
            If mod1.isOverride Then
                tw.Fmt(ETkn.eOverride)
            End If
        End If

        Return tw.GetTokenList()
    End Function

    Public Sub MakeBasicStmtCode(self As Object)
        Dim tw As New TTokenWriter(self)

        If self Is Nothing Then
            tw.Fmt("null stmt", ETkn.eNL)
            Exit Sub

        End If

        If TypeOf self Is TStmt Then

            With CType(self, TStmt)

                If .BeforeSrc IsNot Nothing Then
                    Dim v = .BeforeSrc.Replace(vbCr, "").Split(New Char() {vbLf(0)})
                    For Each s In v
                        tw.Fmt(s, ETkn.eNL)
                    Next
                End If

                If .ComStmt IsNot Nothing Then
                    For Each tkn_f In .ComStmt
                        tw.TAB(.TabStmt)
                        tw.Fmt(tkn_f, ETkn.eNL)
                    Next
                End If
                If TypeOf self Is TAsn OrElse TypeOf self Is TCall OrElse TypeOf self Is TVarDecl Then
                    tw.TAB(.TabStmt)
                    If TypeOf self Is TAsn Then
                        With CType(self, TAsn)
                            tw.Fmt(.RelAsn.ArgApp(0).TokenList, .RelAsn.TypeApp, .RelAsn.ArgApp(1).TokenList)
                        End With

                    ElseIf TypeOf self Is TCall Then
                        With CType(self, TCall)
                            tw.Fmt(.AppCall.TokenList)
                        End With

                    ElseIf TypeOf self Is TVarDecl Then
                        With CType(self, TVarDecl)

                            tw.Fmt(ModifierSrc(.ModDecl))
                            If .ModDecl Is Nothing OrElse Not .ModDecl.isPublic AndAlso Not .ModDecl.isShared Then
                                tw.Fmt(ETkn.eDim)
                            End If

                            tw.Fmt(Laminate((From var1 In .VarDecl Select var1.TokenListVar), New TTkn(ETkn.eComma, self)))

                        End With
                    End If

                    If .TailCom <> "" Then

                        tw.Fmt(New TTkn(ETkn.eComment, .TailCom))
                    End If
                    tw.Fmt(ETkn.eNL)

                ElseIf TypeOf self Is TIfBlc Then
                    With CType(self, TIfBlc)
                        Dim if1 As TIf, i1 As Integer

                        if1 = CType(.ParentStmt, TIf)
                        i1 = if1.IfBlc.IndexOf(CType(self, TIfBlc))
                        Debug.Assert(i1 <> -1)

                        If i1 = 0 Then
                            tw.Fmt(ETkn.eIf, .CndIf.TokenList, ETkn.eThen, ETkn.eNL)
                        Else
                            If .CndIf IsNot Nothing Then
                                tw.Fmt(ETkn.eElseIf, .CndIf.TokenList, ETkn.eThen, ETkn.eNL)
                            Else
                                tw.Fmt(ETkn.eElse, ETkn.eNL)
                            End If
                        End If

                        tw.Fmt(.BlcIf.TokenList)
                    End With

                ElseIf TypeOf self Is TIf Then

                    With CType(self, TIf)
                        For Each if_blc In .IfBlc
                            tw.Fmt(if_blc.TokenList)
                        Next

                        tw.Fmt(ETkn.eEndIf, ETkn.eNL)
                    End With

                ElseIf TypeOf self Is TCase Then
                    With CType(self, TCase)
                        If Not .DefaultCase Then
                            tw.Fmt(ETkn.eCase, Laminate((From trm In .TrmCase Select trm.TokenList), New TTkn(ETkn.eComma, self)), ETkn.eNL)
                        Else
                            tw.Fmt(ETkn.eCase, ETkn.eElse, ETkn.eNL)
                        End If
                        tw.Fmt(.BlcCase.TokenList)
                    End With

                ElseIf TypeOf self Is TSelect Then
                    With CType(self, TSelect)
                        tw.Fmt(ETkn.eSelect, ETkn.eCase, .TrmSel.TokenList, ETkn.eNL)

                        For Each cas1 In .CaseSel
                            tw.Fmt(cas1.TokenList)
                        Next

                        tw.Fmt(ETkn.eEndSelect, ETkn.eNL)
                    End With

                ElseIf TypeOf self Is TTry Then
                    With CType(self, TTry)
                        tw.Fmt(ETkn.eTry, ETkn.eNL)
                        tw.Fmt(.BlcTry.TokenList)
                        tw.Fmt(ETkn.eCatch, Laminate((From var1 In .VarCatch Select var1.TokenListVar), New TTkn(ETkn.eComma, self)))
                        tw.Fmt(.BlcCatch)
                        tw.Fmt(ETkn.eEndTry, ETkn.eNL)
                    End With

                ElseIf TypeOf self Is TWith Then
                    With CType(self, TWith)
                        tw.Fmt(ETkn.eWith, .TermWith.TokenList, ETkn.eNL)
                        tw.Fmt(.BlcWith.TokenList)
                        tw.Fmt(ETkn.eEndWith, ETkn.eNL)
                    End With

                ElseIf TypeOf self Is TFor Then
                    With CType(self, TFor)
                        If .IsDo Then
                            tw.Fmt(ETkn.eDo, ETkn.eWhile, .CndFor.TokenList, ETkn.eNL)
                            tw.Fmt(.BlcFor.TokenList)
                            tw.Fmt(ETkn.eLoop, ETkn.eNL)

                        ElseIf .InVarFor IsNot Nothing Then
                            tw.Fmt(ETkn.eFor, ETkn.eEach, .InVarFor.TokenListVar, ETkn.eIn, .InTrmFor.TokenList, ETkn.eNL)
                            tw.Fmt(.BlcFor.TokenList)
                            tw.Fmt(ETkn.eNext, ETkn.eNL)

                        ElseIf .FromFor IsNot Nothing Then
                            tw.Fmt(ETkn.eFor, .IdxFor.TokenList, ETkn.eEq, .FromFor.TokenList, ETkn.eTo, .ToFor.TokenList)

                            If .StepFor IsNot Nothing Then
                                tw.Fmt(ETkn.eStep, .StepFor.TokenList)
                            End If
                            tw.Fmt(ETkn.eNL)

                            tw.Fmt(.BlcFor.TokenList)
                            tw.Fmt(ETkn.eNext, ETkn.eNL)
                        Else
                            Debug.Assert(False, "For Src Bas")
                        End If
                    End With

                ElseIf TypeOf self Is TReDim Then
                    With CType(self, TReDim)
                        tw.Fmt(ETkn.eReDim, .TrmReDim.TokenList, ETkn.eLP, Laminate((From trm In .DimReDim Select trm.TokenList), New TTkn(ETkn.eComma, self)), ETkn.eNL)
                    End With

                ElseIf TypeOf self Is TBlc Then
                    With CType(self, TBlc)
                        For Each stmt In .StmtBlc
                            tw.Fmt(stmt.TokenList)
                        Next
                    End With

                ElseIf TypeOf self Is TRet Then
                    With CType(self, TRet)
                        tw.Fmt(ETkn.eReturn)
                        If .TrmRet IsNot Nothing Then
                            tw.Fmt(.TrmRet.TokenList)
                        End If
                        tw.Fmt(ETkn.eNL)

                    End With

                ElseIf TypeOf self Is TThrow Then
                    With CType(self, TThrow)
                        tw.Fmt(ETkn.eThrow, .TrmThrow.TokenList, ETkn.eNL)
                    End With

                ElseIf TypeOf self Is TComment Then
                    With CType(self, TComment)
                        For Each s In .LineCom
                            tw.Fmt(New TTkn(ETkn.eComment, s), ETkn.eNL)
                        Next
                    End With

                Else
                    Select Case .TypeStmt
                        Case ETkn.eExitDo, ETkn.eExitFor, ETkn.eExitSub
                            Select Case .TypeStmt
                                Case ETkn.eExitDo
                                    tw.Fmt(ETkn.eExitDo, ETkn.eNL)
                                Case ETkn.eExitFor
                                    tw.Fmt(ETkn.eExitFor, ETkn.eNL)
                                Case ETkn.eExitSub
                                    tw.Fmt(ETkn.eExitSub, ETkn.eNL)
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
                        tw.Fmt(s, ETkn.eNL)
                    Next
                End If

            End With
        End If

    End Sub
End Class
