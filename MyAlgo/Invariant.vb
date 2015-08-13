Imports System.Diagnostics

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

    Public Sub MakeBasicCode(self As Object)
        If TypeOf self Is TTerm Then
            With CType(self, TTerm)
                If TypeOf self Is TCns Then
                    With CType(self, TCns)
                        Dim tkn1 As TTkn

                        Select Case .TypeAtm
                            Case ETkn.eChar
                                tkn1 = New TTkn("""" + Escape(.NameRef) + """c", self)
                            Case ETkn.eString
                                tkn1 = New TTkn("""" + Escape(.NameRef) + """", self)
                            Case ETkn.eRegEx
                                tkn1 = New TTkn(Escape(.NameRef), self)
                            Case ETkn.eInt
                                tkn1 = New TTkn(.NameRef, self)
                            Case ETkn.eHex
                                tkn1 = New TTkn(.NameRef, self)
                                Debug.Assert(TSys.Substring(.NameRef, 0, 2) = "&H")
                            Case Else
                                tkn1 = Nothing
                                Debug.Assert(False)
                        End Select

                        .TokenList = New List(Of TTkn)() From {tkn1}
                    End With

                ElseIf TypeOf self Is TArray Then
                    With CType(self, TArray)
                        Dim vtkn As New List(Of TTkn)

                        vtkn.Add(New TTkn(ETkn.eLC, self))
                        vtkn.AddRange(Laminate((From trm In .TrmArr Select trm.TokenList), New TTkn(ETkn.eComma, self)))
                        vtkn.Add(New TTkn(ETkn.eRC, self))

                        .TokenList = vtkn
                    End With
                ElseIf TypeOf self Is TDot Then
                    With CType(self, TDot)

                        Dim vtkn As New List(Of TTkn)

                        If .TrmDot IsNot Nothing Then
                            vtkn.AddRange(.TrmDot.TokenList)
                        End If
                        vtkn.Add(New TTkn(ETkn.eDot, self))
                        vtkn.Add(New TTkn(.NameRef, self))
                    End With
                ElseIf TypeOf self Is TRef Then
                    With CType(self, TRef)

                        .TokenList = New List(Of TTkn)() From {New TTkn(.NameRef, self)}
                    End With
                ElseIf .IsOpr() Then
                    With CType(self, TApp)
                        If .Negation Then
                            ' ソース生成の前にNegationは除去すべき?
                            Debug.Assert(False)
                        End If
                        Select Case .TypeApp
                            Case ETkn.eOR, ETkn.eAnd, ETkn.eAnp, ETkn.eVLine
                                .TokenList = Laminate((From trm In .ArgApp Select trm.TokenList), New TTkn(.TypeApp, self))
                            Case ETkn.eNot
                                Dim vtkn As New List(Of TTkn)

                                vtkn.Add(New TTkn(ETkn.eNot, self))
                                vtkn.AddRange(.ArgApp(0).TokenList)

                                .TokenList = vtkn
                            Case Else
                                Debug.Assert(False)
                        End Select

                    End With
                ElseIf .IsApp() Then
                    With CType(self, TApp)
                        Dim vtkn As New List(Of TTkn)
                        Select Case .TypeApp
                            Case ETkn.eADD, ETkn.eMns, ETkn.eMUL, ETkn.eDIV, ETkn.eMOD
                                If .ArgApp.Count = 1 AndAlso (.TypeApp = ETkn.eADD OrElse .TypeApp = ETkn.eMns) Then
                                    vtkn.Add(New TTkn(.TypeApp, self))
                                    vtkn.AddRange(.ArgApp(0).TokenList)
                                Else

                                    vtkn.AddRange(.ArgApp(0).TokenList)
                                    vtkn.Add(New TTkn(.TypeApp, self))
                                    vtkn.AddRange(.ArgApp(1).TokenList)
                                End If

                            Case ETkn.eAppCall

                                vtkn.AddRange(.FncApp.TokenList)
                                vtkn.AddRange(AppArgTokenList(self))

                            Case ETkn.eBaseCall

                                vtkn.Add(New TTkn(ETkn.eBase, self))
                                vtkn.Add(New TTkn(ETkn.eDot, self))
                                vtkn.AddRange(.FncApp.TokenList)
                                vtkn.AddRange(AppArgTokenList(self))

                            Case ETkn.eBaseNew

                                vtkn.Add(New TTkn(ETkn.eBase, self))
                                vtkn.Add(New TTkn(ETkn.eDot, self))
                                vtkn.Add(New TTkn(ETkn.eNew, self))
                                vtkn.AddRange(AppArgTokenList(self))

                            Case ETkn.eNew
                                Debug.Assert(.NewApp IsNot Nothing)

                                vtkn.Add(New TTkn(ETkn.eNew, self))

                                If .IniApp Is Nothing Then
                                    ' 初期値がない場合

                                    vtkn.AddRange(.NewApp.TokenListCls)
                                    vtkn.AddRange(AppArgTokenList(self))
                                Else
                                    ' 初期値がある場合

                                    If .NewApp.IsArray() Then
                                        ' 配列の場合

                                        vtkn.AddRange(.NewApp.GenCla(0).TokenListCls)
                                        vtkn.AddRange(AppArgTokenList(self))
                                    Else
                                        ' 配列でない場合

                                        vtkn.AddRange(.NewApp.TokenListCls)
                                        vtkn.AddRange(AppArgTokenList(self))

                                        vtkn.Add(New TTkn(ETkn.eFrom, self))

                                    End If
                                    vtkn.AddRange(.IniApp.TokenList)
                                End If

                            Case ETkn.eAs, ETkn.eCast

                                vtkn.Add(New TTkn(ETkn.eCType, self))
                                vtkn.Add(New TTkn(ETkn.eLP, self))
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(ETkn.eComma, self))
                                vtkn.AddRange(.ClassApp.TokenListCls)
                                vtkn.Add(New TTkn(ETkn.eRP, self))

                            Case ETkn.eGetType
                                vtkn.Add(New TTkn(ETkn.eGetType, self))
                                vtkn.Add(New TTkn(ETkn.eLP, self))
                                vtkn.AddRange(.ClassApp.TokenListCls)
                                vtkn.Add(New TTkn(ETkn.eRP, self))

                            Case ETkn.eQUE
                                vtkn.Add(New TTkn(ETkn.eIIF, self))
                                vtkn.Add(New TTkn(ETkn.eLP, self))
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(ETkn.eComma, self))
                                vtkn.AddRange(.ArgApp(1).TokenList)
                                vtkn.Add(New TTkn(ETkn.eComma, self))
                                vtkn.AddRange(.ArgApp(2).TokenList)
                                vtkn.Add(New TTkn(ETkn.eRP, self))

                            Case ETkn.eTypeof
                                vtkn.Add(New TTkn(ETkn.eTypeof, self))
                                vtkn.Add(New TTkn(ETkn.eLP, self))
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(ETkn.eRP, self))

                            Case ETkn.eAddressOf
                                vtkn.Add(New TTkn(ETkn.eAddressOf, self))
                                vtkn.AddRange(.ArgApp(0).TokenList)

                            Case Else
                                Debug.WriteLine("Err Trm Src2:{0}", .TypeApp)
                                Debug.Assert(False)
                        End Select
                        .TokenList = vtkn

                    End With
                ElseIf .IsRel() Then
                    With CType(self, TApp)
                        Dim tp1 As TCls, tp2 As TCls
                        Dim vtkn As New List(Of TTkn)

                        Select Case .TypeApp
                            Case ETkn.eEq, ETkn.eNE
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                tp1 = TPrj.Prj.GetTermType(.ArgApp(0))
                                tp2 = TPrj.Prj.GetTermType(.ArgApp(1))
                                If tp1 Is Nothing OrElse tp2 Is Nothing Then
                                    ' Debug.WriteLine("");
                                    ' tp1 = TPrj.Prj.GetTermType(.ArgApp[0]);
                                    ' tp2 = TPrj.Prj.GetTermType(.ArgApp[1]);
                                End If
                                If tp1 IsNot Nothing AndAlso (tp1.IsAtomType() OrElse tp1.KndCla = EClass.eStructCla) OrElse tp2 IsNot Nothing AndAlso (tp2.IsAtomType() OrElse tp2.KndCla = EClass.eStructCla) Then
                                    vtkn.Add(New TTkn(.TypeApp, self))
                                Else
                                    If .TypeApp = ETkn.eNE Then
                                        vtkn.Add(New TTkn(ETkn.eIsNot, self))
                                    Else
                                        vtkn.Add(New TTkn(ETkn.eIs, self))
                                    End If
                                End If
                                vtkn.AddRange(.ArgApp(1).TokenList)
                            Case ETkn.eASN, ETkn.eLT, ETkn.eGT, ETkn.eADDEQ, ETkn.eSUBEQ, ETkn.eMULEQ, ETkn.eDIVEQ, ETkn.eMODEQ, ETkn.eLE, ETkn.eGE
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(.TypeApp, self))
                                vtkn.AddRange(.ArgApp(1).TokenList)
                            Case ETkn.eIsNot
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(ETkn.eIsNot, self))
                                vtkn.AddRange(.ArgApp(1).TokenList)

                            Case ETkn.eTypeof
                                vtkn.Add(New TTkn(ETkn.eTypeof, self))
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(ETkn.eIs, self))
                                vtkn.AddRange(.ArgApp(1).TokenList)

                            Case ETkn.eIs
                                vtkn.AddRange(.ArgApp(0).TokenList)
                                vtkn.Add(New TTkn(ETkn.eIs, self))
                                vtkn.AddRange(.ArgApp(1).TokenList)
                            Case Else
                                Debug.Assert(False)
                        End Select

                        .TokenList = vtkn

                    End With
                ElseIf TypeOf self Is TPar Then
                    With CType(self, TPar)
                        Dim vtkn As New List(Of TTkn)
                        If .TrmPar.IsApp() AndAlso CType(.TrmPar, TApp).TypeApp = ETkn.eCast Then
                            vtkn.AddRange(.TrmPar.TokenList)
                        Else
                            vtkn.Add(New TTkn(ETkn.eLP, self))
                            vtkn.AddRange(.TrmPar.TokenList)
                            vtkn.Add(New TTkn(ETkn.eRP, self))
                        End If
                        .TokenList = vtkn

                    End With
                ElseIf TypeOf self Is TFrom Then
                    With CType(self, TFrom)
                        Dim vtkn As New List(Of TTkn)
                        vtkn.Add(New TTkn(ETkn.eFrom, self))
                        vtkn.AddRange(.VarFrom.TokenListVar)
                        vtkn.Add(New TTkn(ETkn.eIn, self))
                        vtkn.AddRange(.SeqFrom.TokenList)

                        If .CndFrom IsNot Nothing Then

                            vtkn.Add(New TTkn(ETkn.eWhere, self))
                            vtkn.AddRange(.CndFrom.TokenList)
                        End If

                        If .SelFrom IsNot Nothing Then

                            vtkn.Add(New TTkn(ETkn.eSelect, self))
                            vtkn.AddRange(.SelFrom.TokenList)
                        End If

                        If .TakeFrom IsNot Nothing Then

                            vtkn.Add(New TTkn(ETkn.eTake, self))
                            vtkn.AddRange(.TakeFrom.TokenList)
                        End If

                        .TokenList = vtkn
                    End With

                ElseIf TypeOf self Is TAggregate Then
                    With CType(self, TAggregate)
                        Dim vtkn As New List(Of TTkn)
                        vtkn.Add(New TTkn(ETkn.eAggregate, self))
                        vtkn.AddRange(.VarAggr.TokenListVar)
                        vtkn.Add(New TTkn(ETkn.eIn, self))
                        vtkn.AddRange(.SeqAggr.TokenList)

                        vtkn.Add(New TTkn(ETkn.eInto, self))

                        Select Case .FunctionAggr
                            Case EAggregateFunction.eSum
                                vtkn.Add(New TTkn("Sum", self))
                            Case EAggregateFunction.eMax
                                vtkn.Add(New TTkn("Max", self))
                            Case EAggregateFunction.eMin
                                vtkn.Add(New TTkn("Min", self))
                            Case Else
                                Debug.Assert(False)
                        End Select
                        vtkn.Add(New TTkn(ETkn.eLP, self))

                        vtkn.AddRange(.IntoAggr.TokenList)

                        vtkn.Add(New TTkn(ETkn.eRP, self))

                        .TokenList = vtkn
                    End With
                Else
                    Debug.Assert(False)
                End If

            End With
        End If

    End Sub

End Class
