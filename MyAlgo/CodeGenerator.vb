Imports System.Diagnostics

' -------------------------------------------------------------------------------- EFigType
Public Enum EFigType
    eSymFig
    eClassFig
    eResFig
    eVarFig
    eRefFig
    eLabelFig
    eNumFig
    eStrFig
    eComFig
    eUnknownFig
End Enum

' -------------------------------------------------------------------------------- TCodeGenerator
Public MustInherit Class TCodeGenerator
    Public vLineFig As New TList(Of FLine)
    Dim CurLine As New FLine
    Public vTknNameMK As Dictionary(Of ETkn, String)
    Public PrjMK As TPrj
    Public vDelegate As New TList(Of TDelegatePair)
    Public TabMK As Integer
    Public Shared HTMLHead As String = "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Transitional//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"">" + vbCr + vbLf + "<html xmlns=""http://www.w3.org/1999/xhtml"" >" + vbCr + vbLf + "<head>" + vbCr + vbLf + "<meta charset=""utf-8"" />" + vbCr + vbLf + "<title>Untitled Page</title>" + vbCr + vbLf + "<style type=""text/css"">" + vbCr + vbLf + ".reserved {" + vbCr + vbLf + vbTab + "color: blue;" + vbCr + vbLf + "}" + vbCr + vbLf + ".class {" + vbCr + vbLf + vbTab + "color: Teal;" + vbCr + "" + vbLf + "}" + vbCr + vbLf + ".string {" + vbCr + vbLf + vbTab + "color: red;" + vbCr + vbLf + "}" + vbCr + vbLf + ".comment {" + vbCr + vbLf + vbTab + "color: #008000;" + vbCr + vbLf + "}" + vbCr + vbLf + "</style>" + vbCr + vbLf + "</head>" + vbCr + vbLf + "<body>"

    Public Sub New(prj1 As TPrj)
        vTknNameMK = prj1.vTknNamePrj
        PrjMK = prj1
    End Sub

    '   単語を追加する
    Public Sub WordAdd(str1 As String, type1 As EFigType, obj1 As Object)
        Dim txt1 As FText
        If type1 = EFigType.eComFig Then
            txt1 = New FText(type1, obj1, vTknNameMK(ETkn.eLineComment) + " " + str1)
        Else
            txt1 = New FText(type1, obj1, str1)
        End If
        txt1.TabTxt = TabMK
        TabMK = 0
        CurLine.AddTextLine(txt1)
    End Sub

    '   単語を追加する
    Public Sub WordAdd(tkn1 As ETkn, type1 As EFigType, obj1 As Object)
        Dim txt1 As FText
        If vTknNameMK.ContainsKey(tkn1) Then
            txt1 = New FText(tkn1, type1, obj1, vTknNameMK(tkn1))
        Else
            txt1 = New FText(tkn1, type1, obj1, TSys.Format("未対応:{0}", tkn1))
            Debug.WriteLine("未対応:{0}", tkn1)
        End If
        txt1.TabTxt = TabMK
        TabMK = 0
        CurLine.AddTextLine(txt1)
    End Sub

    '   単語を追加する
    Public Sub WordAdd(str1 As String, tkn1 As ETkn, type1 As EFigType, obj1 As Object)
        Dim txt1 As FText
        txt1 = New FText(tkn1, type1, obj1, str1)
        txt1.TabTxt = TabMK
        TabMK = 0
        CurLine.AddTextLine(txt1)
    End Sub

    Public Sub Fmt(o1 As Object)
        Dim tkn1 As TTkn, ref1 As TRef

        If TypeOf o1 Is TTkn Then

            tkn1 = CType(o1, TTkn)
            If Char.IsLetter(tkn1.StrTkn(0)) Then

                WordAdd(tkn1.StrTkn, tkn1.TypeTkn, EFigType.eResFig, Nothing)
            Else
                WordAdd(tkn1.StrTkn, tkn1.TypeTkn, EFigType.eSymFig, Nothing)
            End If
        ElseIf TypeOf o1 Is TDot Then
            ref1 = CType(o1, TRef)
            If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(ref1.NameRef) Then
                WordAdd(PrjMK.ClassNameTable(ref1.NameRef), ETkn.eRef, EFigType.eRefFig, ref1)
            Else
                WordAdd(ref1.NameRef, ETkn.eRef, EFigType.eRefFig, ref1)
            End If

        ElseIf TypeOf o1 Is TRef Then

            ref1 = CType(o1, TRef)
            If ref1.VarRef IsNot Nothing Then
                If TypeOf ref1.VarRef Is TCls Then
                    TypeSrc(CType(ref1.VarRef, TCls))
                Else
                    If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(ref1.NameRef) Then
                        WordAdd(PrjMK.ClassNameTable(ref1.NameRef), ETkn.eRef, EFigType.eRefFig, ref1)
                    Else
                        WordAdd(ref1.NameRef, ETkn.eRef, EFigType.eRefFig, ref1)
                    End If
                End If
            Else
                WordAdd(ref1.NameRef, ETkn.eRef, EFigType.eRefFig, ref1)
            End If

        ElseIf TypeOf o1 Is TCls Then
            WordAdd(TypeName(CType(o1, TCls).NameCla()), EFigType.eClassFig, o1)

        ElseIf TypeOf o1 Is TVar Then
            If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(CType(o1, TVar).NameVar) Then
                WordAdd(PrjMK.ClassNameTable(CType(o1, TVar).NameVar), EFigType.eVarFig, o1)
            Else
                WordAdd(CType(o1, TVar).NameVar, EFigType.eVarFig, o1)
            End If


        ElseIf TypeOf o1 Is ETkn Then
            If Char.IsLetter(vTknNameMK(o1)(0)) Then
                WordAdd(CType(o1, ETkn), EFigType.eResFig, Nothing)
            Else
                WordAdd(CType(o1, ETkn), EFigType.eSymFig, Nothing)
            End If
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

    '   タブを追加する
    Public Sub Tab(tab1 As Integer)
        TabMK = tab1
    End Sub

    '   改行を追加する
    Public Sub NL(obj As Object)
        CurLine.ObjFig = obj
        vLineFig.Add(CurLine)
        CurLine = New FLine()
    End Sub

    '   改行を追加する
    Public Sub NL()
        vLineFig.Add(CurLine)
        CurLine = New FLine()
    End Sub

    Public Sub ClearFM()
        vLineFig.Clear()
        CurLine = New FLine()
    End Sub

    Public MustOverride Function Escape(str1 As String) As String
    Public MustOverride Sub AppSrc(app1 As TApp)
    Public MustOverride Sub CnsSrc(cns1 As TCns)

    Public Sub ArraySrc(arr1 As TArray)
        Dim i1 As Integer

        WordAdd("{", EFigType.eSymFig, arr1)
        For i1 = 0 To arr1.TrmArr.Count - 1
            If i1 <> 0 Then
                WordAdd(",", EFigType.eSymFig, arr1)
            End If
            TrmSrc(arr1.TrmArr(i1))
        Next
        WordAdd("}", EFigType.eSymFig, arr1)
    End Sub

    Public MustOverride Sub DotSrc(dot1 As TDot)
    Public MustOverride Sub RefSrc(ref1 As TRef)
    Public MustOverride Sub RelSrc(rel1 As TApp)

    Public Sub OprSrc(opr1 As TApp)
        Dim i1 As Integer

        Select Case opr1.TypeApp
            Case ETkn.eOR, ETkn.eAnd, ETkn.eAnp, ETkn.eVLine
                For i1 = 0 To opr1.ArgApp.Count - 1
                    If i1 <> 0 Then
                        WordAdd(vTknNameMK(opr1.TypeApp), EFigType.eSymFig, opr1)
                    End If
                    TrmSrc(opr1.ArgApp(i1))
                Next
            Case Else
                Debug.Assert(opr1.TypeApp = ETkn.eNot OrElse opr1.Negation)

                WordAdd(vTknNameMK(ETkn.eNot), EFigType.eSymFig, opr1)
                If opr1.TypeApp = ETkn.eNot Then
                    TrmSrc(opr1.ArgApp(0))
                Else
                    opr1.Negation = False
                    TrmSrc(opr1)
                    opr1.Negation = True
                End If
        End Select
    End Sub

    Public MustOverride Sub ParSrc(par1 As TPar)
    Public MustOverride Sub FromSrc(from1 As TFrom)
    Public MustOverride Sub AggregateSrc(aggr1 As TAggregate)

    Public Sub TrmSrc(trm1 As TTerm)
        If trm1 IsNot Nothing Then
            If TypeOf trm1 Is TCns Then
                CnsSrc(CType(trm1, TCns))
            ElseIf TypeOf trm1 Is TArray Then
                ArraySrc(CType(trm1, TArray))
            ElseIf TypeOf trm1 Is TDot Then
                DotSrc(CType(trm1, TDot))
            ElseIf TypeOf trm1 Is TRef Then
                RefSrc(CType(trm1, TRef))
            ElseIf trm1.IsOpr() Then
                OprSrc(CType(trm1, TApp))
            ElseIf trm1.IsApp() Then
                AppSrc(CType(trm1, TApp))
            ElseIf trm1.IsRel() Then
                RelSrc(CType(trm1, TApp))
            ElseIf TypeOf trm1 Is TPar Then
                ParSrc(CType(trm1, TPar))
            ElseIf TypeOf trm1 Is TFrom Then
                FromSrc(CType(trm1, TFrom))
            ElseIf TypeOf trm1 Is TAggregate Then
                AggregateSrc(CType(trm1, TAggregate))
            Else
                Debug.Assert(False)
            End If
        End If
    End Sub

    Public MustOverride Sub IfSrc(if1 As TIf, tab1 As Integer)
    Public MustOverride Sub ForSrc(for1 As TFor, tab1 As Integer)
    Public MustOverride Sub SimpleStmtSrc(stmt1 As TStmt, tab1 As Integer)
    Public MustOverride Sub SelectSrc(swt1 As TSelect, tab1 As Integer)
    Public MustOverride Sub TrySrc(try1 As TTry, tab1 As Integer)
    Public MustOverride Sub WithSrc(with1 As TWith, tab1 As Integer)

    ' コメントのソースを作る
    Public Sub ComSrc(com1 As TComment, tab1 As Integer, obj1 As Object)
        If com1 IsNot Nothing Then
            For Each s In com1.LineCom
                If s <> "" Then
                    Tab(tab1)
                    WordAdd(s, EFigType.eComFig, obj1)
                End If
                NL(obj1)
            Next
        End If
    End Sub

    Public MustOverride Sub StmtSrc(stmt1 As TStmt, tab1 As Integer)

    Public Sub AppArg(app1 As TApp)
        Dim i1 As Integer
        WordAdd("(", EFigType.eSymFig, Me)
        For i1 = 0 To app1.ArgApp.Count - 1
            If i1 <> 0 Then
                WordAdd(",", EFigType.eSymFig, Me)
            End If
            TrmSrc(app1.ArgApp(i1))
        Next
        WordAdd(")", EFigType.eSymFig, Me)
    End Sub

    Public MustOverride Sub BlcSrc(obj1 As Object, type1 As ETkn, blc1 As TBlc, tab1 As Integer)
    Public MustOverride Sub VarSrc(var1 As TVar)

    '  変数リストのソースを作る
    Public Sub VarListSrc(vvar As TList(Of TVar), obj1 As Object)
        Dim i1 As Integer
        Dim var1 As TVar
        WordAdd("(", EFigType.eSymFig, obj1)
        For i1 = 0 To vvar.Count - 1
            var1 = vvar(i1)
            If i1 <> 0 Then
                WordAdd(",", EFigType.eSymFig, obj1)
            End If
            VarSrc(var1)
        Next
        WordAdd(")", EFigType.eSymFig, obj1)
    End Sub

    Public MustOverride Sub FncSrc(fnc1 As TFnc)
    Public MustOverride Function TypeName(name1 As String) As String
    Public MustOverride Sub TypeSrc(type1 As TCls)
    Public MustOverride Sub VarDeclSrc(dcl1 As TVarDecl, tab1 As Integer)

    Public Sub ModifierSrc(obj1 As Object, mod1 As TModifier)
        If mod1 IsNot Nothing Then
            If mod1.isPublic Then
                WordAdd(ETkn.ePublic, EFigType.eResFig, obj1)
            End If
            If mod1.isShared Then
                WordAdd(ETkn.eShared, EFigType.eResFig, obj1)
            End If
            If mod1.isConst Then
                WordAdd(ETkn.eConst, EFigType.eResFig, obj1)
            End If
            If mod1.isVirtual Then
                WordAdd(ETkn.eVirtual, EFigType.eResFig, obj1)
            End If
            If mod1.isMustOverride Then
                WordAdd(ETkn.eMustOverride, EFigType.eResFig, obj1)
            End If
            If mod1.isOverride Then
                WordAdd(ETkn.eOverride, EFigType.eResFig, obj1)
            End If
        End If
    End Sub

    Public Function ClassType(type1 As EFigType) As String
        Select Case type1
            Case EFigType.eResFig
                Return "class=""reserved"""
            Case EFigType.eClassFig
                Return "class=""class"""
            Case EFigType.eStrFig
                Return "class=""string"""
            Case EFigType.eComFig
                Return "class=""comment"""
        End Select
        Return ""
    End Function

    Public MustOverride Function MakeSrcText() As String

    Public Function MakeSrcHTML(src1 As TSrc) As String
        Dim sw As TStringWriter
        Dim var1 As TVar
        Dim ref1 As TRef

        sw = New TStringWriter()
        sw.WriteLine(HTMLHead + "<pre><code>")
        For Each line1 In vLineFig
            For Each txt1 In line1.TextLine
                If txt1.TabTxt <> 0 Then
                    sw.Write(TSys.StringRepeat(" ", txt1.TabTxt * 4))
                End If
                Select Case txt1.TypeFig
                    Case EFigType.eVarFig
                        var1 = CType(txt1.ObjFig, TVar)
                        sw.Write(" <a name=""var{0}"" {1}>{2}</a>", var1.IdxVar, ClassType(txt1.TypeFig), txt1.TextTxt)
                    Case EFigType.eRefFig
                        ref1 = CType(txt1.ObjFig, TRef)
                        PrjMK.CheckRefVar(ref1)
                        If ref1.VarRef Is Nothing Then
                            sw.Write(" <span {0}>{1}</span>", ClassType(txt1.TypeFig), txt1.TextTxt)
                        Else
                            If TypeOf ref1.VarRef Is TFld OrElse TypeOf ref1.VarRef Is TFnc Then
                                sw.Write(" <a href=""../{0}/{1}.html"" {2}>{3}</a>", TPrj.GetHtmlFileName(ref1.VarRef.GetClassVar()), TPrj.GetHtmlFileName(ref1.VarRef), ClassType(txt1.TypeFig), txt1.TextTxt)
                            Else
                                sw.Write(" <a href=""#var{0}"" {1}>{2}</a>", ref1.VarRef.IdxVar, ClassType(txt1.TypeFig), txt1.TextTxt)
                            End If
                        End If
                    Case EFigType.eComFig
                        sw.Write("<span {0}>{1}</span>", ClassType(txt1.TypeFig), txt1.TextTxt)
                    Case EFigType.eSymFig, EFigType.eStrFig
                        sw.Write(" <span {0}>{1}</span>", ClassType(txt1.TypeFig), txt1.TextTxt.Replace("<", "&lt;").Replace(">", "&gt;"))
                    Case Else
                        sw.Write(" <span {0}>{1}</span>", ClassType(txt1.TypeFig), txt1.TextTxt)
                End Select
            Next
            sw.WriteLine("")
        Next
        sw.WriteLine("</code></pre></body></html>")
        Return sw.ToString()
    End Function

    Public Sub CopyObj(cla1 As TCls)
        Dim dic1 As New Dictionary(Of Object, Object), sw As TStringWriter

        sw = New TStringWriter()

    End Sub
End Class

' -------------------------------------------------------------------------------- FFig
Public Class FFig
    Public TypeFig As EFigType
    Public ObjFig As Object
    Public RectFig As TRect
    Public Function PosFig() As TPnt
        Return RectFig.PosR
    End Function

    Public Function SizeFig() As TPnt
        Return RectFig.SizeR
    End Function

    Public Function LeftFig() As Double
        Return PosFig().XPnt
    End Function

    Public Function TopFig() As Double
        Return PosFig().YPnt
    End Function

    Public Function RightFig() As Double
        Return PosFig().XPnt + SizeFig().XPnt
    End Function

    Public Function BottomFig() As Double
        Return PosFig().YPnt + SizeFig().YPnt
    End Function

    Public Function IntersectFig(fig1 As FFig) As Boolean
        Return RectFig.Intersect(fig1.RectFig)
    End Function

    Public Sub New(type1 As EFigType, obj1 As Object)
        TypeFig = type1
        ObjFig = obj1
    End Sub

End Class

' -------------------------------------------------------------------------------- FBlc
Public Class FBlc
    Inherits FFig
    Public FigBlc As New TList(Of FFig)
    Public Sub New(type1 As EFigType, obj1 As Object)
        MyBase.New(type1, obj1)
    End Sub

    Public Sub AddFig(fig1 As FFig)
        FigBlc.Add(fig1)
    End Sub

End Class

' -------------------------------------------------------------------------------- FLine
Public Class FLine
    Inherits FBlc
    Public ExpLine As EExpand = EExpand.eNone
    Public TextLine As New TList(Of FText)
    Public Sub New()
        MyBase.New(EFigType.eUnknownFig, Nothing)
    End Sub

    Public Sub AddTextLine(txt1 As FText)
        TextLine.Add(txt1)
    End Sub

End Class

' -------------------------------------------------------------------------------- FText
Public Class FText
    Inherits FFig
    Public TypeTxt As EFigType = EFigType.eUnknownFig
    Public TknTxt As ETkn = ETkn.eUnknown
    Public TabTxt As Integer
    Public TextTxt As String

    Public Sub New(tkn1 As ETkn, type1 As EFigType, obj1 As Object, txt1 As String)
        MyBase.New(type1, obj1)
        TypeTxt = type1
        TknTxt = tkn1
        TextTxt = txt1
    End Sub

    Public Sub New(type1 As EFigType, obj1 As Object, txt1 As String)
        MyBase.New(type1, obj1)
        TypeTxt = type1
        If txt1 = "is" Then
            Debug.WriteLine("@e")
        End If
        TextTxt = txt1
    End Sub

End Class
