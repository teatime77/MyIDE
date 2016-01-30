Imports System.Diagnostics

' -------------------------------------------------------------------------------- EFigType
Public Enum EFigType
    SymFig
    ClassFig
    ResFig
    VarFig
    RefFig
    LabelFig
    NumFig
    StrFig
    ComFig
    UnknownFig
End Enum

' -------------------------------------------------------------------------------- TCodeGenerator
Public MustInherit Class TCodeGenerator
    Public vLineFig As New TList(Of FLine)
    Dim CurLine As New FLine
    Public PrjMK As TProject
    Public ParserCG As TSourceParser
    Public vDelegate As New TList(Of TDelegatePair)
    Public TabMK As Integer
    Public Shared HTMLHead As String = "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Transitional//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"">" + vbCr + vbLf + "<html xmlns=""http://www.w3.org/1999/xhtml"" >" + vbCr + vbLf + "<head>" + vbCr + vbLf + "<meta charset=""utf-8"" />" + vbCr + vbLf + "<title>Untitled Page</title>" + vbCr + vbLf + "<style type=""text/css"">" + vbCr + vbLf + ".reserved {" + vbCr + vbLf + vbTab + "color: blue;" + vbCr + vbLf + "}" + vbCr + vbLf + ".class {" + vbCr + vbLf + vbTab + "color: Teal;" + vbCr + "" + vbLf + "}" + vbCr + vbLf + ".string {" + vbCr + vbLf + vbTab + "color: red;" + vbCr + vbLf + "}" + vbCr + vbLf + ".comment {" + vbCr + vbLf + vbTab + "color: #008000;" + vbCr + vbLf + "}" + vbCr + vbLf + "</style>" + vbCr + vbLf + "</head>" + vbCr + vbLf + "<body>"

    Public Sub New(prj1 As TProject, parser As TSourceParser)
        PrjMK = prj1
        ParserCG = parser
    End Sub

    '   単語を追加する
    Public Sub WordAdd(str1 As String, type1 As EFigType, obj1 As Object)
        Dim txt1 As FText
        If type1 = EFigType.ComFig Then
            txt1 = New FText(type1, obj1, ParserCG.vTknName(EToken.LineComment) + " " + str1)
        Else
            txt1 = New FText(type1, obj1, str1)
        End If
        txt1.TabTxt = TabMK
        TabMK = 0
        CurLine.AddTextLine(txt1)
    End Sub

    '   単語を追加する
    Public Sub WordAdd(tkn1 As EToken, type1 As EFigType, obj1 As Object)
        Dim txt1 As FText
        If ParserCG.vTknName.ContainsKey(tkn1) Then
            txt1 = New FText(tkn1, type1, obj1, ParserCG.vTknName(tkn1))
        Else
            txt1 = New FText(tkn1, type1, obj1, TSys.Format("未対応:{0}", tkn1))
            Debug.WriteLine("未対応:{0}", tkn1)
        End If
        txt1.TabTxt = TabMK
        TabMK = 0
        CurLine.AddTextLine(txt1)
    End Sub

    '   単語を追加する
    Public Sub WordAdd(str1 As String, tkn1 As EToken, type1 As EFigType, obj1 As Object)
        Dim txt1 As FText
        txt1 = New FText(tkn1, type1, obj1, str1)
        txt1.TabTxt = TabMK
        TabMK = 0
        CurLine.AddTextLine(txt1)
    End Sub

    Public Sub Fmt(o1 As Object)
        Dim tkn1 As TToken, ref1 As TReference

        If TypeOf o1 Is TToken Then

            tkn1 = CType(o1, TToken)
            If Char.IsLetter(tkn1.StrTkn(0)) Then

                WordAdd(tkn1.StrTkn, tkn1.TypeTkn, EFigType.ResFig, Nothing)
            Else
                WordAdd(tkn1.StrTkn, tkn1.TypeTkn, EFigType.SymFig, Nothing)
            End If
        ElseIf TypeOf o1 Is TDot Then
            ref1 = CType(o1, TReference)
            If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(ref1.NameRef) Then
                WordAdd(PrjMK.ClassNameTable(ref1.NameRef), EToken.Ref, EFigType.RefFig, ref1)
            Else
                WordAdd(ref1.NameRef, EToken.Ref, EFigType.RefFig, ref1)
            End If

        ElseIf TypeOf o1 Is TReference Then

            ref1 = CType(o1, TReference)
            If ref1.VarRef IsNot Nothing Then
                If TypeOf ref1.VarRef Is TClass Then
                    TypeSrc(CType(ref1.VarRef, TClass))
                Else
                    If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(ref1.NameRef) Then
                        WordAdd(PrjMK.ClassNameTable(ref1.NameRef), EToken.Ref, EFigType.RefFig, ref1)
                    Else
                        WordAdd(ref1.NameRef, EToken.Ref, EFigType.RefFig, ref1)
                    End If
                End If
            Else
                WordAdd(ref1.NameRef, EToken.Ref, EFigType.RefFig, ref1)
            End If

        ElseIf TypeOf o1 Is TClass Then
            WordAdd(TypeName(CType(o1, TClass).NameCla()), EFigType.ClassFig, o1)

        ElseIf TypeOf o1 Is TVariable Then
            If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(CType(o1, TVariable).NameVar) Then
                WordAdd(PrjMK.ClassNameTable(CType(o1, TVariable).NameVar), EFigType.VarFig, o1)
            Else
                WordAdd(CType(o1, TVariable).NameVar, EFigType.VarFig, o1)
            End If


        ElseIf TypeOf o1 Is EToken Then
            If ParserCG.vTknName.ContainsKey(CType(o1, EToken)) Then
                If Char.IsLetter(ParserCG.vTknName(o1)(0)) Then
                    WordAdd(CType(o1, EToken), EFigType.ResFig, Nothing)
                Else
                    WordAdd(CType(o1, EToken), EFigType.SymFig, Nothing)
                End If
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
    Public MustOverride Sub AppSrc(app1 As TApply)
    Public MustOverride Sub CnsSrc(cns1 As TConstant)

    Public Sub ArraySrc(arr1 As TArray)
        Dim i1 As Integer

        WordAdd("{", EFigType.SymFig, arr1)
        For i1 = 0 To arr1.TrmArr.Count - 1
            If i1 <> 0 Then
                WordAdd(",", EFigType.SymFig, arr1)
            End If
            TrmSrc(arr1.TrmArr(i1))
        Next
        WordAdd("}", EFigType.SymFig, arr1)
    End Sub

    Public MustOverride Sub DotSrc(dot1 As TDot)
    Public MustOverride Sub RefSrc(ref1 As TReference)
    Public MustOverride Sub RelSrc(rel1 As TApply)

    Public Sub OprSrc(opr1 As TApply)
        Dim i1 As Integer

        Select Case opr1.TypeApp
            Case EToken.OR_, EToken.And_, EToken.Anp, EToken.BitOR
                For i1 = 0 To opr1.ArgApp.Count - 1
                    If i1 <> 0 Then
                        WordAdd(ParserCG.vTknName(opr1.TypeApp), EFigType.SymFig, opr1)
                    End If
                    TrmSrc(opr1.ArgApp(i1))
                Next
            Case Else
                Debug.Assert(opr1.TypeApp = EToken.Not_ OrElse opr1.Negation)

                WordAdd(ParserCG.vTknName(EToken.Not_), EFigType.SymFig, opr1)
                If opr1.TypeApp = EToken.Not_ Then
                    TrmSrc(opr1.ArgApp(0))
                Else
                    opr1.Negation = False
                    TrmSrc(opr1)
                    opr1.Negation = True
                End If
        End Select
    End Sub

    Public MustOverride Sub ParSrc(par1 As TParenthesis)
    Public MustOverride Sub FromSrc(from1 As TFrom)
    Public MustOverride Sub AggregateSrc(aggr1 As TAggregate)

    Public Sub TrmSrc(trm1 As TTerm)
        If trm1 IsNot Nothing Then
            If TypeOf trm1 Is TConstant Then
                CnsSrc(CType(trm1, TConstant))
            ElseIf TypeOf trm1 Is TArray Then
                ArraySrc(CType(trm1, TArray))
            ElseIf TypeOf trm1 Is TDot Then
                DotSrc(CType(trm1, TDot))
            ElseIf TypeOf trm1 Is TReference Then
                RefSrc(CType(trm1, TReference))
            ElseIf trm1.IsOpr() Then
                OprSrc(CType(trm1, TApply))
            ElseIf trm1.IsApp() Then
                AppSrc(CType(trm1, TApply))
            ElseIf trm1.IsRel() Then
                RelSrc(CType(trm1, TApply))
            ElseIf TypeOf trm1 Is TParenthesis Then
                ParSrc(CType(trm1, TParenthesis))
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
    Public MustOverride Sub SimpleStmtSrc(stmt1 As TStatement, tab1 As Integer)
    Public MustOverride Sub SelectSrc(swt1 As TSelect, tab1 As Integer)
    Public MustOverride Sub TrySrc(try1 As TTry, tab1 As Integer)

    ' コメントのソースを作る
    Public Sub ComSrc(com1 As TComment, tab1 As Integer, obj1 As Object)
        If com1 IsNot Nothing Then
            For Each s In com1.LineCom
                If s <> "" Then
                    Tab(tab1)
                    WordAdd(s, EFigType.ComFig, obj1)
                End If
                NL(obj1)
            Next
        End If
    End Sub

    Public MustOverride Sub StmtSrc(stmt1 As TStatement, tab1 As Integer)

    Public Sub AppArg(app1 As TApply)
        Dim i1 As Integer
        WordAdd("(", EFigType.SymFig, Me)
        For i1 = 0 To app1.ArgApp.Count - 1
            If i1 <> 0 Then
                WordAdd(",", EFigType.SymFig, Me)
            End If
            TrmSrc(app1.ArgApp(i1))
        Next
        WordAdd(")", EFigType.SymFig, Me)
    End Sub

    Public MustOverride Sub BlcSrc(obj1 As Object, type1 As EToken, blc1 As TBlock, tab1 As Integer)
    Public MustOverride Sub VarSrc(var1 As TVariable)

    '  変数リストのソースを作る
    Public Sub VarListSrc(vvar As TList(Of TVariable), obj1 As Object)
        Dim i1 As Integer
        Dim var1 As TVariable
        WordAdd("(", EFigType.SymFig, obj1)
        For i1 = 0 To vvar.Count - 1
            var1 = vvar(i1)
            If i1 <> 0 Then
                WordAdd(",", EFigType.SymFig, obj1)
            End If
            VarSrc(var1)
        Next
        WordAdd(")", EFigType.SymFig, obj1)
    End Sub

    Public MustOverride Sub FncSrc(fnc1 As TFunction)
    Public MustOverride Function TypeName(name1 As String) As String
    Public MustOverride Sub TypeSrc(type1 As TClass)
    Public MustOverride Sub VarDeclSrc(dcl1 As TVariableDeclaration, tab1 As Integer)

    Public Sub ModifierSrc(obj1 As Object, mod1 As TModifier)
        If mod1 IsNot Nothing Then
            If mod1.isPublic Then
                WordAdd(EToken.Public_, EFigType.ResFig, obj1)
            End If
            If mod1.isShared Then
                WordAdd(EToken.Shared_, EFigType.ResFig, obj1)
            End If
            If mod1.isIterator Then
                WordAdd(EToken.Iterator_, EFigType.ResFig, obj1)
            End If
            If mod1.isConst Then
                WordAdd(EToken.Const_, EFigType.ResFig, obj1)
            End If
            If mod1.isVirtual Then
                WordAdd(EToken.Virtual, EFigType.ResFig, obj1)
            End If
            If mod1.isMustOverride Then
                WordAdd(EToken.MustOverride_, EFigType.ResFig, obj1)
            End If
            If mod1.isOverride Then
                WordAdd(EToken.Override, EFigType.ResFig, obj1)
            End If
        End If
    End Sub

    Public Function ClassType(type1 As EFigType) As String
        Select Case type1
            Case EFigType.ResFig
                Return "class=""reserved"""
            Case EFigType.ClassFig
                Return "class=""class"""
            Case EFigType.StrFig
                Return "class=""string"""
            Case EFigType.ComFig
                Return "class=""comment"""
        End Select
        Return ""
    End Function

    Public MustOverride Function MakeSrcText() As String

    Public Function MakeSrcHTML(src1 As TSourceFile) As String
        Dim sw As TStringWriter
        Dim var1 As TVariable
        Dim ref1 As TReference

        sw = New TStringWriter()
        sw.WriteLine(HTMLHead + "<pre><code>")
        For Each line1 In vLineFig
            For Each txt1 In line1.TextLine
                If txt1.TabTxt <> 0 Then
                    sw.Write(TSys.StringRepeat(" ", txt1.TabTxt * 4))
                End If
                Select Case txt1.TypeFig
                    Case EFigType.VarFig
                        var1 = CType(txt1.ObjFig, TVariable)
                        sw.Write(" <a name=""var{0}"" {1}>{2}</a>", var1.IdxVar, ClassType(txt1.TypeFig), txt1.TextTxt)
                    Case EFigType.RefFig
                        ref1 = CType(txt1.ObjFig, TReference)
                        PrjMK.CheckRefVar(ref1)
                        If ref1.VarRef Is Nothing Then
                            sw.Write(" <span {0}>{1}</span>", ClassType(txt1.TypeFig), txt1.TextTxt)
                        Else
                            If TypeOf ref1.VarRef Is TField OrElse TypeOf ref1.VarRef Is TFunction Then
                                sw.Write(" <a href=""../{0}/{1}.html"" {2}>{3}</a>", TProject.GetHtmlFileName(ref1.VarRef.GetClassVar()), TProject.GetHtmlFileName(ref1.VarRef), ClassType(txt1.TypeFig), txt1.TextTxt)
                            Else
                                sw.Write(" <a href=""#var{0}"" {1}>{2}</a>", ref1.VarRef.IdxVar, ClassType(txt1.TypeFig), txt1.TextTxt)
                            End If
                        End If
                    Case EFigType.ComFig
                        sw.Write("<span {0}>{1}</span>", ClassType(txt1.TypeFig), txt1.TextTxt)
                    Case EFigType.SymFig, EFigType.StrFig
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

    Public Sub CopyObj(cla1 As TClass)
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
    Public ExpLine As EExpand = EExpand.None
    Public TextLine As New TList(Of FText)
    Public Sub New()
        MyBase.New(EFigType.UnknownFig, Nothing)
    End Sub

    Public Sub AddTextLine(txt1 As FText)
        TextLine.Add(txt1)
    End Sub

End Class

' -------------------------------------------------------------------------------- FText
Public Class FText
    Inherits FFig
    Public TypeTxt As EFigType = EFigType.UnknownFig
    Public TknTxt As EToken = EToken.Unknown
    Public TabTxt As Integer
    Public TextTxt As String

    Public Sub New(tkn1 As EToken, type1 As EFigType, obj1 As Object, txt1 As String)
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
