Imports System.IO

Partial Public Class TProject

    Public Sub MakeAllHtml()
        Dim all_class_dir As String, class_dir As String, paht1 As String, sw As TStringWriter, index_sw As New StringWriter, class_sw As StringWriter
        Dim indent As Integer, com_str As String, fname As String, html_file_name As String, html_class_file_name As String, svg_file_name As String
        Dim vfnc As List(Of TFunction), idx As Integer, def_ref As Boolean, vref As List(Of TReference), vfld As TList(Of TField), fld2 As TField

        all_class_dir = OutputDirectory + "\html"
        index_sw.WriteLine(TCodeGenerator.HTMLHead)
        index_sw.WriteLine("<ul>")

        '  すべてのソースに対し
        For Each src1 In SrcPrj
            src1.FigSrc = New TBasicCodeGenerator(Me, ParsePrj)

            ' ソース内のすべてのクラスに対し
            For Each cls1 In src1.ClaSrc
                If cls1.GenCla Is Nothing Then
                    ' 総称型でない場合

                    html_class_file_name = GetHtmlFileName(cls1)
                    index_sw.WriteLine(vbTab + "<li><a href=""{0}/{0}.html"">{1}</a></li>", html_class_file_name, cls1.NameCla())

                    class_dir = all_class_dir + "\" + html_class_file_name
                    TDirectory.CreateDirectory(class_dir)

                    class_sw = New StringWriter()

                    class_sw.WriteLine(TCodeGenerator.HTMLHead)
                    class_sw.WriteLine("<h1>{0} クラス</h1>", cls1.NameVar)

                    class_sw.WriteLine("<h2>継承階層</h2>")
                    If cls1.SuperClassList.Count <> 0 Then

                        indent = WriteInheritanceHierarchy(class_sw, cls1.SuperClassList(0))
                    End If

                    class_sw.WriteLine("<p style=""text-indent:{0}em"">{1}</p>", indent * 2, cls1.NameVar)

                    For Each sub_class In cls1.SubClassList
                        If SimpleParameterizedClassList.Contains(sub_class) Then
                            class_sw.WriteLine("<p style=""text-indent:{0}em""><a href=""../{1}/{1}.html"">{1}</a></p>", (indent + 1) * 2, sub_class.NameVar)
                        End If
                    Next

                    Select Case cls1.KndCla
                        Case EClass.EnumCla
                        Case EClass.DelegateCla
                        Case Else

                            class_sw.WriteLine("<h2>フィールド</h2>")

                            class_sw.WriteLine("<table border=""1""><thead><tr><th>名前</th><th>説明</th></tr></thead><tbody>")

                            ' クラス内のすべてのフィールドに対し
                            For Each fld1 In cls1.FldCla
                                html_file_name = GetHtmlFileName(fld1)
                                class_sw.WriteLine("<tr>")
                                class_sw.WriteLine("<td><a href=""{0}.html"">{1}</a></td><td>{2}</td>", html_file_name, fld1.NameVar, fld1.TailCom)
                                class_sw.WriteLine("</tr>")

                                paht1 = class_dir + "\" + html_file_name + ".html"
                                sw = New TStringWriter()

                                sw.WriteLine(TCodeGenerator.HTMLHead)
                                sw.WriteLine("<h2>{0} フィールド</h2>", fld1.NameVar)

                                ' フィールドに値を代入している関数とフィールドの値を参照している関数
                                For idx = 0 To 1
                                    def_ref = If(idx = 0, True, False)
                                    sw.WriteLine("<h2>{0}</h2>", If(def_ref, "このフィールドに値を代入している関数", "このフィールドの値を参照している関数"))

                                    vref = New TList(Of TReference)(From ref1 In fld1.RefVar Where ref1.DefRef = def_ref)
                                    If vref.Count = 0 Then
                                        sw.WriteLine("<h3>なし</h3>")
                                    Else
                                        vfnc = New List(Of TFunction)()
                                        sw.WriteLine("<ul>")
                                        For Each ref1 In vref
                                            If Not vfnc.Contains(ref1.FunctionTrm) Then
                                                ' 未処理の場合

                                                vfnc.Add(ref1.FunctionTrm)
                                                If ref1.FunctionTrm Is Nothing Then
                                                    sw.Write("<li>{0}</li>", ref1.NameRef)
                                                Else
                                                    sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(ref1.FunctionTrm.GetClassVar()), GetHtmlFileName(ref1.FunctionTrm), ref1.FunctionTrm.FullName())
                                                End If
                                            End If
                                        Next
                                        sw.WriteLine("</ul>")
                                    End If
                                Next

                                If fld1.ClaFld IsNot Nothing AndAlso fld1.ClaFld.SrcCla IsNot Nothing Then

                                    fname = TPath.GetFileNameWithoutExtension(fld1.ClaFld.SrcCla.FileSrc)
                                    sw.WriteLine(" <a href=""../_src/{0}.html#var{1}"" >ソース</a><br/>", fname, fld1.IdxVar)
                                End If

                                svg_file_name = GetHtmlFileName(cls1) + "_" + GetHtmlFileName(fld1)
                                sw.WriteLine("<a href=""../_dot/{0}_define.svg"" >値の代入への経路</a><br/>", svg_file_name)
                                sw.WriteLine("<a href=""../_dot/{0}_use.svg"" >値の使用への経路</a><br/>", svg_file_name)

                                sw.WriteLine("</body></html>")

                                TFile.WriteAllText(paht1, sw.ToString())
                            Next

                            class_sw.WriteLine("</tbody></table>")

                            class_sw.WriteLine("<h2>関数</h2>")
                            class_sw.WriteLine("<table border=""1""><thead><tr><th>名前</th><th>説明</th></tr></thead><tbody>")

                            ' クラス内のすべての関数に対し
                            For Each fnc1 In cls1.FncCla
                                If fnc1.ComVar IsNot Nothing Then
                                    com_str = fnc1.ComVar.GetFirstLine()
                                Else
                                    com_str = ""
                                End If

                                html_file_name = GetHtmlFileName(fnc1)

                                class_sw.WriteLine("<tr>")
                                class_sw.WriteLine("<td><a href=""{0}.html"">{1}</a></td><td>{2}</td>", html_file_name, fnc1.NameVar, com_str)
                                class_sw.WriteLine("</tr>")

                                paht1 = class_dir + "\" + html_file_name + ".html"

                                sw = New TStringWriter()
                                sw.WriteLine(TCodeGenerator.HTMLHead)
                                sw.WriteLine("<h1>{0} 関数</h1>", fnc1.NameVar)

                                If fnc1.ComVar IsNot Nothing Then

                                    sw.WriteLine("<h2>説明</h2>")
                                    For Each s In fnc1.ComVar.LineCom
                                        sw.WriteLine("{0}<br/>", s)
                                    Next
                                End If

                                sw.WriteLine("<h2>この関数から呼ぶ関数</h2>")
                                If fnc1.CallTo.Count = 0 Then
                                    sw.WriteLine("<h3>なし</h3>")
                                Else
                                    sw.WriteLine("<ul>")
                                    For Each fnc2 In fnc1.CallTo
                                        sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(fnc2.GetClassVar()), GetHtmlFileName(fnc2), fnc2.FullName())
                                    Next
                                    sw.WriteLine("</ul>")
                                End If

                                sw.WriteLine("<h2>この関数を呼ぶ関数</h2>")
                                If fnc1.CallFrom.Count = 0 Then
                                    sw.WriteLine("<h3>なし</h3>")
                                Else
                                    sw.WriteLine("<ul>")
                                    For Each fnc2 In fnc1.CallFrom
                                        sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(fnc2.GetClassVar()), GetHtmlFileName(fnc2), fnc2.FullName())
                                    Next
                                    sw.WriteLine("</ul>")
                                End If

                                ' 関数が値を代入しているフィールドと、関数が値を参照しているフィールド
                                For idx = 0 To 1
                                    def_ref = If(idx = 0, True, False)
                                    sw.WriteLine("<h2>{0}</h2>", If(def_ref, "この関数が値を代入しているフィールド", "この関数が値を参照しているフィールド"))

                                    vref = New TList(Of TReference)(From ref1 In fnc1.RefFnc Where ref1.DefRef = def_ref AndAlso ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TField)
                                    If vref.Count = 0 Then
                                        sw.WriteLine("<h3>なし</h3>")
                                    Else
                                        vfld = New TList(Of TField)()
                                        sw.WriteLine("<ul>")
                                        For Each ref1 In vref
                                            fld2 = CType(ref1.VarRef, TField)
                                            If Not vfld.Contains(fld2) Then
                                                ' 未処理の場合

                                                vfld.Add(fld2)
                                                sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(fld2.ClaFld), GetHtmlFileName(fld2), fld2.ToString())
                                            End If
                                        Next
                                        sw.WriteLine("</ul>")
                                    End If
                                Next

                                If fnc1.ClaFnc IsNot Nothing AndAlso fnc1.ClaFnc.SrcCla IsNot Nothing Then

                                    fname = TPath.GetFileNameWithoutExtension(fnc1.ClaFnc.SrcCla.FileSrc)
                                    sw.WriteLine(" <a href=""../_src/{0}.html#var{1}"" >ソース</a>", fname, fnc1.IdxVar)
                                End If

                                sw.WriteLine("</body></html>")

                                TFile.WriteAllText(paht1, sw.ToString())
                            Next

                            class_sw.WriteLine("</tbody></table>")
                    End Select

                    class_sw.WriteLine("</body></html>")
                    TFile.WriteAllText(class_dir + "\" + html_class_file_name + ".html", class_sw.ToString())
                End If
            Next
        Next

        index_sw.WriteLine("</ul>")
        index_sw.WriteLine("</body></html>")

        TFile.WriteAllText(all_class_dir + "\class_index.html", index_sw.ToString())
    End Sub

    Public Shared Function TrmStr(fm As TBasicCodeGenerator, trm1 As TTerm) As String
        Dim s As String

        fm.ClearFM()
        fm.TrmSrc(trm1)
        fm.NL()
        s = fm.MakeSrcText()

        Return s.Substring(0, s.IndexOf(vbCr))
    End Function

    Public Sub MakeAllBasicSrc(parser As TSourceParser)
        For Each src_f In SrcPrj
            src_f.FigSrc = New TBasicCodeGenerator(Me, parser)
            CurSrc = src_f

            If parser.LanguageSP = ELanguage.Basic Then

                src_f.FigSrc.MakeBasicSrc(src_f)
                src_f.FigSrc.OutputBasicTextHTML(src_f, OutputDirectory + "\")
            End If
        Next
    End Sub

    Sub FncHtml(sw As TStringWriter, fnc1 As TFunction)
        Dim fname As String

        If fnc1.ClaFnc IsNot Nothing AndAlso fnc1.ClaFnc.SrcCla IsNot Nothing Then
            fname = TPath.GetFileNameWithoutExtension(fnc1.ClaFnc.SrcCla.FileSrc)
            sw.Write(" <a href=""./{0}.html#var{1}"" >{2}</a>", fname, fnc1.IdxVar, fnc1.FullName())
        Else
            sw.Write(" {0}", fnc1.FullName())
        End If

    End Sub

    ' 間接呼び出しをCallToAll.htmlに書く
    Sub DmpCallToAll()
        Dim v1 As New TList(Of TFunction), v2 As New TList(Of TFunction), fnc1 As TFunction
        Dim sw As TStringWriter

        If theMain Is Nothing Then
            Exit Sub
        End If

        sw = New TStringWriter()
        sw.WriteLine(TCodeGenerator.HTMLHead)

        For Each cla1 In SimpleParameterizedClassList
            For Each fnc1 In cla1.FncCla
                FncHtml(sw, fnc1)
                sw.Write("&nbsp; : オーバーロード&nbsp;")
                For Each fnc2 In fnc1.EqOvredFncAll
                    sw.Write("&nbsp;")
                    FncHtml(sw, fnc2)
                Next
                sw.WriteLine("<br/>")
            Next
        Next

        v1.Add(theMain)
        Do While v1.Count <> 0

            fnc1 = v1(0)
            v1.RemoveAt(0)

            v2.Add(fnc1)

            FncHtml(sw, fnc1)

            sw.Write("&nbsp; : 直接&nbsp;")
            sw.Write("[")
            For Each fnc2 In fnc1.CallTo
                sw.Write("&nbsp;")
                FncHtml(sw, fnc2)
            Next
            sw.Write("]")

            For Each fnc2 In fnc1.CallTo
                For Each fnc3 In fnc2.EqOvredFncAll
                    sw.Write("&nbsp;")
                    FncHtml(sw, fnc3)
                Next
            Next
            sw.WriteLine("<br/>")

            sw.Write("&nbsp; : 間接&nbsp;")
            For Each fnc2 In fnc1.CallToAll
                sw.Write("&nbsp;")
                FncHtml(sw, fnc2)

                If Not v2.Contains(fnc2) Then
                    v1.Add(fnc2)
                End If
            Next
            sw.WriteLine("<br/>")
        Loop

        sw.WriteLine("</body></html>")
        TFile.WriteAllText(OutputDirectory + "\html\CallToAll.html", sw.ToString())
    End Sub

    Public Sub MakePrjAllSrc()
        Dim out_dir As String
        Dim java_code As TJavaCodeGenerator, sw As TStringWriter
        Dim dic1 As Dictionary(Of String, EToken), vtkn_name As Dictionary(Of EToken, String)
        Dim fname As String

        out_dir = OutputDirectory + "\"

        '-------------------------------------------------- Javaのソースを作る
        dic1 = TJavaCodeGenerator.TknAddJava()
        vtkn_name = TJavaCodeGenerator.ETknToStringDic(dic1)
        SetDicMemNameJava()

        sw = New TStringWriter()
        sw.WriteLine("package info.skyrecipe.kitchencabinet;")
        sw.WriteLine("import java.util.List;")
        sw.WriteLine("import java.util.ArrayList;")
        sw.WriteLine("import java.util.Stack;")
        sw.WriteLine("import java.util.HashMap;")
        sw.WriteLine("import java.io.StringWriter;")
        sw.WriteLine("import java.lang.reflect.Type;")
        sw.WriteLine("public class TTest {}")
        ' for ???
        For Each src_f In SrcPrj
            '  すべてのソースに対し

            fname = TPath.GetFileName(src_f.FileSrc)
            If fname <> "@lib.vb" AndAlso fname <> "Sys.vb" AndAlso fname <> "WindowsForms.vb" Then

                java_code = New TJavaCodeGenerator(Me, ParsePrj)
                java_code.ParserCG.vTknName = vtkn_name
                java_code.MakeJavaSrc(src_f, out_dir, sw)

                ' for ???
                For Each del In java_code.vDelegate
                    java_code.MakeDelegateClassJava(del, sw)
                Next
            End If
        Next
        TFile.WriteAllText("C:\usr\prj\workspace\KitchenCabinet\src\info\skyrecipe\kitchencabinet\TTest.java", sw.ToString())

        dicMemName = Nothing
        dicClassMemName = Nothing
    End Sub

    Public Sub CodeAnalysis()

        ' 間接呼び出しをCallToAll.htmlに書く
        DmpCallToAll()

        ''''
        For Each cla1 In SimpleParameterizedClassList
            For Each fld1 In cla1.FldCla
                If fld1.RefVar.Count = 0 Then
                    'Debug.WriteLine("未使用 {0}.{1} {2}", cla1.NameVar, fld1.NameVar, cla1.SrcCla.FileSrc)
                End If
            Next

            For Each fnc1 In cla1.FncCla
                If Not fnc1.Reachable Then
                    'Debug.WriteLine("未使用 {0}.{1} {2}", cla1.NameVar, fnc1.NameVar, cla1.SrcCla.FileSrc)
                End If
            Next
        Next
        ''''

        Debug.WriteLine("Build Bas 終了")

        MakePrjAllSrc()
    End Sub
End Class

Partial Public Class TSourceFile
    Public FigSrc As TBasicCodeGenerator
End Class

Partial Public Class TApply

    Public Sub TrmSrcSet(mk As TCodeGenerator, trm1 As TTerm)
        Select Case KndApp

            Case EApply.ListApp, EApply.DictionaryApp
                Debug.Assert(ArgApp.Count = 1)

                mk.TrmSrc(FncApp)
                mk.WordAdd(".", EFigType.SymFig, Me)
                If KndApp = EApply.ListApp Then
                    mk.WordAdd("set", EFigType.RefFig, Me)
                Else
                    mk.WordAdd("put", EFigType.RefFig, Me)
                End If
                mk.WordAdd("(", EFigType.SymFig, Me)
                mk.TrmSrc(ArgApp(0))
                mk.WordAdd(",", EFigType.SymFig, Me)
                mk.TrmSrc(trm1)
                mk.WordAdd(")", EFigType.SymFig, Me)
            Case Else
                Debug.Assert(False)
        End Select
    End Sub
End Class


' -------------------------------------------------------------------------------- TNaviCSE
Public Class TNaviCSE
    Inherits TNavi

    Public Overrides Sub NaviTerm(trm1 As TTerm, arg1 As Object)
        Dim dat1 As TNaviCSEDat, s1 As String, s2 As String

        If trm1 Is Nothing OrElse arg1 Is Nothing Then
            Exit Sub
        Else
            If Not TypeOf trm1 Is TDot Then
                MyBase.NaviTerm(trm1, arg1)
            Else
                MyBase.NaviTerm(CType(trm1, TDot).TrmDot, arg1)

                dat1 = CType(arg1, TNaviCSEDat)

                For Each trm2 In dat1.vTrm
                    If Sys.IsEqTrm(trm1, trm2) Then
                        s1 = TProject.TrmStr(dat1.BFM, trm1)
                        s2 = TProject.TrmStr(dat1.BFM, trm2)
                        Debug.Assert(s1 = s2)
                        '                        Debug.WriteLine("共通部分式 {0} {1}", dat1.FncCSE.FullName(), s1)
                        Exit For
                    End If
                Next

                dat1.vTrm.Add(trm1)
            End If
        End If
    End Sub

    Public Overrides Sub NaviFunction(fnc1 As TFunction, arg1 As Object)
        Dim dat1 As New TNaviCSEDat

        If fnc1.BlcFnc IsNot Nothing Then
            dat1.FncCSE = fnc1
            NaviBlock(fnc1.BlcFnc, dat1)
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TNaviCSEDat
Public Class TNaviCSEDat
    Public FncCSE As TFunction
    Public vTrm As New TList(Of TTerm)
    Public BFM As TBasicCodeGenerator

    Public Sub New()
        BFM = New TBasicCodeGenerator(TProject.Prj, TProject.Prj.ParsePrj)
    End Sub
End Class