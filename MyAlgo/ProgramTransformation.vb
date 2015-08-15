Imports System.Xml
Imports System.Xml.Serialization
Imports System.IO
Imports System.Diagnostics




Public Class TProgramTransformation
    'Public Shared Function WriteTranslationTable(path As String) As String
    '    Dim v As New TList(Of TNameTable), name_table As TNameTable

    '    name_table = New TNameTable()
    '    name_table.LocalNameList.Add(New TLocalName("abbr", "TCls"))
    '    name_table.LocalNameList.Add(New TLocalName("jp", "クラス"))
    '    name_table.LocalNameList.Add(New TLocalName("en", "TClass"))
    '    name_table.LocalNameList.Add(New TLocalName("ch", "類型"))

    '    v.Add(name_table)

    '    name_table = New TNameTable()
    '    name_table.LocalNameList.Add(New TLocalName("abbr", "TFnc"))
    '    name_table.LocalNameList.Add(New TLocalName("jp", "関数"))
    '    name_table.LocalNameList.Add(New TLocalName("en", "TFunction"))
    '    name_table.LocalNameList.Add(New TLocalName("ch", "函数"))

    '    v.Add(name_table)

    '    name_table = New TNameTable()
    '    name_table.LocalNameList.Add(New TLocalName("abbr", "TFld"))
    '    name_table.LocalNameList.Add(New TLocalName("jp", "フィールド"))
    '    name_table.LocalNameList.Add(New TLocalName("en", "TField"))
    '    name_table.LocalNameList.Add(New TLocalName("ch", "場"))

    '    v.Add(name_table)

    '    Dim serializer As New XmlSerializer(GetType(TList(Of TNameTable)))
    '    Dim writer As New StreamWriter("C:\usr\prj\MyIDE\etc\a.xml")

    '    serializer.Serialize(writer, v)
    '    writer.Close()

    '    Return ""
    'End Function

    'Public Shared Sub ReadTranslationTable()
    '    Dim v As TList(Of TNameTable)
    '    Dim serializer As New XmlSerializer(GetType(TList(Of TNameTable)))
    '    Dim fs As New FileStream("C:\usr\prj\MyIDE\etc\a.xml", FileMode.Open)

    '    v = CType(serializer.Deserialize(fs), TList(Of TNameTable))
    '    fs.Close()

    '    For Each name_table In v
    '        For Each local_name In name_table.LocalNameList
    '            Debug.WriteLine(vbTab + "{0},{1}", local_name.CultureId, local_name.LocalName)
    '        Next
    '        Debug.WriteLine("")
    '    Next
    'End Sub

    Public Shared Function WriteTranslationTable(path As String) As String
        Dim v As New TList(Of TList(Of TLocalName)), name_table As TList(Of TLocalName)

        name_table = New TList(Of TLocalName)()
        name_table.Add(New TLocalName("abbr", "TCls"))
        name_table.Add(New TLocalName("jp", "クラス"))
        name_table.Add(New TLocalName("en", "TClass"))
        name_table.Add(New TLocalName("ch", "類型"))

        v.Add(name_table)

        name_table = New TList(Of TLocalName)()
        name_table.Add(New TLocalName("abbr", "TFnc"))
        name_table.Add(New TLocalName("jp", "関数"))
        name_table.Add(New TLocalName("en", "TFunction"))
        name_table.Add(New TLocalName("ch", "函数"))

        v.Add(name_table)

        name_table = New TList(Of TLocalName)()
        name_table.Add(New TLocalName("abbr", "TFld"))
        name_table.Add(New TLocalName("jp", "フィールド"))
        name_table.Add(New TLocalName("en", "TField"))
        name_table.Add(New TLocalName("ch", "場"))

        v.Add(name_table)

        Dim serializer As New XmlSerializer(GetType(TList(Of TList(Of TLocalName))))
        Dim writer As New StreamWriter("C:\usr\prj\MyIDE\etc\a.xml")

        serializer.Serialize(writer, v)
        writer.Close()

        Return ""
    End Function

    Public Shared Function ReadTranslationTable() As TList(Of TList(Of TLocalName))
        Dim v As TList(Of TList(Of TLocalName))
        Dim serializer As New XmlSerializer(GetType(TList(Of TList(Of TLocalName))))
        Dim fs As New FileStream("C:\usr\prj\MyIDE\etc\a.xml", FileMode.Open)

        v = CType(serializer.Deserialize(fs), TList(Of TList(Of TLocalName)))
        fs.Close()

        For Each name_table In v
            For Each local_name In name_table
                Debug.WriteLine(vbTab + "{0},{1}", local_name.CultureId, local_name.LocalName)
            Next
        Next

        Return v
    End Function

    Public Shared Function ReadClassNameTable(dir_path As String, file_name As String, idx As Integer) As Dictionary(Of String, String)
        Dim dic As New Dictionary(Of String, String)
        Dim vline As String(), line2 As String, line3 As String, vname As String()

        vline = TFile.ReadAllLines(dir_path, file_name)
        For Each line1 In vline

            ' 連続するタブを１つにする
            line2 = line1
            Do While True
                line3 = line2.Replace(vbTab + vbTab, vbTab)
                If line3 = line2 Then
                    Exit Do
                End If
                line2 = line3
            Loop

            vname = line2.Split(New Char() {vbTab(0)})

            If idx < vname.Length Then
                dic.Add(vname(0), vname(idx))
            End If
        Next

        Return dic
    End Function


    Public Shared Function FindLocalName(v As TList(Of TLocalName), id As String) As String
        For Each x In v
            If x.CultureId = id Then
                Return x.LocalName
            End If
        Next

        Return Nothing
    End Function

    Public Shared Function MakeTranslationDictionary(src_id As String, dst_id As String) As Dictionary(Of String, String)
        Dim vv As TList(Of TList(Of TLocalName))
        Dim dic As New Dictionary(Of String, String)
        Dim src_name As String, dst_name As String

        vv = ReadTranslationTable()
        For Each v In vv
            src_name = FindLocalName(v, src_id)
            dst_name = FindLocalName(v, dst_id)
            If src_name IsNot Nothing AndAlso dst_name IsNot Nothing Then
                dic.Add(src_name, dst_name)
            End If
        Next

        Return dic
    End Function
End Class

Public Class TLocalName
    Public CultureId As String
    Public LocalName As String

    Public Sub New()
    End Sub

    Public Sub New(culture_id As String, local_name As String)
        CultureId = culture_id
        LocalName = local_name
    End Sub
End Class

Public Class TNameTable
    Public LocalNameList As New TList(Of TLocalName)

    Public Sub New()
    End Sub
End Class

Public Class TTestDataflow
    Dim gApp As TApplication
    Dim gPrj As TPrj
    Dim gDataflow As TDataflow
    Dim PrjIdx As Integer
    Dim PrjFiles As New TList(Of String())
    Dim vSrc As New TList(Of String)

    Public Sub New()

        gApp = New TApplication()

        gApp.MainForm = New TForm()
        gApp.MainForm.Interval = 10



        Dim v = New String() {"StackPanel", "Circle", "View"}
        'Dim v = New String() {"StackPanel"}
        For Each x In v

            PrjFiles.Add(New String() {"@lib.vb", "System.vb", x + ".vb"})
            vSrc.Add("")
        Next

        For PrjIdx = 0 To PrjFiles.Count - 1
            LoadPrj()
        Next

    End Sub


    Sub LoadPrj()
        Dim prj1 As TPrj, nav2 As TNavCSE, data_flow As TDataflow

        prj1 = New TPrj()
        prj1.SrcFileNames = PrjFiles(PrjIdx)

        ' オリジナルのソースを読む
        gPrj = prj1
        prj1.SrcDir = "C:\usr\prj\MyIDE\ViewSrc"
        prj1.OutDir = prj1.SrcDir + "\out\MyView"
        prj1.MainClassName = "TNaviView"
        prj1.MainFunctionName = "GlobalRule"

        For Each fname In prj1.SrcFileNames
            Dim src1 As TSrc, s As String, vtext As String()

            s = File.ReadAllText(prj1.SrcDir + "\" + fname)
            vtext = s.Replace(vbCr, "").Split(New Char() {vbLf(0)})
            src1 = New TSrc(fname, vtext)
            prj1.SrcPrj.Add(src1)
        Next

        prj1.Compile()

        Dim src2 As TSrc

        src2 = prj1.SrcPrj(2)
        Debug.Print("---------------------------------------------------- 不変条件 {0}", prj1.SrcFileNames(2))


        ' 名前と予約語を変えて出力する

        ' 名前と予約語を変えたソースを読む

        ' オリジナルのソースを出力する

        data_flow = New TDataflow()
        data_flow.SetChangeableFldList(prj1)

        For Each changeable_fld In data_flow.ChangeableFldList
            data_flow.ChangeableFld = changeable_fld
            Debug.WriteLine("Analyze Dataflow {0}", changeable_fld.ToString())

            ' 値が変化し得るフィールドを解析する
            data_flow.AnalyzeChangeableFld()
        Next

        Dim sw As New StringWriter

        'sw.WriteLine("Imports System.Threading")
        sw.WriteLine(data_flow.SyncClassSrc)
        sw.WriteLine("Partial Public Class {0}", data_flow.GlobalRule.ClaFnc.NameVar)
        sw.WriteLine("Inherits TNaviRule")
        sw.WriteLine(data_flow.RuleSW.ToString())
        sw.WriteLine("End Class")

        vSrc(PrjIdx) = sw.ToString()


        ' コード解析
        prj1.CodeAnalysis()

        Debug.WriteLine("共通部分式")
        nav2 = New TNavCSE()
        nav2.NavPrj(prj1, Nothing)

        '-------------------------------------------------- データフロー解析のタイマー表示
        gDataflow = New TDataflow()
        gDataflow.SetChangeableFldList(gPrj)
        gDataflow.ChangeableFld = gDataflow.ChangeableFldList(0)
        gDataflow.AnalyzeChangeableFld()
    End Sub
End Class