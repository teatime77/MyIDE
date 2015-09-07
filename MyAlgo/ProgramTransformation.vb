Imports System.Xml
Imports System.Xml.Serialization
Imports System.IO
Imports System.Diagnostics

Public Class TProgramTransformation
    'Public Shared Function WriteTranslationTable(path As String) As String
    '    Dim v As New TList(Of TNameTable), name_table As TNameTable

    '    name_table = New TNameTable()
    '    name_table.LocalNameList.Add(New TLocalName("abbr", "TClass"))
    '    name_table.LocalNameList.Add(New TLocalName("jp", "クラス"))
    '    name_table.LocalNameList.Add(New TLocalName("en", "TClass"))
    '    name_table.LocalNameList.Add(New TLocalName("ch", "類型"))

    '    v.Add(name_table)

    '    name_table = New TNameTable()
    '    name_table.LocalNameList.Add(New TLocalName("abbr", "TFunction"))
    '    name_table.LocalNameList.Add(New TLocalName("jp", "関数"))
    '    name_table.LocalNameList.Add(New TLocalName("en", "TFunction"))
    '    name_table.LocalNameList.Add(New TLocalName("ch", "函数"))

    '    v.Add(name_table)

    '    name_table = New TNameTable()
    '    name_table.LocalNameList.Add(New TLocalName("abbr", "TField"))
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
        name_table.Add(New TLocalName("abbr", "TClass"))
        name_table.Add(New TLocalName("jp", "クラス"))
        name_table.Add(New TLocalName("en", "TClass"))
        name_table.Add(New TLocalName("ch", "類型"))

        v.Add(name_table)

        name_table = New TList(Of TLocalName)()
        name_table.Add(New TLocalName("abbr", "TFunction"))
        name_table.Add(New TLocalName("jp", "関数"))
        name_table.Add(New TLocalName("en", "TFunction"))
        name_table.Add(New TLocalName("ch", "函数"))

        v.Add(name_table)

        name_table = New TList(Of TLocalName)()
        name_table.Add(New TLocalName("abbr", "TField"))
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

    Public Shared Function ReadClassNameTable(file_path As String, idx As Integer) As Dictionary(Of String, String)
        Dim dic As New Dictionary(Of String, String)
        Dim vline As String(), line2 As String, line3 As String, vname As String()

        vline = TFile.ReadAllLines(file_path)
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
