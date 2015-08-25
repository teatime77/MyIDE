Imports System.Diagnostics
Imports System.Xml.Serialization
Imports System.Text
Imports System.IO

Public Class TBuilder
    Public RefSugiyamaGraph As New Dictionary(Of TField, TList(Of TList(Of TNode)))

    Public Sub Build(project_path As String)
        'XmlSerializerオブジェクトを作成
        Dim serializer As New XmlSerializer(GetType(TProjectSettings))

        '読み込むファイルを開く
        Dim sr As New StreamReader(project_path, Encoding.UTF8)

        'XMLファイルから読み込み、逆シリアル化する
        Dim project_settings As TProjectSettings = CType(serializer.Deserialize(sr), TProjectSettings)
        'ファイルを閉じる
        sr.Close()

        For Each s In project_settings.SourceFileList
            Debug.Print(s)
        Next
        WriteObject(project_path, project_settings)

        Dim prj1 As TProject, nav2 As TNavCSE


        ' オリジナルのソースを読む
        prj1 = New TProject()
        If project_settings.ClassNameTable <> "" Then
            prj1.ClassNameTable = TProgramTransformation.ReadClassNameTable(project_settings.ClassNameTable, 2)
        End If

        prj1.SrcFileNames = project_settings.SourceFileList
        prj1.SrcDir = project_settings.SourceDirectory
        prj1.OutDir = project_settings.OutputDirectory
        prj1.MainClassName = project_settings.MainClassName
        prj1.MainFunctionName = project_settings.MainFunctionName
        prj1.OutputNotUsed = project_settings.OutputNotUsed

        prj1.MakeSrcPrj()
        prj1.Compile()
        prj1.MakeSrc()

        ' コード解析
        prj1.CodeAnalysis()

        Debug.WriteLine("共通部分式")
        nav2 = New TNavCSE()
        nav2.NavPrj(prj1, Nothing)

        ' 変数参照のグラフを作る
        MakeRefSugiyamaGraph(prj1)

        If project_settings.MakeReferenceGraph Then
            prj1.MakeReferenceGraph()
        End If

    End Sub

    Public Sub WriteObject(xml_path As String, obj As Object)
        Dim serializer As New XmlSerializer(GetType(TProjectSettings))

        '書き込むファイルを開く（UTF-8 BOM無し）
        Dim sw As New StreamWriter(xml_path, False, Encoding.UTF8)

        'Dim obj As New TProjectSettings
        'obj.SourceFileList.Add("Builder.vb")
        'obj.SourceFileList.Add("Logic.vb")


        'シリアル化し、XMLファイルに保存する
        serializer.Serialize(sw, obj)
        'ファイルを閉じる
        sw.Close()

    End Sub

    ' 変数参照のグラフを作る
    Public Sub MakeRefSugiyamaGraph(prj1 As TProject)
        Dim dic1 As New Dictionary(Of Object, TFlowNode), vnd As TList(Of TNode)
        Dim dgr As TDrawGraph, dot_dir As String, dot_path As String, L As TList(Of TList(Of TNode))

        RefSugiyamaGraph.Clear()

        ' すべてのクラスに対し
        For Each cla1 In prj1.vCla

            If prj1.vCla.IndexOf(cla1) Mod 25 = 0 Then
                Debug.WriteLine("Make Ref Graph {0}", prj1.vCla.IndexOf(cla1))
            End If

            ' すべてのフィールドに対し
            For Each fld1 In cla1.FldCla

                If cla1.NameCla() = "TApply" AndAlso fld1.NameVar = "TypeApp" Then


                    ' ノードの辞書を初期化する
                    TFlowNode.CntNd = 0
                    dic1 = New Dictionary(Of Object, TFlowNode)()

                    ' すべてのフィールド参照に対し
                    For Each ref1 In fld1.RefVar

                        If ref1.FncRef.Reachable Then
                            ' 到達可能の関数内で参照されている場合

                            ' 変数参照のノードをグラフに追加する
                            prj1.AddRefGraph(dic1, ref1)
                        End If
                    Next

                    ' ノードの集合からグラフを作る
                    vnd = TGraph.Node2Graph(New TList(Of TFlowNode)(dic1.Values))

                    dgr = New TDrawGraph(vnd)
                    TDrawGraph.CheckGraph(dgr.AllNode)

                    dot_dir = prj1.OutDir + "\html\_dot"
                    TDirectory.CreateDirectory(dot_dir)

                    dot_path = dot_dir + "\" + cla1.NameCla() + "_" + fld1.NameVar

                    ' dotファイルに書く
                    TGraph.WriteDotFile("オリジナル", dgr.AllNode, Nothing, dot_path + "-1.dot")

                    L = dgr.SugiyamaGraph(dot_path)

                    RefSugiyamaGraph.Add(fld1, L)
                End If
            Next
        Next
    End Sub
End Class

Public Class TProjectSettings
    Public Sub New()
    End Sub

    Public SourceDirectory As String = ""
    Public OutputDirectory As String = ""
    Public SourceFileList As String()
    Public MainClassName As String = ""
    Public MainFunctionName As String = ""
    Public OutputNotUsed As Boolean = True
    Public MakeReferenceGraph As Boolean = False
    Public ClassNameTable As String = ""
End Class