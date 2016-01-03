Module Module1

    Sub Main()
        Dim project_name_list As String() = {"App1", "InvariantBasicOrigin", "CSharp", "TypeScript", "StackPanel", "Circle", "View", "MyView"}
        For Each project_name In project_name_list
            Dim prj1 As TProject = TProject.MakeProject("C:\usr\prj\MyIDE\InvariantBasicOrigin\" + project_name + ".xml")
            prj1.OutputSourceFile()
        Next
    End Sub

End Module
