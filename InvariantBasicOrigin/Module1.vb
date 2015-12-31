Module Module1

    Sub Main()
        Dim project_name_list As String() = {"App1", "CSharp", "TypeScript", "StackPanel", "Circle", "View", "MyView", "InvariantBasicOrigin"}
        For Each project_name In project_name_list
            TProject.MakeProject("C:\usr\prj\MyIDE\InvariantBasicOrigin\" + project_name + ".xml")
        Next
    End Sub

End Module
