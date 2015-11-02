Module Module1

    Sub Main()
        Dim builder As New TBuilder

        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\CSharp.xml")

        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\StackPanel.xml")
        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\Circle.xml")
        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\View.xml")

        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\Miyu.xml")
        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\MyAlgo.xml")
        builder.Build("C:\usr\prj\MyIDE\InvariantBasicOrigin\MyView.xml")
    End Sub

End Module
