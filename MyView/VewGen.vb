Imports System.Threading
Public Class TSync_TView_DesiredWidth
    Inherits TSync
    Public ParentSync As TSync_TView_DesiredWidth
    Public PrevSync As TSync_TView_DesiredWidth
    Public Wait_TView_Left As New ManualResetEvent(False)
    Public Wait_TView_ActualWidth As New ManualResetEvent(False)
    Public Sub New(self1 As Object, parent1 As TSync_TView_DesiredWidth)
        SelfSync = self1
        ParentSync = parent1
    End Sub
End Class


Partial Public Class TNaviView
    Public Sub GlobalRule_TView_DesiredWidth(_sync As TSync_TView_DesiredWidth)
        Dim _current As TView = CType(_sync.SelfSync, TView)
        If TypeOf _current Is TStackPanel Then
            With CType(_current, TStackPanel)
                Dim sync_list_TView_DesiredWidth_TPanel_Children = (From _x In .Children Select New TSync_TView_DesiredWidth(_x, _sync)).ToList()
                If .Children.Count <> 0 Then
                    Dim _thread_TView_DesiredWidth_TPanel_Children = New Thread(Sub()
                                                                                    Parallel.ForEach(sync_list_TView_DesiredWidth_TPanel_Children, AddressOf GlobalRule_TView_DesiredWidth)
                                                                                End Sub)
                    _thread_TView_DesiredWidth_TPanel_Children.Start()
                End If

                .ActualWidth = Aggregate _child In .Children Into Sum(_child.DesiredWidth)
                _sync.Wait_TView_ActualWidth.Set()
                For Each ctrl In .Children
                    With ctrl
                        If IsFirst Then
                            _sync.ParentSync.Wait_TView_Left.WaitOne()
                            .Left = ._ParentControl.Left
                            _sync.Wait_TView_Left.Set()
                        Else
                            _sync.PrevSync.Wait_TView_ActualWidth.WaitOne()
                            .Left = ._Prev.Left + ._Prev.ActualWidth + 10
                            _sync.Wait_TView_Left.Set()
                        End If
                    End With
                Next
            End With
        ElseIf TypeOf _current Is TCircle Then
            With CType(_current, TCircle)
                .ActualWidth = .DesiredWidth
                _sync.Wait_TView_ActualWidth.Set()
            End With
        End If
    End Sub



End Class

Partial Public Class TView
    Public Overridable Sub GlobalRule(_sync As TSync_TView_DesiredWidth)

    End Sub
End Class

Partial Public Class TPanel
    Public Overrides Sub GlobalRule(_sync As TSync_TView_DesiredWidth)
        MyBase.GlobalRule(_sync)
    End Sub

End Class

Partial Public Class TStackPanel
    Public Overrides Sub GlobalRule(_sync As TSync_TView_DesiredWidth)
        MyBase.GlobalRule(_sync)
    End Sub
End Class