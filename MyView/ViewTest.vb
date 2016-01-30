
'-------------------------------------------------------------------------------- TViewTest
Public Class TViewTest
    Inherits TWindowApplication

    Public DeleteButton As TButton

    Public Overrides Sub Main()
        InitializeApplication()

        For Each ev In EventList

            HandleEvent(ev)

            If UserEvent IsNot Nothing Then

                Select Case UserEvent.TypeEv
                    Case EEvent.Click
                        ' クリックの場合

                        If UserEvent.Source Is DeleteButton Then
                            ' 削除ボタンの場合
                        End If
                End Select

                UserEvent = Nothing
            End If
        Next
    End Sub
End Class
