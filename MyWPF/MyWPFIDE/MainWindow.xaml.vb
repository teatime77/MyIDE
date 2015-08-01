Imports System.Windows.Threading

Class MainWindow
    Dim gIDE As TIDE
    Dim gTimerInterval As Integer
    Dim gTimer As DispatcherTimer

    Private Sub Window_Loaded(sender As Object, e As RoutedEventArgs)
        'TGraphics.Handle	= IntPtr((void*)hWnd)
        TGraphics.InitGraphics()
        'GetClientRect(hWnd, &rc1)
        gIDE = New TIDE(New TPnt(RootCanvas.ActualWidth, RootCanvas.ActualHeight))
        gIDE.MainForm.GrFrm.CanGr = RootCanvas
        gIDE.LoadApp()

        gTimerInterval = gIDE.MainForm.Interval
        If gTimerInterval <> 0 Then
            gTimer = New DispatcherTimer()
            gTimer.Interval = New TimeSpan(0, 0, 0, 0, gIDE.MainForm.Interval)
            AddHandler gTimer.Tick, AddressOf Timer_Tick
            gTimer.Start()
        End If

    End Sub

    Private Sub Timer_Tick(ByVal sender As Object, ByVal e As EventArgs)
        If TWnd.Capture IsNot Nothing AndAlso TWnd.Capture.MousePressHandler IsNot Nothing Then
            TWnd.MouseEvent.TypeEv = EEvent.eMousePress
            gIDE.EventHandler(TWnd.MouseEvent)
        End If

        gIDE.EventHandler(New TEvent(EEvent.eOnTimer))

        If gTimerInterval <> gIDE.MainForm.Interval Then
            gTimerInterval = gIDE.MainForm.Interval
            gTimer.Stop()

            gTimerInterval = gIDE.MainForm.Interval
            gTimer.Interval = New TimeSpan(0, 0, 0, 0, gIDE.MainForm.Interval)
            gTimer.Start()
        End If

    End Sub

    Private Sub RootCanvas_SizeChanged(sender As Object, e As SizeChangedEventArgs) Handles RootCanvas.SizeChanged
        If gIDE Is Nothing Then
            Exit Sub
        End If

        gIDE.EventHandler(New TFormEvent(EEvent.eOnResize, gIDE.MainForm, New TPnt(RootCanvas.ActualWidth, RootCanvas.ActualHeight)))
    End Sub

    Private Sub SetMouseEvent(e As MouseEventArgs)
        Dim pt As Point

        TWnd.MouseEvent.Shift = ((Keyboard.Modifiers And ModifierKeys.Shift) <> 0)
        TWnd.MouseEvent.Control = ((Keyboard.Modifiers And ModifierKeys.Control) <> 0)
        TWnd.MouseEvent.Alt = ((Keyboard.Modifiers And ModifierKeys.Alt) <> 0)

        pt = e.GetPosition(RootCanvas)
        TWnd.MouseEvent.PosEv.XPnt = pt.X
        TWnd.MouseEvent.PosEv.YPnt = pt.Y
        TWnd.MouseEvent.FormEv = gIDE.MainForm
    End Sub
    Private Sub RootCanvas_MouseDown(sender As Object, e As MouseButtonEventArgs) Handles RootCanvas.MouseDown
        SetMouseEvent(e)
        TWnd.MouseEvent.TypeEv = EEvent.eMouseDown
        TWnd.MouseEvent.MouseDownTime = DateTime.Now

        RootCanvas.Focus()
        RootCanvas.CaptureMouse()

        gIDE.EventHandler(TWnd.MouseEvent)
    End Sub

    Private Sub RootCanvas_MouseMove(sender As Object, e As MouseEventArgs) Handles RootCanvas.MouseMove
        SetMouseEvent(e)
        TWnd.MouseEvent.TypeEv = EEvent.eMouseMove

        gIDE.EventHandler(TWnd.MouseEvent)
    End Sub

    Private Sub RootCanvas_MouseUp(sender As Object, e As MouseButtonEventArgs) Handles RootCanvas.MouseUp
        SetMouseEvent(e)
        TWnd.MouseEvent.TypeEv = EEvent.eMouseUp

        gIDE.EventHandler(TWnd.MouseEvent)
        RootCanvas.ReleaseMouseCapture()
    End Sub

    Private Function GetKeyCode(k As Key) As Integer
        Select Case k
            Case Key.PageUp
                Return EKeyCode.PageUp
            Case Key.PageDown
                Return EKeyCode.PageDown
            Case Key.End
                Return EKeyCode.EndKey
            Case Key.Home
                Return EKeyCode.Home
            Case Key.Left
                Return EKeyCode.Left
            Case Key.Up
                Return EKeyCode.Up
            Case Key.Right
                Return EKeyCode.Right
            Case Key.Down
                Return EKeyCode.Down
            Case Key.Delete
                Return EKeyCode.Delete
            Case Key.Enter
                Return EKeyCode.Enter
            Case Key.Back
                Return EKeyCode.Back
            Case Key.Tab
                Return EKeyCode.Tab
            Case Else
                Return 0
        End Select
    End Function

    Private Sub RootCanvas_KeyDown(sender As Object, e As KeyEventArgs) Handles RootCanvas.KeyDown
        TWnd.KeyEvent.KeyCode = GetKeyCode(e.Key)
        TWnd.KeyEvent.Shift = ((Keyboard.Modifiers And ModifierKeys.Shift) <> 0)
        TWnd.KeyEvent.Control = ((Keyboard.Modifiers And ModifierKeys.Control) <> 0)
        TWnd.KeyEvent.Alt = ((Keyboard.Modifiers And ModifierKeys.Alt) <> 0)
        TWnd.KeyEvent.Repeat = e.IsRepeat
        TWnd.KeyEvent.TypeEv = EEvent.eOnKeyDown
        TWnd.KeyEvent.FormEv = gIDE.MainForm

        gIDE.EventHandler(TWnd.KeyEvent)
    End Sub
End Class
