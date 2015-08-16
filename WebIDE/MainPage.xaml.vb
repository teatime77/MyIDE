Imports System.Net.Http
Imports System.Threading.Tasks
Imports System.Diagnostics
Imports System.Windows.Threading
Imports System.IO

Partial Public Class MainPage
    Inherits UserControl

    Dim gApp As TApplication
    Dim gPrj As TProject
    Dim gTimerInterval As Integer
    Dim gTimer As DispatcherTimer
    Dim gDataflowTimer As DispatcherTimer
    Dim gDataflow As TDataflow
    Dim EnumAnalyzeChangeableFld As IEnumerator(Of EAnalyzeChangeableFld)
    Dim EditRefChangeableUpStmt As New TSrcEdit
    Dim HomeUri As String
    Dim gClient As New HttpClient
    Dim PrjIdx As Integer
    Dim PrjFiles As New TList(Of String())
    Dim vSrc As New TList(Of String)

    Public Sub New()
        InitializeComponent()
    End Sub

    Private Sub UserControl_Loaded(sender As Object, e As RoutedEventArgs)
        Dim abs_uri As String, uri2 As String

        TSys.IsWeb = True

        lblMsg.Content = Application.Current.Host.Source.AbsolutePath
        abs_uri = Application.Current.Host.Source.AbsoluteUri
        uri2 = abs_uri.Substring(0, abs_uri.LastIndexOf("/"))
        HomeUri = uri2.Substring(0, uri2.LastIndexOf("/"))

        Debug.WriteLine("Abs URI:{0} Home URI:{1}", abs_uri, HomeUri)

        gApp = New TApplication()

        gApp.MainForm = New TForm()
        gApp.MainForm.Interval = 10

        gTimerInterval = gApp.MainForm.Interval

        gDataflowTimer = New DispatcherTimer()
        gDataflowTimer.Interval = TimeSpan.FromMilliseconds(1)
        AddHandler gDataflowTimer.Tick, AddressOf DataflowTimer_Tick

        Dim v = New String() {"StackPanel", "Circle", "View"}
        'Dim v = New String() {"StackPanel"}
        For Each x In v

            PrjFiles.Add(New String() {"lib.txt", "System.txt", x + ".txt"})
            cmb_Project.Items.Add(x)
            vSrc.Add("")
        Next

        PrjIdx = 0

        LoadPrj()
    End Sub

    Async Sub LoadPrj()
        Dim prj1 As TProject, nav2 As TNavCSE, data_flow As TDataflow

        prj1 = New TProject()
        prj1.SrcFileNames = PrjFiles(PrjIdx)

        ' オリジナルのソースを読む
        gPrj = prj1
        prj1.SrcDir = HomeUri + "/ViewSrc" ' "http://localhost:49618/ViewSrc"
        prj1.OutDir = prj1.SrcDir + "\out\MyView"
        prj1.MainClassName = "TNaviView"
        prj1.MainFunctionName = "GlobalRule"

        For Each fname In prj1.SrcFileNames
            Dim src1 As TSourceFile, s As String, vtext As String()

            s = Await gClient.GetStringAsync(New Uri(prj1.SrcDir + "/" + fname))
            vtext = s.Replace(vbCr, "").Split(New Char() {vbLf(0)})
            If fname = "lib.txt" Then
                src1 = New TSourceFile("@lib.txt", vtext)
            Else
                src1 = New TSourceFile(fname, vtext)
            End If
            prj1.SrcPrj.Add(src1)

            txtSrc.Text = s.ToString()
        Next

        prj1.Compile()

        Dim src2 As TSourceFile

        src2 = prj1.SrcPrj(2)

        ShowObj(RootCanvas, src2)

        ' 名前と予約語を変えて出力する

        ' 名前と予約語を変えたソースを読む

        ' オリジナルのソースを出力する

        data_flow = New TDataflow()
        data_flow.SetChangeableFldList(prj1)

        For Each changeable_fld In data_flow.ChangeableFldList
            data_flow.ChangeableFld = changeable_fld
            Debug.WriteLine("Analyze Dataflow {0}", changeable_fld.ToString())

            ' 値が変化し得るフィールドを解析する
            Dim enum_changeable_fld As IEnumerator(Of EAnalyzeChangeableFld) = data_flow.AnalyzeChangeableFld()

            Do While enum_changeable_fld.MoveNext()
                Select Case enum_changeable_fld.Current
                    Case EAnalyzeChangeableFld.Consistency

                        'ShowObj(canRefChangeableUpStmt, data_flow.RefChangeableUpStmt)
                End Select
            Loop
        Next

        Dim sw As New StringWriter

        'sw.WriteLine("Imports System.Threading")
        sw.WriteLine(data_flow.SyncClassSrc)
        sw.WriteLine("Partial Public Class {0}", data_flow.GlobalRule.ClaFnc.NameVar)
        sw.WriteLine("Inherits TNaviRule")
        sw.WriteLine(data_flow.RuleSW.ToString())
        sw.WriteLine("End Class")

        vSrc(PrjIdx) = sw.ToString()
        txtSrc.Text = vSrc(PrjIdx)


        ' コード解析
        prj1.CodeAnalysis()

        Debug.WriteLine("共通部分式")
        nav2 = New TNavCSE()
        nav2.NavPrj(prj1, Nothing)

        '-------------------------------------------------- データフロー解析のタイマー表示
        gDataflow = New TDataflow()
        gDataflow.SetChangeableFldList(gPrj)
        gDataflow.ChangeableFld = gDataflow.ChangeableFldList(0)
        EnumAnalyzeChangeableFld = gDataflow.AnalyzeChangeableFld()
        gDataflowTimer.Start()
    End Sub

    Sub SetFontSize(font1 As TFont)
        Dim sz As TPnt

        sz = TGraphics.MeasureText("M", font1)
        font1.CharHFont = sz.YPnt
        font1.CharWFont = 6.9
    End Sub

    Public Sub MakeTextLineFig(dc As TDrawCmp, pos As TPnt, line As FLine, font1 As TFont, sel_ref As TReference)
        Dim x2 As Integer, brs As TColor, s As String, w As Integer

        x2 = 2
        ' for ???
        For Each txt In line.TextLine

            If sel_ref IsNot Nothing AndAlso TypeOf txt.ObjFig Is TReference AndAlso sel_ref.VarRef Is CType(txt.ObjFig, TReference).VarRef Then
                brs = TColor.Orange
            Else
                Select Case txt.TypeTxt
                    Case EFigType.eResFig
                        brs = TColor.Blue
                    Case EFigType.eStrFig
                        brs = TColor.Red
                    Case EFigType.eComFig
                        brs = TColor.Green
                    Case Else
                        brs = TColor.Black
                End Select
            End If

            If txt.TabTxt <> 0 Then
                x2 += 4 * txt.TabTxt
            End If

            Select Case txt.TypeFig
                Case EFigType.eSymFig
                    If txt.TextTxt.Length = 1 Then
                        Select Case txt.TextTxt(0)
                            Case "("c, ")"c, "["c, "]"c, "{"c, "}"c, "."c
                                s = txt.TextTxt
                            Case Else
                                s = " " + txt.TextTxt + " "
                        End Select
                    Else
                        s = " " + txt.TextTxt + " "
                    End If
                Case EFigType.eResFig
                    Select Case txt.TknTxt
                        Case EToken.eAs, EToken.eTo, EToken.eIs, EToken.eIsNot
                            s = " " + txt.TextTxt + " "
                        Case EToken.eThen
                            s = " " + txt.TextTxt
                        Case Else
                            s = txt.TextTxt + " "
                    End Select
                Case EFigType.eRefFig
                    Select Case txt.TknTxt
                        Case EToken.eRef
                            If txt.TextTxt = "null" Then
                                s = "Nothing"
                            ElseIf txt.TextTxt = "this" Then
                                s = "Me"
                            Else
                                s = txt.TextTxt
                            End If
                        Case Else
                            s = txt.TextTxt
                    End Select
                Case Else
                    s = txt.TextTxt
            End Select

            If s IsNot Nothing Then
                Dim pos1 As TPnt

                w = TSrcEdit.StrLen(s, s.Length)
                pos1 = pos + New TPnt(x2 * font1.CharWFont, 0)
                dc.AddDrawCmp(New TDrawString(s, font1, brs, pos1, txt.ObjFig))
                x2 += w
            End If
        Next
    End Sub

    Function MakeDrawBG(sz As TPnt) As TDrawCmp
        Dim cd As TDrawCmp
        Dim BorderColor As TColor = TColor.Gray
        Dim BorderW As Double = 2

        cd = New TDrawCmp()
        cd.AddDrawCmp(New TFillRectangle(TColor.White, 0, 0, sz.XPnt, sz.YPnt))
        cd.AddDrawCmp(New TDrawRectangle(BorderColor, BorderW, Math.Floor(BorderW / 2), Math.Floor(BorderW / 2), sz.XPnt - BorderW, sz.YPnt - BorderW))

        Return cd
    End Function

    Function ShowObj(can1 As Canvas, obj As Object) As TBasicCodeGenerator
        Dim code_gen As New TBasicCodeGenerator(gPrj)

        can1.Children.Clear()
        If obj Is Nothing Then
            Return code_gen
        End If

        If TypeOf obj Is TStatement Then
            Dim stmt1 As TStatement = CType(obj, TStatement)


            If TypeOf stmt1 Is TAssignment OrElse TypeOf stmt1 Is TCall Then
                code_gen.StmtSrc(stmt1, 0)
            ElseIf TypeOf stmt1 Is TIfBlock Then
                code_gen.IfBlcHeadSrc(stmt1, 0)
                code_gen.NL(stmt1)
            ElseIf TypeOf stmt1 Is TSelect Then
                code_gen.SelectHeaderSrc(CType(stmt1, TSelect), 0)
            Else
                Debug.WriteLine("")
            End If

        ElseIf TypeOf obj Is TTerm Then
            code_gen.TrmSrc(CType(obj, TTerm))
            code_gen.NL(obj)

        ElseIf TypeOf obj Is TSourceFile Then
            code_gen.MakeBasicSrc(CType(obj, TSourceFile))

        ElseIf TypeOf obj Is TFunction Then
            code_gen.FncSrc(CType(obj, TFunction))

        Else

            Debug.WriteLine("")
        End If

        If code_gen.vLineFig.Count = 0 Then
            Return code_gen
        End If

        ShowCanCodeGen(can1, code_gen)


        Return code_gen
    End Function

    Sub ShowCanCodeGen(can1 As Canvas, code_gen As TBasicCodeGenerator)
        Dim dc As TDrawCmp
        Dim sz As TPnt
        Dim font1 As TFont
        Dim gr As TGraphics
        Dim y As Double

        font1 = New TFont(EFont.eGothic, 10)
        SetFontSize(font1)

        can1.Width = 160 * font1.CharWFont
        can1.Height = code_gen.vLineFig.Count * font1.CharHFont
        sz = New TPnt(can1.ActualWidth, can1.ActualHeight)
        dc = MakeDrawBG(sz)

        y = 0
        For Each line1 In code_gen.vLineFig

            MakeTextLineFig(dc, New TPnt(0, y), line1, font1, Nothing)
            y += font1.CharHFont
        Next

        gr = New TGraphics(Nothing, Nothing)
        gr.CanGr = can1
        dc.Draw(gr)

    End Sub

    Private Sub btnStart_Click(sender As Object, e As RoutedEventArgs) Handles btnStart.Click
    End Sub


    Private Sub DataflowTimer_Tick(ByVal sender As Object, ByVal e As EventArgs)

        Dim x = EnumAnalyzeChangeableFld.MoveNext()
        If Not x Then

            gDataflowTimer.Stop()

            ShowCanCodeGen(canGenSrc, gDataflow.RuleCodeGen)

            PrjIdx += 1
            If PrjIdx < PrjFiles.Count Then
                LoadPrj()
            End If
            Exit Sub
        End If

        'Debug.WriteLine("EnumAnalyzeChangeableFld {0} {1}", x, EnumAnalyzeChangeableFld.Current)
        Select Case EnumAnalyzeChangeableFld.Current
            Case EAnalyzeChangeableFld.ChangeableCondition
                ShowObj(canChangeableCondition, gDataflow.Change.ConditionChn)
                ShowObj(canNormalizedCondition, gDataflow.NormalizedCondition)
                ShowObj(canPreCondition, gDataflow.PreCondition)

            Case EAnalyzeChangeableFld.Consistency
                '-------------------------------------------------- 変数の値を参照している場所を表示する。
                '-------------------------------------------------- 修正後の代入時の条件を表示する。
                '-------------------------------------------------- 文を実行する前提条件を表示する。
                ShowObj(canRefChangeableUpStmt, gDataflow.RefChangeableUpStmt)

            Case EAnalyzeChangeableFld.ValueChangePropagation
                Dim stmt1 As TStatement = TDataflow.UpStmt(gDataflow.ChangePropagation.RefChn)

                ShowObj(canChangePropagation, stmt1)

            Case EAnalyzeChangeableFld.LocalVariableAssignment
                ShowObj(canLocalVariableAssignment, gDataflow.LocalVariableAssignment)

        End Select
    End Sub


    Private Sub Timer_Tick(ByVal sender As Object, ByVal e As EventArgs)
        If TWnd.Capture IsNot Nothing AndAlso TWnd.Capture.MousePressHandler IsNot Nothing Then
            TWnd.MouseEvent.TypeEv = EEvent.eMousePress
            gApp.EventHandler(TWnd.MouseEvent)
        End If

        gApp.EventHandler(New TEvent(EEvent.eOnTimer))

        If gTimerInterval <> gApp.MainForm.Interval Then
            gTimerInterval = gApp.MainForm.Interval
            gTimer.Stop()

            gTimerInterval = gApp.MainForm.Interval
            gTimer.Interval = New TimeSpan(0, 0, 0, 0, gApp.MainForm.Interval)
            gTimer.Start()
        End If

    End Sub

    Private Sub RootCanvas_SizeChanged(sender As Object, e As SizeChangedEventArgs) Handles RootCanvas.SizeChanged
        If gApp Is Nothing Then
            Exit Sub
        End If

        gApp.EventHandler(New TFormEvent(EEvent.eOnResize, gApp.MainForm, New TPnt(RootCanvas.ActualWidth, RootCanvas.ActualHeight)))
    End Sub

    Private Sub SetMouseEvent(e As MouseEventArgs)
        Dim pt As Point

        TWnd.MouseEvent.Shift = ((Keyboard.Modifiers And ModifierKeys.Shift) <> 0)
        TWnd.MouseEvent.Control = ((Keyboard.Modifiers And ModifierKeys.Control) <> 0)
        TWnd.MouseEvent.Alt = ((Keyboard.Modifiers And ModifierKeys.Alt) <> 0)

        pt = e.GetPosition(RootCanvas)
        TWnd.MouseEvent.PosEv.XPnt = pt.X
        TWnd.MouseEvent.PosEv.YPnt = pt.Y
        TWnd.MouseEvent.FormEv = gApp.MainForm
    End Sub

    Private Sub RootCanvas_MouseDown(sender As Object, e As MouseButtonEventArgs) Handles RootCanvas.MouseLeftButtonDown
        If TypeOf e.OriginalSource Is TextBlock Then
            Dim txt As TextBlock = CType(e.OriginalSource, TextBlock)

            Debug.WriteLine("mouse down {0}", txt.Tag)
        End If
        SetMouseEvent(e)
        TWnd.MouseEvent.TypeEv = EEvent.eMouseDown
        TWnd.MouseEvent.MouseDownTime = DateTime.Now

        '        RootCanvas.Focus()
        RootCanvas.CaptureMouse()

        gApp.EventHandler(TWnd.MouseEvent)
    End Sub

    Private Sub RootCanvas_MouseMove(sender As Object, e As MouseEventArgs) Handles RootCanvas.MouseMove
        SetMouseEvent(e)
        TWnd.MouseEvent.TypeEv = EEvent.eMouseMove

        gApp.EventHandler(TWnd.MouseEvent)
    End Sub

    Private Sub RootCanvas_MouseUp(sender As Object, e As MouseButtonEventArgs) Handles RootCanvas.MouseLeftButtonUp
        SetMouseEvent(e)
        TWnd.MouseEvent.TypeEv = EEvent.eMouseUp

        gApp.EventHandler(TWnd.MouseEvent)
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
        '        TWnd.KeyEvent.Repeat = e.IsRepeat
        TWnd.KeyEvent.TypeEv = EEvent.eOnKeyDown
        TWnd.KeyEvent.FormEv = gApp.MainForm

        gApp.EventHandler(TWnd.KeyEvent)
    End Sub

    Private Sub cmb_Project_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles cmb_Project.SelectionChanged
        If cmb_Project.SelectedIndex <> -1 AndAlso cmb_Project.SelectedIndex < vSrc.Count Then
            txtSrc.Text = vSrc(cmb_Project.SelectedIndex)
        End If
    End Sub
End Class