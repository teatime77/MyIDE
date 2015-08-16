Imports System.Diagnostics

Public Delegate Sub TDrawHandler(fig As TFig)
Public Delegate Sub TOnClick(sender As Object)

Public Delegate Sub TMouseEventHandler(e As TMouseEvent)
Public Delegate Sub TScrollEventHandler(e As TScrollEventArgs)

'-------------------------------------------------------------------------------- ECursor
Public Enum ECursor
    eARROW
    eIBEAM
    eWAIT
    eCROSS
    eUPARROW
    eSIZE
    eICON
    eSIZENWSE
    eSIZENESW
    eSIZEWE
    eSIZENS
    eSIZEALL
    eNO
    eHAND
    eAPPSTARTING
    eHELP
End Enum

'-------------------------------------------------------------------------------- EEvent
Public Enum EEvent
    eOnChar
    eOnKeyDown
    eMouseDown
    eMousePress
    eMouseMove
    eMouseUp
    ePaint
    eOnResize
    eOnTimer
End Enum

'-------------------------------------------------------------------------------- EBoundary
Public Enum EBoundary
	eTopLeft
	eTopMid
	eTopRight
	eMidLeft
	eMidMid
	eMidRight
	eBottomLeft
	eBottomMid
	eBottomRight
	eNon
End Enum

'-------------------------------------------------------------------------------- EOrientation
Public Enum EOrientation
    eHorizontal
    eVertical
End Enum

'-------------------------------------------------------------------------------- ETknPos
Public Enum ETknPos
    eTextTknPos
    eExpandTknPos
End Enum

Public Class TFig
    Public PenFig As TColor
    Public PosFig As TPnt
    Public SizeFig As TPnt
    Public RootWnd As TForm
    Public UpWnd As TWnd
    Public Visible As Boolean = True
    Public DrawFig As TDraw

    Public DrawHandler As TDrawHandler

    Public Sub New()
    End Sub

    Public Function LeftWnd() As Double
        Return PosFig.XPnt
    End Function

    Public Function TopWnd() As Double
        Return PosFig.YPnt
    End Function

    Public Function RightWnd() As Double
        Return PosFig.XPnt + SizeFig.XPnt
    End Function

    Public Function BottomWnd() As Double
        Return PosFig.YPnt + SizeFig.YPnt
    End Function

    Public Function WidthWnd() As Double
        Return SizeFig.XPnt
    End Function

    Public Function HeightWnd() As Double
        Return SizeFig.YPnt
    End Function

    Public Overridable Sub SetPos(pos As TPnt)
        PosFig = pos
    End Sub

    Public Overridable Sub SetWidth(w As Double)
        SizeFig.XPnt = w
    End Sub

    Public Overridable Sub SetHeight(h As Double)
        SizeFig.YPnt = h
    End Sub

    Public Overridable Sub SetSize(w As Double, h As Double)
        SetWidth(w)
        SetHeight(h)
    End Sub

    Public Overridable Sub SetSize(sz As TPnt)
        SetWidth(sz.XPnt)
        SetHeight(sz.YPnt)
    End Sub

    Public Overridable Sub SetAutoSize()
    End Sub

    Public Overridable Sub SetAutoWidth()
    End Sub

    Public Overridable Sub SetAutoHeight()
    End Sub

    Public Function AbsPos() As TPnt
        Dim p As TPnt, wnd As TWnd

        p = PosFig
        wnd = UpWnd
        Do While wnd IsNot Nothing
            p = wnd.PosFig + wnd.ClientPos + p
            If wnd.UpWnd IsNot Nothing AndAlso TypeOf wnd.UpWnd Is TScrollView AndAlso CType(wnd.UpWnd, TScrollView).Content Is wnd Then
                p = p + CType(wnd.UpWnd, TScrollView).ViewPos
            End If
            wnd = wnd.UpWnd
        Loop

        Return p
    End Function

    Public Overridable Sub SetStartPos(st As TPnt)
        PosFig = st
    End Sub

    Public Overridable Sub SetEndPos(down As TPnt, ed As TPnt)
        SizeFig.XPnt = Math.Abs(ed.XPnt - down.XPnt)
        PosFig.XPnt = Math.Min(down.XPnt, ed.XPnt)

        SizeFig.YPnt = Math.Abs(ed.YPnt - down.YPnt)
        PosFig.YPnt = Math.Min(down.YPnt, ed.YPnt)
    End Sub

    Public Overridable Sub SetDrawFig()
        If DrawHandler IsNot Nothing Then
            DrawHandler.Invoke(Me)
        End If
    End Sub

    Public Sub Dmp(tab As String)
        Debug.WriteLine("{0}{1} {2} {3}", tab, PosFig, SizeFig, Me)
        If TypeOf Me Is TCmpWnd Then
            ' for Add
            For Each wnd In CType(Me, TCmpWnd).ChildFig
                wnd.Dmp(tab + "    ")
            Next
        End If
    End Sub
End Class

'-------------------------------------------------------------------------------- TLineFig
Public Class TLineFig
    Inherits TFig
    Public StartLine As TPnt
    Public EndLine As TPnt

    Public Sub New()
        PenFig = TColor.Blue
    End Sub

    Public Overrides Sub SetStartPos(st As TPnt)
        StartLine = st
    End Sub

    Public Overrides Sub SetEndPos(down As TPnt, ed As TPnt)
        EndLine = ed
    End Sub

    Public Overrides Sub SetDrawFig()
        DrawFig = New TDrawLine(PenFig, StartLine, EndLine)
        DrawFig.VisDraw = Visible
    End Sub
End Class

'-------------------------------------------------------------------------------- TSplineFig
Public Class TSplineFig
    Inherits TFig
    Public PntSpl As New TList(Of TPnt)

    Public Sub New()
        PenFig = TColor.Blue
    End Sub

    Public Sub AddPntSpl(p As TPnt)
        PntSpl.Add(p)
    End Sub

    Public Overrides Sub SetDrawFig()
        DrawFig = New TDrawSpline(PenFig, PntSpl)
        DrawFig.VisDraw = Visible
    End Sub
End Class

'-------------------------------------------------------------------------------- TWnd
Public Class TWnd
    Inherits TFig
    Public ChildFig As New TList(Of TFig)
    Public Shared PadX As Integer = 5
    Public Shared PadY As Integer = 5
    Public Shared KeyEvent As New TKeyEvent
    Public Shared MouseEvent As New TMouseEvent
    Public Shared Capture As TWnd
    Public Shared MouseDownPos As TPnt
    Public Shared ScrollW As Integer = 15
    Public Shared DefBgBrs As TColor = TColor.White
    Public ClientPos As TPnt
    Public ClientSize As TPnt
    Public FntTxt As TFont
    Public BrsTxt As TColor
    Public BgBrs As TColor
    Public CharH As Double
    Public CharW As Double
    Public BorderW As Double = 2
    Public BorderColor As TColor = TColor.Gray
    Public AnchorLeft As Boolean = True
    Public AnchorTop As Boolean = True
    Public AnchorRight As Double = Double.NaN
    Public AnchorBottom As Double = Double.NaN
    Public Tag As Object

    Public MouseDownHandler As TMouseEventHandler
    Public MouseMoveHandler As TMouseEventHandler
    Public MousePressHandler As TMouseEventHandler
    Public MouseUpHandler As TMouseEventHandler
    Public ClickHandler As TMouseEventHandler

    Public Sub New()
        BgBrs = DefBgBrs
    End Sub

    Public Sub AddChildFig(fig As TFig)
        ChildFig.Add(fig)
        fig.RootWnd = RootWnd
        fig.UpWnd = Me
    End Sub

    Public Sub InsChildFig(idx As Integer, fig As TFig)
        ChildFig.Insert(idx, fig)
        fig.RootWnd = RootWnd
        fig.UpWnd = Me
    End Sub

    Public Sub InitFntBrs()
        Dim sz As TPnt

        FntTxt = New TFont(EFont.eGothic, 10)
        sz = TGraphics.MeasureText("M", FntTxt)
        CharH = sz.YPnt
        CharW = 6.9
        BrsTxt = TColor.Black
        BgBrs = TColor.White
    End Sub

    Public Overridable Sub RemakeDrawFig()
        If RootWnd IsNot Nothing Then

            If Not RootWnd.GrFrm.RemakeDrawFigList.Contains(Me) Then
                RootWnd.GrFrm.RemakeDrawFigList.Add(Me)
            End If
        End If
    End Sub

    Public Overridable Sub OnChar(ch As Char)
    End Sub

    Public Overridable Sub OnClick(ev As TInputEvent)
    End Sub

    Public Overridable Sub OnKeyDown(ev As TKeyEvent)
    End Sub

    Public Overridable Sub OnKeyUp(ev As TKeyEvent)
    End Sub

    Public Overridable Sub OnMouseDown(ev As TMouseEvent)
    End Sub

    Public Overridable Sub OnMouseMove(ev As TMouseEvent)
    End Sub

    Public Overridable Sub OnMouseUp(ev As TMouseEvent)
    End Sub

    Public Shared Function Hello(a As Integer, b As Integer) As Integer
        Return a + b
    End Function

    Public Overridable Sub SetLeft(x As Double)
        PosFig.XPnt = x
    End Sub

    Public Overridable Sub SetTop(y As Double)
        PosFig.YPnt = y
    End Sub

    Public Overrides Sub SetWidth(w As Double)
        MyBase.SetWidth(w)
        ClientPos.XPnt = BorderW
        ClientSize.XPnt = w - BorderW * 2
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)
        ClientPos.YPnt = BorderW
        ClientSize.YPnt = h - BorderW * 2
    End Sub

    Public Function AbsPosC() As TPnt
        Return AbsPos() + ClientPos
    End Function

    Public Function ToClientPos(pt As TPnt) As TPnt
        Return pt - AbsPosC()
    End Function

    Public Function DrawBG() As TDrawCmp
        Dim cd As TDrawCmp

        cd = New TDrawCmp()
        cd.AddDrawCmp(New TFillRectangle(TColor.White, 0, 0, SizeFig.XPnt, SizeFig.YPnt))
        cd.AddDrawCmp(New TDrawRectangle(BorderColor, BorderW, Math.Floor(BorderW / 2), Math.Floor(BorderW / 2), SizeFig.XPnt - BorderW, SizeFig.YPnt - BorderW))

        Return cd
    End Function

    Public Sub SetDrawFigChild(cd As TDrawCmp)
        ' for ???
        For Each fig In ChildFig
            If fig.Visible Then
                fig.SetDrawFig()
                If fig.DrawFig IsNot Nothing AndAlso fig.DrawFig.VisDraw Then
                    If TypeOf fig Is TWnd Then

                        fig.DrawFig.PosDraw = CType(fig, TWnd).ClientPos + fig.PosFig
                    Else
                        fig.DrawFig.PosDraw = fig.PosFig
                    End If
                    cd.AddDrawCmp(fig.DrawFig)
                End If
            End If
        Next
    End Sub

    Public Overrides Sub SetDrawFig()
        Dim cd As TDrawCmp

        If DrawHandler IsNot Nothing Then
            DrawHandler.Invoke(Me)
            Exit Sub
        End If

        cd = DrawBG()
        SetDrawFigChild(cd)
        cd.VisDraw = Visible
        DrawFig = cd
    End Sub

    Public Function IsAncestorWnd(wnd1 As TWnd) As Boolean
        Dim wnd2 As TWnd

        wnd2 = wnd1
        Do While wnd2 IsNot Nothing
            If wnd2 Is Me Then
                Return True
            End If
            wnd2 = wnd2.UpWnd
        Loop

        Return False
    End Function
End Class

Public Class TGetWndByPos
    Public FigGetWnd As TFig
    Public BdrGetWnd As EBoundary
    Public DiffGetWnd As Double

    Public Sub New()
        FigGetWnd = Nothing
        BdrGetWnd = EBoundary.eNon
        DiffGetWnd = Double.MaxValue
    End Sub
End Class

'-------------------------------------------------------------------------------- TCmpWnd
Public Class TCmpWnd
    Inherits TWnd

    Public Sub GetWndByPos(pt As TPnt, ret As TGetWndByPos)
        Dim x1 As Double, y1 As Double, x2 As Double, y2 As Double, i As Integer, j As Integer, dify As Double, difx As Double, dif2 As Double
        Dim BdrMG As Double = 0, cwnd As TCmpWnd

        Debug.Assert(pt = ToClientPos(TWnd.MouseEvent.PosEv))
        ' for 
        For Each wnd In ChildFig
            If wnd.Visible Then
                x1 = wnd.LeftWnd()
                y1 = wnd.TopWnd()
                x2 = wnd.RightWnd()
                y2 = wnd.BottomWnd()
                If x1 - BdrMG <= pt.XPnt AndAlso pt.XPnt < x2 + BdrMG AndAlso y1 - BdrMG <= pt.YPnt AndAlso pt.YPnt < y2 + BdrMG Then
                    If y1 - BdrMG <= pt.YPnt AndAlso pt.YPnt < y1 + BdrMG Then
                        i = 0
                        dify = Math.Abs(pt.YPnt - y1)
                    ElseIf y2 - BdrMG <= pt.YPnt AndAlso pt.YPnt < y2 + BdrMG Then
                        i = 2
                        dify = Math.Abs(pt.YPnt - y2)
                    Else
                        i = 1
                        dify = 0
                    End If

                    If x1 - BdrMG <= pt.XPnt AndAlso pt.XPnt < x1 + BdrMG Then
                        j = 0
                        difx = Math.Abs(pt.XPnt - x1)
                    ElseIf x2 - BdrMG <= pt.XPnt AndAlso pt.XPnt < x2 + BdrMG Then
                        j = 2
                        difx = Math.Abs(pt.XPnt - x2)
                    Else
                        j = 1
                        difx = 0
                    End If

                    dif2 = Math.Sqrt(difx * difx + dify * dify)
                    If dif2 <= ret.DiffGetWnd Then

                        ret.FigGetWnd = wnd
                        ret.BdrGetWnd = CType(i * 3 + j, EBoundary)
                        ret.DiffGetWnd = dif2
                    End If

                    If TypeOf wnd Is TCmpWnd Then
                        cwnd = CType(wnd, TCmpWnd)
                        cwnd.GetWndByPos(pt - (cwnd.PosFig + cwnd.ClientPos), ret)
                    End If
                End If
            End If
        Next
    End Sub
End Class

'-------------------------------------------------------------------------------- TPanel
Public Class TPanel
    Inherits TCmpWnd
    Public ChildWnd As New TList(Of TWnd)

    Public Overrides Sub SetPos(pos As TPnt)
        MyBase.SetPos(pos)
    End Sub

    Public Overrides Sub SetWidth(w As Double)
        MyBase.SetWidth(w)

        ' for Call
        For Each wnd In ChildWnd
            If wnd.AnchorLeft AndAlso Not Double.IsNaN(wnd.AnchorRight) Then
                wnd.SetWidth(ClientSize.XPnt - (wnd.PosFig.XPnt + wnd.AnchorRight))
            ElseIf wnd.AnchorLeft Then
                wnd.SetWidth(wnd.SizeFig.XPnt)
            ElseIf Not Double.IsNaN(wnd.AnchorRight) Then
                wnd.PosFig.XPnt = ClientSize.XPnt - (wnd.SizeFig.XPnt + wnd.AnchorRight)
                wnd.SetWidth(wnd.SizeFig.XPnt)
            End If
        Next
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)

        ' for Call
        For Each wnd In ChildWnd
            If wnd.AnchorTop AndAlso Not Double.IsNaN(wnd.AnchorBottom) Then
                wnd.SetHeight(ClientSize.YPnt - (wnd.PosFig.YPnt + wnd.AnchorBottom))
            ElseIf wnd.AnchorTop Then
                wnd.SetHeight(wnd.SizeFig.YPnt)
            ElseIf Not Double.IsNaN(wnd.AnchorBottom) Then
                wnd.PosFig.YPnt = ClientSize.YPnt - (wnd.SizeFig.YPnt + wnd.AnchorBottom)
                wnd.SetHeight(wnd.SizeFig.YPnt)
            End If
        Next
    End Sub

    Public Sub AddChildWnd(wnd As TWnd)
        AddChildFig(wnd)
        ChildWnd.Add(wnd)
    End Sub

    Public Function Content() As TWnd
        Return ChildWnd(0)
    End Function

    Public Overrides Sub SetAutoSize()
        Debug.Assert(ChildWnd.Count = 1)

        Content().SetPos(New TPnt(PadX, PadY))
        Content.SetAutoSize()
        SetSize(New TPnt(PadX + Content().WidthWnd() + PadX, PadY + Content().HeightWnd() + PadY))
    End Sub
End Class

'-------------------------------------------------------------------------------- TStackPanel
Public Class TStackPanel
    Inherits TPanel
    Public OriStp As EOrientation = EOrientation.eHorizontal

    Public Sub New(ori As EOrientation)
        OriStp = ori
        BorderW = 0
        BorderColor = TColor.Orange
    End Sub

    Public Overrides Sub SetAutoSize()
        Dim x As Double, y As Double, max_w As Double, max_h As Double

        If OriStp = EOrientation.eHorizontal Then
            ' 横方向に並べる場合

            x = PadX
            max_h = 0
            For Each wnd In ChildWnd
                wnd.SetPos(New TPnt(x, PadY))
                wnd.SetAutoSize()
                max_h = Math.Max(max_h, wnd.HeightWnd())

                x += wnd.WidthWnd() + PadX
            Next

            SetSize(New TPnt(x, PadY + max_h + PadY))
        Else
            ' 縦方向に並べる場合

            y = PadY
            max_w = 0
            For Each wnd In ChildWnd
                wnd.SetPos(New TPnt(PadX, y))
                wnd.SetAutoSize()
                max_w = Math.Max(max_w, wnd.WidthWnd())

                y += wnd.HeightWnd() + PadY
            Next

            SetSize(New TPnt(PadX + max_w + PadX, y))
        End If
    End Sub
End Class

'-------------------------------------------------------------------------------- TGridGraph
Public Class TGridGraph
    Inherits TPanel
    Public NodeGrid As SNode(,)

    Public Sub New(L As TList(Of TList(Of TNode)))
        Dim max_nrow As Integer, snd As SNode, max_w As Double(), x As Double, y As Double, col As Integer, row As Integer, Lk As TList(Of TNode)
        Dim w As Double, h As Double

        ' 最大行数を求める
        max_nrow = 0
        For Each Lk In L
            max_nrow = Math.Max(max_nrow, Lk.Count)
        Next

        ' ノード図形の配列の領域を確保する
        ReDim NodeGrid(max_nrow - 1, L.Count - 1)
        ReDim max_w(L.Count - 1)

        ' すべての桁に対し
        x = 0
        For col = 0 To L.Count - 1
            Lk = L(col)

            ' すべての行に対し
            max_w(col) = 0
            For row = 0 To Lk.Count - 1

                If Lk(row).DatNode IsNot Nothing Then
                    ' ダミーノードでない場合

                    ' ノード図形を作る
                    snd = New SNode(Lk(row))
                    AddChildFig(snd)

                    ' 最大幅をセットする
                    max_w(col) = Math.Max(max_w(col), snd.WidthWnd())

                    NodeGrid(row, col) = snd
                End If
            Next

            ' すべての行に対し
            y = 10
            For row = 0 To Lk.Count - 1
                If Lk(row).DatNode IsNot Nothing Then
                    ' ダミーノードでない場合

                    ' ノード図形の位置をセットする
                    NodeGrid(row, col).SetPos(New TPnt(x, y))
                End If
                y += 20
            Next

            x += max_w(col) + 10
        Next

        w = x + BorderW * 2
        h = y + BorderW * 2

        SizeFig.XPnt = w
        ClientPos.XPnt = BorderW
        ClientSize.XPnt = w - BorderW * 2

        SizeFig.YPnt = h
        ClientPos.YPnt = BorderW
        ClientSize.YPnt = h - BorderW * 2
    End Sub
End Class

'-------------------------------------------------------------------------------- TPopup
Public Class TPopup
    Inherits TPanel

    Public Sub New()
        BorderW = 0
        BorderColor = TColor.Green
    End Sub
End Class

'-------------------------------------------------------------------------------- TForm
Public Class TForm
    Inherits TPanel
    Public GrFrm As TGraphics
    Public bMouseDown As Boolean
    Public MouseDownWnd As TWnd
    Public ActivePopup As TPopup
    Dim FocusWnd As TWnd
    Public Interval As Integer

    Public Sub New()
        RootWnd = Me
    End Sub

    Public Function GetFocus() As TWnd
        Return FocusWnd
    End Function

    Public Overrides Sub SetPos(pos As TPnt)
        MyBase.SetPos(pos)
    End Sub

    Public Overrides Sub SetSize(sz As TPnt)
        Dim old_sz As TPnt

        old_sz = SizeFig
        MyBase.SetSize(sz)

        If old_sz = sz AndAlso GrFrm IsNot Nothing Then
            Exit Sub
        End If

        If GrFrm IsNot Nothing Then
            GrFrm.DisposeGraphics()
        End If

        GrFrm = New TGraphics(GrFrm, Me)
    End Sub

    Public Overrides Sub OnChar(ch As Char)
        If FocusWnd IsNot Nothing Then
            FocusWnd.OnChar(ch)
        End If
    End Sub

    Public Overrides Sub OnKeyDown(ev As TKeyEvent)
        If FocusWnd IsNot Nothing Then
            FocusWnd.OnKeyDown(ev)
        End If
    End Sub

    Public Sub OnResize(sz As TPnt)
        Dim sz_sv As TPnt

        If sz = SizeFig Then
            Exit Sub
        End If
        sz_sv = SizeFig

        SetSize(sz)
    End Sub

    Public Overrides Sub OnMouseDown(ev As TMouseEvent)
        Dim ret As New TGetWndByPos

        Capture = Nothing
        MouseDownPos = ev.PosEv
        bMouseDown = True
        If ActivePopup IsNot Nothing Then
            ActivePopup.GetWndByPos(ActivePopup.ToClientPos(ev.PosEv), ret)
        Else
            GetWndByPos(ev.PosEv - ClientPos, ret)
        End If
        MouseDownWnd = CType(ret.FigGetWnd, TWnd)
        If MouseDownWnd IsNot Nothing Then
            FocusWnd = MouseDownWnd
            ev.SrcEv = MouseDownWnd
            If FocusWnd.MouseDownHandler IsNot Nothing Then
                MouseDownWnd.MouseDownHandler.Invoke(ev)
            Else
                MouseDownWnd.OnMouseDown(ev)
            End If
        End If
    End Sub

    Public Overrides Sub OnMouseMove(ev As TMouseEvent)
        Dim wnd As TWnd, ret As New TGetWndByPos


        If Capture IsNot Nothing Then
            wnd = Capture
        Else

            TGraphics.SetCursor(ECursor.eARROW)
            GetWndByPos(ev.PosEv - ClientPos, ret)
            wnd = CType(ret.FigGetWnd, TWnd)
        End If

        If wnd IsNot Nothing Then
            ev.SrcEv = wnd
            If wnd.MouseMoveHandler IsNot Nothing Then
                wnd.MouseMoveHandler.Invoke(ev)
            Else
                wnd.OnMouseMove(ev)
            End If
        End If
    End Sub

    Public Overrides Sub OnMouseUp(ev As TMouseEvent)
        Dim wnd As TWnd, ret As New TGetWndByPos

        bMouseDown = False

        If Capture IsNot Nothing Then
            wnd = Capture
        Else
            If ActivePopup IsNot Nothing Then
                ActivePopup.GetWndByPos(ActivePopup.ToClientPos(ev.PosEv), ret)
            Else
                GetWndByPos(ev.PosEv - ClientPos, ret)
            End If
            wnd = CType(ret.FigGetWnd, TWnd)
        End If

        If wnd IsNot Nothing Then
            ev.SrcEv = wnd

            If wnd.MouseUpHandler IsNot Nothing Then
                wnd.MouseUpHandler.Invoke(ev)
            Else
                wnd.OnMouseUp(ev)
            End If

            If wnd Is MouseDownWnd Then
                ' クリックした場合

                If wnd.ClickHandler IsNot Nothing Then
                    wnd.ClickHandler.Invoke(ev)
                End If
                wnd.OnClick(ev)
            End If
        End If
        MouseDownWnd = Nothing

        If ActivePopup IsNot Nothing AndAlso ActivePopup.IsAncestorWnd(wnd) Then
            ActivePopup = Nothing
            RemakeDrawFig()
        End If
        Capture = Nothing
        TGraphics.SetCursor(ECursor.eARROW)
    End Sub

End Class

'-------------------------------------------------------------------------------- TLabel
Public Class TLabel
    Inherits TWnd
    Public TextWnd As String

    Public Sub InitLabel()
        FntTxt = New TFont(EFont.eGothic, 10)
        BrsTxt = TColor.Black
        'TextWnd = "ラベル"
    End Sub

    Public Sub New()
        InitLabel()
    End Sub

    Public Sub New(txt As String)
        InitLabel()
        TextWnd = txt

        SetAutoSize()
    End Sub

    Public Overrides Sub SetAutoSize()
        Dim sz As TPnt

        sz = TGraphics.MeasureText(TextWnd, FntTxt)
        SetSize(sz.XPnt + 2 * (ClientPos.XPnt + 1), sz.YPnt + 2 * (ClientPos.YPnt + 1))
    End Sub

    Public Overrides Sub SetDrawFig()
        MyBase.SetDrawFig()
        DrawFig.VisDraw = Visible
        CType(DrawFig, TDrawCmp).AddDrawCmp(New TDrawString(TextWnd, FntTxt, BrsTxt, New TPnt(1, 1)))
    End Sub
End Class

'-------------------------------------------------------------------------------- TButton
Public Class TButton
    Inherits TLabel

    Public Sub New()
    End Sub

    Public Sub New(txt As String)
        MyBase.New(txt)
    End Sub
End Class

'-------------------------------------------------------------------------------- TRadioButton
Public Class TRadioButton
    Inherits TLabel
    Public Checked As Boolean
    Public RadioButtonClickHandler As TOnClick

    Public Sub New()
    End Sub

    Public Sub New(txt As String)
        Dim sz As TPnt

        InitLabel()
        TextWnd = txt

        sz = TGraphics.MeasureText("○ " + txt, FntTxt)
        SetSize(sz.XPnt + 2 * (ClientPos.XPnt + 1), sz.YPnt + 2 * (ClientPos.YPnt + 2))
    End Sub

    Public Overrides Sub OnClick(ev As TInputEvent)
        Dim rad As TRadioButton

        ' for Let
        For Each wnd In CType(UpWnd, TPanel).ChildWnd
            If TypeOf wnd Is TRadioButton Then
                rad = CType(wnd, TRadioButton)
                If rad.Checked Then

                    rad.Checked = False
                    rad.RemakeDrawFig()
                End If
            End If
        Next
        Checked = True
        If RadioButtonClickHandler IsNot Nothing Then
            RadioButtonClickHandler.Invoke(Me)
        End If

        RemakeDrawFig()
    End Sub

    Public Overrides Sub SetDrawFig()
        Dim cd As TDrawCmp
        Dim pos As TPnt

        If DrawHandler IsNot Nothing Then
            DrawHandler.Invoke(Me)
            Exit Sub
        End If

        cd = DrawBG()
        cd.VisDraw = Visible

        pos = ClientPos + New TPnt(1, 1)
        If Checked Then
            cd.AddDrawCmp(New TDrawString("◎" + TextWnd, FntTxt, BrsTxt, pos))
        Else
            cd.AddDrawCmp(New TDrawString("○" + TextWnd, FntTxt, BrsTxt, pos))
        End If

        DrawFig = cd
    End Sub
End Class

'-------------------------------------------------------------------------------- TScrollBar
Public Class TScrollBar
    Inherits TCmpWnd
    Public PrvScb As New TButton
    Public NxtScb As New TButton
    Public SldScb As New TWnd
    Public OriScb As EOrientation
    Public ScrollEventHandler As TScrollEventHandler
    Public Minimum As Integer = 0
    Public Maximum As Integer = 100
    Public Value As Integer = 50
    Public SmallChange As Integer = 1
    Dim ValSv As Integer

    Public Sub New(ori As EOrientation)
        OriScb = ori

        BorderW = 1
        BorderColor = TColor.Orange
        PrvScb.BorderColor = TColor.Navy
        NxtScb.BorderColor = TColor.Navy
        PrvScb.SetSize(New TPnt(ScrollW - 2 * BorderW, ScrollW - 2 * BorderW))
        NxtScb.SetSize(New TPnt(ScrollW - 2 * BorderW, ScrollW - 2 * BorderW))

        PrvScb.MouseDownHandler = AddressOf OnPrvScb_MouseDown
        NxtScb.MouseDownHandler = AddressOf OnNxtScb_MouseDown

        PrvScb.MousePressHandler = AddressOf OnPrvScb_MousePress
        NxtScb.MousePressHandler = AddressOf OnNxtScb_MousePress

        PrvScb.ClickHandler = AddressOf Me.OnPrvScb_Click
        NxtScb.ClickHandler = AddressOf OnNxtScb_Click

        PrvScb.SetLeft(0)
        PrvScb.SetTop(0)
        If OriScb = EOrientation.eHorizontal Then
            NxtScb.SetTop(0)
        Else
            NxtScb.SetLeft(0)
        End If

        AddChildFig(PrvScb)
        AddChildFig(NxtScb)
        AddChildFig(SldScb)
    End Sub

    Public Overrides Sub SetWidth(w As Double)
        MyBase.SetWidth(w)

        If OriScb = EOrientation.eHorizontal Then
            NxtScb.SetLeft(ClientSize.XPnt - NxtScb.SizeFig.XPnt)
        End If
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)

        If OriScb = EOrientation.eVertical Then
            NxtScb.SetTop(ClientSize.YPnt - NxtScb.SizeFig.YPnt)
        End If
    End Sub

    Public Sub SetValue(n As Integer)
        Value = Math.Min(Maximum, Math.Max(Minimum, n))
    End Sub

    Public Overrides Sub OnMouseDown(ev As TMouseEvent)
        Capture = Me
        ValSv = Value
        Debug.WriteLine("scroll bar mouse down")
    End Sub

    Public Overrides Sub OnMouseMove(ev As TMouseEvent)
        Dim se As TScrollEventArgs
        Dim f As Integer, val As Integer

        If Capture Is Me Then
            Debug.WriteLine("scroll bar mouse drag")
            se = New TScrollEventArgs(Me)
            If OriScb = EOrientation.eHorizontal Then
                f = ValSv + CType(ev.PosEv.XPnt - MouseDownPos.XPnt, Integer)
            Else
                f = ValSv + CType(ev.PosEv.YPnt - MouseDownPos.YPnt, Integer)
            End If
            val = Math.Min(Maximum, Math.Max(Minimum, f))
            If val <> Value Then
                Value = val
                If ScrollEventHandler IsNot Nothing Then
                    ScrollEventHandler.Invoke(se)
                End If
            End If

        Else
            Debug.WriteLine("scroll bar mouse move")
        End If
    End Sub

    Public Sub PrvStep()
        Dim se As TScrollEventArgs

        If Minimum < Value Then
            se = New TScrollEventArgs(Me)
            Value = Math.Max(Minimum, Value - SmallChange)
            If ScrollEventHandler IsNot Nothing Then
                ScrollEventHandler.Invoke(se)
            End If
        End If

    End Sub

    Public Sub NxtStep()
        Dim se As TScrollEventArgs

        If Value < Maximum Then
            se = New TScrollEventArgs(Me)
            Value = Math.Min(Maximum, Value + SmallChange)
            If ScrollEventHandler IsNot Nothing Then
                ScrollEventHandler.Invoke(se)
            End If
        End If
    End Sub

    Public Sub OnPrvScb_Click(e As TMouseEvent)
        Debug.WriteLine("Prv Scb Click")

        PrvStep()
    End Sub

    Public Sub OnNxtScb_Click(e As TMouseEvent)
        Debug.WriteLine("Nxt Scb Click")

        NxtStep()
    End Sub

    Public Sub OnPrvScb_MouseDown(e As TMouseEvent)
        Capture = PrvScb

        Debug.WriteLine("Prv Scb mouse down")
    End Sub

    Public Sub OnNxtScb_MouseDown(e As TMouseEvent)
        Capture = NxtScb

        Debug.WriteLine("Nxt Scb mouse down")
    End Sub

    Public Sub OnPrvScb_MousePress(e As TMouseEvent)
        Debug.WriteLine("Prv Scb mouse press")

        PrvStep()
    End Sub

    Public Sub OnNxtScb_MousePress(e As TMouseEvent)
        Debug.WriteLine("Nxt Scb mouse press")

        NxtStep()
    End Sub
End Class

'-------------------------------------------------------------------------------- TScrollView
Public Class TScrollView
    Inherits TCmpWnd
    Public ViewPos As TPnt
    Public ViewSize As TPnt
    Public Content As TWnd
    Dim ViewPosSv As TPnt

    Public HSbScv As TScrollBar
    Public VSbScv As TScrollBar

    Public Sub New()
        HSbScv = New TScrollBar(EOrientation.eHorizontal)
        VSbScv = New TScrollBar(EOrientation.eVertical)

        HSbScv.SetHeight(ScrollW)
        VSbScv.SetWidth(ScrollW)
        HSbScv.SetLeft(0)
        VSbScv.SetTop(0)
        HSbScv.SetValue(0)
        VSbScv.SetValue(0)

        HSbScv.ScrollEventHandler = AddressOf OnHsb_Scroll
        VSbScv.ScrollEventHandler = AddressOf OnVsb_Scroll

        AddChildFig(HSbScv)
        AddChildFig(VSbScv)

        ViewPos.XPnt = HSbScv.Value * 10
        ViewPos.YPnt = VSbScv.Value * 10
    End Sub

    Public Sub SetContent(wnd As TWnd)
        If Content IsNot Nothing Then
            ChildFig.Remove(Content)
        End If

        Content = wnd
        Content.UpWnd = Me
        InsChildFig(0, Content)
    End Sub

    Public Overrides Sub SetWidth(w As Double)
        MyBase.SetWidth(w)

        ClientSize.XPnt -= ScrollW

        If Content IsNot Nothing Then
            Content.SetWidth(ClientSize.XPnt)
        End If
        If HSbScv IsNot Nothing Then
            HSbScv.SetWidth(ClientSize.XPnt)
        End If
        If VSbScv IsNot Nothing Then
            VSbScv.SetLeft(ClientSize.XPnt)
        End If
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)

        ClientSize.YPnt -= ScrollW

        If Content IsNot Nothing Then
            Content.SetHeight(ClientSize.YPnt)
        End If
        If HSbScv IsNot Nothing Then
            HSbScv.SetTop(ClientSize.YPnt)
        End If
        If VSbScv IsNot Nothing Then
            VSbScv.SetHeight(ClientSize.YPnt)
        End If
    End Sub

    Public Overrides Sub OnMouseDown(ev As TMouseEvent)
        ViewPosSv = ViewPos
        Debug.WriteLine("scroll view mouse down {0}", ViewPos)
    End Sub

    Public Overrides Sub OnMouseMove(ev As TMouseEvent)
        If Capture IsNot Nothing AndAlso Capture Is Content Then
            ViewPos = ViewPosSv + (ev.PosEv - MouseDownPos)
            HSbScv.SetValue(CType(ViewPos.XPnt / 10, Integer))
            VSbScv.SetValue(CType(ViewPos.YPnt / 10, Integer))
            Debug.WriteLine("scroll view mouse drag {0}", ViewPos)
            RemakeDrawFig()
        End If
    End Sub

    Public Overrides Sub SetDrawFig()
        Dim cd As TDrawCmp

        cd = DrawBG()
        cd.VisDraw = Visible

        ' for ???
        For Each fig In ChildFig
            fig.SetDrawFig()
            If TypeOf fig Is TWnd Then

                If fig Is Content Then
                    fig.DrawFig.PosDraw = CType(fig, TWnd).ClientPos - ViewPos + fig.PosFig

                    cd.AddDrawCmp(New TPushClip(ClientPos, ClientSize))
                    cd.AddDrawCmp(fig.DrawFig)
                    cd.AddDrawCmp(New TPopClip())
                Else

                    fig.DrawFig.PosDraw = CType(fig, TWnd).ClientPos + fig.PosFig
                    cd.AddDrawCmp(fig.DrawFig)
                End If
            Else
                fig.DrawFig.PosDraw = fig.PosFig
                cd.AddDrawCmp(fig.DrawFig)
            End If
        Next

        DrawFig = cd
    End Sub

    Public Sub OnHsb_Scroll(e As TScrollEventArgs)
        ViewPos.XPnt = HSbScv.Value * 10
        Debug.WriteLine("Hsb scroll {0} {1}", CType(e.SrcEv, TScrollBar).Value, ViewPos)
        RemakeDrawFig()
    End Sub

    Public Sub OnVsb_Scroll(e As TScrollEventArgs)
        ViewPos.YPnt = VSbScv.Value * 10
        Debug.WriteLine("Vsb scroll {0} {1}", CType(e.SrcEv, TScrollBar).Value, ViewPos)
        RemakeDrawFig()
    End Sub
End Class

'-------------------------------------------------------------------------------- TListBox
Public Class TListBox
    Inherits TScrollView
    Public SelectedIndex As Integer
    Dim SelBrs As TColor
    Dim TextLB As New TList(Of String)
    Dim DataLB As New TList(Of Object)
    Dim vSelectedIndexChanged As New TList(Of ISelectedIndexChangedListener)
    Dim ItemClick As TOnClick

    Public Sub OnItemClick(sender As Object)

    End Sub

    Public Sub New()
        InitFntBrs()
        SelBrs = TColor.Blue
        ViewPos = New TPnt(0, 0)
        SelectedIndex = -1
        '		ViewSize = New TPnt(80 * CharW, vStr.Count * CharH)

        ItemClick = AddressOf OnItemClick
        ItemClick.Invoke(Me)
    End Sub

    Public Overrides Sub OnClick(ev As TInputEvent)
        Dim pt As TPnt, st As Integer, old_idx As Integer

        pt = ev.PosEv - AbsPosC()
        st = CType(Math.Floor((ViewPos.YPnt + pt.YPnt) / CharH), Integer)
        If st <> SelectedIndex AndAlso 0 <= st AndAlso st < TextLB.Count Then
            old_idx = SelectedIndex
            SelectedIndex = st
            ' for Call
            For Each lis In vSelectedIndexChanged
                lis.OnSelectedIndexChanged(Me, old_idx, SelectedIndex)
            Next
            RemakeDrawFig()
        End If
    End Sub

    Public Overrides Sub SetDrawFig()
        Dim ln As Integer, y As Double, st As Integer
        Dim cd As TDrawCmp, pos As TPnt

        cd = DrawBG()

        st = CType(Math.Floor(ViewPos.YPnt / CharH), Integer)

        ' for Seq
        For ln = st To TextLB.Count - 1
            y = ln * CharH - ViewPos.YPnt
            pos = ClientPos + New TPnt(0.0F, y)
            If ln = SelectedIndex Then
                cd.AddDrawCmp(New TDrawString(TextLB(ln), FntTxt, SelBrs, pos))
            Else
                cd.AddDrawCmp(New TDrawString(TextLB(ln), FntTxt, BrsTxt, pos))
            End If
        Next

        DrawFig = cd
        'gr.AddRepaint(Me)
    End Sub

    Public Sub AddLB(txt As String, dat As Object)
        TextLB.Add(txt)
        DataLB.Add(dat)
    End Sub

    Public Sub AddSelectedIndexChangedListener(lis As ISelectedIndexChangedListener)
        If Not vSelectedIndexChanged.Contains(lis) Then
            vSelectedIndexChanged.Add(lis)
        End If
    End Sub
End Class

'-------------------------------------------------------------------------------- TTreeNode
Public Class TTreeNode
    Inherits TWnd
    Public TreeTrn As TTreeView
    Public TextTrn As String
    Public DatTrn As Object
    Public NodesTrn As New TList(Of TTreeNode)

    Public Sub New(txt As String, dat As Object)
        TextTrn = txt
        DatTrn = dat
    End Sub

    Public Overrides Sub SetAutoSize()
        Dim sz As TPnt

        sz = TGraphics.MeasureText(TextTrn, TreeTrn.FntTxt)
        SetSize(New TPnt(2 + sz.XPnt + 2, 2 + sz.YPnt + 2))
    End Sub

    Public Function AddTrn(txt As String, dat As Object) As TTreeNode
        Dim nd1 As TTreeNode

        nd1 = New TTreeNode(txt, dat)
        nd1.TreeTrn = TreeTrn
        NodesTrn.Add(nd1)

        Return nd1
    End Function

    Public Overrides Sub SetDrawFig()
        Dim cd As TDrawCmp

        cd = New TDrawCmp()

        cd.AddDrawCmp(New TDrawString(TextTrn, TreeTrn.FntTxt, TreeTrn.BrsTxt, New TPnt(2, 2)))

        DrawFig = cd
    End Sub

End Class

'-------------------------------------------------------------------------------- TTreeView
Public Class TTreeView
    Inherits TScrollView
    Dim SelBrs As TColor
    Public NodesTrv As New TList(Of TTreeNode)

    Public Sub New()
        InitFntBrs()
        SelBrs = TColor.Blue
        ViewPos = New TPnt(0, 0)
    End Sub

    Public Overrides Sub OnClick(ev As TInputEvent)
    End Sub

    Public Overrides Sub SetDrawFig()
        Dim cd As TDrawCmp, pos As TPnt

        cd = DrawBG()

        pos = ClientPos
        For Each trn In NodesTrv
            trn.SetDrawFig()
            If trn.DrawFig IsNot Nothing Then

                trn.DrawFig.PosDraw = pos
                cd.AddDrawCmp(trn.DrawFig)

                pos.YPnt += 2 + trn.SizeFig.YPnt
            End If
        Next

        DrawFig = cd
    End Sub

    Public Function AddTrv(txt As String, dat As Object) As TTreeNode
        Dim nd1 As TTreeNode

        nd1 = New TTreeNode(txt, dat)
        nd1.TreeTrn = Me
        nd1.SetAutoSize()
        NodesTrv.Add(nd1)

        Return nd1
    End Function
End Class

'-------------------------------------------------------------------------------- TSplitContainer
Public Class TSplitContainer
    Inherits TCmpWnd
    Public OrientationSpc As EOrientation
    Public RatioSpc As Double
    Dim 分割中 As Boolean
    Dim SplitLen As Double

    Public Sub New()
        ChildFig.Add(New TWnd())
        ChildFig.Add(New TWnd())
    End Sub

    Public Sub SetSplitWind(wnd As TWnd, idx As Integer)
        Debug.Assert(idx = 0 OrElse idx = 1)
        ChildFig(idx).UpWnd = Nothing
        wnd.UpWnd = Me
        wnd.RootWnd = RootWnd
        ChildFig(idx) = wnd
    End Sub

    Sub SplitH(w1 As Double)
        ChildFig(0).SetWidth(w1)
        ChildFig(1).SetWidth(ClientSize.XPnt - (1 + 3 + 1) - ChildFig(0).SizeFig.XPnt)
        ChildFig(0).SetPos(New TPnt(1, 1))
        ChildFig(1).SetPos(New TPnt(1 + ChildFig(0).SizeFig.XPnt + 3, 1))
    End Sub

    Public Overrides Sub SetWidth(w As Double)
        MyBase.SetWidth(w)

        Select Case OrientationSpc
            Case EOrientation.eHorizontal

                SplitH((ClientSize.XPnt - (1 + 3 + 1)) * RatioSpc)
        End Select
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)

        Select Case OrientationSpc
            Case EOrientation.eHorizontal
                ChildFig(0).SetHeight(ClientSize.YPnt - 2)
                ChildFig(1).SetHeight(ClientSize.YPnt - 2)
        End Select
    End Sub

    Function 分割線上(ev As TMouseEvent) As Boolean
        Dim pt1 As TPnt, x1 As Double

        pt1 = ToClientPos(ev.PosEv)
        x1 = ChildFig(1).LeftWnd()

        Return Math.Abs(pt1.XPnt - x1) < 10
    End Function

    Public Overrides Sub OnMouseDown(ev As TMouseEvent)
        If 分割線上(ev) Then
            TGraphics.SetCursor(ECursor.eSIZEWE)
            TWnd.Capture = Me
            分割中 = True
            SplitLen = ChildFig(0).SizeFig.XPnt
        End If
    End Sub

    Public Overrides Sub OnMouseMove(ev As TMouseEvent)
        Dim len1 As Double

        If Capture Is Me Then
            If 分割中 Then
                len1 = SplitLen + ev.PosEv.XPnt - MouseDownPos.XPnt
                If 5 < len1 AndAlso len1 < ClientSize.XPnt - (1 + 3 + 1) - 5 Then

                    SplitH(len1)
                    RemakeDrawFig()
                End If
            End If
        Else
            If 分割線上(ev) Then
                TGraphics.SetCursor(ECursor.eSIZEWE)
            End If
        End If
    End Sub

    Public Overrides Sub OnMouseUp(ev As TMouseEvent)
    End Sub
End Class

'-------------------------------------------------------------------------------- TTabControl
Public Class TTabControl
    Inherits TCmpWnd
    Public Shared TabH As Integer = 20
    Public TabName As New TList(Of TButton)
    Public TabWnd As New TList(Of TWnd)
    Public SelectedIndex As Integer = 0

    Public Sub AddTab(name As String, wnd As TWnd)
        Dim btn As TButton

        btn = New TButton(name)
        If TabName.Count = 0 Then
            btn.PosFig.XPnt = 0
        Else
            btn.PosFig.XPnt = TabName(TabName.Count - 1).RightWnd() + 1
            wnd.Visible = False            
        End If
        btn.ClickHandler = AddressOf NameBtn_Clicked

        TabName.Add(btn)

        wnd.PosFig = New TPnt(1, TabH + 1)

        TabWnd.Add(wnd)

        AddChildFig(btn)
        AddChildFig(wnd)
    End Sub

    Public Sub NameBtn_Clicked(e As TMouseEvent)
        Dim i1 As Integer

        SelectedIndex = TabName.IndexOf(CType(e.SrcEv, TButton))
        For i1 = 0 To TabWnd.Count - 1
            TabWnd(i1).Visible = (i1 = SelectedIndex)
            If TabWnd(i1).DrawFig IsNot Nothing Then
                TabWnd(i1).DrawFig.VisDraw = (i1 = SelectedIndex)
            End If
        Next
        Debug.WriteLine("NameBtn_Clicked {0}", SelectedIndex)
        RemakeDrawFig()
    End Sub

    Public Overrides Sub SetWidth(w As Double)
        MyBase.SetWidth(w)

        ' for Call
        For Each wnd In TabWnd
            wnd.SetWidth(ClientSize.XPnt - 2 * wnd.PosFig.XPnt)
        Next
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)

        ' for Call
        For Each wnd In TabWnd
            wnd.SetHeight(ClientSize.YPnt - (wnd.PosFig.YPnt + 1))
        Next
    End Sub
End Class

'-------------------------------------------------------------------------------- TDesWnd
Public Class TDesWnd
    Inherits TPanel
    Public SelTool As Object
    Dim NewFigDes As TFig
    Dim DownPosDes As TPnt

    Public Sub New(tool_box As TPanel)
        Dim rad1 As TRadioButton

        BorderColor = TColor.Blue

        ' for Let
        For Each btn In tool_box.ChildWnd
            rad1 = CType(btn, TRadioButton)
            rad1.RadioButtonClickHandler = AddressOf OnToolClick
        Next

        MouseDownHandler = AddressOf OnDes_MouseDown
        MouseMoveHandler = AddressOf OnDes_MouseMove
    End Sub

    Public Sub OnToolClick(sender As Object)
        SelTool = CType(sender, TRadioButton).Tag
        Debug.WriteLine("ツール選択 {0}", SelTool)
    End Sub

    Public Sub OnDes_MouseDown(e As TMouseEvent)
        Dim tp As Type

        If SelTool Is Nothing Then
            Exit Sub
        End If

        tp = CType(SelTool, Type)
        DownPosDes = e.PosEv - AbsPosC()

        NewFigDes = CType(TSys.CreateObject(tp), TFig)
        NewFigDes.SetStartPos(DownPosDes)
        NewFigDes.SetEndPos(DownPosDes, DownPosDes)

        If TypeOf NewFigDes Is TWnd Then
            AddChildWnd(NewFigDes)
        Else
            AddChildFig(NewFigDes)
        End If
        TWnd.Capture = Me
    End Sub

    Public Sub OnDes_MouseMove(e As TMouseEvent)
        Dim pt1 As TPnt

        pt1 = e.PosEv - AbsPosC()
        If TWnd.Capture Is Me Then
            If NewFigDes IsNot Nothing Then
                NewFigDes.SetEndPos(DownPosDes, pt1)
                RemakeDrawFig()
            End If
        End If
    End Sub

End Class

'-------------------------------------------------------------------------------- TTextBox
Public Class TAbsTextBox
    Inherits TScrollView

    Public vTknPos As New TList(Of TTknPos)
    Public SelRef As TReference

    Public Sub MakeTextLineFig(dc As TDrawCmp, pos As TPnt, line As FLine)
        Dim x2 As Integer, brs As TColor, s As String, tpos As TTknPos, w As Integer

        x2 = 2
        ' for ???
        For Each txt In line.TextLine
            'x2 += txt.TabTxt
            'If txt.ObjFig Is Nothing Then
            '    Debug.WriteLine("draw {0} {1} {2}", txt.TextTxt, txt.TypeTxt, txt.TknTxt)
            'Else
            '    Debug.WriteLine("draw {0} {1} {2} {3}", txt.TextTxt, txt.TypeTxt, txt.TknTxt, txt.ObjFig.GetType())
            'End If

            If SelRef IsNot Nothing AndAlso TypeOf txt.ObjFig Is TReference AndAlso SelRef.VarRef Is CType(txt.ObjFig, TReference).VarRef Then
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

                w = TSrcEdit.StrLen(s, s.Length)
                tpos = New TTknPos(ETknPos.eTextTknPos, txt.ObjFig, pos + New TPnt(x2 * CharW, 0), New TPnt(w * CharW, CharH))
                'Debug.WriteLine("tpos {0} {1} {2}", tpos.PosPos, tpos.SizePos, s)
                vTknPos.Add(tpos)
                dc.AddDrawCmp(New TDrawString(s, FntTxt, brs, tpos.PosPos, txt.ObjFig))
                x2 += w
            End If
        Next
    End Sub
End Class

'-------------------------------------------------------------------------------- TTextBox
Public Class TTextBox
    Inherits TAbsTextBox
    Public Shared CursorPen As TColor = TColor.Blue
    Public vStr As New TList(Of String)
    Public DrawList As TDrawList
    Public mDrawCursorFig As TDraw

    ' 現在の行位置
    Public LineIdx As Integer
    ' 現在の文字位置
    Public CurPos As Integer
    ' 次頁、前頁、↓、↑キーで行位置を変え始めた時の行内の文字位置
    Public CurPosUD As Integer
    Public PageSize As Integer
    Public vUndo As New TList(Of TDiff)

    Public Sub New()
        InitFntBrs()

        SetTextLines(New String() {})
    End Sub

    Public Overrides Sub SetSize(sz As TPnt)
        MyBase.SetSize(sz)
        PageSize = CType(Math.Floor(ClientSize.YPnt / CharH), Integer)
    End Sub

    Public Sub SetTextLines(v1 As String())
        Dim i1 As Integer

        vUndo.Clear()
        vStr.Clear()
        vStr.AddRange((New TUtil(Of String)()).ArrayToList(v1))

        ' タブを空白に置き換える
        ' for Seq
        For i1 = 0 To vStr.Count - 1
            If vStr(i1).IndexOf(vbTab) <> -1 Then
                vStr(i1) = vStr(i1).Replace("" + vbTab, "    ")
            End If
        Next

        LineIdx = 0
        CurPos = 0
        CurPosUD = -1

        ViewPos = New TPnt(0, 0)
        ViewSize = New TPnt(80 * CharW, vStr.Count * CharH)
        RemakeDrawFig()
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)
        PageSize = CType(Math.Floor(ClientSize.YPnt / CharH), Integer)
    End Sub

    Public Overridable Function DrawTextLineFig(ln As Integer, pos As TPnt) As TDraw
        Return New TDrawString(vStr(ln), FntTxt, BrsTxt, pos)
    End Function

    Public Overrides Sub OnMouseDown(ev As TMouseEvent)
        Dim pt As TPnt, ln_idx As Integer, ln As Integer, k As Integer

        If vStr.Count = 0 Then
            Exit Sub
        End If

        ' 現在の行位置を保存する
        ln_idx = LineIdx

        ' クライアント領域の先頭からのマウスダウン位置を得る
        pt = ViewPos + (ev.PosEv - AbsPosC())

        ' 新しい行位置を得る        
        ln = CType(Math.Max(0, Math.Min(vStr.Count - 1, Math.Round(pt.YPnt / CharH))), Integer)
        If ln_idx <> ln Then
            ' 行位置が変わった場合

            ' 現在の行位置の一行の表示を更新する
            UpdateLine(False)
        End If
        LineIdx = ln

        ' 行位置とX座標から、行内の文字位置を返す
        k = GetCurPosByPos(LineIdx, pt.XPnt)
        CurPos = Math.Max(0, Math.Min(vStr(LineIdx).Length, k))

        ' 新しい行位置の一行の表示を更新する
        UpdateLine(True)
    End Sub

    Public Overrides Sub SetDrawFig()
        Dim ln As Integer, y As Double, st As Integer
        Dim cd As TDrawCmp

        cd = DrawBG()
        cd.VisDraw = Visible

        cd.AddDrawCmp(New TPushClip(New TPnt(), ClientSize))

        ' ビュー内の表示開始行
        st = CType(ViewPos.YPnt / CharH, Integer)

        DrawList = New TDrawList()
        DrawList.BaseList = st
        ' for Seq
        For ln = st To vStr.Count - 1
            y = ln * CharH - ViewPos.YPnt
            If ClientSize.YPnt < y Then
                ' クライアント領域を越えた場合

                Exit For
            End If
            DrawList.AddDrawCmp(DrawTextLineFig(ln, ClientPos + New TPnt(0, y)))
        Next
        cd.AddDrawCmp(DrawList)

        ' カーソルを描く
        mDrawCursorFig = DrawCursorFig(ClientPos)
        cd.AddDrawCmp(mDrawCursorFig)

        cd.AddDrawCmp(New TPopClip())

        DrawFig = cd
    End Sub

    ' 現在行のY座標
    Public Function LineY() As Double
        Return LineIdx * CharH - ViewPos.YPnt
    End Function

    Public Function GetCursorPos() As TPnt
        Return AbsPos() + New TPnt(StrLen(vStr(LineIdx), CurPos) * CharW, LineY())
    End Function

    ' カーソルを描く
    Public Function DrawCursorFig(pos As TPnt) As TDrawLine
        Dim x As Double, y As Double

        If vStr.Count = 0 Then
            x = 0
        Else
            x = StrLen(vStr(LineIdx), CurPos) * CharW
        End If

        y = LineY()
        Return New TDrawLine(CursorPen, pos + New TPnt(x, y), pos + New TPnt(x, y + CharH - 1))
    End Function

    Public Sub UpdateCursor(draw_cursor As Boolean)
        Dim i2 As Integer, dcmp As TDrawCmp

        dcmp = CType(DrawFig, TDrawCmp)
        i2 = dcmp.DrawCmp.IndexOf(mDrawCursorFig)
        Debug.Assert(i2 <> -1)
        If draw_cursor Then
            mDrawCursorFig = DrawCursorFig(ClientPos)
            dcmp.SetDrawCmp(i2, mDrawCursorFig)
        Else
            mDrawCursorFig.VisDraw = False
        End If
    End Sub

    ' 一行の表示を更新する
    Public Overridable Sub UpdateLine(draw_cursor As Boolean)
        Dim y As Double, i1 As Integer, pos As TPnt

        i1 = LineIdx - DrawList.BaseList
        If 0 <= i1 AndAlso i1 < DrawList.DrawCmp.Count Then

            y = LineY()
            DrawList.SetDrawCmp(i1, DrawTextLineFig(LineIdx, ClientPos + New TPnt(0, y)))
        Else
            Exit Sub
        End If

        UpdateCursor(draw_cursor)

        ' 再描画する
        RootWnd.GrFrm.Offset = DrawFig.AbsPosDraw()
        DrawFig.Draw(RootWnd.GrFrm)

        pos = AbsPosC() + New TPnt(0, y)
        RootWnd.GrFrm.AddRepaintRect(New TRect(pos, New TPnt(ClientSize.XPnt, CharH)))
    End Sub

    ' 指定した範囲を単一行のテキストで置換する
    Public Sub ChangeText(st_line As Integer, st_col As Integer, ed_line As Integer, ed_col As Integer, txt1 As String)
        Dim vtxt As String()

        vtxt = New String(0) {}
        vtxt(0) = txt1
        ChangeMultiText(st_line, st_col, ed_line, ed_col, vtxt)
    End Sub

    ' 指定した範囲を複数行のテキストで置換する
    Public Sub ChangeMultiText(st_line As Integer, st_col As Integer, ed_line As Integer, ed_col As Integer, new_txt As String())
        Dim dif1 As TDiff

        dif1 = New TDiff(st_line, st_col, new_txt)
        vUndo.Add(dif1)

        DiffEdit(st_line, st_col, ed_line, ed_col, new_txt, dif1)
    End Sub

    ' 指定した範囲を複数行のテキストで置換し、差分情報を保存する
    Public Overridable Sub DiffEdit(st_line As Integer, st_col As Integer, ed_line As Integer, ed_col As Integer, new_txt As String(), dif1 As TDiff)
        Dim st_txt As String, ed_txt As String, ln As Integer, new_cnt As Integer, old_cnt As Integer, i As Integer

        old_cnt = ed_line + 1 - st_line
        new_cnt = new_txt.Length

        st_txt = vStr(st_line)
        ed_txt = vStr(ed_line)

        If dif1 IsNot Nothing Then
            ' 差分情報を保存する場合

            dif1.OldTextDf = New String(ed_line - st_line) {}
            If st_line = ed_line Then
                ' 一行のみ置換する場合

                dif1.OldTextDf(0) = TSys.Substring(st_txt, st_col, ed_col)
            Else
                ' 複数行を置換する場合

                ' 最初の行を保存する
                dif1.OldTextDf(0) = st_txt.Substring(st_col)

                ' 最後の行を保存する
                dif1.OldTextDf(old_cnt - 1) = TSys.Substring(ed_txt, 0, ed_col)

                ' 最初と最後の行以外を保存する
                ' for Seq
                For i = 2 To old_cnt - 1
                    ' ln = old_cnt - 2 To 1
                    ln = old_cnt - i
                    dif1.OldTextDf(ln) = vStr(st_line + ln)
                Next
            End If
        End If

        ' 行が減った場合は削除する
        ' for ???
        For ln = 1 To old_cnt - new_cnt
            vStr.RemoveAt(st_line + 1)
        Next

        ' 行が増えた場合は追加する
        ' for ???
        For ln = 1 To new_cnt - old_cnt
            vStr.Insert(st_line + 1, "")
        Next

        If new_txt.Length = 1 Then
            ' 一行のみ挿入する場合

            vStr(st_line) = TSys.Substring(st_txt, 0, st_col) + new_txt(0) + ed_txt.Substring(ed_col)
        Else
            ' 複数行を挿入する場合

            ' 最初の行に代入する
            vStr(st_line) = TSys.Substring(st_txt, 0, st_col) + new_txt(0)

            ' 2行目以降に代入する
            ' for Seq
            For ln = 1 To new_cnt - 1
                If ln < new_cnt - 1 Then
                    ' 最後の行でない場合

                    vStr(st_line + ln) = new_txt(ln)
                Else
                    ' 最後の行の場合

                    vStr(st_line + ln) = new_txt(ln) + ed_txt.Substring(ed_col)
                End If
            Next
        End If
    End Sub

    ' 差分情報を使いテキストを元に戻す
    Public Overridable Sub Undo(dif1 As TDiff)
        ' 指定した範囲を複数行のテキストで置換する
        DiffEdit(dif1.StartLine, dif1.StartCol, dif1.EndLine, dif1.EndCol, dif1.OldTextDf, Nothing)

        LineIdx = dif1.StartLine
        CurPos = dif1.StartCol
        If dif1.OldTextDf.Length = 1 AndAlso dif1.NewTextDf.Length = 1 Then
            ' 一行のみ変化した場合

            ' 一行の表示を更新する
            UpdateLine(True)
        Else
            ' 複数行が変化した場合

            ' テキストボックス全体を再描画する
            RemakeDrawFig()
        End If
    End Sub


    Public Overrides Sub OnKeyDown(ev As TKeyEvent)
        Dim s As String
        Dim old_line As Integer, old_pos As Integer
        Dim vtxt As String(), ln_sv As Integer
        Dim dif1 As TDiff

        s = vStr(LineIdx)
        Debug.WriteLine("key:{0:X} shift:{1} control:{2} alt:{3} repeat:{4}", ev.KeyCode, ev.Shift, ev.Control, ev.Alt, ev.Repeat)

        old_line = LineIdx
        old_pos = CurPos

        If ev.Control Then
            If ev.KeyCode = AscW("Z"c) AndAlso vUndo.Count <> 0 Then

                dif1 = vUndo(vUndo.Count - 1)
                vUndo.RemoveAt(vUndo.Count - 1)
                Undo(dif1)
            End If
            Exit Sub
        End If
        Select Case ev.KeyCode
            Case EKeyCode.PageUp, EKeyCode.PageDown, EKeyCode.Up, EKeyCode.Down
                If CurPosUD = -1 Then
                    ' ' 次頁、前頁、↓、↑キーで行位置を変え始めた場合

                    CurPosUD = CurPos
                End If
            Case Else
                CurPosUD = -1
        End Select

        Select Case ev.KeyCode
            Case EKeyCode.PageUp
                ViewPos.YPnt = Math.Max(ViewPos.YPnt - PageSize * CharH, 0)
                LineIdx = Math.Max(LineIdx - PageSize, 0)
                CurPos = Math.Min(CurPosUD, vStr(LineIdx).Length)
                RemakeDrawFig()

            Case EKeyCode.PageDown
                ViewPos.YPnt = Math.Max(0, Math.Min(ViewPos.YPnt + PageSize * CharH, ViewSize.YPnt - PageSize * CharH))
                LineIdx = Math.Min(LineIdx + PageSize, vStr.Count - 1)
                CurPos = Math.Min(CurPosUD, vStr(LineIdx).Length)
                RemakeDrawFig()

            Case EKeyCode.EndKey
                CurPos = vStr(LineIdx).Count
                UpdateLine(True)

            Case EKeyCode.Home
                CurPos = 0
                UpdateLine(True)

            Case EKeyCode.Left
                If 0 < CurPos Then
                    CurPos = CurPos - 1
                    UpdateLine(True)
                ElseIf 0 < LineIdx Then
                    UpdateLine(False)
                    LineIdx = LineIdx - 1
                    CurPos = vStr(LineIdx).Count
                    UpdateLine(True)
                End If

            Case EKeyCode.Right
                If CurPos + 1 <= s.Count Then
                    CurPos = CurPos + 1
                    UpdateLine(True)
                ElseIf LineIdx + 1 < vStr.Count Then
                    UpdateLine(False)
                    LineIdx = LineIdx + 1
                    CurPos = 0
                    UpdateLine(True)
                End If

            Case EKeyCode.Up
                If 0 < LineIdx Then
                    UpdateLine(False)
                    LineIdx = LineIdx - 1
                    CurPos = Math.Min(CurPosUD, vStr(LineIdx).Count)
                    If LineY() < 0 Then
                        ViewPos.YPnt = ViewPos.YPnt - CharH
                        RemakeDrawFig()
                    Else
                        UpdateLine(True)
                    End If
                End If

            Case EKeyCode.Down
                If LineIdx + 1 < vStr.Count Then
                    UpdateLine(False)
                    LineIdx = LineIdx + 1
                    CurPos = Math.Min(CurPosUD, vStr(LineIdx).Count)
                    If PageSize * CharH <= LineY() Then
                        ViewPos.YPnt = ViewPos.YPnt + CharH
                        RemakeDrawFig()
                    Else
                        UpdateLine(True)
                    End If
                End If

            Case EKeyCode.Back
                If 0 < CurPos Then
                    ChangeText(LineIdx, CurPos - 1, LineIdx, CurPos, "")
                    CurPos = CurPos - 1
                    UpdateLine(True)
                ElseIf 0 < LineIdx Then
                    ' 前の行と連結する

                    CurPos = vStr(LineIdx - 1).Length
                    ChangeText(LineIdx - 1, CurPos, LineIdx, 0, "")
                    LineIdx = LineIdx - 1
                    RemakeDrawFig()

                End If

            Case EKeyCode.Delete
                If CurPos < s.Count Then
                    ChangeText(LineIdx, CurPos, LineIdx, CurPos + 1, "")
                    UpdateLine(True)
                ElseIf LineIdx + 1 < vStr.Count Then
                    ' 次の行と連結する

                    ChangeText(LineIdx, CurPos, LineIdx + 1, 0, "")
                    RemakeDrawFig()
                End If

            Case EKeyCode.Enter

                ln_sv = LineIdx
                vtxt = New String(1) {}
                vtxt(0) = ""
                vtxt(1) = ""
                ChangeMultiText(LineIdx, CurPos, LineIdx, CurPos, vtxt)
                LineIdx = ln_sv + 1
                CurPos = 0
                RemakeDrawFig()

            Case EKeyCode.Tab
        End Select
    End Sub

    Public Sub ReDo(dif1 As TDiff)

    End Sub

    Public Overrides Sub OnChar(ch As Char)

        CurPosUD = -1
        If AscW(ch) < 32 Then
            ' コントロールキーの場合

            Debug.WriteLine("char:{0:X} {1}", AscW(ch), ch)
            Exit Sub
        End If

        ' 一文字を挿入する
        ChangeText(LineIdx, CurPos, LineIdx, CurPos, "" + ch)
        CurPos = CurPos + 1
        UpdateLine(True)
    End Sub

    ' 指定した文字列と文字位置の桁幅を返す
    Public Shared Function StrLen(s As String, k As Integer) As Integer
        Dim w As Integer, i As Integer

        w = 0
        ' for ???
        For i = 0 To k - 1
            w += IIf(256 < AscW(s(i)), 2, 1)
        Next

        Return w
    End Function

    ' 行位置とX座標から、行内の文字位置を返す
    Public Function GetCurPosByPos(ln As Integer, x1 As Double) As Integer
        Dim x2 As Double, i As Integer, s1 As String

        If vStr.Count <= ln Then
            ' 行が行数を超える場合

            Return 0
        End If

        s1 = vStr(ln)
        ' 最初の文字の中間のX位置
        x2 = CharW / 2
        ' for ???
        For i = 0 To s1.Length - 1
            If x1 < x2 Then
                Return i
            End If

            x2 += IIf(256 < AscW(s1(i)), 2, 1) * CharW
        Next

        Return s1.Length
    End Function
End Class

'-------------------------------------------------------------------------------- TSrcEditAbs
Public Class TSrcEditAbs
    Inherits TTextBox
    Public SrcEdit As TSourceFile
    Public Shared BrsSrc As TColor()

    Public Sub New()
        MyBase.New()
        Dim cnt As Integer, i As Integer, black_brs As TColor = TColor.Black, blue_brs As TColor = TColor.Blue, green_brs As TColor = TColor.Green
        Dim red_brs As TColor = TColor.Red

        cnt = CType(EToken.eMAX_ETkn, Integer)

        If BrsSrc IsNot Nothing Then
            Exit Sub
        End If
        BrsSrc = New TColor(cnt) {}
        ' for Let
        For i = 0 To cnt - 1
            BrsSrc(i) = blue_brs
        Next

        BrsSrc(EToken.eId) = black_brs
        BrsSrc(EToken.eInt) = black_brs

        BrsSrc(EToken.eDot) = black_brs
        BrsSrc(EToken.eLP) = black_brs
        BrsSrc(EToken.eRP) = black_brs
        BrsSrc(EToken.eLB) = black_brs
        BrsSrc(EToken.eRB) = black_brs
        BrsSrc(EToken.eLC) = black_brs
        BrsSrc(EToken.eRC) = black_brs
        BrsSrc(EToken.eComma) = black_brs
        BrsSrc(EToken.eMMB) = black_brs

        BrsSrc(EToken.eADD) = black_brs
        BrsSrc(EToken.eMns) = black_brs
        BrsSrc(EToken.eMUL) = black_brs
        BrsSrc(EToken.eDIV) = black_brs
        BrsSrc(EToken.eMOD) = black_brs
        BrsSrc(EToken.eQUE) = black_brs

        BrsSrc(EToken.eEq) = black_brs
        BrsSrc(EToken.eNE) = black_brs
        BrsSrc(EToken.eASN) = black_brs
        BrsSrc(EToken.eLT) = black_brs
        BrsSrc(EToken.eGT) = black_brs
        BrsSrc(EToken.eLE) = black_brs
        BrsSrc(EToken.eGE) = black_brs
        BrsSrc(EToken.eADDEQ) = black_brs
        BrsSrc(EToken.eSUBEQ) = black_brs
        BrsSrc(EToken.eMULEQ) = black_brs
        BrsSrc(EToken.eDIVEQ) = black_brs
        BrsSrc(EToken.eMODEQ) = black_brs

        BrsSrc(EToken.eString) = red_brs
        BrsSrc(EToken.eChar) = red_brs

        BrsSrc(EToken.eLineComment) = green_brs
        BrsSrc(EToken.eBlockComment) = green_brs

        '		BrsSrc(EToken) = black_brs
    End Sub
End Class

'-------------------------------------------------------------------------------- TSrcEdit
Public Class TSrcEdit
    Inherits TSrcEditAbs

    Public Sub New()
        MyBase.New()
    End Sub

    Public Overrides Function DrawTextLineFig(ln As Integer, pos As TPnt) As TDraw
        Dim x2 As Integer, stmt1 As TStatement
        Dim dc As TDrawCmp

        dc = New TDrawCmp()
        If SrcEdit IsNot Nothing AndAlso SrcEdit.FigSrc IsNot Nothing AndAlso ln < SrcEdit.FigSrc.vLineFig.Count Then
            MakeTextLineFig(dc, pos, SrcEdit.FigSrc.vLineFig(ln))
        ElseIf SrcEdit IsNot Nothing AndAlso SrcEdit.StmtSrc IsNot Nothing AndAlso ln < SrcEdit.StmtSrc.Count Then
            stmt1 = SrcEdit.StmtSrc(ln)
            If stmt1 Is Nothing Then
                dc.AddDrawCmp(New TDrawString(vStr(ln), FntTxt, BrsSrc(EToken.eLineComment), pos))

            ElseIf stmt1.vTknStmt Is Nothing Then
                dc.AddDrawCmp(New TDrawString(vStr(ln), FntTxt, BrsSrc(EToken.eLineComment), pos))
            Else
                x2 = 0

                ' for ???
                For Each tkn In stmt1.vTknStmt
                    If tkn.SpcTkn <> 0 Then
                        x2 += tkn.SpcTkn * CharW
                    End If
                    Select Case tkn.TypeTkn
                        Case EToken.eString
                            dc.AddDrawCmp(New TDrawString("""" + tkn.StrTkn + """", FntTxt, BrsSrc(tkn.TypeTkn), pos + New TPnt(x2, 0)))
                            x2 += 2 * CharW
                        Case EToken.eChar
                            dc.AddDrawCmp(New TDrawString("""" + tkn.StrTkn + """c", FntTxt, BrsSrc(tkn.TypeTkn), pos + New TPnt(x2, 0)))
                            x2 += 3 * CharW
                        Case Else
                            dc.AddDrawCmp(New TDrawString(tkn.StrTkn, FntTxt, BrsSrc(tkn.TypeTkn), pos + New TPnt(x2, 0)))
                    End Select
                    x2 += StrLen(tkn.StrTkn, tkn.StrTkn.Length) * CharW
                Next
            End If
        Else
            dc.AddDrawCmp(New TDrawString(vStr(ln), FntTxt, BrsTxt, pos))
        End If

        Return dc
    End Function

    Public Overrides Sub DiffEdit(st_line As Integer, st_col As Integer, old_ed_line As Integer, old_ed_col As Integer, new_txt As String(), dif1 As TDiff)
        Dim ln As Integer, new_cnt As Integer, old_cnt As Integer
        Dim stmt1 As TStatement, par1 As TBasicParser

        MyBase.DiffEdit(st_line, st_col, old_ed_line, old_ed_col, new_txt, dif1)

        If SrcEdit Is Nothing Then
            ' ソーステキストがない場合

            Exit Sub
        End If

        old_cnt = old_ed_line + 1 - st_line
        new_cnt = new_txt.Length

        ' 行が減った場合は削除する
        ' for Del
        For ln = 1 To old_cnt - new_cnt
            SrcEdit.StmtSrc.RemoveAt(st_line + 1)
        Next

        ' 行が増えた場合は追加する
        ' for ???
        For ln = 1 To new_cnt - old_cnt
            SrcEdit.StmtSrc.Insert(st_line + 1, Nothing)
        Next

        ' パーサーを得る
        par1 = TIDE.theIDE.PrjIDE.ParsePrj

        ' すべての変更行に対し
        ' for ???
        For ln = st_line To st_line + new_txt.Length - 1
            ' 字句解析をする
            par1.CurVTkn = par1.Lex(vStr(ln))
            stmt1 = Nothing
            If par1.CurVTkn.Count <> 0 Then
                ' トークンがある場合

                ' 文解析をする
                stmt1 = par1.ReadStatement()
            End If

            ' 文の配列に代入する
            SrcEdit.StmtSrc(ln) = stmt1
        Next
    End Sub
End Class

'-------------------------------------------------------------------------------- TSrcBrowser
Public Class TSrcBrowser
    Inherits TAbsTextBox
    Public PageSize As Integer

    Public Sub New()
        MyBase.New()
        InitFntBrs()
    End Sub

    Public Overrides Sub SetSize(sz As TPnt)
        MyBase.SetSize(sz)
        PageSize = CType(Math.Floor(ClientSize.YPnt / CharH), Integer)
    End Sub

    Public Overrides Sub SetHeight(h As Double)
        MyBase.SetHeight(h)
        PageSize = CType(Math.Floor(ClientSize.YPnt / CharH), Integer)
    End Sub


    Public Overrides Sub SetDrawFig()
        Dim ln As Integer, y As Double, st As Integer
        Dim cd As TDrawCmp, dl As TDrawList, line As FLine, txt1 As FText, ln2 As Integer, obj As Object, exp As EExpand, exp_sv As EExpand, prv_obj As Object
        Dim cur_obj As Object
        Dim vexp As New Stack(Of EExpand)
        Dim vobj As New Stack(Of Object)

        cd = DrawBG()
        cd.VisDraw = Visible

        cd.AddDrawCmp(New TPushClip(New TPnt(), ClientSize))

        ' ビュー内の表示開始行
        st = CType(ViewPos.YPnt / CharH, Integer)

        vTknPos.Clear()
        dl = New TDrawList()
        dl.BaseList = st

        txt1 = Nothing
        prv_obj = Nothing
        cur_obj = Nothing

        ln2 = 0
        ' for ???
        For Each src In TProject.Prj.SrcPrj
            If src.FigSrc IsNot Nothing Then
                obj = Nothing
                exp = EExpand.eNone

                ' for ???
                For ln = 0 To src.FigSrc.vLineFig.Count - 1
                    y = ln2 * CharH - ViewPos.YPnt
                    If ClientSize.YPnt < y Then
                        ' クライアント領域を越えた場合

                        Exit For
                    End If

                    ' 現在の状態を保存する
                    exp_sv = exp

                    line = src.FigSrc.vLineFig(ln)
                    If line.TextLine.Count <> 0 AndAlso line.TextLine(0).TypeFig <> EFigType.eComFig AndAlso line.TextLine(0).ObjFig IsNot Nothing Then
                        ' 空行やコメントでなくオブジェクトが設定されている場合

                        txt1 = line.TextLine(0)
                        If txt1.TknTxt = EToken.eEnd Then
                            ' 終了の場合

                            If cur_obj Is txt1.ObjFig Then
                                ' 現在のオブジェクトの終了の場合

                                exp = vexp.Pop()
                                cur_obj = vobj.Pop()
                            End If
                        Else
                            ' 終了でない場合

                            If txt1.ObjFig IsNot prv_obj Then
                                ' オブジェクトが変わった場合

                                If TypeOf txt1.ObjFig Is TClass OrElse (TypeOf txt1.ObjFig Is TFunction AndAlso CType(txt1.ObjFig, TFunction).BlcFnc IsNot Nothing) Then
                                    ' クラスかメソッドの開始の場合

                                    vexp.Push(exp)
                                    vobj.Push(cur_obj)

                                    If exp <> EExpand.eCollapse Then
                                        ' 親は折りたたんでない場合

                                        If line.ExpLine = EExpand.eNone OrElse line.ExpLine = EExpand.eCollapse Then
                                            ' 未設定か折りたたむ場合

                                            ' 折りたたむ
                                            exp = EExpand.eCollapse
                                            line.ExpLine = EExpand.eCollapse
                                        End If
                                    End If

                                    ' 現在のオブジェクトを保存する
                                    cur_obj = txt1.ObjFig
                                End If
                            End If
                        End If

                        ' 直前行のオブジェクトを保存する
                        prv_obj = txt1.ObjFig
                    End If

                    If exp_sv <> EExpand.eCollapse Then
                        ' 折りたたまない場合

                        If 0 <= y Then

                            dl.AddDrawCmp(DrawTextLineFig(src, ln, ClientPos + New TPnt(0, y)))
                        End If

                        ln2 += 1
                    End If
                Next
            End If
        Next

        cd.AddDrawCmp(dl)

        cd.AddDrawCmp(New TPopClip())

        DrawFig = cd
    End Sub

    Public Function DrawTextLineFig(src As TSourceFile, ln As Integer, pos As TPnt) As TDraw
        Dim dc As TDrawCmp, line As FLine, brs As TColor, tpos As TTknPos

        dc = New TDrawCmp()
        Debug.Assert(ln < src.FigSrc.vLineFig.Count)
        line = src.FigSrc.vLineFig(ln)

        If line.ExpLine = EExpand.eCollapse OrElse line.ExpLine = EExpand.eExpand Then
            tpos = New TTknPos(ETknPos.eExpandTknPos, src.FigSrc.vLineFig(ln), pos, New TPnt(1 * CharW, CharH))
            vTknPos.Add(tpos)

            brs = TColor.Blue
            If line.ExpLine = EExpand.eCollapse Then
                dc.AddDrawCmp(New TDrawString("+", FntTxt, brs, tpos.PosPos))
            Else
                dc.AddDrawCmp(New TDrawString("-", FntTxt, brs, tpos.PosPos))
            End If
        End If

        MakeTextLineFig(dc, pos, line)

        Return dc
    End Function

    Public Overrides Sub OnKeyDown(ev As TKeyEvent)
        Select Case ev.KeyCode
            Case EKeyCode.PageUp
                ViewPos.YPnt = Math.Max(ViewPos.YPnt - PageSize * CharH, 0)
                RemakeDrawFig()

            Case EKeyCode.PageDown
                ViewPos.YPnt = Math.Max(0, Math.Min(ViewPos.YPnt + PageSize * CharH, ViewSize.YPnt - PageSize * CharH))
                RemakeDrawFig()

            Case EKeyCode.Up
                ViewPos.YPnt = Math.Max(ViewPos.YPnt - CharH, 0)
                RemakeDrawFig()

            Case EKeyCode.Down
                ViewPos.YPnt = Math.Max(0, Math.Min(ViewPos.YPnt + CharH, ViewSize.YPnt - PageSize * CharH))
                RemakeDrawFig()
        End Select
    End Sub

    Public Sub SetSrcBrw()
        Dim cnt As Integer

        ViewPos = New TPnt(0, 0)

        ' 全ソースの行数を数える
        cnt = 0
        ' for ???
        For Each src In TProject.Prj.SrcPrj
            If src.FigSrc IsNot Nothing Then
                cnt += src.FigSrc.vLineFig.Count
            End If
        Next

        ViewSize = New TPnt(80 * CharW, cnt * CharH)
    End Sub

    Public Overrides Sub OnMouseDown(ev As TMouseEvent)
        Dim pt As TPnt, var1 As TVariable, line As FLine
        Dim stmt1 As TStatement

        ' クライアント領域の先頭からのマウスダウン位置を得る
        pt = ev.PosEv - AbsPosC()

        ' for ???
        For Each tpos In vTknPos
            If tpos.PosPos.XPnt <= pt.XPnt AndAlso pt.XPnt < tpos.PosPos.XPnt + tpos.SizePos.XPnt Then
                If tpos.PosPos.YPnt <= pt.YPnt AndAlso pt.YPnt < tpos.PosPos.YPnt + tpos.SizePos.YPnt Then
                    Select Case tpos.TypePos
                        Case ETknPos.eTextTknPos
                            If tpos.ObjPos IsNot Nothing Then
                                If TypeOf tpos.ObjPos Is TReference Then
                                    SelRef = CType(tpos.ObjPos, TReference)
                                    Debug.WriteLine("Ref {0}", SelRef.NameRef)
                                    TIDE.SrcBrwsrMenu.SetPos(ev.PosEv)
                                    RootWnd.ActivePopup = TIDE.SrcBrwsrMenu
                                    RootWnd.RemakeDrawFig()

                                    ' for ???
                                    For Each src In TProject.Prj.SrcPrj
                                        If src.FigSrc IsNot Nothing Then
                                            ' for ???
                                            For Each line2 In src.FigSrc.vLineFig
                                                If line2.ObjFig IsNot Nothing AndAlso TypeOf line2.ObjFig Is TStatement Then

                                                    stmt1 = CType(line2.ObjFig, TStatement)
                                                    ' for Find
                                                    For Each ref1 In stmt1.RefStmt
                                                        If ref1.VarRef Is SelRef.VarRef Then
                                                            Debug.WriteLine("参照 {0}", ref1.NameRef)
                                                            Exit For
                                                        End If
                                                    Next
                                                End If
                                            Next
                                        End If
                                    Next

                                    RemakeDrawFig()
                                ElseIf TypeOf tpos.ObjPos Is TVariable Then
                                    var1 = CType(tpos.ObjPos, TVariable)
                                    Debug.WriteLine("Var {0} {1}", var1.NameVar, var1)
                                Else
                                    Debug.WriteLine("brw {0}", tpos.ObjPos)
                                End If
                            End If
                        Case ETknPos.eExpandTknPos
                            line = CType(tpos.ObjPos, FLine)
                            If line.ExpLine = EExpand.eCollapse Then
                                line.ExpLine = EExpand.eExpand
                            Else
                                line.ExpLine = EExpand.eCollapse
                            End If

                            RemakeDrawFig()
                    End Select
                End If
            End If
        Next
    End Sub
End Class



Public Class TComboBox
End Class

Public Class TCheckBox
End Class

Public Class TMenu
End Class

Public Class TGrid
End Class

Public Class TPictureBox
End Class

Public Class TTrackBar
End Class

Public Class TGroupBox
End Class

Public Class TLinkLabel
End Class

Public Class TTableLayoutPanel
End Class

Public Class TEllipse
End Class

'-------------------------------------------------------------------------------- TTknPos
Public Class TTknPos
    Public TypePos As ETknPos
    Public ObjPos As Object
    Public PosPos As TPnt
    Public SizePos As TPnt

    Public Sub New(type As ETknPos, obj As Object, pos As TPnt, sz As TPnt)
        TypePos = type
        ObjPos = obj
        PosPos = pos
        SizePos = sz
    End Sub
End Class

'-------------------------------------------------------------------------------- EKeyCode
Public Class EKeyCode
    Public Const PageUp As Integer = &H21
    Public Const PageDown As Integer = &H22
    Public Const EndKey As Integer = &H23
    Public Const Home As Integer = &H24
    Public Const Left As Integer = &H25
    Public Const Up As Integer = &H26
    Public Const Right As Integer = &H27
    Public Const Down As Integer = &H28
    Public Const Delete As Integer = &H2E
    Public Const Enter As Integer = &HD
    Public Const Back As Integer = &H8
    Public Const Tab As Integer = &H9
End Class

'-------------------------------------------------------------------------------- TEvent
Public Class TEvent
    Public TypeEv As EEvent
    Public FormEv As TForm

    Public Sub New()
    End Sub

    Public Sub New(tp As EEvent)
        TypeEv = tp
    End Sub
End Class

'-------------------------------------------------------------------------------- TFormEvent
Public Class TFormEvent
    Inherits TEvent
    Public SizeEv As TPnt

    Public Sub New(tp As EEvent, frm As TForm, pt As TPnt)
        MyBase.New(tp)
        FormEv = frm
        SizeEv = pt
    End Sub
End Class

'-------------------------------------------------------------------------------- TInputEvent
Public Class TInputEvent
    Inherits TEvent
    Public SrcEv As Object
    Public Shift As Boolean
    Public Alt As Boolean
    Public Control As Boolean
    Public PosEv As TPnt
    Public CharEv As Char

    Public Sub New()
    End Sub

    Public Sub New(src As Object)
        SrcEv = src
    End Sub
End Class

'-------------------------------------------------------------------------------- TMouseEvent
Public Class TMouseEvent
    Inherits TInputEvent
    Public MouseDownTime As DateTime

    Public Sub New()
    End Sub
End Class

'-------------------------------------------------------------------------------- TKeyEvent
Public Class TKeyEvent
    Inherits TInputEvent
    Public KeyCode As Integer
    Public Repeat As Boolean

    Public Sub New()
    End Sub

    Public Sub New(key_code As Integer, shift1 As Boolean, alt1 As Boolean, ctr1 As Boolean)
        KeyCode = key_code
        Shift = shift1
        Alt = alt1
        Control = ctr1
    End Sub
End Class

'-------------------------------------------------------------------------------- TScrollEventArgs
Public Class TScrollEventArgs
    Inherits TInputEvent

    Public Sub New()
    End Sub

    Public Sub New(src As Object)
        MyBase.New(src)
    End Sub
End Class

Public Interface ISelectedIndexChangedListener
    Sub OnSelectedIndexChanged(wnd As TWnd, old_idx As Integer, new_idx As Integer)
End Interface

' -------------------------------------------------------------------------------- TApplication
Public Class TApplication
    Public Shared theApp As TApplication
    Public MainForm As TForm
    Public EventApp As New TList(Of TEvent)

    Public Sub EventHandler(ev As TEvent)
        Dim kev As TKeyEvent, mev As TMouseEvent, fev As TFormEvent

        Select Case ev.TypeEv
            Case EEvent.eMouseDown, EEvent.eMousePress, EEvent.eMouseMove, EEvent.eMouseUp
                mev = CType(ev, TMouseEvent)
                Select Case ev.TypeEv
                    Case EEvent.eMouseDown
                        ev.FormEv.OnMouseDown(mev)
                    Case EEvent.eMousePress
                        TWnd.Capture.MousePressHandler(mev)
                    Case EEvent.eMouseMove
                        ev.FormEv.OnMouseMove(mev)
                    Case EEvent.eMouseUp
                        ev.FormEv.OnMouseUp(mev)
                End Select
                UpdateDrawApp()

            Case EEvent.eOnChar, EEvent.eOnKeyDown
                kev = CType(ev, TKeyEvent)
                Select Case ev.TypeEv
                    Case EEvent.eOnChar
                        ev.FormEv.OnChar(CType(ev, TKeyEvent).CharEv)
                    Case EEvent.eOnKeyDown
                        ev.FormEv.OnKeyDown(kev)
                End Select
                UpdateDrawApp()

            Case EEvent.ePaint
                UpdateDrawApp()

            Case EEvent.eOnResize
                fev = CType(ev, TFormEvent)
                fev.FormEv.OnResize(fev.SizeEv)
                fev.FormEv.RemakeDrawFig()
                UpdateDrawApp()

            Case EEvent.eOnTimer
                OnTimer()
                UpdateDrawApp()
        End Select
    End Sub

    Public Sub LoadApp()
        InitApp()
        SetDrawFigApp()
        UpdateDrawApp()
    End Sub

    Public Overridable Sub InitApp()
    End Sub

    Public Overridable Sub OnTimer()
    End Sub

    Public Sub SetDrawFigApp()
        MainForm.SetDrawFig()
    End Sub

    Public Sub UpdateDrawApp()
        If MainForm.GrFrm IsNot Nothing Then
            MainForm.GrFrm.UpdateDraw(MainForm)
        End If
    End Sub
End Class
' テキストの差分情報
Public Class TDiff
    Public StartLine As Integer
    Public StartCol As Integer
    Public EndLine As Integer
    Public EndCol As Integer
    Public OldTextDf As String()
    Public NewTextDf As String()

    Public Sub New(st_line As Integer, st_col As Integer, new_text As String())
        Dim ed_txt As String

        ' 置換の開始行、開始桁と挿入するテキストを保存する
        StartLine = st_line
        StartCol = st_col
        NewTextDf = new_text.Clone()

        ' 置換後の終了行を保存する
        EndLine = StartLine + NewTextDf.Length - 1

        ed_txt = NewTextDf(NewTextDf.Length - 1)
        If NewTextDf.Length = 1 Then
            ' 一行のテキストで置換する場合

            ' 置換後の終了桁 = 開始桁 + 最終行の長さ
            EndCol = StartCol + ed_txt.Length
        Else
            ' 複数行のテキストで置換する場合

            ' 置換後の終了桁 = 最終行の長さ
            EndCol = ed_txt.Length
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- SGraph
Public Class SGraph
    Inherits TPanel

End Class

' -------------------------------------------------------------------------------- SNode
Public Class SNode
    Inherits TWnd
    Public Shared FntNode As New TFont(EFont.eGothic, 10)
    Public DatSNode As Object
    Public TextNode As String

    Public Sub InitNode()
        FntTxt = FntNode
        BrsTxt = TColor.Black
    End Sub

    Public Sub New()
        InitNode()
    End Sub

    Public Sub New(dat As Object)
        InitNode()
        DatSNode = dat
        TextNode = dat.ToString()

        SetAutoSize()
    End Sub

    Public Overrides Sub SetAutoSize()
        Dim sz As TPnt

        sz = TGraphics.MeasureText(TextNode, FntTxt)
        SetSize(sz.XPnt + 2 * (ClientPos.XPnt + 1), sz.YPnt + 2 * (ClientPos.YPnt + 1))
    End Sub

    Public Overrides Sub SetDrawFig()
        MyBase.SetDrawFig()
        DrawFig.VisDraw = Visible
        CType(DrawFig, TDrawCmp).AddDrawCmp(New TDrawString(TextNode, FntTxt, BrsTxt, New TPnt(1, 1)))
    End Sub
End Class
