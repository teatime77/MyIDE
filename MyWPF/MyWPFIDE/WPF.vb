Imports System.Windows.Media
Imports System.Text
Imports System.Diagnostics

' -------------------------------------------------------------------------------- TDraw
Public Class TDraw
    Public Shared IdCnt As Integer
    Public IdDraw As Integer
    Public UpDraw As TDrawCmp
    Public VisDraw As Boolean = True
    Public PosDraw As TPnt
    Public FEDraw As FrameworkElement

    Public Sub New()
        IdDraw = IdCnt
        IdCnt = IdCnt + 1
    End Sub

    Public Overridable Sub Draw(gr As TGraphics)
    End Sub

    Public Sub ClearDraw(gr As TGraphics)
        Dim dc As TDrawCmp

        If FEDraw IsNot Nothing Then
            gr.CanGr.Children.Remove(FEDraw)
        End If

        If TypeOf Me Is TDrawCmp Then
            dc = CType(Me, TDrawCmp)
            For Each d In dc.DrawCmp
                d.ClearDraw(gr)
            Next
        End If

    End Sub

    Public Overrides Function ToString() As String
        If UpDraw Is Nothing Then
            Return String.Format("{0} {1}", IdDraw, Me.GetType())
        Else
            Return String.Format("{0} {1} {2}", IdDraw, Me.GetType(), UpDraw.IdDraw)
        End If
    End Function

    Public Function AbsPosDraw() As TPnt
        Dim pt As TPnt, dr As TDraw

        pt = PosDraw
        dr = UpDraw
        Do While dr IsNot Nothing
            pt = pt + dr.PosDraw
            dr = dr.UpDraw
        Loop
        Return pt
    End Function
End Class

' -------------------------------------------------------------------------------- TDrawCmp
Public Class TDrawCmp
    Inherits TDraw
    Public DrawCmp As New List(Of TDraw)

    Public Sub AddDrawCmp(d As TDraw)
        DrawCmp.Add(d)
        d.UpDraw = Me
    End Sub

    Public Sub SetDrawCmp(idx As Integer, d As TDraw)
        DrawCmp(idx) = d
        d.UpDraw = Me
    End Sub

    Public Sub RepDrawCmp(old_draw As TDraw, new_draw As TDraw)
        Dim idx As Integer

        idx = DrawCmp.IndexOf(old_draw)
        Debug.Assert(idx <> -1)
        SetDrawCmp(idx, new_draw)
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        Dim pt As TPnt

        If Not VisDraw Then
            Exit Sub
        End If
        For Each dr In DrawCmp
            pt = gr.Offset
            gr.Offset = gr.Offset + dr.PosDraw

            dr.Draw(gr)

            gr.Offset = pt
        Next
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawList
Public Class TDrawList
    Inherits TDrawCmp
    Public BaseList As Integer
End Class

' -------------------------------------------------------------------------------- TDrawEllipse
Public Class TDrawEllipse
    Inherits TDraw
    Public ColEll As TColor
    Public PosEll As TPnt
    Public SizeEll As TPnt

    Public Sub New(col As TColor, x As Double, y As Double, width As Double, height As Double)
        ColEll = col
        PosEll = New TPnt(x, y)
        SizeEll = New TPnt(width, height)
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        Dim el As Ellipse

        If Not VisDraw Then
            Exit Sub
        End If

        If FEDraw Is Nothing Then
            el = New Ellipse()
            FEDraw = el
            gr.CanGr.Children.Add(FEDraw)
        Else
            el = CType(FEDraw, Ellipse)
        End If

        Canvas.SetLeft(FEDraw, gr.Offset.XPnt + PosEll.XPnt)
        Canvas.SetTop(FEDraw, gr.Offset.YPnt + PosEll.YPnt)
        If 0 < SizeEll.XPnt AndAlso 0 < SizeEll.YPnt Then
            FEDraw.Width = SizeEll.XPnt
            FEDraw.Height = SizeEll.YPnt
        End If
        el.Stroke = New SolidColorBrush(ColEll.ColCol)
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawLine
Public Class TDrawLine
    Inherits TDraw
    Public ColLine As TColor
    Public StartLine As TPnt
    Public EndLine As TPnt

    Public Sub New(col As TColor, x1 As Double, y1 As Double, x2 As Double, y2 As Double)
        ColLine = col
        StartLine = New TPnt(x1, y1)
        EndLine = New TPnt(x2, y2)
    End Sub

    Public Sub New(col As TColor, st As TPnt, ed As TPnt)
        ColLine = col
        StartLine = st
        EndLine = ed
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        Dim ln As Line

        If Not VisDraw Then
            Exit Sub
        End If

        If FEDraw Is Nothing Then
            ln = New Line()
            FEDraw = ln
            gr.CanGr.Children.Add(FEDraw)
        Else
            ln = CType(FEDraw, Line)
        End If

        ln.X1 = gr.Offset.XPnt + StartLine.XPnt
        ln.Y1 = gr.Offset.YPnt + StartLine.YPnt
        ln.X2 = gr.Offset.XPnt + EndLine.XPnt
        ln.Y2 = gr.Offset.YPnt + EndLine.YPnt
        ln.Stroke = New SolidColorBrush(ColLine.ColCol)
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawSpline
Public Class TDrawSpline
    Inherits TDraw
    Public ColDSpl As TColor
    Public PntDSpl As TList(Of TPnt)

    Public Sub New(col As TColor, vpnt As TList(Of TPnt))
        ColDSpl = col
        PntDSpl = New TList(Of TPnt)()
        For Each p In vpnt
            PntDSpl.Add(p)
        Next
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        If Not VisDraw Then
            Exit Sub
        End If
        '        gr.DrawSpline(ColDSpl, PntDSpl)
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawFillRectangle
Public Class TDrawFillRectangle
    Inherits TDraw
    Public ColRect As TColor
    Public PosRect As TPnt
    Public SizeRect As TPnt
    Public RectRect As Rectangle

    Public Overrides Sub Draw(gr As TGraphics)
        If FEDraw Is Nothing Then
            RectRect = New Rectangle()
            FEDraw = RectRect
            gr.CanGr.Children.Add(FEDraw)
        End If

        Canvas.SetLeft(FEDraw, gr.Offset.XPnt + PosRect.XPnt)
        Canvas.SetTop(FEDraw, gr.Offset.YPnt + PosRect.YPnt)
        If 0 < SizeRect.XPnt AndAlso 0 < SizeRect.YPnt Then
            FEDraw.Width = SizeRect.XPnt
            FEDraw.Height = SizeRect.YPnt
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawRectangle
Public Class TDrawRectangle
    Inherits TDrawFillRectangle
    Public BdrWRect As Double

    Public Sub New(col As TColor, pen_width As Double, x As Double, y As Double, width As Double, height As Double)
        ColRect = col
        BdrWRect = pen_width
        PosRect = New TPnt(x, y)
        SizeRect = New TPnt(width, height)
        If width < 0 Then
            Debug.WriteLine("")
        End If
        'Debug.WriteLine("rect {0} {1}", width, height)
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        If Not VisDraw Then
            Exit Sub
        End If

        MyBase.Draw(gr)
        RectRect.Stroke = New SolidColorBrush(ColRect.ColCol)
    End Sub
End Class

' -------------------------------------------------------------------------------- TFillRectangle
Public Class TFillRectangle
    Inherits TDrawFillRectangle

    Public Sub New(col As TColor, x As Double, y As Double, width As Double, height As Double)
        ColRect = col
        PosRect = New TPnt(x, y)
        SizeRect = New TPnt(width, height)
    End Sub

    Public Sub New(col As TColor, pos As TPnt, sz As TPnt)
        ColRect = col
        PosRect = pos
        SizeRect = sz
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        If Not VisDraw Then
            Exit Sub
        End If

        MyBase.Draw(gr)
        RectRect.Fill = New SolidColorBrush(ColRect.ColCol)
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawString
Public Class TDrawString
    Inherits TDraw
    Public TxtStr As String
    Public FntStr As TFont
    Public ColStr As TColor
    Public PosStr As TPnt
    Public ObjStr As Object
    Public TxtBlc As TextBlock

    Public Sub New(s As String, font As TFont, col As TColor, pos As TPnt)
        TxtStr = s
        FntStr = font
        ColStr = col
        PosStr = pos
    End Sub

    Public Sub New(s As String, font As TFont, col As TColor, pos As TPnt, obj As Object)
        TxtStr = s
        FntStr = font
        ColStr = col
        PosStr = pos
        ObjStr = obj
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        If Not VisDraw Then
            Exit Sub
        End If

        If FEDraw Is Nothing Then
            TxtBlc = New TextBlock()
            FEDraw = TxtBlc
            gr.CanGr.Children.Add(FEDraw)
        End If

        Canvas.SetLeft(FEDraw, gr.Offset.XPnt + PosStr.XPnt)
        Canvas.SetTop(FEDraw, gr.Offset.YPnt + PosStr.YPnt)
        TxtBlc.Text = TxtStr
        TxtBlc.Tag = ObjStr
        TxtBlc.Foreground = New SolidColorBrush(ColStr.ColCol)
    End Sub

    Public Overrides Function ToString() As String
        If UpDraw Is Nothing Then
            Return String.Format("{0} {1} {2}", IdCnt, Me.GetType(), TxtStr)
        Else
            Return String.Format("{0} {1} {2} {3}", IdCnt, Me.GetType(), UpDraw.IdDraw, TxtStr)
        End If
    End Function
End Class

' -------------------------------------------------------------------------------- TPushClip
Public Class TPushClip
    Inherits TDraw
    Public PosClip As TPnt
    Public SizeClip As TPnt

    Public Sub New(pos As TPnt, sz As TPnt)
        PosClip = pos
        SizeClip = sz
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
    End Sub
End Class

' -------------------------------------------------------------------------------- TPopClip
Public Class TPopClip
    Inherits TDraw

    Public Overrides Sub Draw(gr As TGraphics)
    End Sub
End Class

' -------------------------------------------------------------------------------- TGraphics
Public Class TGraphics
    Public Shared Handle As IntPtr
    Public Offset As TPnt
    Dim RectGr As New List(Of TRect)
    Public mClip As TRect
    Public Shared FontDic As Dictionary(Of Integer, FontFamily) = New Dictionary(Of Integer, FontFamily)()
    Public RemakeDrawFigList As List(Of TFig) = New List(Of TFig)()
    Public CanGr As Canvas

    Public Shared Sub InitGraphics()
    End Sub

    Sub New(old_gr As TGraphics, frm As TForm)
        If old_gr IsNot Nothing Then
            CanGr = old_gr.CanGr
        End If
    End Sub

    Public Sub DisposeGraphics()
    End Sub


    Public Sub AddRepaintRect(rc As TRect)
        RectGr.Add(rc)
    End Sub

    Public Sub AddRepaint(wnd As TWnd)
        Dim pt As TPnt

        pt = wnd.AbsPos()
        AddRepaintRect(New TRect(pt, wnd.SizeFig))
    End Sub

    Public Shared Function MeasureText(s As String, fnt As TFont) As TPnt
        Dim w As Integer, i As Integer

        w = 0
        For i = 0 To s.Length - 1
            w += IIf(256 < AscW(s(i)), 2, 1)
        Next

        Return New TPnt(w * fnt.SizeFont * 7.0 / 10.0, fnt.SizeFont * 14.0 / 10.0)
    End Function

    Public Shared Sub SetCursor(cur As ECursor)
        Select Case cur
            Case ECursor.eARROW
            Case ECursor.eSIZEWE
            Case Else
                Debug.Assert(False)
        End Select

    End Sub

    Public Shared Sub DmpDraw(tab As String, sw As TStringWriter, dr As TDraw)
        Dim dcmp As TDrawCmp, tab2 As String

        sw.WriteLine(tab + dr.ToString())
        If TypeOf dr Is TDrawCmp Then
            dcmp = CType(dr, TDrawCmp)
            tab2 = tab + "    "
            For Each d In dcmp.DrawCmp
                DmpDraw(tab2, sw, d)
            Next
        End If
    End Sub

    Public Sub UpdateDraw(frm As TForm)
        Dim old_draw As TDraw
        'Dim sw As New TStringWriter

        For Each fig In RemakeDrawFigList
            If fig.Visible AndAlso fig.DrawFig IsNot Nothing Then

                ' DrawFigを再作成する
                old_draw = fig.DrawFig
                fig.SetDrawFig()
                fig.DrawFig.PosDraw = old_draw.PosDraw

                If old_draw.UpDraw IsNot Nothing Then
                    ' 親のDrawがある場合

                    old_draw.UpDraw.RepDrawCmp(old_draw, fig.DrawFig)
                End If
                old_draw.ClearDraw(Me)

                ' 再描画する
                Offset = fig.DrawFig.AbsPosDraw()
                fig.DrawFig.Draw(Me)
            End If
        Next
        RemakeDrawFigList.Clear()
    End Sub
End Class

' -------------------------------------------------------------------------------- TFont
Public Class TFont
    Public FontFont As FontFamily
    Public SizeFont As Integer
    Public CharHFont As Double
    Public CharWFont As Double

    Public Sub New(familyName As EFont, emSize As Integer)
        If Not TGraphics.FontDic.TryGetValue(emSize, FontFont) Then
            FontFont = New FontFamily("MS Gothic")
            TGraphics.FontDic.Add(emSize, FontFont)
        End If
        SizeFont = emSize
    End Sub
End Class

' -------------------------------------------------------------------------------- TColor
Public Structure TColor
    Public Shared Red As TColor = New TColor(Colors.Red)
    Public Shared Green As TColor = New TColor(Colors.Green)
    Public Shared Blue As TColor = New TColor(Colors.Blue)
    Public Shared Black As TColor = New TColor(Colors.Black)
    Public Shared White As TColor = New TColor(Colors.White)
    Public Shared Navy As TColor = New TColor(Color.FromArgb(255, 0, 0, 128))
    Public Shared SkyBlue As TColor = New TColor(Color.FromArgb(&HFF, &H87, &HCE, &HEB))
    Public Shared Gray As TColor = New TColor(Colors.Gray)
    Public Shared Orange As TColor = New TColor(Colors.Orange)

    Public ACol As Byte
    Public RCol As Byte
    Public GCol As Byte
    Public BCol As Byte
    Public ColCol As Color

    Public Sub New(col As Color)
        ACol = col.A
        RCol = col.R
        GCol = col.G
        BCol = col.B

        ColCol = Color.FromArgb(col.A, col.R, col.G, col.B)
    End Sub

End Structure
