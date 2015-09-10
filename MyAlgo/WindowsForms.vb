Imports System.Drawing
Imports System.Drawing.Text
Imports System.Drawing.Imaging
Imports System.Text
Imports System.Windows.Forms
Imports MySys

' -------------------------------------------------------------------------------- TDraw
Public Class TDraw
    Public Shared IdCnt As Integer
    Public IdDraw As Integer
    Public UpDraw As TDrawCmp
    Public VisDraw As Boolean = True
    Public PosDraw As TPnt

    Public Sub New()
        IdDraw = IdCnt
        IdCnt = IdCnt + 1
    End Sub

    Public Overridable Sub Draw(gr As TGraphics)
    End Sub

    Public Overrides Function ToString() As String
        If UpDraw Is Nothing Then
            Return TSys.Format("{0} {1}", IdDraw, Me.GetType())
        Else
            Return TSys.Format("{0} {1} {2}", IdDraw, Me.GetType(), UpDraw.IdDraw)
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
    Public DrawCmp As New TList(Of TDraw)

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
        If Not VisDraw Then
            Exit Sub
        End If
        gr.DrawEllipse(ColEll, PosEll.XPnt, PosEll.YPnt, SizeEll.XPnt, SizeEll.YPnt)
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
        If Not VisDraw Then
            Exit Sub
        End If
        gr.DrawLine(ColLine, StartLine, EndLine)
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
        gr.DrawSpline(ColDSpl, PntDSpl)
    End Sub
End Class

' -------------------------------------------------------------------------------- TDrawRectangle
Public Class TDrawRectangle
    Inherits TDraw
    Public ColRect As TColor
    Public BdrWRect As Double
    Public PosRect As TPnt
    Public SizeRect As TPnt

    Public Sub New(col As TColor, pen_width As Double, x As Double, y As Double, width As Double, height As Double)
        ColRect = col
        BdrWRect = pen_width
        PosRect = New TPnt(x, y)
        SizeRect = New TPnt(width, height)
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        If Not VisDraw Then
            Exit Sub
        End If
        gr.DrawRectangle(ColRect, BdrWRect, PosRect.XPnt, PosRect.YPnt, SizeRect.XPnt, SizeRect.YPnt)
    End Sub
End Class

' -------------------------------------------------------------------------------- TFillRectangle
Public Class TFillRectangle
    Inherits TDraw
    Public ColFillRect As TColor
    Public PosFillRect As TPnt
    Public SizeFillRect As TPnt

    Public Sub New(col As TColor, x As Double, y As Double, width As Double, height As Double)
        ColFillRect = col
        PosFillRect = New TPnt(x, y)
        SizeFillRect = New TPnt(width, height)
    End Sub

    Public Sub New(col As TColor, pos As TPnt, sz As TPnt)
        ColFillRect = col
        PosFillRect = pos
        SizeFillRect = sz
    End Sub

    Public Overrides Sub Draw(gr As TGraphics)
        If Not VisDraw Then
            Exit Sub
        End If
        gr.FillRectangle(ColFillRect, PosFillRect.XPnt, PosFillRect.YPnt, SizeFillRect.XPnt, SizeFillRect.YPnt)
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
        gr.DrawString(TxtStr, FntStr, ColStr, PosStr.XPnt, PosStr.YPnt)
    End Sub

    Public Overrides Function ToString() As String
        If UpDraw Is Nothing Then
            Return TSys.Format("{0} {1} {2}", IdCnt, Me.GetType(), TxtStr)
        Else
            Return TSys.Format("{0} {1} {2} {3}", IdCnt, Me.GetType(), UpDraw.IdDraw, TxtStr)
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
        gr.vClip.Add(gr.mClip)
        gr.SetClip(gr.Offset + PosClip, SizeClip)
        '        gr.mBgGr.Clip = New Region(New RectangleF(RectClip.LeftRect(), RectClip.TopRect(), RectClip.WidthRect(), RectClip.HeightRect()))
    End Sub
End Class

' -------------------------------------------------------------------------------- TPopClip
Public Class TPopClip
    Inherits TDraw

    Public Overrides Sub Draw(gr As TGraphics)
        Dim rc As TRect

        rc = gr.vClip(gr.vClip.Count - 1)
        gr.vClip.RemoveAt(gr.vClip.Count - 1)
        '        gr.SetClip(rc.PosR, rc.SizeR)
        gr.PopClip()
    End Sub
End Class

' -------------------------------------------------------------------------------- TGraphics
Public Class TGraphics
    Public Shared theGr As Graphics
    Public Shared Handle As IntPtr
    Public Offset As TPnt
    Dim RectGr As New TList(Of TRect)
    Dim mBgBmp As Bitmap
    Public mBgGr As Graphics
    Dim mFgGr As Graphics
    Public mClip As TRect
    Public vClip As New TList(Of TRect)
    Shared PenDic As New Dictionary(Of Integer, Pen)
    Shared BrsDic As New Dictionary(Of Integer, SolidBrush)
    Public Shared FontDic As New Dictionary(Of Integer, Font)
    Public RemakeDrawFigList As New TList(Of TFig)

    Shared Function GetPen(col As TColor, w As Integer) As Pen
        Dim pen As Pen = Nothing, n As Integer

        Debug.Assert(0 <= w AndAlso w < 256)
        n = w * &HFF000000 + (col.ArgbCol And &HFFFFFF)

        If PenDic.ContainsKey(n) Then
            pen = PenDic(n)
        Else
            If w = 0 Then
                pen = New Pen(Color.FromArgb(col.ArgbCol))
            Else
                pen = New Pen(Color.FromArgb(col.ArgbCol), CSng(w))
            End If
            PenDic.Add(n, pen)
        End If

        Return pen
    End Function

    Shared Function GetBrs(col As TColor) As SolidBrush
        Dim brs As SolidBrush = Nothing

        If BrsDic.ContainsKey(col.ArgbCol) Then
            brs = BrsDic(col.ArgbCol)
        Else
            brs = New SolidBrush(Color.FromArgb(col.ArgbCol))
            BrsDic.Add(col.ArgbCol, brs)
        End If

        Return brs
    End Function

    Public Shared Sub InitGraphics()
        theGr = Graphics.FromHwnd(Handle)
    End Sub

    Sub New(old_gr As TGraphics, frm As TForm)
        mBgBmp = New Bitmap(CInt(frm.SizeFig.XPnt), CInt(frm.SizeFig.YPnt), PixelFormat.Format24bppRgb)
        mBgGr = Graphics.FromImage(mBgBmp)
        mFgGr = Graphics.FromHwnd(Handle)
    End Sub

    Public Sub Clear(color As Color)
        mBgGr.Clear(color)
    End Sub

    Public Sub DisposeGraphics()
        mBgBmp.Dispose()
        mBgGr.Dispose()
        mFgGr.Dispose()
    End Sub

    Public Sub DrawEllipse(col As TColor, x As Double, y As Double, width As Double, height As Double)
        mBgGr.DrawEllipse(GetPen(col, 0), CSng(Offset.XPnt + x), CSng(Offset.YPnt + y), CSng(width), CSng(height))
    End Sub

    Public Sub DrawLine(col As TColor, x1 As Double, y1 As Double, x2 As Double, y2 As Double)
        mBgGr.DrawLine(GetPen(col, 0), CSng(Offset.XPnt + x1), CSng(Offset.YPnt + y1), CSng(Offset.XPnt + x2), CSng(Offset.YPnt + y2))
    End Sub

    Public Sub DrawLine(col As TColor, st As TPnt, ed As TPnt)
        mBgGr.DrawLine(GetPen(col, 0), CSng(Offset.XPnt + st.XPnt), CSng(Offset.YPnt + st.YPnt), CSng(Offset.XPnt + ed.XPnt), CSng(Offset.YPnt + ed.YPnt))
    End Sub

    Public Sub DrawSpline(col As TColor, vpnt As TList(Of TPnt))
        Dim pen1 As Pen, vpnt2 As PointF(), i1 As Integer

        pen1 = GetPen(col, 0)
        ReDim vpnt2(vpnt.Count - 1)
        For i1 = 0 To vpnt.Count - 1
            vpnt2(i1) = New PointF(CSng(Offset.XPnt + vpnt(i1).XPnt), CSng(Offset.YPnt + vpnt(i1).YPnt))
        Next

        mBgGr.DrawCurve(pen1, vpnt2)
    End Sub

    Public Sub DrawRectangle(col As TColor, pen_width As Double, x As Double, y As Double, width As Double, height As Double)
        mBgGr.DrawRectangle(GetPen(col, CInt(pen_width)), CSng(Offset.XPnt + x), CSng(Offset.YPnt + y), CSng(width), CSng(height))
    End Sub

    Public Sub DrawString(s As String, font As TFont, col As TColor, x As Double, y As Double)
        mBgGr.DrawString(s, font.FontFont, GetBrs(col), CSng(Offset.XPnt + x), CSng(Offset.YPnt + y))
    End Sub

    Public Sub FillRectangle(col As TColor, x As Double, y As Double, width As Double, height As Double)
        mBgGr.FillRectangle(GetBrs(col), CSng(Offset.XPnt + x), CSng(Offset.YPnt + y), CSng(width), CSng(height))
    End Sub

    Public Sub AddRepaintRect(rc As TRect)
        RectGr.Add(rc)
    End Sub

    Public Sub AddRepaint(wnd As TWnd)
        Dim pt As TPnt

        pt = wnd.AbsPos()
        AddRepaintRect(New TRect(pt, wnd.SizeFig))
    End Sub

    Public Sub Update()
        For Each rc In RectGr
            mFgGr.DrawImage(mBgBmp, CSng(rc.LeftRect()), CSng(rc.TopRect()), New RectangleF(CSng(rc.LeftRect()), CSng(rc.TopRect()), CSng(rc.SizeR.XPnt), CSng(rc.SizeR.YPnt)), GraphicsUnit.Pixel)
        Next
        RectGr.Clear()
    End Sub

    Public Sub SetClip(pos As TPnt, sz As TPnt)
        mClip = New TRect(pos, sz)
        mBgGr.Clip = New Region(New RectangleF(CSng(pos.XPnt), CSng(pos.YPnt), CSng(sz.XPnt), CSng(sz.YPnt)))
    End Sub

    Public Sub PopClip()
        mClip = Nothing
        mBgGr.Clip = New Region()
    End Sub

    Public Sub SetWndClip(wnd As TWnd)
        Dim pt As TPnt

        pt = wnd.AbsPosC()
        SetClip(pt, wnd.ClientSize)
    End Sub

    Public Shared Function MeasureText(s As String, fnt As TFont) As TPnt
        Dim sz As Size

        sz = TextRenderer.MeasureText(s, fnt.FontFont)
        Return New TPnt(sz.Width, sz.Height)
    End Function

    Public Shared Sub SetCursor(cur As ECursor)
        Select Case cur
            Case ECursor.eARROW
                TSysC.SetCursor(ECursor.eARROW)
            Case ECursor.eSIZEWE
                TSysC.SetCursor(ECursor.eSIZEWE)
            Case Else
                Debug.Assert(False)
        End Select

    End Sub

    Public Sub UpdateDraw(frm As TForm)
        Dim old_draw As TDraw
        'Dim sw As New TStringWriter

        For Each fig In RemakeDrawFigList
            If fig.Visible Then
                Offset = fig.AbsPos()
                AddRepaintRect(New TRect(Offset, fig.SizeFig))

                ' DrawFigを再作成する
                old_draw = fig.DrawFig
                fig.SetDrawFig()
                If fig.DrawFig IsNot Nothing AndAlso fig.DrawFig.VisDraw Then
                    fig.DrawFig.PosDraw = old_draw.PosDraw

                    If old_draw.UpDraw IsNot Nothing Then
                        ' 親のDrawがある場合

                        old_draw.UpDraw.RepDrawCmp(old_draw, fig.DrawFig)
                    End If

                    ' 再描画する
                    Offset = fig.DrawFig.AbsPosDraw()
                    fig.DrawFig.Draw(Me)
                End If
            End If
        Next
        RemakeDrawFigList.Clear()

        If frm.ActivePopup IsNot Nothing Then

            frm.ActivePopup.SetDrawFig()
            Offset = frm.ActivePopup.AbsPos()
            frm.ActivePopup.DrawFig.Draw(Me)
        End If

        Update()
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
End Class

' -------------------------------------------------------------------------------- TFont
Public Class TFont
    Public FontFont As Font

    Public Sub New(familyName As EFont, emSize As Integer)
        If TGraphics.FontDic.ContainsKey(emSize) Then
            FontFont = TGraphics.FontDic(emSize)
        Else
            FontFont = New Font("MS Gothic", CSng(emSize))
            TGraphics.FontDic.Add(emSize, FontFont)
        End If
    End Sub
End Class

' -------------------------------------------------------------------------------- TColor
Public Structure TColor
    Public Shared Red As New TColor(Color.Red)
    Public Shared Green As New TColor(Color.Green)
    Public Shared Blue As New TColor(Color.Blue)
    Public Shared Black As New TColor(Color.Black)
    Public Shared White As New TColor(Color.White)
    Public Shared Navy As New TColor(Color.Navy)
    Public Shared SkyBlue As New TColor(Color.SkyBlue)
    Public Shared Gray As New TColor(Color.Gray)
    Public Shared Orange As New TColor(Color.Orange)

    Public ArgbCol As Integer

    Public Sub New(col As Color)
        ArgbCol = col.ToArgb()
    End Sub

End Structure
