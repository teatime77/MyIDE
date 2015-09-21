Imports System.Windows.Media
Imports System.Globalization

Public Class TSync
    Public SelfSync As Object
End Class

'-------------------------------------------------------------------------------- EOrientation
Public Enum EOrientation
	eHorizontal
	eVertical
End Enum


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
	eMouseMove
	eMouseUp
	ePaint
	eOnResize
	eOnTimer
	eClick
End Enum

'-------------------------------------------------------------------------------- EBoundaryPosition
Public Enum EBoundaryPosition
	eTopLeft
	eTopMiddle
	eTopRight
	eMiddleLeft
	eMiddleMiddle
	eMiddleRight
	eBottomLeft
	eBottomMiddle
	eBottomRight
	eNon
End Enum

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

'-------------------------------------------------------------------------------- TView
Public Class TView
	Public Context As ENaviViewContext
    Public _ParentControl As TControl
    Public _Prev As TControl

	Public Left As Double
	Public Top As Double

	Public Width As Double
	Public Height As Double

	Public ActualWidth As Double
	Public ActualHeight As Double

	Public DesiredWidth As Double
	Public DesiredHeight As Double

	Public AbsoluteX As Integer
	Public AbsoluteY As Integer

	Public Visible As Boolean

    Public MarginLeft As Double
    Public MarginTop As Double
    Public MarginRight As Double
    Public MarginBottom As Double
    Public MarginMiddleHorizontal As Double
    Public MarginMiddleVertical As Double

	Public Bitmap As TBitmap
	Public BackgroundBitmap As TBitmap
	Public BackgroundImage As TBitmap
End Class

'-------------------------------------------------------------------------------- TDrag
Public Class TDrag
	Public 対象のオブジェクト As Object				 ' 
	Public 変化させるプロパティ As Object		 '
	Public プロパティの値の計算式 As Object					 ' 
End Class

'-------------------------------------------------------------------------------- TControl
Public Class TControl
	Inherits TView
	Public BorderWidth As Double
	Public BorderColor As TColor
	Public MousePressBorderColor As TColor
	Public MouseOverBorderColor As TColor

	Public BackgroundColor As TColor
	Public MousePressBackgroundColor As TColor
	Public MouseOverBackgroundColor As TColor

	Public Anchor As TAnchorStyle
	Public ClientLeft As Double
	Public ClientTop As Double
	Public ClientWidth As Double
	Public ClientHight As Double

	Public LeftPadding As Double
	Public TopPadding As Double
	Public RightPadding As Double
	Public BottomPadding As Double

	Public AutoSize As Boolean

	Public Data As Object
End Class

'-------------------------------------------------------------------------------- TViewGroup
Public Class TViewGroup
	Inherits TView
End Class

'-------------------------------------------------------------------------------- TPanel
Public Class TPanel
	Inherits TControl
	Public Children As New List(Of TControl)

	Public HorizontalPadding As Double
	Public VerticalPadding As Double
	Public ChildrenScale As Double
End Class

'-------------------------------------------------------------------------------- TCanvas
Public Class TCanvas
	Inherits TPanel

End Class

'-------------------------------------------------------------------------------- TStackPanel
Public Class TStackPanel
	Inherits TPanel

	Public Orientation As EOrientation
End Class

'-------------------------------------------------------------------------------- TPopup
Public Class TPopup

End Class

'-------------------------------------------------------------------------------- TForm
Public Class TForm
	Inherits TControl
	Public Container As TPanel
End Class

'-------------------------------------------------------------------------------- TTextBlock
Public Class TTextBlock
	Inherits TControl
	Public Font As TFont
	Public Text As String
	Public TextWidth As Double
	Public TextHeight As Double

	Public DataFormat As String
End Class

'-------------------------------------------------------------------------------- TLabel
Public Class TLabel
	Inherits TTextBlock
End Class

'-------------------------------------------------------------------------------- TButton
Public Class TButton
	Inherits TTextBlock
End Class

'-------------------------------------------------------------------------------- TTextBox
Public Class TTextBox
	Inherits TTextBlock
End Class

'-------------------------------------------------------------------------------- TRadioButton
Public Class TRadioButton
	Inherits TControl
End Class

'-------------------------------------------------------------------------------- TScrollBar
Public Class TScrollBar
	Inherits TControl

	Public Orientation As EOrientation
	Public PrevButton As TButton
	Public NextButton As TButton
	Public Thumb As TThumb

	Public Maximum As Double
	Public Minimum As Double

	Public LowValue As Double
	Public HighValue As Double
End Class

'-------------------------------------------------------------------------------- TScrollView
Public Class TScrollView
	Inherits TControl
	Public HorizontalScrollBar As TScrollBar
	Public VerticalScrollBar As TScrollBar

	Public ContentWidth As Double
	Public ContentHeight As Double

	Public ViewOffsetX As Double
	Public ViewOffsetY As Double
End Class

'-------------------------------------------------------------------------------- TListBox
Public Class TListBox
	Public Items As New List(Of TView)
End Class

'-------------------------------------------------------------------------------- TTreeViewItem
Public Class TTreeViewItem
	Inherits TTextBlock
    Public _ParentTVI As TTreeViewItem
	Public Indent As Double
	Public Expanded As Boolean

	Public ChildrenTVI As New List(Of TTreeViewItem)
End Class

'-------------------------------------------------------------------------------- TTreeView
Public Class TTreeView
	Inherits TScrollView
	Public Root As New TTreeViewItem
End Class

'-------------------------------------------------------------------------------- TSplitContainer
Public Class TSplitContainer

End Class

'-------------------------------------------------------------------------------- TTabPage
Public Class TTabPage
	Public Text As String

	Public TabButton As TButton
	Public Panel As TPanel
End Class

'-------------------------------------------------------------------------------- TTabControl
Public Class TTabControl
	Public TabPages As New List(Of TTabPage)

End Class

'-------------------------------------------------------------------------------- TDesWnd
Public Class TDesWnd

End Class

'-------------------------------------------------------------------------------- TSrcEditAbs
Public Class TSrcEditAbs

End Class

'-------------------------------------------------------------------------------- TSrcEdit
Public Class TSrcEdit

End Class

'-------------------------------------------------------------------------------- TSrcBrowser
Public Class TSrcBrowser

End Class

'-------------------------------------------------------------------------------- TThumb
Public Class TThumb
	Inherits TControl
End Class

'-------------------------------------------------------------------------------- TComboBox
Public Class TComboBox
End Class

'-------------------------------------------------------------------------------- TCheckBox
Public Class TCheckBox
End Class

'-------------------------------------------------------------------------------- TMenu
Public Class TMenu
End Class

'-------------------------------------------------------------------------------- TPictureBox
Public Class TGrid
End Class

Public Class TPictureBox
End Class

'-------------------------------------------------------------------------------- TSlider
Public Class TSlider
	Public Thumb As TThumb
End Class

'-------------------------------------------------------------------------------- TGroupBox
Public Class TGroupBox
	Public Text As String
End Class

'-------------------------------------------------------------------------------- TGraph
Public Class TGraph

End Class

'-------------------------------------------------------------------------------- TGraphNode
Public Class TGraphNode

End Class

'-------------------------------------------------------------------------------- TAnchorStyle
Public Class TAnchorStyle
	Public Left As Boolean
	Public Top As Boolean
	Public Right As Boolean
	Public Bottom As Boolean
End Class

Public Enum ENaviViewContext
	eStartButton
	eStartLabel
	eStartScrollBar
	eTreeViewItem
	eTreeView
	eStartCanvas
	eStartStackPanel
	eStartPanel
	eStartView
	eStartForm
	eStartControl
End Enum

'-------------------------------------------------------------------------------- TNaviView
Public Class TNaviView
    Inherits TSystem
    Public Application As TWindowApplication

    Public StopNavi As Boolean = False
    Public Result As TControl

    Public Margin As Integer
    Public BoundaryPosition As EBoundaryPosition

    Public Sub GetBorderByPos(view As TView, ParamArray args As Object())
        With view
            Dim x As Integer, y As Integer
            x = CType(args(0), Integer)
            y = CType(args(1), Integer)

            If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin AndAlso .AbsoluteY - Margin <= y AndAlso y < .AbsoluteY + .ActualHeight + Margin Then

                If .AbsoluteY - Margin <= y AndAlso y < .AbsoluteY + Margin Then
                    ' 上辺にある場合

                    If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + Margin Then
                        ' 左辺にある場合

                        BoundaryPosition = EBoundaryPosition.eTopLeft
                    ElseIf .AbsoluteX + .ActualWidth - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin Then
                        ' 右辺にある場合

                        BoundaryPosition = EBoundaryPosition.eTopMiddle
                    Else
                        ' 左辺や右辺にない場合

                        BoundaryPosition = EBoundaryPosition.eTopRight
                    End If
                ElseIf .AbsoluteY + .ActualHeight - Margin <= y AndAlso y < .AbsoluteY + .ActualHeight + Margin Then
                    ' 下辺にある場合

                    If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + Margin Then
                        ' 左辺にある場合

                        BoundaryPosition = EBoundaryPosition.eBottomLeft
                    ElseIf .AbsoluteX + .ActualWidth - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin Then
                        ' 右辺にある場合

                        BoundaryPosition = EBoundaryPosition.eBottomRight
                    Else
                        ' 左辺や右辺にない場合

                        BoundaryPosition = EBoundaryPosition.eBottomMiddle
                    End If
                Else
                    ' 上辺や下辺にない場合

                    If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + Margin Then
                        ' 左辺にある場合

                        BoundaryPosition = EBoundaryPosition.eMiddleLeft
                    ElseIf .AbsoluteX + .ActualWidth - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin Then
                        ' 右辺にある場合

                        BoundaryPosition = EBoundaryPosition.eMiddleRight
                    Else
                        ' 左辺や右辺にない場合

                        BoundaryPosition = EBoundaryPosition.eMiddleMiddle
                    End If
                End If

                Result = view
                StopNavi = True
            End If
        End With
    End Sub

    ' 描画を定義する
    Public Sub SetBitmap(view As TView, ParamArray args As Object())
        If TypeOf view Is TControl Then
            With CType(view, TControl)
                Dim dst As TBitmap, dst1 As TBitmap, clip As TRegion, background_color As TColor, border_color As TColor

                dst = CType(args(0), TBitmap)
                clip = CType(args(1), TRegion)

                ' 背景描画の定義
                If Application.ControlUnderMouse Is view Then
                    If Application.MouseDown Then

                        background_color = .MousePressBackgroundColor
                        border_color = .MousePressBorderColor
                    Else

                        background_color = .MouseOverBackgroundColor
                        border_color = .MouseOverBorderColor
                    End If
                Else
                    background_color = .BackgroundColor
                    border_color = .BorderColor
                End If

                dst1 = MakeBitmap(CInt(.ActualWidth), CInt(.ActualHeight), background_color)
                .BackgroundBitmap = DrawRectangle(dst1, 0, 0, CInt(.ActualWidth), CInt(.ActualHeight), CInt(.BorderWidth), border_color)

                If TypeOf view Is TPanel Then
                    With CType(view, TPanel)
                        Dim bmp_sum As New TBitmap

                        For Each ctrl In .Children
                            If IsFirst() Then
                                ' 最初の場合

                                bmp_sum = DrawBitmap(.BackgroundImage, ctrl.Left, ctrl.Top, ctrl.Bitmap)
                            Else
                                ' 最初でない場合

                                bmp_sum = DrawBitmap(CType(PrevValue(bmp_sum), TBitmap), ctrl.Left, ctrl.Top, ctrl.Bitmap)
                            End If
                        Next
                        .Bitmap = bmp_sum
                    End With
                End If
            End With
        End If
    End Sub

    Public Function TextSize(font As TFont, text As String) As Size
        Return font.MeasureText(text)
    End Function

    Public Sub ParallelForEach(children_fld As Object)
    End Sub

    Public Sub GlobalRule(_current As TView)
        If TypeOf _current Is TControl Then
            With CType(_current, TControl)
                If TypeOf _current Is TPanel Then
                    With CType(_current, TPanel)

                        ParallelForEach(.Children)

                        If TypeOf _current Is TCanvas Then
                            With CType(_current, TCanvas)

                                For Each ctrl In .Children
                                    With ctrl

                                        If Not Double.IsNaN(.MarginLeft) Then
                                            ' 左のマージンが有効の場合

                                            .Left = .MarginLeft

                                            If Not Double.IsNaN(.MarginRight) Then
                                                ' 右のマージンが有効の場合

                                                .ActualWidth = ._ParentControl.Width - .MarginRight - .Left
                                            Else
                                                ' 右のマージンが無効の場合

                                                .ActualWidth = .DesiredWidth
                                            End If
                                        Else
                                            ' 左のマージンが無効の場合

                                            Debug.Assert(Not Double.IsNaN(.MarginRight))

                                            .ActualWidth = .DesiredWidth
                                            .Left = ._ParentControl.ActualWidth - .MarginRight - .ActualWidth
                                        End If

                                        If Not Double.IsNaN(.MarginTop) Then
                                            ' 上のマージンが有効の場合

                                            .Top = .MarginTop

                                            If Not Double.IsNaN(.MarginBottom) Then
                                                ' 下のマージンが有効の場合

                                                .ActualHeight = ._ParentControl.Height - .MarginBottom - .Top
                                            Else
                                                ' 下のマージンが無効の場合

                                                .ActualHeight = .DesiredHeight
                                            End If
                                        Else
                                            ' 上のマージンが無効の場合

                                            Debug.Assert(Not Double.IsNaN(.MarginBottom))

                                            .ActualHeight = .DesiredHeight
                                            .Top = ._ParentControl.ActualHeight - .MarginBottom - .ActualHeight
                                        End If
                                    End With
                                Next
                            End With

                        ElseIf TypeOf _current Is TStackPanel Then
                            With CType(_current, TStackPanel)
                                Dim children_width_sum As Double, children_height_sum As Double, interval As Double, scale As Double

                                Select Case .Children.Count
                                    Case 0
                                    Case 1
                                    Case Else

                                        Select Case .Orientation
                                            ' 水平方向に並べる場合

                                            Case EOrientation.eHorizontal
                                                children_width_sum = Aggregate _child In .Children Into Sum(_child.DesiredWidth)

                                                If children_width_sum <= .ClientWidth Then

                                                    interval = (.ClientWidth - children_width_sum) / (.Children.Count - 1)

                                                    For Each ctrl In .Children
                                                        With ctrl
                                                            .ActualWidth = .DesiredWidth
                                                        End With
                                                    Next
                                                Else

                                                    interval = 0

                                                    scale = .ClientWidth / children_width_sum
                                                    For Each ctrl In .Children
                                                        With ctrl
                                                            .ActualWidth = scale * .DesiredWidth
                                                        End With
                                                    Next
                                                End If

                                                For Each ctrl In .Children
                                                    With ctrl
                                                        If IsFirst() Then
                                                            ' 最初の場合

                                                            .Left = ._ParentControl.ClientLeft
                                                        Else
                                                            ' 最初でない場合

                                                            .Left = ._Prev.Left + ._Prev.ActualWidth + interval
                                                        End If
                                                    End With
                                                Next

                                                For Each ctrl In .Children
                                                    With ctrl
                                                        .ActualHeight = .DesiredHeight
                                                    End With
                                                Next

                                            Case EOrientation.eVertical
                                                ' 垂直方向に並べる場合

                                                children_height_sum = Aggregate _child In .Children Into Sum(_child.DesiredHeight)

                                                If children_height_sum <= .ClientHight Then

                                                    interval = (.ClientHight - children_height_sum) / (.Children.Count - 1)

                                                    For Each ctrl In .Children
                                                        With ctrl
                                                            .ActualHeight = .DesiredHeight
                                                        End With
                                                    Next
                                                Else

                                                    interval = 0

                                                    scale = .ClientHight / children_height_sum
                                                    For Each ctrl In .Children
                                                        With ctrl
                                                            .ActualHeight = scale * .DesiredHeight
                                                        End With
                                                    Next
                                                End If

                                                For Each ctrl In .Children
                                                    With ctrl
                                                        If IsFirst() Then
                                                            ' 最初の場合

                                                            .Top = ._ParentControl.ClientTop
                                                        Else
                                                            ' 最初でない場合

                                                            .Top = ._Prev.Top + ._Prev.ActualHeight + interval
                                                        End If
                                                    End With
                                                Next

                                                For Each ctrl In .Children
                                                    With ctrl
                                                        .ActualWidth = .DesiredWidth
                                                    End With
                                                Next
                                        End Select
                                End Select
                            End With
                        End If
                    End With

                ElseIf TypeOf _current Is TScrollView Then
                    With CType(_current, TScrollView)
                        Dim box_size As Double = 10

                        .HorizontalScrollBar.Left = 0
                        .HorizontalScrollBar.Top = .Height - box_size

                        .HorizontalScrollBar.Width = .Width
                        .HorizontalScrollBar.Height = box_size

                        .HorizontalScrollBar.Minimum = 0
                        .HorizontalScrollBar.Maximum = .ContentWidth

                        .VerticalScrollBar.Left = .Width - box_size
                        .VerticalScrollBar.Top = 0

                        .VerticalScrollBar.Width = box_size
                        .VerticalScrollBar.Height = .Height

                        .VerticalScrollBar.Minimum = 0
                        .VerticalScrollBar.Maximum = .ContentHeight

                        .ViewOffsetX = .HorizontalScrollBar.LowValue
                        .ViewOffsetY = .VerticalScrollBar.LowValue

                        If TypeOf _current Is TTreeView Then
                            With CType(_current, TTreeView)

                            End With
                        End If
                    End With

                ElseIf TypeOf _current Is TTextBlock Then
                    With CType(_current, TTextBlock)
                        Dim sz As Size

                        sz = TextSize(.Font, .Text)
                        .TextWidth = sz.Width
                        .TextHeight = sz.Height

                        If TypeOf _current Is TLabel OrElse TypeOf _current Is TButton Then
                            With CType(_current, TLabel)
                                If .AutoSize Then

                                    .DesiredWidth = .LeftPadding + .TextWidth + .RightPadding
                                    .DesiredHeight = .TopPadding + .TextHeight + .BottomPadding
                                Else

                                    .DesiredWidth = .Width
                                    .DesiredHeight = .Height
                                End If

                                If .Data IsNot Nothing Then
                                    If .DataFormat <> "" Then
                                        .Text = String.Format(.DataFormat, .Data)
                                    Else
                                        .Text = .Data.ToString()
                                    End If
                                End If
                            End With

                        ElseIf TypeOf _current Is TTreeViewItem Then
                            With CType(_current, TTreeViewItem)
                                Dim children_height_sum As Double, children_width_max As Double

                                ParallelForEach(.ChildrenTVI)

                                If .ChildrenTVI.Count <> 0 AndAlso .Expanded Then
                                    ' 子があり、展開している場合

                                    children_height_sum = Aggregate _child In .ChildrenTVI Into Sum(_child.ActualHeight)
                                    .ActualHeight = .MarginTop + .TextHeight + .MarginMiddleVertical * .ChildrenTVI.Count + children_height_sum + .MarginBottom

                                    children_width_max = Aggregate _child In .ChildrenTVI Into Max(_child.ActualWidth)
                                    .ActualWidth = Math.Max(.TextWidth, children_width_max)
                                Else
                                    ' 子がないか、折りたたまれている場合

                                    .ActualHeight = .MarginTop + .TextHeight + .MarginBottom
                                    .ActualWidth = .TextWidth
                                End If

                                .Left = ._ParentTVI.Indent

                                If IsFirst() Then
                                    ' 最初の場合

                                    .Top = ._ParentTVI.ClientTop + ._ParentTVI.MarginTop + .TextHeight
                                Else
                                    ' 最初でない場合

                                    .Top = ._Prev.Top + ._Prev.ActualHeight + ._ParentTVI.MarginMiddleVertical
                                End If

                            End With
                        End If
                    End With

                ElseIf TypeOf _current Is TTreeView Then
                    With CType(_current, TTreeView)
                        .ContentWidth = .Root.ActualWidth
                        .ContentHeight = .Root.ActualHeight
                    End With

                ElseIf TypeOf _current Is TScrollBar Then

                    With CType(_current, TScrollBar)
                        Dim button_size As Double, movable_size As Double, thumb_pos As Double, thumb_size As Double

                        Select Case .Orientation
                            Case EOrientation.eHorizontal
                                button_size = .Height
                                movable_size = .ActualWidth - 2 * button_size

                                .NextButton.Left = .Width - button_size
                                .NextButton.Top = 0

                                .Thumb.Top = 0
                                .Thumb.Height = button_size
                            Case EOrientation.eVertical
                                button_size = .Width
                                movable_size = .ActualHeight - 2 * button_size

                                .NextButton.Left = 0
                                .NextButton.Top = .Height - button_size

                                .Thumb.Left = 0
                                .Thumb.Width = button_size
                        End Select

                        .PrevButton.Width = button_size
                        .PrevButton.Height = button_size

                        .NextButton.Width = button_size
                        .NextButton.Height = button_size

                        .PrevButton.Left = 0
                        .PrevButton.Top = 0

                        If True Then
                            ' Thumbの位置・サイズからLowValue・HighValueを求める場合

                            Select Case .Orientation
                                Case EOrientation.eHorizontal
                                    thumb_pos = .Thumb.Left - button_size
                                    thumb_size = .Thumb.Width

                                Case EOrientation.eVertical
                                    thumb_pos = .Thumb.Top - button_size
                                    thumb_size = .Thumb.Height
                            End Select

                            .LowValue = .Minimum + (.Maximum - .Minimum) * thumb_pos / movable_size
                            .HighValue = .LowValue + (.Maximum - .Minimum) * thumb_size / movable_size
                        Else
                            ' LowValue・HighValueからThumbの位置・サイズを求める場合

                            thumb_pos = movable_size * (.LowValue - .Minimum) * (.Maximum - .Minimum)
                            thumb_size = movable_size * (.HighValue - .LowValue) / (.Maximum - .Minimum)

                            Select Case .Orientation
                                Case EOrientation.eHorizontal

                                    .Thumb.Left = button_size + thumb_pos
                                    .Thumb.Width = thumb_size

                                Case EOrientation.eVertical

                                    .Thumb.Top = button_size + thumb_pos
                                    .Thumb.Height = thumb_size
                            End Select
                        End If
                    End With

                ElseIf TypeOf _current Is TForm Then
                    With CType(_current, TForm)
                    End With
                End If

                ' クライアント領域の定義
                .ClientLeft = .Left + .BorderWidth + .LeftPadding
                .ClientTop = .Top + .BorderWidth + .TopPadding

                .ClientWidth = Math.Max(0, .ActualWidth - (.BorderWidth * 2 + .LeftPadding + .RightPadding))
                .ClientHight = Math.Max(0, .ActualHeight - (.BorderWidth * 2 + .TopPadding + .BottomPadding))
            End With
        End If

    End Sub


    Public Sub IndependentRule(view As TView, ParamArray args As Object())
        If TypeOf view Is TControl Then
            With CType(view, TControl)



                If TypeOf view._ParentControl Is TCanvas Then
                    Dim canvas As TCanvas

                    canvas = CType(view._ParentControl, TCanvas)
                    If Not Double.IsNaN(.MarginLeft) Then
                        ' 左のマージンが有効の場合

                        .Left = .MarginLeft

                        If Not Double.IsNaN(.MarginRight) Then
                            ' 右のマージンが有効の場合

                            .ActualWidth = ._ParentControl.Width - .MarginRight - .Left
                        Else
                            ' 右のマージンが無効の場合

                            .ActualWidth = .DesiredWidth
                        End If
                    Else
                        ' 左のマージンが無効の場合

                        Debug.Assert(Not Double.IsNaN(.MarginRight))

                        .ActualWidth = .DesiredWidth

                        .Left = ._ParentControl.ActualWidth - .MarginRight - .ActualWidth
                    End If

                    If Not Double.IsNaN(.MarginTop) Then
                        ' 上のマージンが有効の場合

                        .Top = .MarginTop

                        If Not Double.IsNaN(.MarginBottom) Then
                            ' 下のマージンが有効の場合

                            .ActualHeight = ._ParentControl.Height - .MarginBottom - .Top
                        Else
                            ' 下のマージンが無効の場合

                            .ActualHeight = .DesiredHeight
                        End If
                    Else
                        ' 上のマージンが無効の場合

                        Debug.Assert(Not Double.IsNaN(.MarginBottom))

                        .ActualHeight = .DesiredHeight

                        .Top = ._ParentControl.ActualHeight - .MarginBottom - .ActualHeight
                    End If

                ElseIf TypeOf view._ParentControl Is TStackPanel Then
                    Dim stack_panel As TStackPanel

                    stack_panel = CType(view._ParentControl, TStackPanel)

                    Select Case stack_panel.Orientation

                        Case EOrientation.eHorizontal
                            ' 水平方向に並べる場合

                            .ActualWidth = stack_panel.ChildrenScale * .DesiredWidth

                            If IsFirst() Then
                                ' 最初の場合

                                .Left = stack_panel.ClientLeft
                            Else
                                ' 最初でない場合

                                .Left = ._Prev.Left + ._Prev.ActualWidth + stack_panel.HorizontalPadding
                            End If
                        Case EOrientation.eVertical
                            ' 垂直方向に並べる場合

                            .ActualHeight = stack_panel.ChildrenScale * .DesiredHeight

                            If IsFirst() Then
                                ' 最初の場合

                                .Top = .ClientTop
                            Else
                                ' 最初でない場合

                                .Top = ._Prev.Top + ._Prev.ActualHeight + stack_panel.VerticalPadding
                            End If
                    End Select
                End If

                If TypeOf view Is TScrollView Then

                    With CType(view, TScrollView)
                        Dim box_size As Double = 10

                        .HorizontalScrollBar.Left = 0
                        .HorizontalScrollBar.Top = .Height - box_size

                        .HorizontalScrollBar.Width = .Width
                        .HorizontalScrollBar.Height = box_size

                        .HorizontalScrollBar.Minimum = 0
                        .HorizontalScrollBar.Maximum = .ContentWidth

                        .VerticalScrollBar.Left = .Width - box_size
                        .VerticalScrollBar.Top = 0

                        .VerticalScrollBar.Width = box_size
                        .VerticalScrollBar.Height = .Height

                        .VerticalScrollBar.Minimum = 0
                        .VerticalScrollBar.Maximum = .ContentHeight

                        .ViewOffsetX = .HorizontalScrollBar.LowValue
                        .ViewOffsetY = .VerticalScrollBar.LowValue

                        If TypeOf view Is TTreeView Then

                            With CType(view, TTreeView)

                            End With
                        End If
                    End With

                ElseIf TypeOf view Is TStackPanel Then
                    With CType(view, TStackPanel)
                        Dim children_width_sum As Double, children_height_sum As Double

                        Select Case .Orientation
                            Case EOrientation.eHorizontal
                                ' 水平方向に並べる場合

                                children_width_sum = Aggregate ctrl In .Children Into Sum(ctrl.DesiredWidth)

                                If children_width_sum <= .ClientWidth Then

                                    .HorizontalPadding = (.ClientWidth - children_width_sum) / (.Children.Count - 1)

                                    .ChildrenScale = 1
                                Else

                                    .HorizontalPadding = 0

                                    .ChildrenScale = .ClientWidth / children_width_sum
                                End If
                            Case EOrientation.eVertical
                                ' 垂直方向に並べる場合

                                children_height_sum = Aggregate a_ctrl In .Children Into Sum(a_ctrl.DesiredHeight)

                                If children_height_sum <= .ClientHight Then

                                    .VerticalPadding = (.ClientHight - children_height_sum) / (.Children.Count - 1)

                                    .ChildrenScale = 1
                                Else

                                    .VerticalPadding = 0

                                    .ChildrenScale = .ClientHight / children_height_sum
                                End If
                        End Select

                    End With
                ElseIf TypeOf view Is TTextBlock Then
                    With CType(view, TTextBlock)
                        Dim sz As Size

                        sz = TextSize(.Font, .Text)
                        .TextWidth = sz.Width
                        .TextHeight = sz.Height

                        If TypeOf view Is TLabel Then
                            With CType(view, TLabel)
                                If .AutoSize Then

                                    .DesiredWidth = .LeftPadding + .TextWidth + .RightPadding
                                Else

                                    .DesiredWidth = .Width
                                End If
                            End With

                        ElseIf TypeOf view Is TButton Then
                            With CType(view, TButton)


                            End With

                        ElseIf TypeOf view Is TTreeViewItem Then
                            With CType(view, TTreeViewItem)
                                Dim children_height_sum As Double, children_width_max As Double

                                If .ChildrenTVI.Count <> 0 AndAlso .Expanded Then
                                    ' 子があり、展開している場合

                                    children_height_sum = Aggregate a_ctrl In .ChildrenTVI Into Sum(a_ctrl.DesiredHeight)
                                    .ActualHeight = .MarginTop + .TextHeight + .MarginMiddleVertical * .ChildrenTVI.Count + children_height_sum + .MarginBottom

                                    children_width_max = Aggregate a_ctrl In .ChildrenTVI Into Max(a_ctrl.ActualWidth)
                                    .ActualWidth = Math.Max(.TextWidth, children_width_max)

                                    .Left = .Indent

                                    If IsFirst() Then
                                        ' 最初の場合

                                        .Top = ._ParentControl.ClientTop + .TextHeight
                                    Else
                                        ' 最初でない場合

                                        .Top = ._Prev.Top + ._Prev.ActualHeight + .MarginMiddleVertical
                                    End If
                                Else
                                    ' 子がないか、折りたたまれている場合

                                    .ActualHeight = .MarginTop + .TextHeight + .MarginBottom

                                    .ActualWidth = .TextWidth
                                End If

                            End With
                        End If
                    End With

                ElseIf TypeOf view Is TTreeView Then
                    With CType(view, TTreeView)
                        .ContentWidth = .Root.ActualWidth
                        .ContentHeight = .Root.ActualHeight
                    End With

                ElseIf TypeOf view Is TScrollBar Then

                    With CType(view, TScrollBar)
                        Dim button_size As Double, movable_size As Double, thumb_pos As Double, thumb_size As Double

                        Select Case .Orientation
                            Case EOrientation.eHorizontal
                                button_size = .Height
                                movable_size = .ActualWidth - 2 * button_size

                                .NextButton.Left = .Width - button_size
                                .NextButton.Top = 0

                                .Thumb.Top = 0
                                .Thumb.Height = button_size
                            Case EOrientation.eVertical
                                button_size = .Width
                                movable_size = .ActualHeight - 2 * button_size

                                .NextButton.Left = 0
                                .NextButton.Top = .Height - button_size

                                .Thumb.Left = 0
                                .Thumb.Width = button_size
                        End Select

                        .PrevButton.Width = button_size
                        .PrevButton.Height = button_size

                        .NextButton.Width = button_size
                        .NextButton.Height = button_size

                        .PrevButton.Left = 0
                        .PrevButton.Top = 0

                        If True Then
                            ' Thumbの位置・サイズからLowValue・HighValueを求める場合

                            Select Case .Orientation
                                Case EOrientation.eHorizontal
                                    thumb_pos = .Thumb.Left - button_size
                                    thumb_size = .Thumb.Width

                                Case EOrientation.eVertical
                                    thumb_pos = .Thumb.Top - button_size
                                    thumb_size = .Thumb.Height
                            End Select

                            .LowValue = .Minimum + (.Maximum - .Minimum) * thumb_pos / movable_size
                            .HighValue = .LowValue + (.Maximum - .Minimum) * thumb_size / movable_size
                        Else
                            ' LowValue・HighValueからThumbの位置・サイズを求める場合

                            thumb_pos = movable_size * (.LowValue - .Minimum) * (.Maximum - .Minimum)
                            thumb_size = movable_size * (.HighValue - .LowValue) / (.Maximum - .Minimum)

                            Select Case .Orientation
                                Case EOrientation.eHorizontal

                                    .Thumb.Left = button_size + thumb_pos
                                    .Thumb.Width = thumb_size

                                Case EOrientation.eVertical

                                    .Thumb.Top = button_size + thumb_pos
                                    .Thumb.Height = thumb_size
                            End Select
                        End If
                    End With

                ElseIf TypeOf view Is TForm Then

                    With CType(view, TForm)

                    End With
                End If
            End With
        End If
    End Sub
End Class

'-------------------------------------------------------------------------------- TEvent
Public Class TEvent
    Public TypeEv As EEvent
    Public Source As Object

    Public Sub New()
    End Sub

    Public Sub New(tp As EEvent)
        TypeEv = tp
    End Sub
End Class

'-------------------------------------------------------------------------------- TMouseEvent
Public Class TMouseEvent
    Inherits TEvent
    Public X As Integer
    Public Y As Integer
    Public MouseDownTime As DateTime

    Public Sub New()
    End Sub
End Class

'-------------------------------------------------------------------------------- TKeyEvent
Public Class TKeyEvent
    Inherits TEvent
    Public Shift As Boolean
    Public Alt As Boolean
    Public Control As Boolean
    Public CharEv As Char
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

'-------------------------------------------------------------------------------- TApplication
Public Class TApplication
End Class

'-------------------------------------------------------------------------------- TApplication
Public Class TWindowApplication
    Inherits TApplication

    Public EventList As New List(Of TEvent)

    Public MouseDown As Boolean
    Public ControlUnderMouse As TControl
    Public ControlOnMouseDown As TControl
    Public UserEvent As TEvent

    Public Overridable Sub InitializeApplication()
    End Sub

    Public Overridable Sub Main()
        InitializeApplication()

        For Each ev In EventList
            HandleEvent(ev)
        Next
    End Sub

    Public Overridable Sub HandleEvent(ev As TEvent)
        Dim kev As TKeyEvent, mev As TMouseEvent
        Dim get_control_by_position As TNaviView, ctrl As TControl

        Select Case ev.TypeEv
            Case EEvent.eMouseDown, EEvent.eMouseMove, EEvent.eMouseUp
                mev = CType(ev, TMouseEvent)

                get_control_by_position = New TNaviView()
                '' '' '' '' ''get_control_by_position.NaviForm(Nothing, mev.X, mev.Y)

                ControlUnderMouse = get_control_by_position.Result

				Select Case ev.TypeEv
					Case EEvent.eMouseDown
						MouseDown = True
						ControlOnMouseDown = ControlUnderMouse

					Case EEvent.eMouseMove

					Case EEvent.eMouseUp
						MouseDown = False
						If ControlUnderMouse IsNot Nothing AndAlso ControlUnderMouse Is ControlOnMouseDown Then

							If TypeOf ControlUnderMouse Is TButton Then

								UserEvent = New TEvent()
								UserEvent.Source = ControlUnderMouse
								UserEvent.TypeEv = EEvent.eClick
							End If
						End If

				End Select

			Case EEvent.eOnChar, EEvent.eOnKeyDown
				kev = CType(ev, TKeyEvent)
				Select Case ev.TypeEv
					Case EEvent.eOnChar
					Case EEvent.eOnKeyDown
				End Select

			Case EEvent.ePaint

			Case EEvent.eOnResize
				ctrl = CType(ev.Source, TControl)


			Case EEvent.eOnTimer
		End Select
	End Sub
End Class

'-------------------------------------------------------------------------------- TSystem
Public Class TSystem
    Public Shared Function IsFirst() As Boolean
        Return True
    End Function

    Public Shared Function PrevValue(o As Object) As Object
		Return Nothing
	End Function

	Public Shared Function MakeBitmap(width As Integer, height As Integer, fill_color As TColor) As TBitmap
		Return Nothing
	End Function

	Public Shared Function DrawBitmap(dst As TBitmap, x As Double, y As Double, src As TBitmap) As TBitmap
		Return Nothing
	End Function

	Public Shared Function DrawRectangle(dst As TBitmap, x As Integer, y As Integer, width As Integer, height As Integer, border_width As Integer, border_color As TColor) As TBitmap
		Return Nothing
	End Function

	Public Shared Function FillRectangle(dst As TBitmap, clip As TRegion, x As Integer, y As Integer, width As Integer, height As Integer, fill_color As TColor) As TBitmap
		Return Nothing
	End Function
End Class

'-------------------------------------------------------------------------------- TFont
Public Class TFont
	Public FontTypeFace As Typeface
	Public EmSize As Double

	Public Function MeasureText(text As String) As Size
		Dim formatted_text As FormattedText

		formatted_text = New FormattedText(text, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, FontTypeFace, EmSize, Brushes.Black)

		Return New Size(formatted_text.Width, formatted_text.Height)
	End Function
End Class

'-------------------------------------------------------------------------------- TColor
Public Class TColor
End Class

'-------------------------------------------------------------------------------- TRegion
Public Class TRegion
End Class

'-------------------------------------------------------------------------------- TBitmap
Public Class TBitmap
	Public Width As Integer
	Public Height As Integer

	Public Sub New()
	End Sub

	Public Sub New(width As Integer, height As Integer)
	End Sub
End Class

'-------------------------------------------------------------------------------- 
'															アプリケーション
'-------------------------------------------------------------------------------- 

'-------------------------------------------------------------------------------- T商品
Public Class T商品
	Public 商品名 As String
	Public 単価 As Double
End Class

'-------------------------------------------------------------------------------- T売上明細
Public Class T売上明細
	Public 商品 As T商品
	Public 数量 As Double
	Public 小計 As Double
End Class

'-------------------------------------------------------------------------------- T売上明細表示
Public Class T売上明細表示
	Inherits TCanvas

	Public lbl商品名 As TLabel
	Public lbl単価 As TLabel
	Public txt数量 As TTextBox
	Public lbl小計 As TLabel
End Class

'-------------------------------------------------------------------------------- TMyForm
Public Class TMyForm
	Inherits TForm
	Public 売上明細リスト表示 As TStackPanel
End Class

'-------------------------------------------------------------------------------- TMyApplication
Public Class TMyApplication
	Inherits TApplication
	Public 売上明細リスト As New List(Of T売上明細)

	Public Sub ApplicationRule(obj As Object, ParamArray args As Object())
		If TypeOf obj Is TMyForm Then
			With CType(obj, TMyForm)
				For Each ctrl In .売上明細リスト表示.Children
					Debug.Assert(TypeOf ctrl Is T売上明細表示)
				Next
			End With

		ElseIf TypeOf obj Is T売上明細 Then
			With CType(obj, T売上明細)
				.小計 = .商品.単価 * .数量
			End With

		ElseIf TypeOf obj Is T売上明細表示 Then
            With CType(obj, T売上明細表示)
                Dim dt As T売上明細

                dt = CType(.Data, T売上明細)
                .lbl商品名.Data = dt.商品.商品名
                .lbl単価.Data = dt.商品.単価
                .txt数量.Data = dt.数量
                .lbl小計.Data = dt.小計
            End With

		End If
	End Sub
End Class
