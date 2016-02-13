Imports System.Windows.Media
Imports System.Globalization
Imports System.Threading

Public Class 売上明細
    Public 親 As 売上伝票
    Public 兄 As 売上明細
    Public 明細番号 As Integer
    Public 単価 As Integer
    Public 数量 As Integer
    Public 小計 As Integer
End Class

Public Class 売上伝票
    Public 親 As 売上台帳
    Public 兄 As 売上伝票
    Public 売上明細リスト As New List(Of 売上明細)
    Public 合計 As Integer
End Class

Public Class 売上台帳
    Public 売上伝票リスト As New List(Of 売上伝票)
    Public 合計 As Integer
End Class

Public Class 販売管理
    Sub 不変条件(処理対象 As Object)
        If TypeOf 処理対象 Is 売上明細 Then
            With CType(処理対象, 売上明細)
                .小計 = .単価 * .数量
            End With

        ElseIf TypeOf 処理対象 Is 売上伝票 Then
            With CType(処理対象, 売上伝票)
                .合計 = Aggregate 子 In .売上明細リスト Into Sum(子.小計)

                For Each 子 In .売上明細リスト
                    If 先頭(子) Then
                        子.明細番号 = 1
                    Else
                        子.明細番号 = 子.兄.明細番号 + 1
                    End If
                Next
            End With
        End If


    End Sub


    Function 先頭(node As 売上明細) As Boolean
        Return node.親.売上明細リスト(0) Is node
    End Function
End Class

Public Class 矩形
    Inherits 葉ノード
End Class

Public Class 楕円
    Inherits 葉ノード
End Class

Public Class ノード
    Public X座標 As Integer
    Public Y座標 As Integer

    Public 幅 As Integer
    Public 高さ As Integer

    Public 境界ボックスの幅 As Integer
    Public 境界ボックスの高さ As Integer

    Public 親 As 内部ノード
    Public 兄 As ノード
End Class

Public Class 内部ノード
    Inherits ノード

    Public 子ノード As New List(Of ノード)
End Class

Public Class 葉ノード
    Inherits ノード
End Class

Public Class マイアプリケーション
    Public Sub 不変条件(処理対象 As Object)
        ' 以下のIf文では境界ボックスの幅を定義します。
        If TypeOf 処理対象 Is ノード Then
            With CType(処理対象, ノード)
                ' 処理対象がノードの場合

                If TypeOf 処理対象 Is 内部ノード Then
                    With CType(処理対象, 内部ノード)
                        ' 処理対象が内部ノードの場合

                        ' Aggregate句でSumを使って子の境界ボックスの幅の合計を計算します。
                        Dim 子の境界ボックスの幅の合計 = Aggregate 子 In .子ノード Into Sum(子.境界ボックスの幅)

                        ' 子の間隔は10ピクセルとして、子の間隔の合計を計算します。
                        Dim 子の間隔の合計 = 10 * (.子ノード.Count)

                        ' 境界ボックスの幅は自身の幅と子供達の幅の合計の大きい方です。
                        .境界ボックスの幅 = Math.Max(.幅, 子の境界ボックスの幅の合計 + 子の間隔の合計)
                    End With

                ElseIf TypeOf 処理対象 Is 葉ノード Then
                    With CType(処理対象, 葉ノード)
                        ' 処理対象が葉ノードの場合

                        ' 葉ノードの境界ボックスの幅は自身の幅です。
                        .境界ボックスの幅 = .幅
                    End With
                End If
            End With
        End If

        ' 以下のIf文ではX座標を定義します。
        If TypeOf 処理対象 Is 内部ノード Then
            With CType(処理対象, 内部ノード)
                ' 処理対象が内部ノードの場合

                If .親 Is Nothing Then
                    ' 根ノードの場合

                    .X座標 = 0
                End If

                For Each 子 In .子ノード
                    If 先頭(子) Then
                        ' 先頭の子の場合

                        ' X座標は親のX座標から親の境界ボックスの幅の半分を引いた値です。
                        子.X座標 = 子.親.X座標
                    Else
                        ' 先頭の子でない場合

                        ' X座標は兄のX座標に兄の境界ボックスの幅を足した値です。
                        子.X座標 = 子.兄.X座標 + 子.兄.境界ボックスの幅
                    End If
                Next
            End With
        End If
    End Sub

    Function 先頭(node As ノード) As Boolean
        Return node.親.子ノード(0) Is node
    End Function

    Sub 定型文(自身 As Object)
        If TypeOf 自身 Is ノード Then
            ' 自身がノードの場合

            With CType(自身, ノード)
                ' ノードのフィールドにドット(.)でアクセスできるようにWithで囲みます。


                If TypeOf 自身 Is 内部ノード Then
                    ' 自身が内部ノードの場合

                    With CType(自身, 内部ノード)
                        ' 内部ノードのフィールドにドット(.)でアクセスできるようにWithで囲みます。

                    End With

                ElseIf TypeOf 自身 Is 葉ノード Then
                    ' 自身が葉ノードの場合

                    With CType(自身, 葉ノード)
                        ' 葉ノードのフィールドにドット(.)でアクセスできるようにWithで囲みます。

                    End With
                End If
            End With
        End If

    End Sub
End Class

Public Class TSync
    Public SelfSync As Object
End Class


'-------------------------------------------------------------------------------- ECursor
Public Enum ECursor
    ARROW
    IBEAM
    WAIT
    CROSS
    UPARROW
    SIZE
    ICON
    SIZENWSE
    SIZENESW
    SIZEWE
    SIZENS
    SIZEALL
    NO
    HAND
    APPSTARTING
    HELP
End Enum

'-------------------------------------------------------------------------------- EEvent
Public Enum EEvent
    OnChar
    OnKeyDown
    MouseDown
    MouseMove
    MouseUp
    Paint
    OnResize
    OnTimer
    Click
End Enum

'-------------------------------------------------------------------------------- EBoundaryPosition
Public Enum EBoundaryPosition
    TopLeft
    TopMiddle
    TopRight
    MiddleLeft
    MiddleMiddle
    MiddleRight
    BottomLeft
    BottomMiddle
    BottomRight
    Non
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

'-------------------------------------------------------------------------------- TDrag
Public Class TDrag
	Public 対象のオブジェクト As Object				 ' 
	Public 変化させるプロパティ As Object		 '
	Public プロパティの値の計算式 As Object					 ' 
End Class

'-------------------------------------------------------------------------------- TGraph
Public Class TGraph

End Class

'-------------------------------------------------------------------------------- TGraphNode
Public Class TGraphNode

End Class

Public Enum ENaviViewContext
    StartButton
    StartLabel
    StartScrollBar
    TreeViewItem
    TreeView
    StartCanvas
    StartStackPanel
    StartPanel
    StartView
    StartForm
    StartControl
End Enum

Public Class TNaviRule
    Inherits TSystem

    Public Sub ParallelForEach(children_fld As Object)
    End Sub

    Public Sub WaitAll(v As IEnumerable(Of ManualResetEvent))
        For Each x In v
            x.WaitOne()
        Next
    End Sub

End Class

'-------------------------------------------------------------------------------- TCircle
Public Class TCircle
    Inherits TControl
    Public WPFCircle As New Ellipse

    Public Radius As Double
    Public CenterX As Double
    Public CenterY As Double

    Public Sub _Set_Width()
        WPFCircle.Width = Width
    End Sub

    Public Sub _Set_Height()
        WPFCircle.Height = Height
    End Sub

    Public Sub _Set_Left()
        Canvas.SetLeft(WPFCircle, Left)
    End Sub

    Public Sub _Set_Top()
        Canvas.SetTop(WPFCircle, Top)
    End Sub
End Class

Public Class TNaviCircle
    Inherits TNaviRule

    Public Sub GlobalRule(_current As TView)
        If TypeOf _current Is TCircle Then
            With CType(_current, TCircle)
                .Width = .Radius * 2
                .Height = .Radius * 2
            End With
        End If
    End Sub
End Class

Public Class TNaviStackPanel
    Inherits TNaviRule

    Public Sub GlobalRule(_current As TView)
        If TypeOf _current Is TStackPanel Then
            With CType(_current, TStackPanel)
                .ActualWidth = Aggregate _child In .Children Into Sum(_child.DesiredWidth)

                For Each ctrl In .Children
                    If IsFirst Then
                        ' 最初の場合

                        ctrl.Left = ctrl._ParentControl.Left
                    Else
                        ' 最初でない場合

                        ctrl.Left = ctrl._Prev.Left + ctrl._Prev.ActualWidth + 10
                    End If
                Next
            End With

        ElseIf TypeOf _current Is TCircle Then
            With CType(_current, TCircle)
                .ActualWidth = .DesiredWidth
            End With
        End If
    End Sub
End Class

'-------------------------------------------------------------------------------- TNaviView
Public Class TNaviView
    Inherits TNaviRule
    Public Application As TWindowApplication

    Public StopNavi As Boolean = False
    Public Result As TControl

    Public Margin As Integer
    Public BoundaryPosition As EBoundaryPosition

    Public Sub GetBorderByPos(view As TView, ParamArray args As Object())
        If TypeOf view Is TView Then
            With view
                Dim x As Integer, y As Integer
                x = CType(args(0), Integer)
                y = CType(args(1), Integer)

                If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin AndAlso .AbsoluteY - Margin <= y AndAlso y < .AbsoluteY + .ActualHeight + Margin Then

                    If .AbsoluteY - Margin <= y AndAlso y < .AbsoluteY + Margin Then
                        ' 上辺にある場合

                        If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + Margin Then
                            ' 左辺にある場合

                            BoundaryPosition = EBoundaryPosition.TopLeft
                        ElseIf .AbsoluteX + .ActualWidth - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin Then
                            ' 右辺にある場合

                            BoundaryPosition = EBoundaryPosition.TopMiddle
                        Else
                            ' 左辺や右辺にない場合

                            BoundaryPosition = EBoundaryPosition.TopRight
                        End If
                    ElseIf .AbsoluteY + .ActualHeight - Margin <= y AndAlso y < .AbsoluteY + .ActualHeight + Margin Then
                        ' 下辺にある場合

                        If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + Margin Then
                            ' 左辺にある場合

                            BoundaryPosition = EBoundaryPosition.BottomLeft
                        ElseIf .AbsoluteX + .ActualWidth - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin Then
                            ' 右辺にある場合

                            BoundaryPosition = EBoundaryPosition.BottomRight
                        Else
                            ' 左辺や右辺にない場合

                            BoundaryPosition = EBoundaryPosition.BottomMiddle
                        End If
                    Else
                        ' 上辺や下辺にない場合

                        If .AbsoluteX - Margin <= x AndAlso x < .AbsoluteX + Margin Then
                            ' 左辺にある場合

                            BoundaryPosition = EBoundaryPosition.MiddleLeft
                        ElseIf .AbsoluteX + .ActualWidth - Margin <= x AndAlso x < .AbsoluteX + .ActualWidth + Margin Then
                            ' 右辺にある場合

                            BoundaryPosition = EBoundaryPosition.MiddleRight
                        Else
                            ' 左辺や右辺にない場合

                            BoundaryPosition = EBoundaryPosition.MiddleMiddle
                        End If
                    End If

                    Result = view
                    StopNavi = True
                End If
            End With
        End If
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
                            If IsFirst Then
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

    Public Sub GlobalRule(_current As TView)
        If TypeOf _current Is TControl Then
            With CType(_current, TControl)
                If TypeOf _current Is TPanel Then
                    With CType(_current, TPanel)

                        ParallelForEach(.Children)

                        If TypeOf _current Is TCanvas Then
                            With CType(_current, TCanvas)

                                For Each ctrl In .Children
                                    If Not Double.IsNaN(.MarginLeft) Then
                                        ' 左のマージンが有効の場合

                                        ctrl.Left = ctrl.MarginLeft

                                        If Not Double.IsNaN(ctrl.MarginRight) Then
                                            ' 右のマージンが有効の場合

                                            ctrl.ActualWidth = ctrl._ParentControl.Width - ctrl.MarginRight - ctrl.Left
                                        Else
                                            ' 右のマージンが無効の場合

                                            ctrl.ActualWidth = ctrl.DesiredWidth
                                        End If
                                    Else
                                        ' 左のマージンが無効の場合

                                        Debug.Assert(Not Double.IsNaN(ctrl.MarginRight))

                                        ctrl.ActualWidth = ctrl.DesiredWidth
                                        ctrl.Left = ctrl._ParentControl.ActualWidth - ctrl.MarginRight - ctrl.ActualWidth
                                    End If

                                    If Not Double.IsNaN(ctrl.MarginTop) Then
                                        ' 上のマージンが有効の場合

                                        ctrl.Top = ctrl.MarginTop

                                        If Not Double.IsNaN(ctrl.MarginBottom) Then
                                            ' 下のマージンが有効の場合

                                            ctrl.ActualHeight = ctrl._ParentControl.Height - ctrl.MarginBottom - ctrl.Top
                                        Else
                                            ' 下のマージンが無効の場合

                                            ctrl.ActualHeight = ctrl.DesiredHeight
                                        End If
                                    Else
                                        ' 上のマージンが無効の場合

                                        Debug.Assert(Not Double.IsNaN(ctrl.MarginBottom))

                                        ctrl.ActualHeight = ctrl.DesiredHeight
                                        ctrl.Top = ctrl._ParentControl.ActualHeight - ctrl.MarginBottom - ctrl.ActualHeight
                                    End If
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

                                            Case EOrientation.Horizontal
                                                children_width_sum = Aggregate _child In .Children Into Sum(_child.DesiredWidth)

                                                If children_width_sum <= .ClientWidth Then

                                                    interval = (.ClientWidth - children_width_sum) / (.Children.Count - 1)

                                                    For Each ctrl In .Children
                                                        ctrl.ActualWidth = ctrl.DesiredWidth
                                                    Next
                                                Else

                                                    interval = 0

                                                    scale = .ClientWidth / children_width_sum
                                                    For Each ctrl In .Children
                                                        ctrl.ActualWidth = scale * ctrl.DesiredWidth
                                                    Next
                                                End If

                                                For Each ctrl In .Children
                                                    If IsFirst Then
                                                        ' 最初の場合

                                                        ctrl.Left = ctrl._ParentControl.ClientLeft
                                                    Else
                                                        ' 最初でない場合

                                                        ctrl.Left = ctrl._Prev.Left + ctrl._Prev.ActualWidth + interval
                                                    End If
                                                Next

                                                For Each ctrl In .Children
                                                    ctrl.ActualHeight = ctrl.DesiredHeight
                                                Next

                                            Case EOrientation.Vertical
                                                ' 垂直方向に並べる場合

                                                children_height_sum = Aggregate _child In .Children Into Sum(_child.DesiredHeight)

                                                If children_height_sum <= .ClientHight Then

                                                    interval = (.ClientHight - children_height_sum) / (.Children.Count - 1)

                                                    For Each ctrl In .Children
                                                        ctrl.ActualHeight = ctrl.DesiredHeight
                                                    Next
                                                Else

                                                    interval = 0

                                                    scale = .ClientHight / children_height_sum
                                                    For Each ctrl In .Children
                                                        ctrl.ActualHeight = scale * ctrl.DesiredHeight
                                                    Next
                                                End If

                                                For Each ctrl In .Children
                                                    If IsFirst Then
                                                        ' 最初の場合

                                                        ctrl.Top = ctrl._ParentControl.ClientTop
                                                    Else
                                                        ' 最初でない場合

                                                        ctrl.Top = ctrl._Prev.Top + ctrl._Prev.ActualHeight + interval
                                                    End If
                                                Next

                                                For Each ctrl In .Children
                                                    ctrl.ActualWidth = ctrl.DesiredWidth
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

                                If IsFirst Then
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
                            Case EOrientation.Horizontal
                                button_size = .Height
                                movable_size = .ActualWidth - 2 * button_size

                                .NextButton.Left = .Width - button_size
                                .NextButton.Top = 0

                                .Thumb.Top = 0
                                .Thumb.Height = button_size
                            Case EOrientation.Vertical
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
                                Case EOrientation.Horizontal
                                    thumb_pos = .Thumb.Left - button_size
                                    thumb_size = .Thumb.Width

                                Case EOrientation.Vertical
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
                                Case EOrientation.Horizontal

                                    .Thumb.Left = button_size + thumb_pos
                                    .Thumb.Width = thumb_size

                                Case EOrientation.Vertical

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


    Public Sub GlobalRuleParallelForEach(_sync As TSync_)
        Dim _current As TView = _sync._Self

        If TypeOf _current Is TControl Then
            With CType(_current, TControl)
                If TypeOf _current Is TPanel Then
                    With CType(_current, TPanel)

                        ParallelForEach(.Children)
                        Dim sync_list_ = (From x In .Children Select New TSync_(x, _current)).ToList()
                        '!!!!!!!!!!!!!!!!!!!!!!!!!!!! ToArrayが未対応
                        ''''Dim wait_all_1 = (From x In sync_list_ Select x.Wait_1).ToArray()
                        ''''Dim wait_all_2 = (From x In sync_list_ Select x.Wait_2).ToArray()

                        'Dim t = New Thread(Sub()
                        '                       Parallel.ForEach(sync_list_, AddressOf GlobalRuleParallelForEach)
                        '                   End Sub)
                        't.Start()

                        ''''WaitHandle.WaitAll(wait_all_1)
                        ''''WaitHandle.WaitAll(wait_all_2)

                        If TypeOf _current Is TCanvas Then
                            With CType(_current, TCanvas)

                                For Each ctrl In .Children
                                    If Not Double.IsNaN(ctrl.MarginLeft) Then
                                        ' 左のマージンが有効の場合

                                        ctrl.Left = ctrl.MarginLeft

                                        If Not Double.IsNaN(ctrl.MarginRight) Then
                                            ' 右のマージンが有効の場合

                                            ctrl.ActualWidth = ctrl._ParentControl.Width - ctrl.MarginRight - ctrl.Left
                                        Else
                                            ' 右のマージンが無効の場合

                                            ctrl.ActualWidth = ctrl.DesiredWidth
                                        End If
                                    Else
                                        ' 左のマージンが無効の場合

                                        Debug.Assert(Not Double.IsNaN(ctrl.MarginRight))

                                        ctrl.ActualWidth = ctrl.DesiredWidth
                                        ctrl.Left = ctrl._ParentControl.ActualWidth - ctrl.MarginRight - ctrl.ActualWidth
                                    End If

                                    If Not Double.IsNaN(ctrl.MarginTop) Then
                                        ' 上のマージンが有効の場合

                                        ctrl.Top = ctrl.MarginTop

                                        If Not Double.IsNaN(ctrl.MarginBottom) Then
                                            ' 下のマージンが有効の場合

                                            ctrl.ActualHeight = ctrl._ParentControl.Height - ctrl.MarginBottom - ctrl.Top
                                        Else
                                            ' 下のマージンが無効の場合

                                            ctrl.ActualHeight = ctrl.DesiredHeight
                                        End If
                                    Else
                                        ' 上のマージンが無効の場合

                                        Debug.Assert(Not Double.IsNaN(ctrl.MarginBottom))

                                        ctrl.ActualHeight = ctrl.DesiredHeight
                                        ctrl.Top = ctrl._ParentControl.ActualHeight - ctrl.MarginBottom - ctrl.ActualHeight
                                    End If
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

                                            Case EOrientation.Horizontal
                                                children_width_sum = Aggregate _child In .Children Into Sum(_child.DesiredWidth)

                                                If children_width_sum <= .ClientWidth Then

                                                    interval = (.ClientWidth - children_width_sum) / (.Children.Count - 1)

                                                    For Each ctrl In .Children
                                                        ctrl.ActualWidth = ctrl.DesiredWidth
                                                    Next
                                                Else

                                                    interval = 0

                                                    scale = .ClientWidth / children_width_sum
                                                    For Each ctrl In .Children
                                                        ctrl.ActualWidth = scale * ctrl.DesiredWidth
                                                    Next
                                                End If

                                                For Each ctrl In .Children
                                                    If IsFirst Then
                                                        ' 最初の場合

                                                        ctrl.Left = ctrl._ParentControl.ClientLeft
                                                    Else
                                                        ' 最初でない場合

                                                        ctrl.Left = ctrl._Prev.Left + ctrl._Prev.ActualWidth + interval
                                                    End If
                                                Next

                                                For Each ctrl In .Children
                                                    ctrl.ActualHeight = ctrl.DesiredHeight
                                                Next

                                            Case EOrientation.Vertical
                                                ' 垂直方向に並べる場合

                                                children_height_sum = Aggregate _child In .Children Into Sum(_child.DesiredHeight)

                                                If children_height_sum <= .ClientHight Then

                                                    interval = (.ClientHight - children_height_sum) / (.Children.Count - 1)

                                                    For Each ctrl In .Children
                                                        ctrl.ActualHeight = ctrl.DesiredHeight
                                                    Next
                                                Else

                                                    interval = 0

                                                    scale = .ClientHight / children_height_sum
                                                    For Each ctrl In .Children
                                                        ctrl.ActualHeight = scale * ctrl.DesiredHeight
                                                    Next
                                                End If

                                                For Each ctrl In .Children
                                                    If IsFirst Then
                                                        ' 最初の場合

                                                        ctrl.Top = ctrl._ParentControl.ClientTop
                                                    Else
                                                        ' 最初でない場合

                                                        ctrl.Top = ctrl._Prev.Top + ctrl._Prev.ActualHeight + interval
                                                    End If
                                                Next

                                                For Each ctrl In .Children
                                                    ctrl.ActualWidth = ctrl.DesiredWidth
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

                                If IsFirst Then
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
                            Case EOrientation.Horizontal
                                button_size = .Height
                                movable_size = .ActualWidth - 2 * button_size

                                .NextButton.Left = .Width - button_size
                                .NextButton.Top = 0

                                .Thumb.Top = 0
                                .Thumb.Height = button_size
                            Case EOrientation.Vertical
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
                                Case EOrientation.Horizontal
                                    thumb_pos = .Thumb.Left - button_size
                                    thumb_size = .Thumb.Width

                                Case EOrientation.Vertical
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
                                Case EOrientation.Horizontal

                                    .Thumb.Left = button_size + thumb_pos
                                    .Thumb.Width = thumb_size

                                Case EOrientation.Vertical

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
            Case EEvent.MouseDown, EEvent.MouseMove, EEvent.MouseUp
                mev = CType(ev, TMouseEvent)

                get_control_by_position = New TNaviView()
                '' '' '' '' ''get_control_by_position.NaviForm(Nothing, mev.X, mev.Y)

                ControlUnderMouse = get_control_by_position.Result

				Select Case ev.TypeEv
                    Case EEvent.MouseDown
                        MouseDown = True
						ControlOnMouseDown = ControlUnderMouse

                    Case EEvent.MouseMove

                    Case EEvent.MouseUp
                        MouseDown = False
						If ControlUnderMouse IsNot Nothing AndAlso ControlUnderMouse Is ControlOnMouseDown Then

							If TypeOf ControlUnderMouse Is TButton Then

								UserEvent = New TEvent()
								UserEvent.Source = ControlUnderMouse
                                UserEvent.TypeEv = EEvent.Click
                            End If
						End If

				End Select

            Case EEvent.OnChar, EEvent.OnKeyDown
                kev = CType(ev, TKeyEvent)
				Select Case ev.TypeEv
                    Case EEvent.OnChar
                    Case EEvent.OnKeyDown
                End Select

            Case EEvent.Paint

            Case EEvent.OnResize
                ctrl = CType(ev.Source, TControl)


            Case EEvent.OnTimer
        End Select
	End Sub
End Class

'-------------------------------------------------------------------------------- TSystem
Public Class TSystem
	Public IsFirst As Boolean

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


'-------------------------------------------------------------------------------- TRegion
Public Class TRegion
End Class

Public Class TSync_
    Public _Self As TView
    Public ParentSync As TView
    Public Wait_1 As New ManualResetEvent(False)
    Public Wait_2 As New ManualResetEvent(False)

    Public Sub New(x As TView, parent As TView)
        _Self = x
        ParentSync = parent
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

    Public Sub ApplicationRule(obj As Object)
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


