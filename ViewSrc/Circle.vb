
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

'-------------------------------------------------------------------------------- TView
Public Class TView
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

Public Class TNaviRule
    Inherits TSystem

    Public Sub ParallelForEach(children_fld As Object)
    End Sub
End Class

Public Class TNaviView
    Inherits TNaviRule

    Public Sub GlobalRule(_current As TView)
        If TypeOf _current Is TCircle Then
            With CType(_current, TCircle)
                .Width = .Radius * 2
                .Height = .Radius * 2

                .Left = .CenterX - .Width / 2
                .Top = .CenterY - .Height / 2
            End With
        End If
    End Sub
End Class
