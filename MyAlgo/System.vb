Imports System.IO
Imports System.Net
Imports System.Windows
Imports System.Text
Imports System.Reflection
Imports System.Diagnostics

'-------------------------------------------------------------------------------- TPnt
Public Structure TPnt
    Public XPnt As Double
    Public YPnt As Double

    Public Sub New(x1 As Double, y1 As Double)
        XPnt = x1
        YPnt = y1
    End Sub

    Public Shared Operator *(d As Double, pt1 As TPnt) As TPnt
        Dim pt3 As New TPnt

        pt3.XPnt = d * pt1.XPnt
        pt3.YPnt = d * pt1.YPnt
        Return pt3
    End Operator

    Public Shared Operator +(pt1 As TPnt, pt2 As TPnt) As TPnt
        Dim pt3 As New TPnt
        pt3.XPnt = pt1.XPnt + pt2.XPnt
        pt3.YPnt = pt1.YPnt + pt2.YPnt
        Return pt3
    End Operator

    Public Shared Operator -(pt1 As TPnt, pt2 As TPnt) As TPnt
        Dim pt3 As New TPnt
        pt3.XPnt = pt1.XPnt - pt2.XPnt
        pt3.YPnt = pt1.YPnt - pt2.YPnt
        Return pt3
    End Operator

    Public Shared Operator *(pt1 As TPnt, pt2 As TPnt) As Double
        Return pt1.XPnt * pt2.XPnt + pt1.YPnt * pt2.YPnt
    End Operator

    Public Shared Operator =(pt1 As TPnt, pt2 As TPnt) As Boolean
        Return pt1.XPnt = pt2.XPnt AndAlso pt1.YPnt = pt2.YPnt
    End Operator

    Public Shared Operator <>(pt1 As TPnt, pt2 As TPnt) As Boolean
        Return pt1.XPnt <> pt2.XPnt OrElse pt1.YPnt <> pt2.YPnt
    End Operator

    Public Function LenPnt() As Double
        Return Math.Sqrt(XPnt * XPnt + YPnt * YPnt)
    End Function

    '   単位ベクトルを返す
    Public Function UnitPnt() As TPnt
        Dim len1 As Double
        len1 = LenPnt()
        If len1 = 0 Then
            Debug.Assert(len1 <> 0)
        End If
        Return New TPnt(XPnt / len1, YPnt / len1)
    End Function

    Public Overrides Function ToString() As String
        Return TSys.Format("({0:0.0} {1:0.0})", XPnt, YPnt)
    End Function
End Structure

' -------------------------------------------------------------------------------- TRect
Public Class TRect
    Public PosR As TPnt
    Public SizeR As TPnt

    Public Sub New(x As Double, y As Double, w As Double, h As Double)
        PosR.XPnt = x
        PosR.YPnt = y
        SizeR.XPnt = w
        SizeR.YPnt = h
    End Sub

    Public Sub New(pos As TPnt, sz As TPnt)
        PosR = pos
        SizeR = sz
    End Sub

    Public Function LeftRect() As Double
        Return PosR.XPnt
    End Function

    Public Function TopRect() As Double
        Return PosR.YPnt
    End Function

    Public Function RightRect() As Double
        Return PosR.XPnt + SizeR.XPnt
    End Function

    Public Function BottomRect() As Double
        Return PosR.YPnt + SizeR.YPnt
    End Function

    Public Function WidthRect() As Double
        Return SizeR.XPnt
    End Function

    Public Function HeightRect() As Double
        Return SizeR.YPnt
    End Function

    Public Function Intersect(rect1 As TRect) As Boolean
        Return LeftRect() <= rect1.RightRect() AndAlso rect1.LeftRect() <= RightRect() AndAlso TopRect() <= rect1.BottomRect() AndAlso rect1.TopRect() <= BottomRect()
    End Function

End Class

'-------------------------------------------------------------------------------- EExpand
Public Enum EExpand
    eNone
    eShow
    eHide
    eCollapse
    eExpand
End Enum

' -------------------------------------------------------------------------------- EFont
Public Enum EFont
    eGothic
End Enum

Public Interface IUpList
    Function GetUpList() As Object
End Interface

Public Class TList(Of T)
    Inherits List(Of T)
    Implements IUpList

    <_Parent()> Public UpList As Object

    Public Sub New()
        MyBase.New()
    End Sub

    Public Sub New(v As IEnumerable(Of T))
        MyBase.New(v)
    End Sub

    Public Function GetUpList() As Object Implements IUpList.GetUpList
        Return UpList
    End Function
End Class

' -------------------------------------------------------------------------------- TSys
Public Class TSys
    Public Shared IsWeb As Boolean = False

    Public Shared Function Substring(s As String, st As Integer, ed As Integer) As String
        Return s.Substring(st, ed - st)
    End Function

    Public Shared Function CreateObject(tp As Type) As Object
        Return tp.GetConstructor(Type.EmptyTypes).Invoke(Nothing)
    End Function

    Public Shared Function StringRepeat(s As Char, cnt As Integer) As String
        Return New String(s, cnt)
    End Function

    Public Shared Function StringRepeat(s As String, cnt As Integer) As String
        Return New String(s(0), cnt)
    End Function

    Public Shared Function Split(s As String, c As Char) As String()
        Return s.Split(New Char() {c})
    End Function

    ' 文字列の末尾の空白文字を取り除く
    Public Shared Function TrimEnd(s As String) As String
        Dim k As Integer

        For k = s.Length - 1 To 1 Step -1
            If Not Char.IsWhiteSpace(s(k)) Then
                ' 空白でない場合

                Return s.Substring(0, k + 1)
            End If
        Next

        Return s
    End Function

    Public Shared Function Format(fmt As String, arg1 As Object) As String
        Return String.Format(fmt, arg1)
    End Function

    Public Shared Function Format(fmt As String, arg1 As Object, arg2 As Object) As String
        Return String.Format(fmt, arg1, arg2)
    End Function

    Public Shared Function Format(fmt As String, arg1 As Object, arg2 As Object, arg3 As Object) As String
        Return String.Format(fmt, arg1, arg2, arg3)
    End Function

    Public Shared Function Format(fmt As String, arg1 As Object, arg2 As Object, arg3 As Object, arg4 As Object) As String
        Return String.Format(fmt, arg1, arg2, arg3, arg4)
    End Function

    Public Shared Function GetModuleFileName() As String
        Return Assembly.GetExecutingAssembly().Location
    End Function
End Class

' -------------------------------------------------------------------------------- TUtil
Public Class TUtil(Of T)
    Public Function ArrayToList(v As T()) As TList(Of T)
        Return New TList(Of T)(v)
    End Function
End Class

' -------------------------------------------------------------------------------- TPath
Public Class TPath
    Public Shared Function GetFileName(path1 As String) As String
        Return Path.GetFileName(path1)
    End Function

    Public Shared Function GetFileNameWithoutExtension(path1 As String) As String

        Return Path.GetFileNameWithoutExtension(path1)
    End Function
End Class

' -------------------------------------------------------------------------------- TFile
Public Class TFile

    Public Shared Sub WriteAllText(path1 As String, contents As String, encoding As Encoding)
        If Not TSys.IsWeb Then

            Dim dir1 As String = Path.GetDirectoryName(path1)
            If Not Directory.Exists(dir1) Then
                Directory.CreateDirectory(dir1)
            End If
            File.WriteAllText(path1, contents, encoding)
        End If
    End Sub

    Public Shared Sub WriteAllText(path As String, contents As String)
        If Not TSys.IsWeb Then

            WriteAllText(path, contents, Encoding.UTF8)
        End If
    End Sub

    Public Shared Function ReadAllLines(file_path As String) As String()
        Dim v As List(Of String), va As String(), i As Integer

        v = File.ReadLines(file_path, Encoding.UTF8).ToList()

        ReDim va(v.Count - 1)
        For i = 0 To v.Count - 1
            va(i) = v(i)
        Next

        Return va
    End Function

    Public Shared Function ReadAllText(path As String) As String
        Return File.ReadAllText(path, Encoding.UTF8)
    End Function
End Class

' -------------------------------------------------------------------------------- TDirectory
Public Class TDirectory
    Public Shared Function GetFiles(path As String, searchPattern As String) As TList(Of String)
        Dim v As IEnumerable(Of String)

        v = Directory.EnumerateFiles(path, searchPattern)

        Return New TList(Of String)(v)
    End Function

    Public Shared Function CreateDirectory(path As String) As DirectoryInfo
        If TSys.IsWeb OrElse Directory.Exists(path) Then
            Return Nothing
        End If

        Return Directory.CreateDirectory(path)
    End Function
End Class

' -------------------------------------------------------------------------------- TStringWriter
Public Class TStringWriter
    Dim SW As StringWriter

    Public Sub New()
        SW = New StringWriter()
    End Sub

    Public Sub Append(c As Char)
        SW.Write(c)
    End Sub

    Public Sub Append(s As String)
        SW.Write(s)
    End Sub

    Public Sub Write(c As Char)
        SW.Write(c)
    End Sub

    Public Sub Write(s As String)
        SW.Write(s)
    End Sub

    Public Sub Write(fmt As String, ParamArray args As Object())
        SW.Write(fmt, args)
    End Sub

    Public Sub WriteLine(s As String)
        SW.WriteLine(s)
    End Sub

    Public Sub WriteLine(fmt As String, ParamArray args As Object())
        SW.WriteLine(fmt, args)
    End Sub

    Public Overrides Function ToString() As String
        Return SW.ToString()
    End Function
End Class
