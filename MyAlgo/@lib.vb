Public Class Object
    Public Function Clone() As Object
        Return Nothing
    End Function

    Public Function ToString() As String
        Return ""
    End Function

        Public Function GetType() As Type
        Return Nothing
    End Function
End Class

Public Class System
		Public True As Boolean
		Public False As Boolean
		Public Nothing As Object
    Public vbCr As String
    Public vbLf As String
    Public vbTab As String
    Public vbBack As String
    Public vbNullChar As String
    Public vbCrLf As String
    Public Function IIf(a As Boolean, b As Object, c As Object) As Object
    End Function
    Public Function ChrW(a As Integer) As Char
        Return "a"c
    End Function
    Public Function AscW(c As Char) As Integer
        Return 0
    End Function

    Public Function CSng(n As Object) As Single
        Return 0
    End Function

    Public Function CInt(n As Object) As Integer
        Return 0
    End Function

    Public Function __Add(a As Double, b As Double) As Double
        Return 0
    End Function

    Public Function __Mns(a As Double, b As Double) As Double
        Return 0
    End Function

    Public Function __Mul(a As Double, b As Double) As Double
        Return 0
    End Function

    Public Function __Div(a As Double, b As Double) As Double
        Return 0
    End Function

    Public Function __Mod(a As Double, b As Double) As Double
        Return 0
    End Function
End Class

Public Class Array(Of T)
    Implements IEnumerable(Of T)
    Public Count As Integer
    Public Length As Integer
    Public Function IndexOf(item As T) As Integer
        Return 0
    End Function

    Public Shared Function CreateInstance(ParamArray lengths As Integer()) As Array(Of T)
        Return Nothing
    End Function
End Class

    Public Class Boolean
End Class

    Public Class Char
    Public Shared Function IsLetter(c As Char) As Boolean
        Return True
    End Function
    Public Shared Function IsDigit(c As Char) As Boolean
        Return True
    End Function
    Public Shared Function IsLetterOrDigit(c As Char) As Boolean
        Return True
    End Function
    Public Shared Function IsWhiteSpace(c As Char) As Boolean
        Return True
    End Function
End Class

Public Structure Short
    Inherits Integer
End Structure

Public Structure Single
    Inherits Double
    Public Shared MaxValue As Single
    Public Shared NaN As Single
    Public Shared Function IsNaN(f As Single) As Boolean
        Return True
    End Function
End Structure

Public Structure Double
    Public Shared MaxValue As Double
    Public Shared NaN As Double
    Public Shared Function IsNaN(f As Double) As Boolean
        Return True
    End Function
End Structure

Public Structure Integer
    Inherits Double
    Public Shared Function Parse(s As String) As Integer
        Return 0
    End Function
End Structure

Public Structure DateTime
End Structure

    Public Class String 
    Public Length As Integer
    Public Count As Integer

    Public Sub New(c As Char, count As Integer)
        Return ""
    End Sub
    Public Function CompareTo(strB As String) As Integer
        Return 0
    End Function
    Public Function Substring(startIndex As Integer) As String
        Return ""
    End Function
    Public Function Substring(startIndex As Integer, length As Integer) As String
        Return ""
    End Function
    Public Function ToLower() As String
        Return ""
    End Function
    Public Function Format(s As String, ParamArray args As Object()) As String
        Return ""
    End Function

    Public Function Replace(oldValue As String, newValue As String) As String
        Return ""
    End Function

    Public Function Replace(oldValue As Char, newValue As Char) As String
        Return ""
    End Function

    Public Function Remove(c As Char) As String
        Return ""
    End Function
    Public Function IndexOf(s As String) As Integer
        Return 0
    End Function
    Public Function IndexOf(value As String, startIndex As Integer) As Integer
        Return 0
    End Function
    Public Function Split(ParamArray separator As Char()) As String()
        Return Nothing
    End Function

    Public Function Trim() As String
        Return ""
    End Function

    Public Function TrimEnd() As String
        Return ""
    End Function

    Public Function StartsWith(value As String) As Boolean
        Return True
    End Function
End Class

Public Class Assembly
    Public Location As String

    Public Shared Function GetExecutingAssembly() As Assembly
        Return Nothing
    End Function
End Class

Public Class RoutedEventArgs
End Class

Public Class StringWriter
    Public Function ToString() As String
        Return ""
    End Function

    Public Sub Write(ParamArray args As Object())
    End Sub

    Public Sub WriteLine(ParamArray args As Object())
    End Sub
End Class

Public Class XmlSerializer
    Public Sub New(type As Type)
    End Sub

    Public Sub Serialize(textWriter As TextWriter, o As Object)
    End Sub

    Public Function Deserialize(textReader As TextReader) As Object
        Return Nothing
    End Function

    Public Function Deserialize(stream As Stream) As Object
        Return Nothing
    End Function
End Class

Public MustInherit Class TextReader
    Public Overridable Sub Close()
    End Sub
End Class

Public Class StreamReader
    Inherits TextReader

    Public Sub New(path As String, encoding As Encoding)
    End Sub

    Public Overrides Sub Close()
    End Sub
End Class

Public MustInherit Class TextWriter
    Public Overridable Sub Close()
    End Sub
End Class

Public Class StreamWriter
    Inherits TextWriter

    Public Sub New(path As String)
    End Sub

    Public Sub New(path As String, append As Boolean, encoding As Encoding)
    End Sub

    Public Overrides Sub Close()
    End Sub
End Class

Public Enum FileMode
    Append
    Create
    CreateNew
    Open
    OpenOrCreate
    Truncate
End Enum

Public MustInherit Class Stream
End Class

Public Class FileStream
    Inherits Stream

    Public Sub New(path As String, mode As FileMode)
    End Sub

    Public Overridable Sub Close()
    End Sub
End Class

Public Class ConstructorInfo
    Public Function Invoke(parameters As Object()) As Object
        Return Nothing
    End Function
End Class

Public Class Type
    Public Shared EmptyTypes As Type()

    Public Function ToString() As String
        Return ""
    End Function

    Public Function GetConstructor(types As Type()) As ConstructorInfo
        Return Nothing
    End Function
End Class

Public Class Exception
    Public Message As String
End Class

Public Class Debug
    Sub Print(ParamArray args As Object())
    End Sub

    Sub Write(ParamArray args As Object())
    End Sub

    Sub Assert(b As Boolean)
    End Sub

    Sub Assert(b As Boolean, msg As String)
    End Sub

    Public Sub WriteLine(ParamArray args As Object())
    End Sub
End Class

Public Class Trace
    Public Sub WriteLine(ParamArray args As Object())
    End Sub
End Class

Public Class StackTrace
End Class

Public Class File
    Public Shared Function ReadAllLines(path As String) As String()
        Return Nothing
    End Function

    Public Shared Function ReadLines(path As String, encoding As Encoding) As IEnumerable(Of String)
        Return Nothing
    End Function

    Public Shared Function ReadAllText(path As String) As String
        Return ""
    End Function

    Public Shared Function ReadAllText(path As String, encoding As Encoding) As String
        Return ""
    End Function

    Public Shared Sub WriteAllText(path As String, s As String)
    End Sub

    Public Shared Sub WriteAllText(path As String, contents As String, encoding As Encoding)
    End Sub
End Class

Public Class StringBuilder
    Public Function ToString() As String
        Return ""
    End Function

    Public Shared Sub Append(s As String)
    End Sub
End Class

Public Class Enumerable
    Public Shared Function Distinct(Of TSource)(source As IEnumerable(Of TSource)) As IEnumerable(Of TSource)
        Return Nothing
    End Function
End Class

Public Class IEnumerable(Of T)
    Public Function ToList() As List(Of T)
        Return Nothing
    End Function

    Public Function Count() As Integer
        Return 0
    End Function

    Public Function Any() As Boolean
        Return True
    End Function

    Public Function First() As T
        Return Nothing
    End Function

    Public Function Contains(value As T) As Boolean
        Return True
    End Function

    Public Function ToArray() As List(Of T)
        Return Nothing
    End Function

    Public Function Intersect(v As IEnumerable(Of T)) As IEnumerable(Of T)
        Return Nothing
    End Function

    Public Function Distinct() As IEnumerable(Of T)
        Return Nothing
    End Function
End Class

Public Class IList
End Class

Public Delegate Function Comparison(Of T)(x As T, y As T) As Integer

Public Class List(Of T)
    Implements IList, IEnumerable(Of T)
    Public Count As Integer

    Public Sub New(collection As IEnumerable(Of T))
    End Sub

    Public Sub Add(item As T)
    End Sub

    Public Sub AddRange(collection As IEnumerable(Of T))
    End Sub

    Public Sub Clear()
    End Sub

    Public Function Contains(item As T) As Boolean
        Return True
    End Function

    Public Function IndexOf(item As T) As Integer
        Return 0
    End Function

    Public Sub Insert(index As Integer, item As T)
    End Sub

    Public Function Remove(item As T) As Boolean
        Return True
    End Function

    Public Sub RemoveAt(index As Integer)
    End Sub

    Public Function Last() As T
        Return Nothing
    End Function

    Public Sub Reverse()
    End Sub

    Public Sub Sort()
    End Sub

    Public Sub Sort(f As Comparison(Of T))
    End Sub
End Class

Public Class Dictionary(Of TKey, TValue)
    Public Keys As TKey()
    Public Values As IEnumerable(Of TValue)
    Public Sub Add(key As TKey, value As TValue)
    End Sub

    Public Sub Clear()
    End Sub

    Public Function ContainsKey(key As TKey) As Boolean
        Return True
    End Function

    Public Function Remove(key As TKey) As Boolean
        Return True
    End Function

    Public Function TryGetValue(key As TKey, ByRef value As TValue) As Boolean
        Return True
    End Function
End Class

Public Class Stack(Of T)
    Public Count As Integer

    Public Function Peek() As T
        Return Nothing
    End Function

    Public Function Pop() As T
        Return Nothing
    End Function

    Public Sub Push(item As T)
    End Sub
End Class

Public Class Encoding
    Public Shared ASCII As Encoding
    Public Shared Unicode As Encoding
    Public Shared UTF8 As Encoding

    Public Shared Function GetEncoding(name As String) As Encoding
        Return Nothing
    End Function
End Class

Public Class UTF8Encoding
	Inherits Encoding

	Public Sub New(encoderShouldEmitUTF8Identifier As Boolean)
	End Sub
End Class

Public Class HttpUtility
    Public Shared Function UrlEncode(str As String) As String
        Return ""
    End Function
End Class

Public Class StringSplitOptions
    Public Shared None As StringSplitOptions
End Class

Public Class Path
    Public Shared Function GetFileName(path As String) As String
        Return ""
    End Function

    Public Shared Function GetFileNameWithoutExtension(path As String) As String
        Return ""
    End Function

    Public Shared Function GetDirectoryName(path As String) As String
        Return ""
    End Function
End Class

Public Class DirectoryInfo

End Class

Public Class Directory
    Public Shared Function GetFiles(path As String, searchPattern As String) As String()
        Return Nothing
    End Function

    Public Shared Function EnumerateFiles(path As String, searchPattern As String) As IEnumerable(Of String)
        Return Nothing
    End Function

    Public Shared Function Exists(path As String) As Boolean
        Return True
    End Function

    Public Shared Function CreateDirectory(path As String) As DirectoryInfo
        Return Nothing
    End Function
End Class

Public Class Math
    Public Shared Function Ceiling(a As Double) As Double
        Return 0
    End Function

    Public Shared Function Max(val1 As Integer, val2 As Integer) As Integer
        Return 0
    End Function

    Public Shared Function Max(val1 As Single, val2 As Single) As Single
        Return 0
    End Function

    Public Shared Function Max(val1 As Double, val2 As Double) As Double
        Return 0
    End Function

    Public Shared Function Min(val1 As Integer, val2 As Integer) As Integer
        Return 0
    End Function

    Public Shared Function Min(val1 As Single, val2 As Single) As Single
        Return 0
    End Function

    Public Shared Function Min(val1 As Double, val2 As Double) As Double
        Return 0
    End Function

    Public Shared Function Sqrt(d As Double) As Double
        Return 0
    End Function

    Public Shared Function Abs(d As Double) As Double
        Return 0
    End Function

    Public Shared Function Floor(d As Double) As Double
        Return 0
    End Function

    Public Shared Function Round(d As Double) As Double
        Return 0
    End Function
End Class

Public Structure IntPtr
End Structure

Public Structure Color
    Public Shared Black As Color
    Public Shared White As Color
    Public Shared Red As Color
    Public Shared Green As Color
    Public Shared Blue As Color
    Public Shared Gray As Color
    Public Shared Orange As Color
    Public Shared Pink As Color
    Public Shared Navy As Color
    Public Shared SkyBlue As Color
    Public Shared Violet As Color
    Public Shared Yellow As Color
    Public Shared Aqua As Color
    Public Shared Brown As Color
    Public Shared Cornsilk As Color
    Public Shared Cyan As Color
    Public Shared DarkBlue As Color
    Public Shared DarkRed As Color
    Public Shared DarkGreen As Color

    Public Function ToArgb() As Integer
        Return 0
    End Function

    Public Shared Function FromArgb(argb As Integer) As Color
        Return Black
    End Function
End Structure

Public Class Pen
    Public Sub New(color As Color)
    End Sub

    Public Sub New(color As Color, width As Single)
    End Sub
End Class

Public Class Brush

End Class

Public Class SolidBrush
    Inherits Brush

    Public Sub New(color As Color)
    End Sub
End Class

Public Class Font
    Public Height As Integer
    Public Sub New(familyName As String, emSize As Single)
    End Sub
End Class

Public Class TextRenderer
    Public Shared Function MeasureText(text As String, font As Font) As Size
        Return Nothing
    End Function
End Class

Public Class Graphics
    Public Clip As Region

    Public Shared Function FromImage(image As Image) As Graphics
        Return Nothing
    End Function

    Public Shared Function FromHdc(hdc As IntPtr) As Graphics
        Return Nothing
    End Function

    Public Shared Function FromHwnd(hwnd As IntPtr) As Graphics
        Return Nothing
    End Function

    Public Sub Clear(color As Color)
    End Sub

    Public Sub Dispose()
    End Sub

    Public Sub DrawEllipse(pen As Pen, x As Single, y As Single, width As Single, height As Single)
    End Sub

    Public Sub DrawImage(image As Image, x As Integer, y As Integer)
    End Sub

    Public Sub DrawImage(image As Image, x As Single, y As Single, srcRect As RectangleF, srcUnit As GraphicsUnit)
    End Sub

    Public Sub DrawLine(pen As Pen, x1 As Single, y1 As Single, x2 As Single, y2 As Single)
    End Sub

    Public Sub DrawCurve(pen As Pen, vpnt As PointF())
    End Sub

    Public Sub DrawRectangle(pen As Pen, x As Single, y As Single, width As Single, height As Single)
    End Sub

    Public Sub DrawString(s As String, font As Font, brush As Brush, x As Single, y As Single)
    End Sub

    Public Sub FillRectangle(brush As Brush, x As Single, y As Single, width As Single, height As Single)
    End Sub

    Public Function MeasureString(text As String, font As Font) As SizeF
        Return Nothing
    End Function
End Class

Public Class Image
    Public Sub Dispose()
    End Sub
End Class

Public Class Bitmap
    Inherits Image
    Public Sub New(width As Integer, height As Integer, format As PixelFormat)
    End Sub
End Class

Public Enum PixelFormat
    Format24bppRgb
End Enum

Public Enum GraphicsUnit
    Pixel
End Enum

Public Class Region
    Public Sub New()
    End Sub
    Public Sub New(rect As RectangleF)
    End Sub
End Class

Public Structure PointF
    Public Sub New(x As Single, y As Single)
    End Sub
End Structure

Public Class RectangleF
    Public Sub New(x As Single, y As Single, width As Single, height As Single)
    End Sub
End Class

Public Structure Size
    Public Width As Integer
    Public Height As Integer

    Public Sub New(width As Double, height As Double)
    End Sub
End Structure

Public Structure SizeF
    Public Width As Single
    Public Height As Single
End Structure

'Public Class TSysC
'    Public Shared Sub SetCursor(i As ECursor)
'    End Sub

'    Public Shared Function CreateObject(tp As Type) As Object
'    End Function
'End Class

Public Class Typeface
End Class

Public Class CultureInfo
    Public Shared CurrentCulture As CultureInfo
End Class

Public Class FlowDirection
    Public Const LeftToRight As FlowDirection
End Class

Public Class SolidColorBrush
    Inherits Brush
End Class
Public Class Brushes
    Public Shared Black As SolidColorBrush
End Class

Public Class FormattedText
    Public Width As Double
    Public Height As Double

    Public Sub New(textToFormat As String, culture As CultureInfo, flowDirection As FlowDirection, typeface As Typeface, emSize As Double, foreground As Brush)
    End Sub
End Class

Public Class WaitHandle
    Public Overridable Function WaitOne() As Boolean
        Return True
    End Function
End Class

Public Class EventWaitHandle
    Inherits WaitHandle
End Class

Public Class ManualResetEvent
    Inherits EventWaitHandle

    Public Sub New(initialState As Boolean)
    End Sub
End Class

Public Class UIElement
End Class

Public Class Canvas
    Inherits UIElement

    Public Shared Sub SetLeft(element As UIElement, length As Double)
    End Sub

    Public Shared Sub SetTop(element As UIElement, length As Double)
    End Sub
End Class

Public Class Ellipse
    Inherits UIElement

    Public Width As Double
    Public Height As Double
End Class

Public Class Thread
End Class

Public MustInherit Class Attribute
End Class