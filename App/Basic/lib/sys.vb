
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
    Public undefined As Object

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

    'Dim undefined As Object
    'Dim document As Document
    'Dim console As console

    'Function __Inc(a As number) As number
    '    Return 0
    'End Function

    'Function __Dec(a As number) As number
    '    Return 0
    'End Function

    'Function __BitOr(a As number) As number
    '    Return 0
    'End Function
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

Public Class Type
    Public Shared EmptyTypes As Type()

    Public Function ToString() As String
        Return ""
    End Function
End Class

Public Class Exception
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

Public Class Math
    Public Const PI As Double = 0

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

    Public Shared Function Cos(d As Double) As Double
        Return 0
    End Function

    Public Shared Function Sin(d As Double) As Double
        Return 0
    End Function
End Class

Public MustInherit Class Attribute
End Class
