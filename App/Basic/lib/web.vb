Public Class _Weak
    Inherits Attribute

    Public Sub New()
    End Sub
End Class

Public Class _Invariant
    Inherits Attribute

    Public Sub New()
    End Sub
End Class

Public Class _Parent
    Inherits Attribute

    Public Sub New()
    End Sub
End Class

Public Class TList(Of T)
    Inherits List(Of T)

    <_Parent()> Public UpList As Object

    Public Sub push(x As T)
        Add(x)
    End Sub

    Public Function pop() As T
        Dim x As T = Me(Count - 1)
        RemoveAt(Count - 1)
        Return x
    End Function
End Class

Public Class CanvasRenderingContext2D
    Public fillStyle As String
    Public strokeStyle As String
    Public font As String
    Public textBaseline As String

    Sub beginPath()
    End Sub

    Sub moveTo(x As Double, y As Double)
    End Sub

    Sub lineTo(x As Double, y As Double)
    End Sub

    Sub closePath()
    End Sub

    Sub stroke()
    End Sub

    Sub fillRect(x As Double, y As Double, w As Double, h As Double)
    End Sub

    Sub strokeRect(x As Double, y As Double, w As Double, h As Double)
    End Sub

    Sub clearRect(x As Double, y As Double, w As Double, h As Double)
    End Sub

    Sub arc(x As Double, y As Double, radius As Double, startAngle As Double, endAngle As Double, anticlockwise As Boolean)
    End Sub

    Sub fill()
    End Sub

    Sub strokeText(text As String, x As Double, y As Double, maxWidth As Double)
    End Sub

    Sub fillText(text As String, x As Double, y As Double, maxWidth As Double)
    End Sub

    Sub drawImage(image As Image, offsetX As Double, offsetY As Double)
    End Sub

    Sub save()
    End Sub

    Sub restore()
    End Sub

    Sub translate(x As Double, y As Double)
    End Sub

    Sub rotate(angle As Double)
    End Sub

    Sub scale(x As Double, y As Double)
    End Sub

    Sub setTransform(m11 As Double, m12 As Double, m21 As Double, m22 As Double, dx As Double, dy As Double)
    End Sub
End Class

Public Class HTMLElement
End Class

Public Class HTMLCanvasElement
    Inherits HTMLElement
    Public height As Double
    Public width As Double
    Function getContext(s As String) As CanvasRenderingContext2D
        Return Nothing
    End Function
End Class

Public Class Image
    Inherits HTMLElement
    Public src As String
    Public complete As Boolean
    Public width As Double = Double.NaN
    Public height As Double = Double.NaN
End Class

Public Class Document
    Function getElementById(id As String) As HTMLElement
        Return Nothing
    End Function
End Class

Public Class Element
End Class

Public Class EventTarget
End Class

Public Class MouseEvent
    Public altKey As Boolean
    Public button As Double
    Public buttons As Double
    Public clientX As Double
    Public clientY As Double
    Public ctrlKey As Boolean
    Public fromElement As Element
    Public layerX As Double
    Public layerY As Double
    Public metaKey As Boolean
    Public movementX As Double
    Public movementY As Double
    Public offsetX As Double
    Public offsetY As Double
    Public pageX As Double
    Public pageY As Double
    Public relatedTarget As EventTarget
    Public screenX As Double
    Public screenY As Double
    Public shiftKey As Boolean
    Public toElement As Element
    Public which As Double
    Public x As Double
    Public y As Double
End Class

Public Class ClientRect
    Public bottom As Double
    Public height As Double
    Public left As Double
    Public right As Double
    Public top As Double
    Public width As Double
End Class

Public Class console
    Shared Sub log(msg As String)
    End Sub
End Class
