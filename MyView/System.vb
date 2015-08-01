Imports System.IO
Imports System.Text
Imports System.Reflection

' -------------------------------------------------------------------------------- TPnt
Public Structure TPnt
	Public XPnt As Double
	Public YPnt As Double

	Public Overrides Function ToString() As String
		Return TSys.Format("({0:0.0} {1:0.0})" , XPnt , YPnt)
	End Function 

End Structure 

'  -------------------------------------------------------------------------------- TRect
Public Class TRect
End Class 

' -------------------------------------------------------------------------------- EExpand
Public Enum EExpand
	eNone
	eShow
	eHide
	eCollapse
	eExpand
End Enum 

'  -------------------------------------------------------------------------------- EFont
Public Enum EFont
	eGothic
End Enum 

Public Interface IUpList
End Interface 

Public Class TList(Of T)
	Inherits List(Of T)
	 Implements IUpList

	Public UpList As Object

	Public Sub New ()
		MyBase .New ()
	End Sub 


	Public Sub New (v As IEnumerable(Of T))
		MyBase .New (v)
	End Sub 

End Class 

'  -------------------------------------------------------------------------------- TSys
Public Class TSys
	Public Shared Function Substring(s As String , st As Integer , ed As Integer) As String
		Return s.Substring(st , ed - st)
	End Function 


	Public Shared Function StringRepeat(s As Char , cnt As Integer) As String
		Return New String(s , cnt)
	End Function 


	Public Shared Function StringRepeat(s As String , cnt As Integer) As String
		Return New String(s(0) , cnt)
	End Function 


	Public Shared Function Format(fmt As String , arg1 As Object) As String
		Return String.Format(fmt , arg1)
	End Function 


	Public Shared Function Format(fmt As String , arg1 As Object , arg2 As Object) As String
		Return String.Format(fmt , arg1 , arg2)
	End Function 


	Public Shared Function Format(fmt As String , arg1 As Object , arg2 As Object , arg3 As Object) As String
		Return String.Format(fmt , arg1 , arg2 , arg3)
	End Function 


	Public Shared Function Format(fmt As String , arg1 As Object , arg2 As Object , arg3 As Object , arg4 As Object) As String
		Return String.Format(fmt , arg1 , arg2 , arg3 , arg4)
	End Function 

End Class 

'  -------------------------------------------------------------------------------- TUtil
Public Class TUtil(Of T)
End Class 

'  -------------------------------------------------------------------------------- TPath
Public Class TPath
	Public Shared Function GetFileName(path1 As String) As String
		Return Path.GetFileName(path1)
	End Function 


	Public Shared Function GetFileNameWithoutExtension(path1 As String) As String

		Return Path.GetFileNameWithoutExtension(path1)
	End Function 

End Class 

'  -------------------------------------------------------------------------------- TFile
Public Class TFile
	Public Shared Sub WriteAllText(path As String , contents As String)
		File.WriteAllText(path , contents , Encoding.UTF8)
	End Sub 


	Public Shared Function ReadAllLines(path As String) As String()
		Return File.ReadAllLines(path)
	End Function 

End Class 

'  -------------------------------------------------------------------------------- TDirectory
Public Class TDirectory
	Public Shared Function GetFiles(path As String , searchPattern As String) As TList(Of String)
		Return New TList(Of String)(Directory.GetFiles(path , searchPattern))
	End Function 

End Class 

'  -------------------------------------------------------------------------------- TStringWriter
Public Class TStringWriter
	Dim SW As StringWriter

	Public Sub New ()
		SW = New StringWriter()
	End Sub 


	Public Sub Append(c As Char)
		SW.Write(c)
	End Sub 


	Public Sub Append(s As String)
		SW.Write(s)
	End Sub 


	Public Sub Write(s As String)
		SW.Write(s)
	End Sub 


	Public Sub Write(fmt As String ,  ParamArray args As Object())
		SW.Write(fmt , args)
	End Sub 


	Public Sub WriteLine(s As String)
		SW.WriteLine(s)
	End Sub 


	Public Sub WriteLine(fmt As String ,  ParamArray args As Object())
		SW.WriteLine(fmt , args)
	End Sub 


	Public Overrides Function ToString() As String
		Return SW.ToString()
	End Function 

End Class 
