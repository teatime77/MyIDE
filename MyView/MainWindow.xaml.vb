Imports System.Threading

Public Class MainWindow
    Private Sub Window_Loaded(sender As Object, e As RoutedEventArgs)
        'Dim v1 As New List(Of TView), v As TView, d1 As Double

        'v = New TView()
        'v.DesiredWidth = 1
        'v1.Add(v)

        'v = New TView()
        'v.DesiredWidth = 3
        'v1.Add(v)

        'v = New TView()
        'v.DesiredWidth = 5
        'v1.Add(v)

        'd1 = Aggregate vi In v1 Into Sum(vi.DesiredWidth)
        'Debug.WriteLine("Sum of Desired Width {0}", d1)

        ''データソースの作成
        'Dim numbers() As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

        ''Aggregateで集計実行(即時実行)
        'Dim result = Aggregate number In numbers Into Sum()

        'Debug.WriteLine("{0}", result)

        Dim obj As New TObj(0)
        obj.Children.Add(New TObj(1))
        obj.Children.Add(New TObj(2))
        obj.Children.Add(New TObj(3))

        ParallelTest(New TSyncTest(obj, Nothing))
    End Sub

    Public Sub ParallelTest(sync As TSyncTest)
        Dim _current As TObj = sync.SelfSync

        With _current

            If .Children.Count <> 0 Then

                Dim sync_list_ = (From x In .Children Select New TSyncTest(x, sync)).ToList()
                Dim _prev_sync As TSyncTest = Nothing
                For Each _x In sync_list_
                    _x.PrevSync = _prev_sync
                    _prev_sync = _x
                Next
                Dim wait_all_1 = (From x In sync_list_ Select x.Wait_1).ToArray()
                Dim wait_all_2 = (From x In sync_list_ Select x.Wait_2).ToArray()

                Dim t = New Thread(Sub()
                                       Parallel.ForEach(sync_list_, AddressOf ParallelTest)
                                   End Sub)
                t.Start()

                For Each w In wait_all_1
                    w.WaitOne()
                Next

                'AutoResetEvent.WaitAll(wait_all_1)
                Dim n = Aggregate x In .Children Into Sum(x.Val)
                Debug.WriteLine("Sum = {0}", n)
            Else

                Dim m As Double = 0
                For i = 0 To 10000 * 1000
                    m += Math.Sin(i)
                Next
                .Val = .Id * 10
                sync.Wait_1.Set()
            End If

        End With

    End Sub
End Class

Public Class TObj
    Public Id As Integer
    Public Children As New List(Of TObj)
    Public Val As Double

    Public Sub New(i As Integer)
        Id = i
    End Sub
End Class

Public Class TSyncTest
    Public SelfSync As TObj
    Public ParentSync As TSyncTest
    Public PrevSync As TSyncTest

    Public Wait_1 As New ManualResetEvent(False)
    Public Wait_2 As New ManualResetEvent(False)

    Public Sub New(self1 As TObj, parent1 As TSyncTest)
        SelfSync = self1
        ParentSync = parent1
    End Sub
End Class