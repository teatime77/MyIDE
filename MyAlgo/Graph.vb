Imports System.Diagnostics
Imports System.IO
Imports System.Text

' -------------------------------------------------------------------------------- TGraph
Public Class TGraph
    Public Shared Function CopyNodeList(v1 As TList(Of TNode)) As TList(Of TNode)
        Dim v2 As New TList(Of TNode)

        For Each nd In v1
            v2.Add(nd)
        Next

        Return v2
    End Function

    Public Shared Function SelectSink(v1 As TList(Of TNode)) As TList(Of TNode)
        Dim v2 As New TList(Of TNode)

        For Each nd In v1
            If nd.OutEdge.Count = 0 Then
                v2.Add(nd)
            End If
        Next

        Return v2
    End Function

    Public Shared Function SelectSource(v1 As TList(Of TNode)) As TList(Of TNode)
        Dim v2 As New TList(Of TNode)

        For Each nd In v1
            If nd.InEdge.Count = 0 Then
                v2.Add(nd)
            End If
        Next

        Return v2
    End Function


    ' ノードの集合からdotファイルを作る
    Public Shared Sub Node2Dot(sw1 As TStringWriter, v1 As TList(Of TNode), L As TList(Of TList(Of TNode)))
        Dim fnc1 As TFunction, ref1 As TReference

        For Each nd1 In v1
            If TypeOf nd1.DatNode Is TFunction Then
                fnc1 = CType(nd1.DatNode, TFunction)
                sw1.WriteLine("" + vbTab + "n{0} [shape = box, label = ""{1}""];", nd1.IdxNode, fnc1.FullName())
            ElseIf TypeOf nd1.DatNode Is TReference Then
                ref1 = CType(nd1.DatNode, TReference)
                sw1.WriteLine("" + vbTab + "n{0} [shape = ellipse, label = ""{1}""];", nd1.IdxNode, ref1.NameRef)
            Else
                sw1.WriteLine("" + vbTab + "n{0} [shape = circle];", nd1.IdxNode)
            End If
        Next

        For Each nd1 In v1
            For Each nd2 In nd1.OutNode
                sw1.WriteLine("n{0} -> n{1};", nd1.IdxNode, nd2.IdxNode)
            Next
        Next

        If L IsNot Nothing Then
            For Each Lk In L
                sw1.Write("" + vbTab + "{rank = same")
                For Each nd1 In Lk
                    sw1.Write("; n{0}", nd1.IdxNode)
                Next
                sw1.WriteLine("}")
            Next
        End If
    End Sub

    ' dotファイルに書く
    Public Shared Sub WriteDotFile(title As String, vnd As TList(Of TNode), L As TList(Of TList(Of TNode)), path1 As String)
        Dim sw1 As TStringWriter

        sw1 = New TStringWriter()
        sw1.WriteLine("digraph " + title + " {")
		sw1.WriteLine("" + vbTab + "graph [charset=""UTF-8"", rankdir = LR];")

        ' ノードの集合からdotファイルを作る
        Node2Dot(sw1, vnd, L)

        sw1.WriteLine("}")

		File.WriteAllText(path1, sw1.ToString(), New UTF8Encoding(False))
    End Sub


    ' ノードの集合からグラフを作る
    Public Shared Function Node2Graph(v1 As TList(Of TFlowNode)) As TList(Of TNode)
        Dim v2 As TList(Of TFlowNode), dic1 As New Dictionary(Of TFlowNode, TNode), nd3 As TNode, nd4 As TNode, v3 As TList(Of TNode), i1 As Integer

        For Each nd1 In v1
            If dic1.ContainsKey(nd1) Then
                nd3 = dic1(nd1)
            Else
                If TypeOf nd1 Is TFncNode Then
                    nd3 = New TNode(CType(nd1, TFncNode).FncNode)
                Else
                    nd3 = New TNode(CType(nd1, TRefNode).RefNode)
                End If
                dic1.Add(nd1, nd3)
            End If

            v2 = New TList(Of TFlowNode)(nd1.ToNd.Values)
            For Each nd2 In v2
                If dic1.ContainsKey(nd2) Then
                    nd4 = dic1(nd2)
                Else
                    If TypeOf nd2 Is TFncNode Then
                        nd4 = New TNode(CType(nd2, TFncNode).FncNode)
                    Else
                        nd4 = New TNode(CType(nd2, TRefNode).RefNode)
                    End If
                    dic1.Add(nd2, nd4)

                End If

                nd3.ConnectNode(nd4)
            Next
        Next

        v3 = New TList(Of TNode)(dic1.Values)
        For i1 = 0 To v3.Count - 1
            v3(i1).IdxNode = i1
        Next
        Return v3
    End Function
End Class

' -------------------------------------------------------------------------------- TNode
Public Class TNode
    Public Shared CntNode As Integer
    Public DatNode As Object
    Public IdxNode As Integer
    Public InEdge As New TList(Of TEdge)
    Public OutEdge As New TList(Of TEdge)
    Public InNode As New TList(Of TNode)
    Public OutNode As New TList(Of TNode)
    Public LayIdx As Integer
    Public MedNode As Single

    Public Sub New(dat As Object)
        DatNode = dat
    End Sub

    Public Overrides Function ToString() As String
        If DatNode Is Nothing Then
            Return String.Format("n{0}", IdxNode)
        Else
            Return DatNode.ToString()
        End If
    End Function

    Public Shared Sub RenumNode(v1 As TList(Of TNode))
        Dim i As Integer

        For i = 0 To v1.Count - 1
            v1(i).IdxNode = i
        Next

        CntNode = v1.Count
    End Sub


    Public Function ConnectNode(dst As TNode) As TEdge
        Dim src As TNode, ed1 As New TEdge

        src = Me

        ed1.SrcNode = src
        ed1.DstNode = dst

        src.OutNode.Add(dst)
        dst.InNode.Add(src)

        src.OutEdge.Add(ed1)
        dst.InEdge.Add(ed1)

        Return ed1
    End Function
End Class

' -------------------------------------------------------------------------------- TEdge
Public Class TEdge
    Public SrcNode As TNode
    Public DstNode As TNode

    Public Sub New()
    End Sub

    Public Sub CheckEdge()
        Debug.Assert(SrcNode.OutEdge.Contains(Me))
        Debug.Assert(DstNode.InEdge.Contains(Me))
        Debug.Assert(SrcNode.OutNode.Contains(DstNode))
        Debug.Assert(DstNode.InNode.Contains(SrcNode))
    End Sub

    Public Sub Detach()
        CheckEdge()

        SrcNode.OutEdge.Remove(Me)
        DstNode.InEdge.Remove(Me)

        SrcNode.OutNode.Remove(DstNode)
        DstNode.InNode.Remove(SrcNode)
    End Sub

    Public Sub Attach()
        Debug.Assert(Not SrcNode.OutEdge.Contains(Me))
        Debug.Assert(Not DstNode.InEdge.Contains(Me))
        Debug.Assert(Not SrcNode.OutNode.Contains(DstNode))
        Debug.Assert(Not DstNode.InNode.Contains(SrcNode))

        SrcNode.OutEdge.Add(Me)
        DstNode.InEdge.Add(Me)

        SrcNode.OutNode.Add(DstNode)
        DstNode.InNode.Add(SrcNode)
    End Sub
End Class


' -------------------------------------------------------------------------------- TDrawGraph
Public Class TDrawGraph
    Public AllNode As TList(Of TNode)
    Public RemSelfEdge As New TList(Of TEdge)
    Public RemEdge As New TList(Of TEdge)

    Public Sub New(vnd As TList(Of TNode))
        AllNode = vnd
    End Sub

    Public Shared Sub CheckGraph(vnd As TList(Of TNode))
        For Each nd In vnd
            Debug.Assert(nd.InEdge.Count = nd.InNode.Count)
            Debug.Assert(nd.OutEdge.Count = nd.OutNode.Count)

            For Each ed In nd.InEdge
                ed.CheckEdge()
            Next
            For Each ed In nd.OutEdge
                ed.CheckEdge()
            Next
        Next
    End Sub

    ' 自己循環または2個の循環の辺を取り除く
    Public Sub RemoveSelfTwoCycle()
        Dim v1 As New TList(Of TNode)    ' 未処理ノード列
        Dim v2 As New TList(Of TNode)    ' 未処理ノード列
        Dim vdone As New TList(Of TNode)    ' 処理済みノード列
        Dim i1 As Integer, ed1 As TEdge, dst As TNode
        Dim rem_src_to_dst As Boolean

        For Each nd In AllNode

            ' 自分に行く辺を取り除く
            For i1 = nd.OutEdge.Count - 1 To 0 Step -1
                ed1 = nd.OutEdge(i1)
                If ed1.DstNode Is nd Then
                    ' 自分に行く場合

                    ' 辺を切り離してから、除去リストに入れる
                    ed1.Detach()
                    RemSelfEdge.Add(ed1)
                    Exit For
                End If
            Next

            If nd.InNode.Count = 0 Then
                ' ソースの場合

                v1.Add(nd)
            End If
        Next

        Do While v1.Count <> 0
            For Each src In v1
                ' 処理済みにする
                vdone.Add(src)

                ' すべての行き先の辺に対し
                For i1 = src.OutEdge.Count - 1 To 0 Step -1
                    ed1 = src.OutEdge(i1)
                    dst = ed1.DstNode

                    If dst.OutNode.Contains(src) Then
                        ' 行き先のノードから自分に戻る場合

                        ' ノードをソースに変えない。シンクに変えるのはよい。
                        If dst.InNode.Count = 1 Then
                            ' dstの唯一の入力の場合、dstをソースに変えない

                            rem_src_to_dst = False
                        ElseIf src.InNode.Count = 1 Then
                            ' srcの唯一の入力の場合、srcをソースに変えない

                            rem_src_to_dst = True
                        Else

                            rem_src_to_dst = False
                        End If

                        If rem_src_to_dst Then
                            ' srcから出る辺を取り除く場合

                            'If ed1.DstNode.InNode.Count = 1 Then
                            '    Debug.WriteLine("削除1 X {0}→{1}", ed1.SrcNode.DatNode.ToString(), ed1.DstNode.DatNode.ToString())
                            'Else
                            '    Debug.WriteLine("削除1  {0}→{1}", ed1.SrcNode.DatNode.ToString(), ed1.DstNode.DatNode.ToString())
                            'End If
                            ed1.Detach()
                            RemSelfEdge.Add(ed1)
                        Else
                            ' dstから出る辺を取り除く場合

                            For Each ed2 In dst.OutEdge
                                If ed2.DstNode Is src Then
                                    ' dstから戻る辺の場合

                                    'If ed2.DstNode.InNode.Count = 1 Then
                                    '    Debug.WriteLine("削除2 X {0}→{1}", ed2.SrcNode.DatNode.ToString(), ed2.DstNode.DatNode.ToString())
                                    'Else
                                    '    Debug.WriteLine("削除2  {0}→{1}", ed2.SrcNode.DatNode.ToString(), ed2.DstNode.DatNode.ToString())
                                    'End If
                                    ed2.Detach()
                                    RemSelfEdge.Add(ed2)
                                    Exit For
                                End If
                            Next
                        End If
                    End If

                    If Not vdone.Contains(dst) AndAlso Not v1.Contains(dst) AndAlso Not v2.Contains(dst) Then
                        ' 未処理で処理予定でない場合

                        v2.Add(dst)
                    End If
                Next

            Next

            v1.Clear()
            v1.AddRange(v2)
            v2.Clear()
        Loop
    End Sub

    ' 循環を取り除く
    Public Sub RemoveCycle()
        Dim G As TList(Of TNode), Ea As New TList(Of TEdge), vsink As TList(Of TNode), vsource As TList(Of TNode), i1 As Integer, ed1 As TEdge, nd1 As TNode
        Dim max_d As Integer, max_nd As TNode, d As Integer

        G = TGraph.CopyNodeList(AllNode)
        Do While G.Count <> 0
            Do While True
                ' シンクの列を得る
                vsink = TGraph.SelectSink(G)
                If vsink.Count = 0 Then
                    ' シンクがない場合
                    Exit Do
                End If

                ' すべてのシンクに対し
                For Each nd In vsink
                    ' ndに入るすべての辺に対し
                    For i1 = nd.InEdge.Count - 1 To 0 Step -1
                        ed1 = nd.InEdge(i1)
                        ed1.Detach()

                        Ea.Add(ed1)
                    Next

                    ' Gから取り除く
                    G.Remove(nd)
                Next
            Loop

            ' 辺のないノードをGから取り除く
            For i1 = G.Count - 1 To 0 Step -1
                nd1 = G(i1)
                If nd1.InEdge.Count = 0 AndAlso nd1.OutEdge.Count = 0 Then
                    ' 辺がない場合

                    ' Gから取り除く
                    G.RemoveAt(i1)
                End If
            Next

            Do While True
                ' ソースの列を得る
                vsource = TGraph.SelectSource(G)
                If vsource.Count = 0 Then
                    ' ソースがない場合
                    Exit Do
                End If

                ' すべてのソースに対し
                For Each nd In vsource
                    ' ndから出るすべての辺に対し
                    For i1 = nd.OutEdge.Count - 1 To 0 Step -1
                        ed1 = nd.OutEdge(i1)
                        ed1.Detach()

                        Ea.Add(ed1)
                    Next

                    ' Gから取り除く
                    G.Remove(nd)
                Next
            Loop

            If G.Count = 0 Then
                Exit Do
            End If

            ' 出る辺の数-入る辺の数が最大のノードを探す
            max_nd = Nothing
            For Each nd In G
                d = nd.OutEdge.Count - nd.InEdge.Count
                If max_nd Is Nothing OrElse max_d < d Then
                    max_nd = nd
                    max_d = d
                End If
            Next

            ' max_ndから出る辺をEaに入れる
            Ea.AddRange(max_nd.OutEdge)
            For i1 = max_nd.OutEdge.Count - 1 To 0 Step -1
                ed1 = max_nd.OutEdge(i1)
                ed1.Detach()
            Next

            ' max_ndに入る辺は取り除く
            For i1 = max_nd.InEdge.Count - 1 To 0 Step -1
                ed1 = max_nd.InEdge(i1)
                'If ed1.DstNode.InNode.Count = 1 Then
                '    Debug.WriteLine("削除3 X {0}→{1}", ed1.SrcNode.DatNode.ToString(), ed1.DstNode.DatNode.ToString())
                'Else
                '    Debug.WriteLine("削除3   {0}→{1}", ed1.SrcNode.DatNode.ToString(), ed1.DstNode.DatNode.ToString())
                'End If
                ed1.Detach()
                RemEdge.Add(ed1)
            Next

            G.Remove(max_nd)
        Loop

        For Each ed In Ea
            ed.Attach()
        Next
    End Sub

    ' 推移閉包
    Public Sub TransitiveClosure(G As TList(Of TNode))
        Dim m As Integer(,), i1 As Integer, i2 As Integer, i3 As Integer, nd1 As TNode, changed As Boolean, ed1 As TEdge, idx As Integer

        ReDim m(G.Count - 1, G.Count - 1)

        ' すべてのノードに番号を付ける
        For i1 = 0 To G.Count - 1
            G(i1).IdxNode = i1
        Next

        For i1 = 0 To G.Count - 1
            nd1 = G(i1)
            For Each nd2 In nd1.OutNode
                m(nd1.IdxNode, nd2.IdxNode) = 1
            Next
        Next

        For idx = 1 To 1000
            Debug.WriteLine("推移閉包 {0}", idx)

            changed = False
            For i1 = 0 To G.Count - 1
                For i2 = 0 To G.Count - 1
                    If m(i1, i2) <> 0 Then
                        For i3 = 0 To G.Count - 1
                            If m(i2, i3) <> 0 AndAlso m(i1, i3) <> 2 Then
                                changed = True
                                m(i1, i3) = 2
                            End If
                        Next
                    End If
                Next
            Next

            If Not changed Then
                Exit For
            End If
        Next

        For i1 = 0 To G.Count - 1
            nd1 = G(i1)
            For i2 = nd1.OutEdge.Count - 1 To 0 Step -1
                ed1 = nd1.OutEdge(i2)

                If m(i1, ed1.DstNode.IdxNode) = 2 Then
                    ' 推移の場合

                    'If ed1.DstNode.InNode.Count = 1 Then
                    '    Debug.WriteLine("削除 推移 X {0}→{1}", ed1.SrcNode.DatNode.ToString(), ed1.DstNode.DatNode.ToString())
                    'Else
                    '    Debug.WriteLine("削除 推移   {0}→{1}", ed1.SrcNode.DatNode.ToString(), ed1.DstNode.DatNode.ToString())
                    'End If
                    ed1.Detach()
                    RemEdge.Add(ed1)
                End If
            Next
        Next
    End Sub

    Public Function MaxLmbIdx(lmb As Integer(), A As TList(Of TNode)) As Integer
        Dim max_i As Integer, max_lmb As Integer, i1 As Integer, n As Integer

        max_i = 0
        max_lmb = lmb(0)
        For i1 = 1 To A.Count - 1
            n = lmb(A(i1).IdxNode)
            If max_lmb < n Then
                max_i = i1
                max_lmb = n
            End If
        Next

        Return max_i
    End Function

    ' A < B ならTrueを返す
    Public Function Compare(lmb As Integer(), A As TList(Of TNode), B As TList(Of TNode)) As Boolean
        Dim a_i As Integer, b_i As Integer, a_max As Integer, b_max As Integer, A_ As TList(Of TNode), B_ As TList(Of TNode)

        If A.Count = 0 Then
            Return True
        End If

        If B.Count = 0 Then
            Return False
        End If

        a_i = MaxLmbIdx(lmb, A)
        b_i = MaxLmbIdx(lmb, B)

        a_max = lmb(A(a_i).IdxNode)
        b_max = lmb(B(b_i).IdxNode)

        If a_max < b_max Then
            Return True
        End If
        If b_max < a_max Then
            Return False
        End If

        A_ = TGraph.CopyNodeList(A)
        B_ = TGraph.CopyNodeList(B)

        A_.RemoveAt(a_i)
        B_.RemoveAt(b_i)

        Return Compare(lmb, A_, B_)
    End Function

    ' A ⊆ B ならTrueを返す
    Public Function IsSubset(A As TList(Of TNode), B As TList(Of TNode)) As Boolean
        ' すべてのAの要素に対し
        For Each nd In A
            If Not B.Contains(nd) Then
                ' Bに含まれない場合

                Return False
            End If
        Next

        Return True
    End Function

    Public Function CoffmanGraham(G As TList(Of TNode), w As Integer) As TList(Of TList(Of TNode))
        Dim lmb As Integer(), i1 As Integer, i2 As Integer, i3 As Integer, min_i As Integer, vdone As New TList(Of TNode), nd1 As TNode, max_nd As TNode, max_lmb As Integer
        Dim k As Integer, L As New TList(Of TList(Of TNode)), Lunion As New TList(Of TNode), n As Integer

        '------------------------------ 第一段階
        ReDim lmb(G.Count - 1)
        For i1 = 0 To G.Count - 1
            lmb(i1) = G.Count
        Next

        For i1 = 0 To G.Count - 1
            For i2 = 0 To G.Count - 1
                If lmb(i2) = G.Count Then
                    min_i = i2
                    For i3 = i2 + 1 To G.Count - 1
                        If lmb(i3) = G.Count AndAlso Compare(lmb, G(i3).InNode, G(min_i).InNode) Then
                            ' i3 << min_i の場合
                            min_i = i3
                        End If
                    Next

                    lmb(min_i) = i1
                    Exit For
                End If
            Next
        Next

        '------------------------------ 第二段階
        k = 0
        L.Add(New TList(Of TNode)())

        Do While vdone.Count < G.Count
            max_nd = Nothing
            max_lmb = -1
            For i1 = 0 To G.Count - 1
                nd1 = G(i1)
                If Not vdone.Contains(nd1) Then
                    ' 未処理の場合

                    If IsSubset(nd1.OutNode, vdone) Then
                        ' nd1の行き先がすべて処理済みの場合

                        n = lmb(nd1.IdxNode)
                        If max_lmb < n Then
                            ' λが最大の場合

                            max_lmb = n
                            max_nd = nd1
                        End If
                    End If
                End If
            Next

            Debug.Assert(max_nd IsNot Nothing)
            If L(k).Count < w AndAlso IsSubset(max_nd.OutNode, Lunion) Then
            Else
                Lunion.AddRange(L(k))

                k += 1
                L.Add(New TList(Of TNode)())
            End If
            L(k).Add(max_nd)
            max_nd.LayIdx = k

            vdone.Add(max_nd)
        Loop

        ' レイヤの順序を反転してソースのレベルを0にする
        L.Reverse()
        For k = 0 To L.Count - 1
            For Each nd In L(k)
                nd.LayIdx = k
            Next
        Next

        ' 削除した辺を復元する
        'For Each ed In RemEdge
        '    ed.Attach()
        'Next

        Return L
    End Function

    ' ダミーノードを追加する
    Public Sub AddDummyNode(G As TList(Of TNode), L As TList(Of TList(Of TNode)))
        Dim nd2 As TNode, nd3 As TNode, v As TList(Of TEdge), g2 As New TList(Of TNode), dst As TNode, dir1 As Integer

        For Each nd In G

            ' nd.OutNodeは変化しうるのでコピーする
            v = New TList(Of TEdge)(nd.OutEdge)
            For Each ed1 In v
                dst = ed1.DstNode
                Debug.Assert(nd.LayIdx <> dst.LayIdx)

                If 1 < Math.Abs(nd.LayIdx - dst.LayIdx) Then
                    ' レイヤーが離れている場合

                    If nd.LayIdx < dst.LayIdx Then
                        dir1 = 1
                    Else
                        dir1 = -1
                    End If

                    ed1.Detach()

                    nd2 = nd
                    Do While True

                        ' ダミーノードを作る
                        nd3 = New TNode(Nothing)
                        nd3.LayIdx = nd2.LayIdx + dir1
                        nd2.ConnectNode(nd3)
                        g2.Add(nd3)
                        L(nd3.LayIdx).Add(nd3)

                        If nd3.LayIdx + dir1 = dst.LayIdx Then
                            nd3.ConnectNode(dst)
                            Exit Do
                        End If

                        nd2 = nd3
                    Loop
                End If
            Next
        Next

        G.AddRange(g2)
        TNode.RenumNode(G)
    End Sub

    ' 中央値を得る
    Public Function Median(v As TList(Of Integer)) As Single
        Dim i1 As Integer

        i1 = v.Count / 2
        If v.Count Mod 2 = 0 Then
            ' 個数が偶数の場合

            Return (v(i1 - 1) + v(i1)) / 2.0F
        Else
            ' 個数が奇数の場合

            Return v(i1)
        End If
    End Function

    ' ノードを中央値で比較する
    Shared Function CompareNodeByMedian(x As TNode, y As TNode) As Integer
        If x.MedNode > y.MedNode Then
            Return 1
        ElseIf x.MedNode < y.MedNode Then
            Return -1
        Else
            Return 0
        End If
    End Function

    Public Sub CrossingReduction(G As TList(Of TNode), L As TList(Of TList(Of TNode)))
        Dim k As Integer, v As New TList(Of Integer), idx As Integer, vnd As New TList(Of TNode)

        ' 上から2番目のレイヤーから最下位のレイヤーまで
        For k = 1 To L.Count - 1

            ' レイヤー内のすべてのノードに対し
            For Each nd1 In L(k)
                v.Clear()

                vnd.Clear()
                vnd.AddRange(nd1.InNode)
                vnd.AddRange(nd1.OutNode)

                ' すべての入出力ノードに対し
                For Each nd2 In vnd

                    If nd2.LayIdx = nd1.LayIdx - 1 Then

                        ' レイヤー内の位置を得る
                        idx = L(k - 1).IndexOf(nd2)
                        Debug.Assert(idx <> -1)
                        v.Add(idx)
                    Else
                        Debug.Assert(nd2.LayIdx = nd1.LayIdx + 1)
                    End If
                Next

                v.Sort()

                If v.Count = 0 Then
                    nd1.MedNode = 0
                Else

                    ' 中央値を得る
                    nd1.MedNode = Median(v)
                End If
            Next

            ' ノードを中央値でソートする
            L(k).Sort(AddressOf CompareNodeByMedian)
        Next
    End Sub


    Public Function SugiyamaGraph(dot_path As String) As TList(Of TList(Of TNode))
        Dim dic1 As New Dictionary(Of Object, TFlowNode)
        Dim L As TList(Of TList(Of TNode))

        ' 自己循環または2個の循環の辺を取り除く
        RemoveSelfTwoCycle()
        TDrawGraph.CheckGraph(AllNode)

        ' dotファイルに書く
        TGraph.WriteDotFile("自己循環", AllNode, Nothing, dot_path + "-2.dot")

        ' 循環を取り除く
        RemoveCycle()
        TDrawGraph.CheckGraph(AllNode)

        ' dotファイルに書く
        TGraph.WriteDotFile("循環", AllNode, Nothing, dot_path + "-3.dot")

        ' 推移閉包
        TransitiveClosure(AllNode)
        TDrawGraph.CheckGraph(AllNode)

        ' dotファイルに書く
        TGraph.WriteDotFile("推移閉包", AllNode, Nothing, dot_path + "-4.dot")

        ' CoffmanGraham
        L = CoffmanGraham(AllNode, 100)
        TDrawGraph.CheckGraph(AllNode)

        ' dotファイルに書く
        TGraph.WriteDotFile("CoffmanGraham", AllNode, L, dot_path + "-5.dot")

        ' ダミーノードを追加する
        AddDummyNode(AllNode, L)
        TDrawGraph.CheckGraph(AllNode)

        ' dotファイルに書く
        TGraph.WriteDotFile("ダミーノードを追加", AllNode, L, dot_path + "-6.dot")

        CrossingReduction(AllNode, L)
        TDrawGraph.CheckGraph(AllNode)

        ' dotファイルに書く
        TGraph.WriteDotFile("CrossingReduction", AllNode, L, dot_path + "-7.dot")

        Return L
    End Function
End Class
