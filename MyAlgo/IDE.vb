Imports System.Diagnostics

Public Class GNode
    Public NameNd As String
    Public ToNd As TList(Of GNode)
    Public DataNd As TFunction
    Public XNd As Double
    Public YNd As Double

    Public Sub New(name1 As String, dt As Object)
        NameNd = name1
        DataNd = CType(dt, TFunction)
    End Sub

    ' ノード間の距離を返す
    Public Function Dist(nd As GNode) As Double
        '        Return Math.Abs(Math.Max(XNd - nd.XNd, YNd - nd.YNd))
        Dim dx As Double, dy As Double

        dx = XNd - nd.XNd
        dy = YNd - nd.YNd
        Return Math.Sqrt(dx * dx + dy * dy)
    End Function
End Class

Public Class TIDE
    Inherits TApplication
    Implements ISelectedIndexChangedListener

    Public Shared theIDE As TIDE
    Public Shared SrcBrwsrMenu As TPopup
    Public PrjIDE As TProject
    Public SrcTB As TSrcEdit
    Public SrcBrwsr As TSrcBrowser
    Public SrcLB As TListBox
    Public ClaView As TTreeView
    Public ProofView As TScrollView
    Public RefGraph As TGridGraph
    Public FntIDE As TFont
    Public BrsIDE As TColor
    Public CallNode As TList(Of GNode)
    Dim wndGraph As TWnd
    Dim wndDes As TDesWnd

    Public Shared Sub Main(sz As TPnt)
        theApp = New TIDE(sz)
        theApp.LoadApp()
        For Each ev In theApp.EventApp
            theApp.EventHandler(ev)
        Next
    End Sub

    Public Sub mnuCallGraph_Clicked(e As TMouseEvent)
        Debug.WriteLine("CallGraph Click")
    End Sub

    Public Sub mnuRefGraph_Clicked(e As TMouseEvent)
        Debug.WriteLine("RefGraph Click")
    End Sub

    Public Sub メニューを作る()
        Dim btn1 As TButton, stp1 As TStackPanel

        SrcBrwsrMenu = New TPopup()

        stp1 = New TStackPanel(EOrientation.eVertical)

        btn1 = New TButton("呼び出しグラフ")
        btn1.ClickHandler = AddressOf mnuCallGraph_Clicked
        stp1.AddChildWnd(btn1)

        btn1 = New TButton("変数参照のグラフ")
        btn1.ClickHandler = AddressOf mnuRefGraph_Clicked
        stp1.AddChildWnd(btn1)

        SrcBrwsrMenu.AddChildWnd(stp1)
        SrcBrwsrMenu.SetAutoSize()
        SrcBrwsrMenu.SetPos(New TPnt(100, 100))

        MainForm.ActivePopup = SrcBrwsrMenu
    End Sub

    Public Sub ツールボックスを作る(tool_box As TPanel)
        Dim rad1 As TRadioButton, y As Double

        rad1 = New TRadioButton("ポインタ")
        rad1.PosFig = New TPnt(1, 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("直線")
        rad1.PosFig = New TPnt(1, y + 1)
        rad1.Tag = GetType(TLineFig)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("ラベル")
        rad1.PosFig = New TPnt(1, y + 1)
        rad1.Tag = GetType(TLabel)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("ボタン")
        rad1.PosFig = New TPnt(1, y + 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("ラジオボタン")
        rad1.PosFig = New TPnt(1, y + 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("パネル")
        rad1.PosFig = New TPnt(1, y + 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("スクロールバー")
        rad1.PosFig = New TPnt(1, y + 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("スクロールビュー")
        rad1.PosFig = New TPnt(1, y + 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

        rad1 = New TRadioButton("TextBox")
        rad1.PosFig = New TPnt(1, y + 1)
        tool_box.AddChildWnd(rad1)
        y = rad1.BottomWnd()

    End Sub

    Public Sub New(sz As TPnt)
        Dim spc As TSplitContainer, scv As TScrollView, tbc1 As TTabControl, tbc2 As TTabControl, tbc3 As TTabControl, tool_box As TPanel
        Dim spl1 As TSplineFig, nd1 As SNode, grp1 As SGraph

        'LALR.LALRMain("", "", "C:\usr\prj\MyIDE\MyAlgo\LALR\cpp.y")
        theIDE = Me

        FntIDE = New TFont(EFont.eGothic, 10)
        BrsIDE = TColor.Black

        MainForm = New TForm()
        MainForm.BorderColor = TColor.Green
        MainForm.Interval = 10
        MainForm.SetSize(sz)

        tbc1 = New TTabControl()
        tbc1.AnchorBottom = 1
        tbc1.SetPos(New TPnt(1, 1))
        tbc1.SetSize(New TPnt(240, MainForm.ClientSize.YPnt - 2))
        MainForm.AddChildWnd(tbc1)

        SrcLB = New TListBox()
        SrcLB.AddSelectedIndexChangedListener(Me)
        tbc1.AddTab("ｿﾘｭｰｼｮﾝ", SrcLB)

        tool_box = New TPanel()
        tbc1.AddTab("ﾂｰﾙﾎﾞｯｸｽ", tool_box)

        ClaView = New TTreeView()
        ClaView.AddTrv("はじめまして", Nothing)
        ClaView.AddTrv("どうぞよろしく", Nothing)
        tbc1.AddTab("ｸﾗｽﾋﾞｭｰ", ClaView)

        メニューを作る()

        ツールボックスを作る(tool_box)


        spc = New TSplitContainer()
        spc.AnchorRight = 1
        spc.AnchorBottom = 1
        spc.BorderColor = TColor.Blue
        spc.OrientationSpc = EOrientation.eHorizontal
        spc.RatioSpc = 0.5
        MainForm.AddChildWnd(spc)
        spc.SetPos(New TPnt(1 + tbc1.SizeFig.XPnt + 1, 1))

        tbc2 = New TTabControl()
        spc.SetSplitWind(tbc2, 0)

        SrcTB = New TSrcEdit()
        tbc2.AddTab("ソース", SrcTB)

        SrcBrwsr = New TSrcBrowser()
        tbc2.AddTab("ブラウザ", SrcBrwsr)

        wndDes = New TDesWnd(tool_box)
        tbc2.AddTab("デザイナ", wndDes)

        tbc3 = New TTabControl()
        spc.SetSplitWind(tbc3, 1)

        '-------------------------------------------------- 力学モデルのグラフ描画
        scv = New TScrollView()
        scv.BorderColor = TColor.Red
        tbc3.AddTab("グラフ", scv)

        wndGraph = New TWnd()
        wndGraph.DrawHandler = AddressOf OnGraph_Draw
        wndGraph.MouseDownHandler = AddressOf OnScrollView_MouseDown
        wndGraph.MouseMoveHandler = AddressOf OnScrollView_MouseMove
        wndGraph.BorderColor = TColor.SkyBlue
        scv.SetContent(wndGraph)

        '-------------------------------------------------- Layered graph drawing
        ProofView = New TScrollView()
        ProofView.BorderColor = TColor.Orange

        ' スプライン曲線
        spl1 = New TSplineFig()
        spl1.AddPntSpl(New TPnt(100, 100))
        spl1.AddPntSpl(New TPnt(200, 100))
        spl1.AddPntSpl(New TPnt(100, 200))
        spl1.AddPntSpl(New TPnt(300, 300))
        ProofView.AddChildFig(spl1)

        ' グラフ
        grp1 = New SGraph()
        grp1.SetPos(New TPnt(400, 400))

        nd1 = New SNode("ノード1")
        nd1.SetPos(New TPnt(50, 50))
        grp1.AddChildWnd(nd1)

        nd1 = New SNode("ノード2")
        nd1.SetPos(New TPnt(100, 100))
        grp1.AddChildWnd(nd1)

        ProofView.AddChildFig(grp1)

        tbc3.AddTab("証明", ProofView)

        MainForm.SetSize(sz)

        'MainForm.Dmp("")
    End Sub

    Public Function AA(i As Integer) As Integer
        Debug.WriteLine("AA {0}", i)
        Return i * 10
    End Function


    Public Sub MakeGridGraph(L As TList(Of TList(Of TNode)))
        ' グラフの図形を作る
        RefGraph = New TGridGraph(L)
        RefGraph.SetPos(New TPnt(50, 50))
        ProofView.SetContent(RefGraph)
        ProofView.HSbScv.Maximum = RefGraph.ClientSize.XPnt
        ProofView.VSbScv.Maximum = RefGraph.ClientSize.YPnt

        ProofView.RemakeDrawFig()
    End Sub

    Public Sub Test(project_path As String)
        Dim builder As New TBuilder

        builder.Build(project_path)

        For Each L In builder.RefSugiyamaGraph.Values
            MakeGridGraph(L)
        Next
    End Sub

    Public Overrides Sub InitApp()
        'Dim v1 = {1, 2, 3, 4, 5, 6, 7, 8, 9}
        Dim v1 As New TList(Of Integer)
        Dim v3 As TList(Of Integer)
        'Dim v2 = (From i In v1 Where i Mod 2 = 0 Select AA(i)).ToList()
        'Dim i1 As Integer, s As String = "123", o As Object, tp As Type

        If Not TSys.IsWeb Then

            TProgramTransformation.WriteTranslationTable("")
            TProgramTransformation.ReadTranslationTable()
        End If

        v1.Add(1)
        v1.Add(2)
        v1.Add(3)
        v1.Add(4)
        v1.Add(5)

        Dim v4 = (From i In v1 Where i Mod 2 = 0 Select AA(i) Take 1).ToList()
        If v4.Count <> 0 Then
            Debug.WriteLine(v4(0))
        End If

        v3 = New TList(Of Integer)(From i In v1 Where i Mod 2 = 0 Select AA(i) Take 1)
        v3 = New TList(Of Integer)(From i In v1 Select AA(i))
        v3 = New TList(Of Integer)(From i In v1 Where i Mod 2 = 0)

        'Dim v4 = (From i In v1 Select key = i.ToString(), Dat = AA(i)).ToList()

        'o = v4
        'Debug.WriteLine("type {0}", o.GetType())

        'For Each x In v4
        '    o = x
        '    tp = o.GetType()
        '    Debug.WriteLine("key:{0} dat:{1}", x.key, x.Dat)
        '    For Each m In tp.GetMembers()
        '        If m.MemberType = Reflection.MemberTypes.Property Then

        '            Debug.WriteLine("mem {0} {1}", m.Name, m.DeclaringType)
        '        End If
        '    Next
        'Next

        'Try
        '    i1 = s.Length
        '    Throw New Exception()
        'Catch ex As Exception
        '    Debug.WriteLine("")
        'End Try

        'Test("C:\usr\prj\MyIDE\InvariantBasicOrigin\MyAlgo.xml")

        SrcBrwsr.SetSrcBrw()
        SrcBrwsr.RemakeDrawFig()

        LoadPrj(TProject.Prj)
    End Sub


    ' 呼び出しグラフを作る
    Public Function MakeCallGraphNode(prj1 As TProject) As TList(Of GNode)
        Dim dic1 As New Dictionary(Of TFunction, GNode), nd1 As GNode = Nothing, nd2 As GNode = Nothing

        '  すべてのクラスに対し
        ' for ???
        For Each cla1 In prj1.SimpleParameterizedClassList
            '  すべてのメソッドに対し
            ' for ???
            For Each fnc1 In cla1.FncCla
                If fnc1.CallTo.Count <> 0 Then
                    If dic1.ContainsKey(fnc1) Then
                        nd1 = dic1(fnc1)
                    Else
                        nd1 = New GNode(fnc1.NameFnc(), fnc1)
                        dic1.Add(fnc1, nd1)
                    End If
                    ' for ???
                    For Each fnc2 In fnc1.CallTo
                        If dic1.ContainsKey(fnc2) Then
                            nd2 = dic1(fnc2)
                        Else
                            nd2 = New GNode(fnc2.NameFnc(), fnc2)
                            dic1.Add(fnc2, nd2)
                        End If
                        If nd1.ToNd Is Nothing Then
                            nd1.ToNd = New TList(Of GNode)()
                        End If
                        nd1.ToNd.Add(nd2)
                    Next
                End If
            Next
        Next

        Return New TList(Of GNode)(dic1.Values)
    End Function

    Public Sub LoadPrj(prj As TProject)
        Dim n1 As Integer, i As Integer, j As Integer, k As Integer

        PrjIDE = prj

        For Each src In prj.SrcPrj
            SrcLB.AddLB(TPath.GetFileName(src.FileSrc), src)
        Next

        If prj.SrcPrj.Count <> 0 Then
            SrcTB.SetTextLines(prj.SrcPrj(0).vTextSrc)
        End If

        ' 呼び出しグラフを作る
        CallNode = MakeCallGraphNode(prj)

        ' ノードを等間隔に配置する
        n1 = CType(Math.Ceiling(Math.Sqrt(CallNode.Count)), Integer)
        ' for Each nd In CallNode Let
        For Each nd In CallNode
            k = CallNode.IndexOf(nd)
            i = k / n1
            j = k - i * n1
            nd.YNd = 500 + i * 10
            nd.XNd = 500 + j * 10
        Next
    End Sub

    Public Sub OnSelectedIndexChanged(wnd As TWnd, old_idx As Integer, new_idx As Integer) Implements ISelectedIndexChangedListener.OnSelectedIndexChanged
        SrcTB.SrcEdit = PrjIDE.SrcPrj(new_idx)
        SrcTB.SetTextLines(SrcTB.SrcEdit.vTextSrc)
        SrcTB.RemakeDrawFig()
    End Sub

    Public Sub OnScrollView_MouseDown(e As TMouseEvent)
        Debug.WriteLine("mouse down {0}", e.PosEv)
        TWnd.Capture = wndGraph
        wndGraph.UpWnd.OnMouseDown(e)
    End Sub

    Public Sub OnScrollView_MouseMove(e As TMouseEvent)
        If TWnd.Capture Is wndGraph Then
            wndGraph.UpWnd.OnMouseMove(e)
        End If
    End Sub

    Public Sub OnGraph_Draw(fig As TFig)
        Dim pen1 As TColor, dcmp As TDrawCmp

        dcmp = New TDrawCmp()
        dcmp.AddDrawCmp(New TFillRectangle(TColor.White, New TPnt(), wndGraph.SizeFig))

        pen1 = TColor.Green
        dcmp.AddDrawCmp(New TDrawLine(pen1, 100, 100, 200, 200))
        dcmp.AddDrawCmp(New TDrawEllipse(pen1, 200, 200, 300, 400))
        dcmp.AddDrawCmp(New TDrawString("こんにちは", FntIDE, BrsIDE, New TPnt(200, 200)))

        If CallNode IsNot Nothing Then
            ' 呼び出しグラフがある場合

            ' すべてのノードを描画する
            ' for Each nd1 In CallNode Add New TDrawString(nd1.NameNd, FntIDE, BrsIDE, New TPnt(nd1.XNd, nd1.YNd)) To dcmp
            For Each nd1 In CallNode
                dcmp.AddDrawCmp(New TDrawString(nd1.NameNd, FntIDE, BrsIDE, New TPnt(nd1.XNd, nd1.YNd)))
                If nd1.ToNd IsNot Nothing Then
                    ' for Each nd2 In nd1.ToNd Add New TDrawLine(pen1, nd1.XNd, nd1.YNd, nd2.XNd, nd2.YNd) To dcmp
                    For Each nd2 In nd1.ToNd
                        dcmp.AddDrawCmp(New TDrawLine(pen1, nd1.XNd, nd1.YNd, nd2.XNd, nd2.YNd))
                    Next
                End If
            Next
        End If

        wndGraph.DrawFig = dcmp
    End Sub

    ' タイマー処理
    Public Overrides Sub OnTimer()
        Dim dx As Double, dy As Double, d As Double, xmin As Double, ymin As Double, xmax As Double, ymax As Double, w As Double, h As Double
        Dim tbc1 As TTabControl

        tbc1 = CType(wndGraph.UpWnd.UpWnd, TTabControl)
        If tbc1.TabWnd(tbc1.SelectedIndex) IsNot wndGraph.UpWnd Then
            Exit Sub
        End If

        ' ノードを再配置する
        If CallNode Is Nothing Then
            Exit Sub
        End If

        ' for Each nd1 In CallNode Let
        For Each nd1 In CallNode
            dx = 0
            dy = 0
            If nd1.ToNd IsNot Nothing Then
                ' for 数値計算
                For Each nd2 In nd1.ToNd
                    d = nd1.Dist(nd2)
                    If d > 100 Then
                        dx -= 0.01 * (nd1.XNd - nd2.XNd)
                        dy -= 0.01 * (nd1.YNd - nd2.YNd)
                    End If
                Next
            End If

            ' for 数値計算
            For Each nd2 In CallNode
                If nd2 IsNot nd1 Then
                    d = nd1.Dist(nd2)
                    If d < 100 Then
                        dx += 0.01 * (nd1.XNd - nd2.XNd)
                        dy += 0.01 * (nd1.YNd - nd2.YNd)
                    End If
                End If
            Next
            nd1.XNd += dx
            nd1.YNd += dy
        Next

        xmin = 0
        xmax = 0
        ymin = 0
        ymax = 0
        ' for 最小値や最大値を求める
        For Each nd1 In CallNode
            xmin = Math.Min(xmin, nd1.XNd)
            ymin = Math.Min(ymin, nd1.YNd)
            xmax = Math.Max(xmax, nd1.XNd)
            ymax = Math.Max(ymax, nd1.YNd)
        Next

        w = xmax - xmin
        h = ymax - ymin
        ' for Each nd1 In CallNode Let
        For Each nd1 In CallNode
            nd1.XNd = 1000 * (nd1.XNd - xmin) / w
            nd1.YNd = 1000 * (nd1.YNd - ymin) / h
        Next

        wndGraph.RemakeDrawFig()
    End Sub
End Class
