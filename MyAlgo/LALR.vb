Imports System.Diagnostics

'	文法記号
Public Class GrammarSymbol
    Public gramId As Integer
    Public gramName As String
    Public gramSym As String

    Public First As List(Of Terminal)   '	開始記号集合

    Public Sub initGrammarSymbol(i1 As Integer, name As String, sym As String)
        gramId = i1
        gramName = name
        gramSym = "e" + sym
    End Sub

End Class

'	終端記号
Public Class Terminal
    Inherits GrammarSymbol


    Public Sub New(i1 As Integer, name As String, sym As String)
        initGrammarSymbol(i1, name, sym)
        First = New List(Of Terminal)()
        First.Add(Me)
    End Sub
End Class

'	非終端記号
Public Class NonTerminal
    Inherits GrammarSymbol
    Public nontP As List(Of Production) '	生成規則の集合

    Public Sub New(i1 As Integer, name As String)
        initGrammarSymbol(i1, name, name)
        First = New List(Of Terminal)()
        nontP = New List(Of Production)()
    End Sub
End Class

'	生成規則
Public Class Production
    Public prodId As Integer
    Public prodNonT As NonTerminal
    Public prodGram As List(Of GrammarSymbol)
    Public prodText As String

    Public Sub New(la1 As LALR, nont As NonTerminal)
        prodNonT = nont
        prodGram = New List(Of GrammarSymbol)()
        prodId = la1.prodCnt
        la1.prodCnt += 1
    End Sub
End Class

'	LALR(1)項
Public Class LALRItem
    Public itemP As Production      '	生成規則
    Public itemDot As Integer   '	ドットの位置
    Public lookahead As List(Of Terminal)   '	先読みの集合
    Public itemProp As List(Of LALRItem)    '	先読みの伝播先
    Public itemGoto As LALRItem

    Public Sub New(prod1 As Production, dot1 As Integer)
        itemP = prod1
        itemDot = dot1
        lookahead = New List(Of Terminal)()
        itemProp = New List(Of LALRItem)()

    End Sub

End Class

'	状態
Public Class LALRState
    Public statId As Integer
    Public statItems As List(Of LALRItem)   '	LALR(1)項の集合
    Public gotoState As List(Of LALRState)  '	行き先
    Public gotoLabel As List(Of GrammarSymbol)  '	ラベル

    Public Sub New(vitem1 As List(Of LALRItem))
        statItems = vitem1
        gotoState = New List(Of LALRState)()
        gotoLabel = New List(Of GrammarSymbol)()

    End Sub
End Class

Public Class LALR
    Public Shared fileName As String
    Public Shared LogBuf As New TStringWriter

    Public packageName As String
    Public LexName As String
    Public eps As Terminal          '	ε
    Public eofTerm As Terminal      '	EOF
    Public Terminals As List(Of Terminal)       '	終端記号の集合
    Public NonTerminals As List(Of NonTerminal) '	非終端記号の集合
    Public GrammarSymbols As List(Of GrammarSymbol)
    Public States As List(Of LALRState)
    Public topProd As Production
    Public topItem As LALRItem
    Public srcChars As String
    Public srcIdx As Integer
    Public HeaderText As String
    Public ParserName As String
    Public prodCnt As Integer

    Public Shared Sub ExitApp(s As String)
        Debug.WriteLine("Exit App" + s)
    End Sub


    Public Sub New()
        Terminals = New List(Of Terminal)()
        NonTerminals = New List(Of NonTerminal)()
        eps = New Terminal(-1, "eps", Nothing)
    End Sub

    Public Shared Sub LALRMain(package_name As String, lex_name As String, file_name As String)
        Dim la1 As LALR

        la1 = New LALR()
        la1.packageName = package_name
        la1.LexName = lex_name
        fileName = file_name
        Main(la1)
    End Sub



    ' ([ \t\r\n]*(//.*\n)?)*
    Public Shared Sub skipWhite(la1 As LALR)
        Do While True
            Do While la1.srcIdx < la1.srcChars.Length AndAlso Char.IsWhiteSpace(la1.srcChars(la1.srcIdx))
                la1.srcIdx += 1
            Loop
            If la1.srcIdx + 1 < la1.srcChars.Length AndAlso la1.srcChars(la1.srcIdx) = "/"c AndAlso la1.srcChars(la1.srcIdx + 1) = "/"c Then
                Do While la1.srcIdx < la1.srcChars.Length AndAlso la1.srcChars(la1.srcIdx) <> vbLf
                    la1.srcIdx += 1
                Loop
            Else
                Exit Do
            End If
        Loop
    End Sub

    '	非終端記号を探す

    Public Shared Function findNonTerminal(vnont1 As List(Of NonTerminal), name1 As String) As Integer
        Dim nonterm1 As NonTerminal
        Dim i1 As Integer

        For i1 = 0 To vnont1.Count - 1
            nonterm1 = vnont1(i1)
            If nonterm1.gramName = name1 Then
                Return i1
            End If
        Next
        Return -1
    End Function

    ' 終端記号を探す
    Public Shared Function findTerminal(vterm1 As List(Of Terminal), name1 As String) As Terminal
        Dim term1 As Terminal

        For Each term1 In vterm1
            If term1.gramName = name1 Then
                Return term1
            End If
        Next
        Return Nothing
    End Function

    ' 非終端記号を探す。無ければ作る
    Public Shared Function regNonTerminal(vnont1 As List(Of NonTerminal), name1 As String) As NonTerminal
        Dim nonterm1 As NonTerminal
        Dim idx As Integer

        idx = findNonTerminal(vnont1, name1)
        If idx = -1 Then
            nonterm1 = New NonTerminal(vnont1.Count, name1)
            vnont1.Add(nonterm1)
        Else
            nonterm1 = vnont1(idx)
        End If
        Return nonterm1
    End Function

    '  未定義の非終端記号が無いか調べる
    '  省略可能の非終端記号(_opt)を作る
    Public Shared Sub chkGrammar(la1 As LALR, vnont1 As List(Of NonTerminal))
        Dim prod1 As Production
        Dim nonterm1 As NonTerminal
        Dim str1 As String
        Dim idx As Integer

        Dim len As Integer
        Dim iserr As Boolean

        iserr = False
        For Each nonterm1 In vnont1
            If nonterm1.nontP.Count = 0 Then
                ' 生成規則がない場合

                str1 = nonterm1.gramName
                len = str1.Length
                If 4 < len AndAlso str1.Substring(len - 4) = "_opt" Then
                    ' 省略可能の非終端記号(_opt)の場合

                    idx = findNonTerminal(vnont1, str1.Substring(0, len - 4))
                    If idx = -1 Then
                        iserr = True
                        Debug.WriteLine("未定義:" + str1)
                    Else
                        la1.NonTerminals.Add(nonterm1)

                        ' 空の生成規則を作る
                        nonterm1.nontP.Add(New Production(la1, nonterm1))

                        ' 1つの非終端記号からなる生成規則を作る
                        prod1 = New Production(la1, nonterm1)
                        prod1.prodGram.Add(vnont1(idx))
                        nonterm1.nontP.Add(prod1)
                    End If
                Else
                    ' 非終端記号が未定義の場合

                    iserr = True
                    Debug.WriteLine("未定義:" + str1)
                End If
            End If
        Next
        If iserr Then
            ExitApp("")
        End If
    End Sub

    Public Shared Function IsIdentifierStart(c As Char) As Boolean
        Return c = "_"c OrElse Char.IsLetter(c)
    End Function

    Public Shared Function IsIdentifierPart(c As Char) As Boolean
        Return c = "_"c OrElse Char.IsLetterOrDigit(c)
    End Function

    Public Shared Sub log(s As String)
    End Sub

    Public Shared Sub PutLog(s As String)
        LogBuf.Write(s)
    End Sub

    Public Shared Sub writeSrc(la1 As LALR, import_list As List(Of String), use_inverse_fnc As Boolean, file_text As String())
    End Sub

    '	非終端記号を作り、vopr_termまたはvres_termに追加する
    Public Shared Function makeTerminal(la1 As LALR, vopr_term As List(Of Terminal), vres_term As List(Of Terminal), name As String, sym As String) As Terminal
        Dim term1 As Terminal
        Dim vterm As List(Of Terminal)
        Dim str3 As String
        Dim idx As Integer

        term1 = New Terminal(0, name, sym)
        If sym = "EOF" Then
            la1.eofTerm = term1
        End If

        ' 予約語または演算子のリストに文字列順に挿入する
        If IsIdentifierPart(name(0)) Then
            vterm = vres_term
        Else
            vterm = vopr_term
        End If
        For idx = 0 To vterm.Count - 1
            str3 = vterm(idx).gramName
            If 0 < str3.CompareTo(name) Then
                vterm.Insert(idx, term1)
                Exit For
            End If
        Next
        If idx = vterm.Count Then
            vterm.Add(term1)
        End If

        Return term1
    End Function

    ' 文法ファイルを読む
    Public Shared Sub readGrammar(la1 As LALR)
        Dim i1 As Integer
        Dim i2 As Integer
        Dim nonterm1 As NonTerminal
        Dim nonterm2 As NonTerminal
        Dim prod1 As Production
        Dim term1 As Terminal
        Dim text1 As String
        Dim str2 As String
        Dim name As String
        Dim sym As String
        Dim vnont1 As List(Of NonTerminal)
        Dim vopr_term As List(Of Terminal)
        Dim vres_term As List(Of Terminal)

        vnont1 = New List(Of NonTerminal)()
        vopr_term = New List(Of Terminal)()
        vres_term = New List(Of Terminal)()
        ' SysIni()
        text1 = TFile.ReadAllText(fileName)
        la1.srcChars = text1

        la1.ParserName = TPath.GetFileNameWithoutExtension(fileName)

        i1 = text1.IndexOf("%{")
        If i1 = -1 Then
            la1.HeaderText = ""
            la1.srcIdx = 0
        Else
            i2 = text1.IndexOf("%}")
            la1.HeaderText = text1.Substring(i1 + 2, i2)
            la1.srcIdx = i2 + 2
        End If
        skipWhite(la1)
        If Not (la1.srcIdx < la1.srcChars.Length AndAlso la1.srcChars(la1.srcIdx) = "{"c) Then
            ExitApp("readg1")
        End If

        la1.srcIdx += 1

        ' 終端記号を読む
        Do While True
            skipWhite(la1)
            If la1.srcChars.Length <= la1.srcIdx Then
                ExitApp("readg2")
            End If

            If la1.srcChars(la1.srcIdx) = "}"c Then
                la1.srcIdx += 1
                Exit Do
            End If
            If la1.srcChars(la1.srcIdx) <> """"c Then
                ExitApp("readg3" + la1.srcChars(la1.srcIdx))
            End If

            la1.srcIdx += 1
            i1 = la1.srcIdx
            Do While la1.srcIdx < la1.srcChars.Length AndAlso la1.srcChars(la1.srcIdx) <> """"c
                la1.srcIdx += 1
            Loop
            name = la1.srcChars.Substring(i1, la1.srcIdx - i1)
            la1.srcIdx += 1

            skipWhite(la1)
            If Not (la1.srcIdx < la1.srcChars.Length AndAlso IsIdentifierStart(la1.srcChars(la1.srcIdx))) Then
                ExitApp("readg4")
            End If

            i1 = la1.srcIdx
            la1.srcIdx += 1
            Do While IsIdentifierPart(la1.srcChars(la1.srcIdx))
                la1.srcIdx += 1
            Loop
            sym = la1.srcChars.Substring(i1, la1.srcIdx - i1)

            ' 終端記号を作り、vopr_termまたはvres_termに追加する
            makeTerminal(la1, vopr_term, vres_term, name, sym)
        Loop

        la1.Terminals.AddRange(vopr_term)
        la1.Terminals.AddRange(vres_term)

        ' 非終端記号の定義を読む
        Do While la1.srcIdx < la1.srcChars.Length
            skipWhite(la1)
            If la1.srcChars.Length <= la1.srcIdx Then
                Exit Do
            End If
            If Not IsIdentifierStart(la1.srcChars(la1.srcIdx)) Then
                ExitApp("err@ " + text1.Substring(0, la1.srcIdx))
            End If

            i1 = la1.srcIdx
            la1.srcIdx += 1
            Do While IsIdentifierPart(la1.srcChars(la1.srcIdx))
                la1.srcIdx += 1
            Loop
            str2 = la1.srcChars.Substring(i1, la1.srcIdx - i1)
            nonterm1 = regNonTerminal(vnont1, str2)
            la1.NonTerminals.Add(nonterm1)

            skipWhite(la1)
            If la1.srcChars.Length <= la1.srcIdx OrElse la1.srcChars(la1.srcIdx) <> ":"c Then
                ExitApp(text1.Substring(0, la1.srcIdx) + "\n err2")
            End If
            la1.srcIdx += 1

            prod1 = New Production(la1, nonterm1)
            nonterm1.nontP.Add(prod1)

            Do While nonterm1 IsNot Nothing
                skipWhite(la1)
                If la1.srcChars.Length <= la1.srcIdx Then
                    ExitApp("err3")
                End If

                If IsIdentifierStart(la1.srcChars(la1.srcIdx)) Then
                    ' 非終端記号の場合

                    i1 = la1.srcIdx
                    la1.srcIdx += 1
                    Do While IsIdentifierPart(la1.srcChars(la1.srcIdx))
                        la1.srcIdx += 1
                    Loop
                    str2 = la1.srcChars.Substring(i1, la1.srcIdx - i1)
                    nonterm2 = regNonTerminal(vnont1, str2)
                    prod1.prodGram.Add(nonterm2)
                Else
                    Select Case la1.srcChars(la1.srcIdx)
                        Case """"c
                            ' 終端記号の場合

                            la1.srcIdx += 1
                            i1 = la1.srcIdx
                            Do While la1.srcIdx < la1.srcChars.Length AndAlso la1.srcChars(la1.srcIdx) <> """"c
                                la1.srcIdx += 1
                            Loop
                            str2 = la1.srcChars.Substring(i1, la1.srcIdx - i1)
                            la1.srcIdx += 1
                            term1 = findTerminal(la1.Terminals, str2)
                            If term1 Is Nothing Then
                                log("no terminal:" + str2)
                            Else
                                prod1.prodGram.Add(term1)
                            End If

                        Case "|"c
                            ' 次の生成規則を作る

                            la1.srcIdx += 1
                            prod1 = New Production(la1, nonterm1)
                            nonterm1.nontP.Add(prod1)

                        Case ";"c
                            ' 非終端記号の定義の終わり

                            la1.srcIdx += 1
                            nonterm1 = Nothing

                        Case "{"c
                            ' アクション

                            If la1.srcChars.Length <= la1.srcIdx + 1 OrElse la1.srcChars(la1.srcIdx + 1) <> "{"c Then
                                ExitApp("readg:{")
                            End If
                            la1.srcIdx = la1.srcIdx + 2
                            i1 = la1.srcIdx
                            Do While la1.srcIdx + 1 < la1.srcChars.Length AndAlso (la1.srcChars(la1.srcIdx) <> "}"c OrElse la1.srcChars(la1.srcIdx + 1) <> "}"c)
                                la1.srcIdx += 1
                            Loop
                            If la1.srcChars.Length <= la1.srcIdx + 1 Then
                                ExitApp("readg:}")
                            End If
                            prod1.prodText = la1.srcChars.Substring(i1, la1.srcIdx - i1)
                            la1.srcIdx += 2
                        Case Else
                            ExitApp(text1.Substring(0, la1.srcIdx) + "err4:" + la1.srcChars(la1.srcIdx))

                    End Select
                End If
            Loop
        Loop

        ' 未定義の非終端記号が無いか調べる
        chkGrammar(la1, vnont1)
    End Sub

    Public Shared Sub readGrammarEnd(la1 As LALR)
        Dim nonterm1 As NonTerminal
        Dim term1 As Terminal

        ' GrammarSymbolsとgramIdをセットする
        la1.GrammarSymbols = New List(Of GrammarSymbol)()
        For Each term1 In la1.Terminals
            term1.gramId = la1.GrammarSymbols.Count
            la1.GrammarSymbols.Add(term1)
        Next
        For Each nonterm1 In la1.NonTerminals
            nonterm1.gramId = la1.GrammarSymbols.Count
            la1.GrammarSymbols.Add(nonterm1)
        Next

        ' topProdをセットする
        la1.topProd = la1.NonTerminals(0).nontP(0)
    End Sub

    Public Sub DmpGrammar()
        Dim nonterm1 As NonTerminal
        Dim gra1 As GrammarSymbol
        Dim prod1 As Production

        For Each nonterm1 In NonTerminals
            Debug.WriteLine("非終端記号 {0} {1} {2}", nonterm1.gramId, nonterm1.gramName, nonterm1.gramSym)
            For Each prod1 In nonterm1.nontP
                Debug.WriteLine("   生成規則 {0}", prod1.prodText)
                For Each gra1 In prod1.prodGram
                    Debug.WriteLine("       文法記号 {0} {1} {2}", gra1.gramId, gra1.gramName, gra1.gramSym)
                Next
            Next
        Next
    End Sub

    Public Shared Sub Main(la1 As LALR)
        Dim i2 As Integer
        Dim gram1 As GrammarSymbol
        Dim nonterm1 As NonTerminal
        Dim prod1 As Production
        Dim file_text As String()
        Dim term1 As Terminal

        log("read grammar")
        readGrammar(la1)
        '        la1.DmpGrammar()

        log("read grammar end")
        readGrammarEnd(la1)

        log("makefirst")
        makeFirst(la1)

        ' すべての
        For Each nonterm1 In la1.NonTerminals
            PutLog(nonterm1.gramName + " : ")
            For Each term1 In nonterm1.First
                PutLog(" " + term1.gramName)
            Next
            PutLog(vbCrLf)
        Next

        '	LR(0)項の集合の正準集合を作る	P.184
        log("makeLR0state")
        makeLR0States(la1)

        log("make LR1")
        makeLR1(la1)

        For Each nonterm1 In la1.NonTerminals
            PutLog(nonterm1.gramName + vbCrLf)
            For i2 = 0 To nonterm1.nontP.Count - 1
                prod1 = nonterm1.nontP(i2)
                If i2 = 0 Then
                    PutLog(vbTab + ":")
                Else
                    PutLog(vbTab + "|")
                End If
                For Each gram1 In prod1.prodGram
                    If TypeOf gram1 Is Terminal Then
                        PutLog(" """ + gram1.gramName + """")
                    Else
                        PutLog(" " + gram1.gramName)
                    End If
                Next
                PutLog(vbCrLf)
            Next
            PutLog(vbTab + vbCrLf)
        Next

        log("write Src")
        ReDim file_text(2 - 1)
        '''''''        writeSrc(la1, Nothing, False, file_text)
        '''''''        TFile.WriteAllText(file_text(0), file_text(1))

        TFile.WriteAllText("C:\usr\prj\MyIDE\MyAlgo\out\MyIDETest\MyAlgo\LogBuf.txt", LogBuf.ToString())
    End Sub

    '	終端記号、非終端記号を作る
    '	各非終端記号の開始記号集合を求める

    '	LR(0)を作る

    '	各終端記号、非終端記号の開始記号集合を作る
    ' changed <-> First ⊂ First@1
    ' deletable <-> ∃gra1∈生成規則・ gra1は終端記号 or gra1の開始記号集合がεを含まない
    Public Shared Sub makeFirst(la1 As LALR)
        Dim changed As Boolean
        Dim deletable As Boolean
        Dim nonterm1 As NonTerminal
        Dim nonterm2 As NonTerminal
        Dim term1 As Terminal
        Dim vgra1 As List(Of GrammarSymbol)
        Dim gra1 As GrammarSymbol
        Dim prod1 As Production

        changed = True
        Do While changed
            changed = False
            ' すべての非終端記号に対して
            For Each nonterm1 In la1.NonTerminals
                ' すべての生成規則に対して
                For Each prod1 In nonterm1.nontP
                    vgra1 = prod1.prodGram
                    deletable = True
                    ' 生成規則の各要素に対して
                    For Each gra1 In vgra1
                        If TypeOf gra1 Is Terminal Then
                            ' 終端記号の場合

                            term1 = CType(gra1, Terminal)

                            ' 削除可能でない
                            deletable = False
                            If Not nonterm1.First.Contains(term1) Then
                                ' gra1 が nonterm1の開始記号集合に含まれない場合

                                nonterm1.First.Add(term1)
                                changed = True
                            End If
                            Exit For
                        Else
                            ' 非終端記号の場合

                            nonterm2 = CType(gra1, NonTerminal)
                            ' nonterm2のε以外の開始記号集合の要素をnonterm1の開始記号集合に加える
                            For Each term_f In nonterm2.First
                                If term_f IsNot la1.eps AndAlso Not nonterm1.First.Contains(term_f) Then
                                    ' term_fがεでなくnonterm1の開始記号集合に含まれない場合

                                    nonterm1.First.Add(term_f)
                                    changed = True
                                End If
                            Next

                            If Not nonterm2.First.Contains(la1.eps) Then
                                ' nonterm2の開始記号集合がεをまだ含まなければ

                                ' 削除可能とは言えない
                                deletable = False


                                Exit For
                            End If
                        End If
                    Next

                    ' 削除可能で、εが開始記号集合に含まれなければ
                    If deletable AndAlso Not nonterm1.First.Contains(la1.eps) Then

                        ' εを開始記号集合に加える
                        nonterm1.First.Add(la1.eps)
                        changed = True
                    End If
                Next
            Next
        Loop
    End Sub

    '	文法記号列の開始記号集合を得る
    Public Shared Function getFirst(la1 As LALR, vgra1 As List(Of GrammarSymbol)) As List(Of Terminal)
        Dim gra1 As GrammarSymbol
        Dim term1 As Terminal
        Dim vterm1 As List(Of Terminal)

        vterm1 = New List(Of Terminal)()
        For Each gra1 In vgra1

            ' ε以外の開始記号集合をvterm1に追加する。
            For Each term1 In gra1.First
                If term1 IsNot la1.eps AndAlso Not vterm1.Contains(term1) Then
                    vterm1.Add(term1)
                End If
            Next

            If Not gra1.First.Contains(la1.eps) Then
                ' gra1が削除可能でない(εを含んでいない)場合

                Return vterm1
            End If
        Next

        ' 列内のすべての文法記号が削除可能でない(開始記号集合がεを含む)場合は、εをvterm1に追加する。
        vterm1.Add(la1.eps)

        Return vterm1
    End Function



    ' item1	= [A→α・Bβ, {a,...}]
    ' item2	= [B→・γ, {...}]
    ' item1の各先読みaに対し、開始記号集合(βa)の各要素をitem2の先読みに加える
    Public Shared Function addLookahead(la1 As LALR, item1 As LALRItem, item2 As LALRItem) As Boolean
        Dim i1 As Integer
        Dim changed As Boolean
        Dim term1 As Terminal
        Dim term2 As Terminal
        Dim vgram1 As List(Of GrammarSymbol)
        Dim vgram2 As List(Of GrammarSymbol)
        Dim vterm1 As List(Of Terminal)
        Dim item1_lookahead As List(Of Terminal)

        changed = False
        vgram1 = item1.itemP.prodGram

        ' vgram2 = β
        vgram2 = New List(Of GrammarSymbol)()
        For i1 = item1.itemDot + 1 To vgram1.Count - 1
            vgram2.Add(vgram1(i1))
        Next

        ' A→・Aβ ならitem1 = item2なので先読みのリストをコピーする
        If item1 Is item2 Then
            item1_lookahead = New List(Of Terminal)(item1.lookahead)
        Else
            item1_lookahead = item1.lookahead
        End If

        ' Aのすべての先読みに対し
        For Each term2 In item1_lookahead
            ' vgram2 = βa
            vgram2.Add(term2)

            ' vterm1 = 開始記号集合(βa)
            vterm1 = getFirst(la1, vgram2)
            If vterm1.Count = 0 Then
                ExitApp("add look1:" + term2.gramName)
            End If

            ' 開始記号集合(βa)内のすべての項に対し
            For Each term1 In vterm1
                ' term1 = b ∈ 開始記号集合(βa)

                If Not item2.lookahead.Contains(term1) Then
                    ' item2の先読みに含まれていなければ

                    item2.lookahead.Add(term1)
                    changed = True
                End If
            Next
            ' vgram2 = β
            vgram2.RemoveAt(vgram2.Count - 1)
        Next
        If item2.lookahead.Count = 0 Then
            ExitApp("add look2:" + item2.itemP.prodId)
        End If
        Return changed
    End Function

    '	LR(0)またはLR(1)CLOSUREを作る	P.183 図6.5		P.191 図6.10
    Public Shared Function makeLR0Closure(la1 As LALR, vitem1 As List(Of LALRItem), use_lookahead As Boolean) As List(Of LALRItem)
        Dim changed As Boolean
        Dim item1 As LALRItem
        Dim item2 As LALRItem
        Dim item3 As LALRItem
        Dim nonterm1 As NonTerminal
        Dim prod2 As Production
        Dim vgram1 As List(Of GrammarSymbol)
        Dim vitem2 As List(Of LALRItem)
        Dim vadded_item As List(Of LALRItem)
        Dim found As Boolean

        vitem2 = New List(Of LALRItem)(vitem1)
        '        vec2_copyElements(vitem2, vitem1)

        ' changed <-> vitem2に追加 or (use_lookahead and 先読みに追加)
        changed = True
        Do While changed
            changed = False
            vadded_item = New List(Of LALRItem)()

            ' 作成中の閉包内のすべての項に対し
            For Each item1 In vitem2
                vgram1 = item1.itemP.prodGram
                If use_lookahead AndAlso item1.lookahead.Count = 0 Then
                    ExitApp("lr0c:" + item1.itemP.prodId)
                End If
                If item1.itemDot < vgram1.Count AndAlso TypeOf vgram1(item1.itemDot) Is NonTerminal Then
                    ' ドットの右が非終端記号なら

                    nonterm1 = CType(vgram1(item1.itemDot), NonTerminal)

                    ' nonterm1の各生成規則に対し
                    For Each prod2 In nonterm1.nontP

                        ' vitem2内にドットが左端にあり、生成規則が同じ項がすでにあるか探す
                        found = False
                        For Each item2 In vitem2
                            If item2.itemDot = 0 AndAlso item2.itemP Is prod2 Then
                                ' ドットが左端にあり、生成規則が同じ場合

                                If use_lookahead Then
                                    ' 先読みを作る場合
                                    changed = (changed OrElse addLookahead(la1, item1, item2))
                                End If
                                found = True
                                Exit For
                            End If
                        Next
                        If Not found Then
                            ' ない場合

                            item3 = New LALRItem(prod2, 0)
                            vadded_item.Add(item3)
                            If use_lookahead Then
                                ' 先読みを作る場合
                                addLookahead(la1, item1, item3)
                            End If
                            changed = True
                        End If
                    Next
                End If
            Next

            vitem2.AddRange(vadded_item)
        Loop

        Return vitem2
    End Function

    '	LR(0)GOTOを作る	P.183
    '	vitem1 = I
    '	gram1  = X
    Public Shared Function makeLR0Goto(la1 As LALR, vitem1 As List(Of LALRItem), gram1 As GrammarSymbol) As List(Of LALRItem)
        Dim vitem2 As List(Of LALRItem)
        Dim item1 As LALRItem
        Dim item2 As LALRItem
        Dim item3 As LALRItem
        Dim found As Boolean

        vitem2 = New List(Of LALRItem)()

        ' 状態内のすべての項に対し
        For Each item1 In vitem1

            If item1.itemDot < item1.itemP.prodGram.Count AndAlso item1.itemP.prodGram(item1.itemDot) Is gram1 Then
                '	item1 = [A→α・Xβ] の場合  ( "・"の右にgram1がある )

                ' すでにvitem2にあるか探す
                found = False
                For Each item2 In vitem2
                    If item2.itemDot = item1.itemDot + 1 AndAlso item2.itemP Is item1.itemP Then

                        found = True
                        Exit For
                    End If
                Next
                If Not found Then
                    ' なければ追加する

                    item3 = New LALRItem(item1.itemP, item1.itemDot + 1)
                    vitem2.Add(item3)
                End If
            End If
        Next

        ' vitem2の閉包を返す
        Return makeLR0Closure(la1, vitem2, False)
    End Function

    ' 同じ項の集合ならtrueを返す
    Public Shared Function isSameItems(vitem1 As List(Of LALRItem), vitem2 As List(Of LALRItem)) As Boolean
        Dim item1 As LALRItem
        Dim item2 As LALRItem
        Dim ret1 As Boolean
        Dim found As Boolean

        If vitem1.Count <> vitem2.Count Then
            ret1 = False
        Else
            ret1 = True
            For Each item1 In vitem1
                found = False
                For Each item2 In vitem2
                    If item1.itemP Is item2.itemP AndAlso item1.itemDot = item2.itemDot Then
                        ' 生成規則とドットの位置が同じ場合

                        found = True
                        Exit For
                    End If
                Next
                If Not found Then
                    ' 同じ項が見つからなかった場合

                    ret1 = False
                    Exit For
                End If
            Next
        End If
        Return ret1
    End Function

    '	LR(0)項の集合の正準集合を作る	P.184
    Public Shared Sub makeLR0States(la1 As LALR)
        Dim gram1 As GrammarSymbol
        Dim item1 As LALRItem
        Dim item2 As LALRItem
        Dim stat1 As LALRState
        Dim stat2 As LALRState
        Dim stat3 As LALRState
        Dim vitem1 As List(Of LALRItem)
        Dim vitem2 As List(Of LALRItem)
        Dim vstat1 As List(Of LALRState)

        la1.States = New List(Of LALRState)()
        vstat1 = New List(Of LALRState)()
        vitem1 = New List(Of LALRItem)()
        item2 = New LALRItem(la1.topProd, 0)
        vitem1.Add(item2)
        stat1 = New LALRState(makeLR0Closure(la1, vitem1, False))
        la1.States.Add(stat1)
        vstat1.Add(stat1)

        '	topItemをセットする
        For Each item1 In stat1.statItems
            If item1.itemP Is la1.topProd AndAlso item1.itemDot = 0 Then
                la1.topItem = item1
                Exit For
            End If
        Next
        If la1.topItem Is Nothing Then
            ExitApp("makelr0")
        End If

        If item2 Is la1.topItem Then
            log("topItemをセットする必要はない")
        End If

        ' スタックが空でない間ループする
        Do While vstat1.Count <> 0
            stat1 = vstat1(0)

            ' すべての文法記号に対し
            For Each gram1 In la1.GrammarSymbols

                ' LR(0)GOTOを作る
                vitem2 = makeLR0Goto(la1, stat1.statItems, gram1)
                If vitem2.Count <> 0 Then
                    ' LR(0)GOTOが空でない場合

                    ' すでに同じ状態がないか探す
                    stat2 = Nothing
                    For Each stat3 In la1.States
                        If isSameItems(stat3.statItems, vitem2) Then
                            stat2 = stat3
                            Exit For
                        End If
                    Next

                    'なければ新たに作る
                    If stat2 Is Nothing Then
                        stat2 = New LALRState(vitem2)
                        la1.States.Add(stat2)
                        vstat1.Add(stat2)
                        'Debug.WriteLine("LR(0)状態の数 " + la1.States.Count.ToString())
                    End If

                    ' GOTOを設定する
                    stat1.gotoState.Add(stat2)
                    stat1.gotoLabel.Add(gram1)
                End If
            Next
            vstat1.RemoveAt(0)

        Loop
    End Sub

    Public Shared Sub makeLR1(la1 As LALR)
        Dim i1 As Integer, i3 As Integer
        Dim gram1 As GrammarSymbol
        Dim stat1 As LALRState
        Dim stat2 As LALRState
        Dim item1 As LALRItem
        Dim item2 As LALRItem
        Dim item3 As LALRItem
        Dim item3_org As LALRItem
        Dim item4 As LALRItem
        Dim item5 As LALRItem
        Dim item1_Prop As LALRItem
        Dim item1_Goto As LALRItem
        Dim dmyterm As Terminal
        Dim term1 As Terminal
        Dim vgram1 As List(Of GrammarSymbol)
        Dim vitem1 As List(Of LALRItem)
        Dim vitem2 As List(Of LALRItem)
        Dim stcitem As List(Of LALRItem)
        Dim stcterm As List(Of Terminal)

        For i1 = 0 To la1.States.Count - 1
            la1.States(i1).statId = i1
        Next

        '	itemGotoをセットする
        ' すべての状態に対し
        For Each stat1 In la1.States
            ' 状態内のすべての項に対し
            For Each item1 In stat1.statItems

                If item1.itemDot < item1.itemP.prodGram.Count Then
                    ' ドットが右端にない場合

                    gram1 = item1.itemP.prodGram(item1.itemDot)
                    ' 行き先の状態を探す
                    i3 = stat1.gotoLabel.IndexOf(gram1)
                    If i3 = -1 Then
                        ExitApp("makelr1")
                    End If

                    stat2 = stat1.gotoState(i3)
                    ' 行き先の状態内で対応する項をitemGotoに設定する
                    For Each item2 In stat2.statItems
                        If item2.itemP Is item1.itemP AndAlso item2.itemDot = item1.itemDot + 1 Then
                            item1.itemGoto = item2
                            Exit For
                        End If
                    Next
                    If item1.itemGoto Is Nothing Then
                        ExitApp("makelr1:2")
                    End If
                End If
            Next
        Next

        dmyterm = New Terminal(-3, "#", Nothing)
        stcitem = New List(Of LALRItem)()
        stcterm = New List(Of Terminal)()

        '	INSERT(I0, S'->.S,$)
        la1.topItem.lookahead.Add(la1.eofTerm)
        stcitem.Add(la1.topItem)
        stcterm.Add(la1.eofTerm)

        ' すべての状態に対し
        For Each stat1 In la1.States
            Debug.WriteLine("先読み {0}/{1} 項数:{2}", stat1.statId, la1.States.Count, stat1.statItems.Count)

            ' 状態内のすべての項に対し
            For Each item1 In stat1.statItems
                If item1.itemDot = item1.itemP.prodGram.Count AndAlso item1.itemGoto IsNot Nothing Then
                    ExitApp("goto")
                End If
                If item1.itemDot <> item1.itemP.prodGram.Count AndAlso item1.itemGoto Is Nothing Then
                    ExitApp("goto")
                End If
                vgram1 = item1.itemP.prodGram

                If vgram1.Count <> 0 AndAlso (item1.itemDot <> 0 OrElse item1.itemP Is la1.topProd) Then
                    ' 生成規則が空でなく、ドットが左端にないか初期状態の場合

                    ' item1  = [B→γ・β]
                    ' item2  = [B→γ・β,#]
                    ' vitem1 = { [B→γ・β,#] }
                    ' vitem2 = J' = CLOSURE( { [B→γ・β,#] } )
                    item2 = New LALRItem(item1.itemP, item1.itemDot)
                    item2.lookahead.Add(dmyterm)
                    vitem1 = New List(Of LALRItem)()
                    vitem1.Add(item2)
                    vitem2 = makeLR0Closure(la1, vitem1, True)

                    ' vitem2の各項に対し
                    For Each item3 In vitem2
                        If item3.lookahead.Count = 0 Then
                            ExitApp("make LR1:" + item3.itemP.prodId)
                        End If
                        If item3 IsNot item2 Then
                            ' CLOSUREで生成された項の場合

                            Debug.Assert(item3.itemDot = 0, "ドットが左端にない")

                            ' もとの状態内の対応する項を探す
                            item3_org = Nothing
                            For Each item4 In stat1.statItems
                                If item4.itemP Is item3.itemP AndAlso item4.itemDot = 0 Then
                                    ' 生成規則とドットの位置が同じ場合

                                    item3_org = item4
                                    Exit For
                                End If
                            Next
                            If item3_org Is Nothing Then
                                ExitApp("make LR1 閉包:" + item3.itemP.prodId)
                            End If
                            If item3_org.itemGoto Is Nothing AndAlso item3_org.itemP.prodGram.Count <> 0 Then
                                ExitApp("ドットが右端にある")
                            End If

                            ' 各先読みに対し
                            For Each term1 In item3.lookahead

                                If term1 IsNot dmyterm Then
                                    ' ダミーの先読みでない場合

                                    If item3_org.itemGoto Is Nothing Then
                                        ' εの場合

                                        If Not item3_org.lookahead.Contains(term1) Then
                                            ' この先読みが先読み集合にない場合

                                            ' 先読み集合に追加する
                                            item3_org.lookahead.Add(term1)
                                        End If
                                    Else
                                        ' ドットが先頭にある場合

                                        If Not item3_org.itemGoto.lookahead.Contains(term1) Then
                                            ' 行き先の項が先読みを含んでいない場合

                                            ' 行き先の項に先読みを加える
                                            item3_org.itemGoto.lookahead.Add(term1)

                                            ' スタックに積む
                                            stcitem.Add(item3_org.itemGoto)
                                            stcterm.Add(term1)
                                        End If
                                    End If
                                Else
                                    ' ダミーの先読みの場合

                                    If item3_org.itemGoto Is Nothing Then
                                        ' εの場合

                                        ' item3_orgをitem1の先読みの伝播先に加える。
                                        item5 = item3_org
                                    Else
                                        ' ドットが先頭にある場合

                                        ' item3_orgの行き先の項をitem1の先読みの伝播先に加える。
                                        item5 = item3_org.itemGoto
                                    End If

                                    If Not item1.itemProp.Contains(item5) Then
                                        item1.itemProp.Add(item5)
                                    End If
                                End If
                            Next
                        End If
                    Next
                End If
            Next
        Next

        ' スタックが空でない間、先読みを伝播させていく。
        Do While 0 < stcitem.Count
            ' スタックから項と先読みを１つ降ろす。
            item1 = stcitem(0)
            term1 = stcterm(0)
            stcitem.RemoveAt(0)
            stcterm.RemoveAt(0)

            ' 各先読みの伝播先に対し
            For Each item1_Prop In item1.itemProp

                If Not item1_Prop.lookahead.Contains(term1) Then
                    ' 先読みを含んでいない場合

                    ' 先読みを加えて、スタックに積む
                    item1_Prop.lookahead.Add(term1)
                    stcitem.Add(item1_Prop)
                    stcterm.Add(term1)
                End If
            Next

            If item1.itemGoto IsNot Nothing Then
                ' ドットが途中の場合

                item1_Goto = item1.itemGoto
                If Not item1_Goto.lookahead.Contains(term1) Then
                    ' 行き先の項が先読みを含んでいない場合

                    ' 行き先の項に先読みを加えて、行き先の項をスタックに積む
                    item1_Goto.lookahead.Add(term1)
                    stcitem.Add(item1_Goto)
                    stcterm.Add(term1)
                End If
            End If
        Loop        
    End Sub

End Class

Public MustInherit Class TParser
    Inherits TLex
    Public Const iStat As Integer = 0
    Public Const iGram As Integer = 1
    Public Const iAct As Integer = 2
    Public Const iGoto As Integer = 3
    Public Const offProd As Integer = 2

    Public Const LALR_Shift As Integer = 1
    Public Const LALR_Reduce As Integer = 2

    Public mAct As Short(,)
    Public mGot As Short(,)
    Public lexTbl As Short(,)
    Public stcIdx As Integer
    Public vStack As Short()

    Public parLex As TLex
    Public stcBase As Integer
    Public stcVal As Object()
    Public nextTkn As TToken
    Public isParserEof As Boolean
    Public nStat As Integer
    Public nGram As Integer
    Public lexProd As List(Of List(Of Short))
    Public eEOF As Integer
    Public eGoal As Integer

    Public MustOverride Function Reduce(parser___ As TParser, prod___id As Integer, prod___len As Integer) As Object
    Public MustOverride Sub ParserInit(par1 As TParser)

    Public Shared Sub readLexTbl(par1 As TParser)
    End Sub

    Public Shared Sub ParseIni(par1 As TParser)
        Dim i1 As Integer

        par1.ParserInit(par1)
        readLexTbl(par1)
        ReDim par1.mAct(par1.nStat - 1, par1.nGram - 1)
        ReDim par1.mGot(par1.nStat - 1, par1.nGram - 1)
        For i1 = 0 To par1.lexTbl.Length - 1
            par1.mAct(par1.lexTbl(i1, iStat), par1.lexTbl(i1, iGram)) = par1.lexTbl(i1, iAct)
            par1.mGot(par1.lexTbl(i1, iStat), par1.lexTbl(i1, iGram)) = par1.lexTbl(i1, iGoto)
        Next

        par1.stcIdx = 0
        ReDim par1.vStack(256 * 256 - 1)
        ReDim par1.stcVal(256 * 256 - 1)
        par1.vStack(par1.stcIdx) = 0
        par1.nextTkn = Nothing
        par1.isParserEof = False
    End Sub

    Public Shared Sub parserLexErr(par1 As TParser)
        LALR.PutLog("tparser lex err\n")
    End Sub

    Public Overrides Sub lexErr()
        parserLexErr(Me)
    End Sub

    Public Shared Function parserLexEof(par1 As TParser) As Boolean
        Return par1.isParserEof
    End Function

    Public Overrides Function lexEof() As Boolean
        Return parserLexEof(Me)
    End Function

    Public Overrides Function lexNextTkn() As TToken
        Return CType(Parse(Me, True), TToken)
    End Function

    Public Shared Function Parse(par1 As TParser, islex As Boolean) As Object
        Dim act As Short
        Dim n1 As Short
        Dim istat1 As Short
        Dim istat2 As Short
        Dim istat3 As Short
        Dim inont As Short
        Dim obj1 As Object
        Dim tkn1 As TToken
        Dim sym1 As Integer
        Dim terminate As Boolean
        Dim i1 As Integer
        Dim str1 As String

        terminate = False
        If par1.nextTkn Is Nothing Then
            tkn1 = par1.parLex.lexNextTkn()
        Else
            tkn1 = par1.nextTkn
            par1.nextTkn = Nothing
        End If
        sym1 = tkn1.tknSym
        If sym1 = par1.eEOF Then
            Debug.Assert(False)
        End If
        Do While True
            istat1 = par1.vStack(par1.stcIdx)
            act = par1.mAct(istat1, sym1)
            n1 = par1.mGot(istat1, sym1)
            If act = 0 AndAlso Not terminate AndAlso islex Then
                terminate = True
                sym1 = par1.eEOF
                act = par1.mAct(istat1, sym1)
                n1 = par1.mGot(istat1, sym1)
                par1.nextTkn = tkn1
            End If
            Select Case act
                Case 0
                    par1.parLex.lexErr()
                    For i1 = 0 To par1.stcIdx - 1
                        LALR.PutLog(" s" + par1.vStack(i1))
                    Next
                    str1 = "syntax err:" + istat1 + " " + sym1.ToString()
                    LALR.PutLog(str1)
                    Debug.Assert(False)
                '				
                Case LALR_Shift
                    If terminate Then
                        Debug.Assert(False)
                    End If
                    '				TSys.log("shift " + istat1 + " -> " + n1 + "\n")
                    par1.stcIdx += 1
                    If par1.stcIdx >= par1.vStack.Length Then
                        Debug.Assert(False)
                    End If
                    par1.vStack(par1.stcIdx) = n1
                    par1.stcVal(par1.stcIdx) = tkn1
                    If Not par1.parLex.lexEof() Then
                        tkn1 = par1.parLex.lexNextTkn()
                        sym1 = tkn1.tknSym
                    Else
                        sym1 = par1.eEOF
                    End If

                Case LALR_Reduce
                    par1.stcBase = par1.stcIdx - (par1.lexProd(n1).Count - offProd)
                    obj1 = par1.Reduce(par1, n1, par1.lexProd(n1).Count - offProd)
                    inont = par1.lexProd(n1)(inont)
                    par1.stcIdx = par1.stcIdx - (par1.lexProd(n1).Count - offProd)
                    If inont = par1.eGoal Then
                        If sym1 = par1.eEOF AndAlso par1.stcIdx = 0 Then
                            If Not terminate Then
                                par1.isParserEof = True
                            End If
                            Return obj1
                        Else
                            Debug.Assert(False)
                        End If
                    End If
                    If par1.stcIdx < 0 Then
                        Debug.Assert(False)
                    End If
                    istat2 = par1.vStack(par1.stcIdx)
                    istat3 = par1.mGot(istat2, inont)
                    par1.stcIdx += 1
                    If par1.vStack.Length <= par1.stcIdx Then
                        Debug.Assert(False)
                    End If
                    '				TSys.log("reduce " + par1.stcIdx + " " + Enum(par1, inont) + " " + istat1 + " -> " + istat2 + " -> " + istat3 + "\n")
                    par1.vStack(par1.stcIdx) = istat3
                    par1.stcVal(par1.stcIdx) = obj1

            End Select
        Loop

        Return Nothing
    End Function

    Public Shared Function newTErrObj(msg As String, obj1 As Object) As Exception
        Return Nothing
    End Function
End Class

Public MustInherit Class TLex
    '        Inherits Sys

    Public MustOverride Sub lexInitStr(text1 As String, par1 As TParser)
    Public MustOverride Sub lexErr()
    Public MustOverride Function lexEof() As Boolean
    Public MustOverride Function lexNextTkn() As TToken
End Class
