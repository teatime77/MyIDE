Imports System.Diagnostics
'Imports InvariantBasicOrigin

'-------------------------------------------------------------------------------- TScriptParser
' C#の構文解析
Public Class TScriptParser
    Inherits TSourceParser

    Public vTkn As New Dictionary(Of String, EToken)
    Public CurBlc As TBlock
    Public CurPos As Integer
    Public CurTkn As TToken
    Public NxtTkn As TToken
    Dim EOTTkn As TToken
    Public CurVTkn As TList(Of TToken)
    Public CurLineIdx As Integer
    Dim CurLineStr As String

    Public Sub New(prj1 As TProject, lang As ELanguage)
        LanguageSP = lang
        ThisName = "this"
        PrjParse = prj1
        RegTkn()

        TranslationTable.Add("System.True", "true")
        TranslationTable.Add("System.False", "false")
        TranslationTable.Add("Math.Ceiling", "ceiling")
        TranslationTable.Add("Math.Max", "max")
        TranslationTable.Add("Math.Min", "min")
        TranslationTable.Add("Math.Sqrt", "sqrt")
        TranslationTable.Add("Math.Abs", "abs")
        TranslationTable.Add("Math.Floor", "floor")
        TranslationTable.Add("Math.Round", "round")
        TranslationTable.Add("Math.Cos", "cos")
        TranslationTable.Add("Math.Sin", "sin")
    End Sub

    Public Overrides Sub ClearParse()
        CurBlc = Nothing
        CurPos = 0
        CurTkn = Nothing
        NxtTkn = Nothing
        CurVTkn = Nothing
        CurLineIdx = 0
        CurLineStr = ""
    End Sub

    Public Overrides Function NullName() As String
        Return "null"
    End Function

    Public Function GetTkn(type1 As EToken) As TToken
        Dim tkn1 As TToken

        If type1 = CurTkn.TypeTkn OrElse type1 = EToken.eUnknown Then
            tkn1 = CurTkn

            CurPos += 1
            Do While CurPos < CurVTkn.Count AndAlso (CurVTkn(CurPos).TypeTkn = EToken.eLineComment OrElse CurVTkn(CurPos).TypeTkn = EToken.eBlockComment)
                CurPos += 1
            Loop

            If CurPos < CurVTkn.Count Then
                If CurVTkn(CurPos).TypeTkn = EToken.eLowLine Then

                    CurLineIdx += 1
                    CurLineStr = PrjParse.CurSrc.vTextSrc(CurLineIdx)
                    CurVTkn = PrjParse.CurSrc.LineTkn(CurLineIdx)

                    CurPos = 0
                End If
                CurTkn = CurVTkn(CurPos)

                Dim nxt_pos As Integer = CurPos + 1
                Do While nxt_pos < CurVTkn.Count AndAlso (CurVTkn(nxt_pos).TypeTkn = EToken.eLineComment OrElse CurVTkn(nxt_pos).TypeTkn = EToken.eBlockComment)
                    nxt_pos += 1
                Loop

                If nxt_pos < CurVTkn.Count Then
                    NxtTkn = CurVTkn(nxt_pos)
                Else
                    NxtTkn = EOTTkn
                End If
            Else
                CurTkn = EOTTkn
                NxtTkn = EOTTkn
            End If

            'Debug.Print("token {0} {0}", CurTkn.StrTkn, CurTkn.TypeTkn)

            Return tkn1
        Else
            Chk(False, CurLineStr)
            Return Nothing
        End If
    End Function

    ' ジェネリック型の構文解析
    Function ReadGenType(id1 As TToken) As TClass
        Dim tp1 As TClass, tp2 As TClass
        Dim vtp As TList(Of TClass)

        GetTkn(EToken.eLP)
        GetTkn(EToken.eOf)

        vtp = New TList(Of TClass)()
        Do While True
            tp2 = ReadType(False)
            vtp.Add(tp2)
            If CurTkn.TypeTkn <> EToken.eComma Then

                Exit Do
            End If
            GetTkn(EToken.eComma)
        Loop
        GetTkn(EToken.eRP)

        ' ジェネリック型のクラスを得る。
        tp1 = PrjParse.GetAddSpecializedClass(id1.StrTkn, vtp)

        Return tp1
    End Function

    Function ReadType(is_new As Boolean) As TClass
        Dim tp1 As TClass
        Dim id1 As TToken, dim_cnt As Integer

        id1 = GetTkn(EToken.eId)
        If CurTkn.TypeTkn = EToken.eLT Then
            ' ジェネリック型の場合

            ' ジェネリック型の構文解析
            tp1 = ReadGenType(id1)
        Else

            tp1 = PrjParse.GetCla(id1.StrTkn)
            If tp1 Is Nothing Then
                If id1.StrTkn = "Image" Then

                    tp1 = PrjParse.GetCla("HTMLImageElement")
                End If
                If tp1 Is Nothing Then

                    Throw New TError(String.Format("不明なクラス {0}", id1.StrTkn))
                End If
            End If
        End If
        If CurTkn.TypeTkn = EToken.eLB AndAlso (NxtTkn.TypeTkn = EToken.eRB OrElse NxtTkn.TypeTkn = EToken.eComma) Then
            GetTkn(EToken.eLB)
            dim_cnt = 1
            Do While CurTkn.TypeTkn = EToken.eComma
                GetTkn(EToken.eComma)
                dim_cnt += 1
            Loop
            GetTkn(EToken.eRB)
            If Not is_new Then
                tp1 = PrjParse.GetArrCla(tp1, dim_cnt)
            End If
        End If

        Return tp1
    End Function

    Function ReadTailCom() As String
        Dim tkn1 As TToken

        Select Case CurTkn.TypeTkn
            Case EToken.eSM
                Return ""

            Case EToken.eLineComment
                tkn1 = GetTkn(EToken.eLineComment)
                Return tkn1.StrTkn

            Case Else
                Debug.Assert(False)
                Return Nothing
        End Select
    End Function

    Function ReadLineComment() As TStatement
        Dim stmt1 As New TStatement

        stmt1.TypeStmt = EToken.eLineComment
        GetTkn(EToken.eLineComment)
        Return stmt1
    End Function

    Sub Chk(b1 As Boolean, msg As String)
        If Not b1 Then
            Debug.WriteLine("コンパイラ　エラー {0} at {1}", CurLineStr, CurTkn.StrTkn)
            Debug.Assert(False)
        End If
    End Sub

    Sub Chk(b1 As Boolean)
        Chk(b1, "")
    End Sub

    Public Sub RegTkn()
        Dim dic1 As New Dictionary(Of String, EToken)

        EOTTkn = NewToken(EToken.eEOT, "", 0)

        vTkn.Add("abstract", EToken.eAbstract)

        dic1.Add("aggregate", EToken.eAggregate)

        If LanguageSP = ELanguage.CSharp Then

            dic1.Add("as", EToken.eAs)
        End If

        dic1.Add("base", EToken.eBase)
        dic1.Add("break", EToken.eBreak)
        dic1.Add("byval", EToken.eByVal)
        dic1.Add("call", EToken.eCall)
        dic1.Add("case", EToken.eCase)
        dic1.Add("catch", EToken.eCatch)
        dic1.Add("class", EToken.eClass)

        dic1.Add("const", EToken.eConst)
        dic1.Add("constructor", EToken.eConstructor)

        dic1.Add("default", EToken.eDefault)
        dic1.Add("var", EToken.eVar)
        dic1.Add("do", EToken.eDo)
        dic1.Add("each", EToken.eEach)
        dic1.Add("else", EToken.eElse)
        dic1.Add("elseif", EToken.eElseIf)
        'dic1.Add("end", EToken.eEnd)
        dic1.Add("endif", EToken.eEndIf)
        dic1.Add("enum", EToken.eEnum)
        dic1.Add("exit", EToken.eExit)

        dic1.Add("extends", EToken.eExtends)

        dic1.Add("for", EToken.eFor)
        dic1.Add("foreach", EToken.eForeach)
        dic1.Add("from", EToken.eFrom)
        dic1.Add("function", EToken.eFunction)
        dic1.Add("get", EToken.eGet)
        dic1.Add("goto", EToken.eGoto)
        dic1.Add("handles", EToken.eHandles)
        dic1.Add("if", EToken.eIf)
        dic1.Add("implements", EToken.eImplements)

        Select Case LanguageSP
            Case ELanguage.CSharp
                dic1.Add("imports", EToken.eImports)

            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.Java
                dic1.Add("import", EToken.eImports)
        End Select

        dic1.Add("in", EToken.eIn)
        dic1.Add("instanceof", EToken.eInstanceof)
        dic1.Add("interface", EToken.eInterface)
        dic1.Add("into", EToken.eInto)
        dic1.Add("loop", EToken.eLoop)
        dic1.Add("namespace", EToken.eNamespace)
        dic1.Add("new", EToken.eNew)
        dic1.Add("of", EToken.eOf)
        dic1.Add("out", EToken.eOut)

        dic1.Add("package", EToken.ePackage)

        dic1.Add("override", EToken.eOverride)
        dic1.Add("partial", EToken.ePartial)
        dic1.Add("private", EToken.ePrivate)
        dic1.Add("ref", EToken.eRef)
        dic1.Add("return", EToken.eReturn)
        dic1.Add("select", EToken.eSelect)
        If LanguageSP = ELanguage.CSharp Then

            dic1.Add("set", EToken.eSet)
        End If

        dic1.Add("static", EToken.eShared)
        dic1.Add("struct", EToken.eStruct)

        '        dic1.Add("sub", EToken.eSub)
        dic1.Add("switch", EToken.eSwitch)
        dic1.Add("then", EToken.eThen)
        dic1.Add("throw", EToken.eThrow)
        dic1.Add("try", EToken.eTry)
        dic1.Add("using", EToken.eUsing)
        dic1.Add("virtual", EToken.eVirtual)
        dic1.Add("where", EToken.eWhere)
        dic1.Add("while", EToken.eWhile)

        'dic1.Add("@else", EToken.ePElse)
        'dic1.Add("@end", EToken.ePEnd)
        'dic1.Add("@id", EToken.eId)
        'dic1.Add("@if", EToken.ePIf)
        'dic1.Add("@int", EToken.eInt)
        'dic1.Add("@hex", EToken.eHex)
        'dic1.Add("@set", EToken.ePSet)

        dic1.Add("/*", EToken.eBlockComment)
        dic1.Add("//", EToken.eLineComment)

        'dic1.Add("<?", EToken.eXMLST)
        'dic1.Add("<!", EToken.eMATHST)
        'dic1.Add("]@", EToken.eMATHED)

        dic1.Add("=", EToken.eASN)
        dic1.Add("+=", EToken.eADDEQ)
        dic1.Add("-=", EToken.eSUBEQ)
        dic1.Add("*=", EToken.eMULEQ)
        dic1.Add("/=", EToken.eDIVEQ)
        dic1.Add("%=", EToken.eMODEQ)

        dic1.Add("+", EToken.eADD)
        dic1.Add("-", EToken.eMns)
        dic1.Add("%", EToken.eMOD)
        dic1.Add("&", EToken.eAnp)
        dic1.Add("(", EToken.eLP)
        dic1.Add(")", EToken.eRP)
        dic1.Add("*", EToken.eMUL)
        dic1.Add(",", EToken.eComma)
        dic1.Add(".", EToken.eDot)
        dic1.Add("/", EToken.eDIV)
        dic1.Add(":", EToken.eMMB)
        dic1.Add(";", EToken.eSM)
        dic1.Add("?", EToken.Question)
        dic1.Add("[", EToken.eLB)
        dic1.Add("]", EToken.eRB)
        dic1.Add("^", EToken.eHAT)
        dic1.Add("{", EToken.eLC)
        dic1.Add("|", EToken.eBitOR)
        dic1.Add("}", EToken.eRC)
        dic1.Add("~", EToken.eTilde)

        dic1.Add("++", EToken.eINC)
        dic1.Add("--", EToken.eDEC)

        dic1.Add("==", EToken.eEq)
        dic1.Add("!=", EToken.eNE)
        dic1.Add("<", EToken.eLT)
        dic1.Add(">", EToken.eGT)
        dic1.Add("<=", EToken.eLE)
        dic1.Add(">=", EToken.eGE)

        dic1.Add("||", EToken.eOR)
        dic1.Add("&&", EToken.eAnd)
        dic1.Add("!", EToken.eNot)

        'dic1.Add("->", EToken.eRARROW)

        'dic1.Add("∀", EToken.eALL)
        'dic1.Add("∃", EToken.eEXIST)
        'dic1.Add("∈", EToken.eElement)
        'dic1.Add("∧", EToken.eLAnd)
        'dic1.Add("∨", EToken.eLOr)
        'dic1.Add("∩", EToken.eINTERSECTION)
        'dic1.Add("∪", EToken.eUNION)
        'dic1.Add("⊆", EToken.eSUBSET)

        ' for Add
        For Each key1 In dic1.Keys
            vTkn.Add(key1.ToLower(), dic1(key1))
        Next

        If vTknName Is Nothing Then
            vTknName = New Dictionary(Of EToken, String)()
            ' for Add
            For Each key1 In dic1.Keys
                vTknName.Add(dic1(key1), key1)
            Next
        End If

        vTknName.Add(EToken.eAbstract, "")
        If LanguageSP <> ELanguage.CSharp Then
            vTknName.Add(EToken.eAs, ":")
        End If
        vTknName.Add(EToken.eIs, "==")
        vTknName.Add(EToken.ePublic, "")
    End Sub

    Function NewToken(type1 As EToken, str1 As String, pos1 As Integer) As TToken
        Dim tkn1 As New TToken

        tkn1.TypeTkn = type1
        tkn1.StrTkn = str1
        tkn1.PosTkn = pos1

        Return tkn1
    End Function

    Public Overrides Function Lex(src_text As String) As TList(Of TToken)
        Dim v1 As New TList(Of TToken)
        Dim cur1 As Integer, spc As Integer
        Dim src_len As Integer
        Dim k1 As Integer
        Dim ch1 As Char
        Dim ch2 As Char
        Dim str1 As String = Nothing
        Dim type1 As EToken
        Dim prv_type As EToken
        Dim tkn1 As TToken
        Dim ok As Boolean
        Dim dmp As New TStringWriter

        src_len = src_text.Length
        v1 = New TList(Of TToken)()

        cur1 = 0
        prv_type = EToken.eUnknown

        Do While True
            tkn1 = Nothing

            spc = 0
            Do While cur1 < src_len AndAlso Char.IsWhiteSpace(src_text(cur1))
                If src_text(cur1) = vbLf Then
                    dmp.WriteLine("")
                End If
                cur1 += 1
            Loop
            If src_len <= cur1 Then
                Exit Do
            End If

            ch1 = src_text(cur1)
            If cur1 + 1 < src_text.Length Then
                ch2 = src_text(cur1 + 1)
            Else
                ch2 = ChrW(0)
            End If

            If Char.IsDigit(ch1) Then
                ' 数字の場合

                If ch1 = "0"c AndAlso ch2 = "x"c Then
                    ' 16進数の場合

                    ' for Find
                    For k1 = cur1 + 2 To src_text.Length - 1
                        ch2 = src_text(k1)
                        If Not (Char.IsDigit(ch2) OrElse "A"c <= ch2 AndAlso ch2 <= "F"c) Then
                            Exit For
                        End If
                    Next

                    str1 = TSys.Substring(src_text, cur1, k1)
                    tkn1 = NewToken(EToken.eHex, str1, cur1)

                    cur1 = k1
                Else
                    ' 10進数の場合

                    For k1 = cur1 + 1 To src_text.Length - 1
                        ch2 = src_text(k1)
                        If Not Char.IsDigit(ch2) AndAlso ch2 <> "."c Then
                            Exit For
                        End If
                    Next
                    If k1 < src_text.Length AndAlso src_text(k1) = "f"c Then
                        k1 = k1 + 1
                    End If

                    str1 = TSys.Substring(src_text, cur1, k1)
                    tkn1 = NewToken(EToken.eInt, str1, cur1)

                    cur1 = k1
                End If

            ElseIf 256 <= AscW(ch1) OrElse Char.IsLetter(ch1) OrElse ch1 = "_"c Then
                '  英字の場合

                ' for Find
                For k1 = cur1 + 1 To src_text.Length - 1

                    ch2 = src_text(k1)
                    If AscW(ch2) < 256 AndAlso Not Char.IsLetterOrDigit(ch2) AndAlso ch2 <> "_"c Then
                        '  半角で英数字や"_"でない場合

                        Exit For
                    End If
                Next
                str1 = TSys.Substring(src_text, cur1, k1)
                If vTkn.ContainsKey(str1.ToLower()) Then
                    '  予約語の場合

                    type1 = vTkn(str1.ToLower())
                    If type1 = EToken.eGetType AndAlso (prv_type = EToken.eDot OrElse prv_type = EToken.eFunction) Then
                        type1 = EToken.eId
                    ElseIf type1 = EToken.eSelect AndAlso prv_type = EToken.eDot Then
                        type1 = EToken.eId
                    End If
                Else
                    '  識別子の場合

                    type1 = EToken.eId
                End If
                tkn1 = NewToken(type1, str1, cur1)

                cur1 = k1

            ElseIf ch1 = """"c OrElse ch1 = "'"c Then
                '  引用符の場合

                Dim quo1 As Char = ch1
                Dim sw As New TStringWriter
                For k1 = cur1 + 1 To src_len - 1
                    ch2 = src_text(k1)
                    If ch2 = quo1 Then
                        If ch2 = "'"c Then
                            tkn1 = New TToken(EToken.eChar, sw.ToString(), cur1)
                        Else
                            tkn1 = New TToken(EToken.eString, sw.ToString(), cur1)
                        End If
                        cur1 = k1 + 1
                        Exit For
                    End If

                    If ch2 = "\"c Then
                        Debug.Assert(k1 + 1 < src_len)
                        Select Case src_text(k1 + 1)
                            Case "n"c
                                sw.Write(vbLf)
                            Case "r"c
                                sw.Write(vbCr)
                            Case "t"c
                                sw.Write(vbTab)
                            Case "b"c
                                sw.Write(vbBack)
                            Case """"c
                                sw.Write(""""c)
                            Case "'"c
                                sw.Write("'"c)
                            Case "\"c
                                sw.Write("\"c)
                            Case "0"c
                                sw.Write(ChrW(0))
                            Case Else
                                Debug.Assert(False)
                        End Select
                        k1 += 1
                    Else
                        sw.Write(ch2)
                    End If
                Next

            ElseIf ch1 = "/"c AndAlso ch2 = "/"c Then
                ' 行コメントの場合

                k1 = src_text.IndexOf(vbLf, cur1)
                If k1 = -1 Then
                    k1 = src_len
                End If
                str1 = src_text.Substring(cur1, k1 - cur1)
                tkn1 = New TToken(EToken.eLineComment, str1, cur1)
                cur1 = k1

            ElseIf ch1 = "/"c AndAlso ch2 = "*"c Then
                ' 複数行コメントの場合

                k1 = src_text.IndexOf("*/", cur1)
                Debug.Assert(k1 <> -1)
                str1 = src_text.Substring(cur1, k1 + 2 - cur1)
                tkn1 = New TToken(EToken.eBlockComment, str1, cur1)
                cur1 = k1 + 2

            ElseIf ch1 = "@"c Then
                For k1 = cur1 + 1 To src_text.Length - 1
                    ch2 = src_text(k1)
                    If Not Char.IsLetterOrDigit(ch2) AndAlso ch2 <> "_"c Then
                        Exit For
                    End If
                Next
                str1 = src_text.Substring(cur1, k1 - cur1)
                Debug.Assert(str1 = "@_Weak" OrElse str1 = "@_Invariant" OrElse str1 = "@_Parent")

                tkn1 = New TToken(EToken.Attribute, str1, cur1)
                cur1 += str1.Length

            Else
                '  記号の場合

                ok = False
                type1 = EToken.eUnknown
                If cur1 + 1 < src_len Then
                    '  2文字の記号を調べる

                    str1 = TSys.Substring(src_text, cur1, cur1 + 2)
                    ok = vTkn.ContainsKey(str1)
                    If ok Then
                        type1 = vTkn(str1)
                    End If
                End If
                If Not ok Then
                    '  1文字の記号を調べる

                    str1 = TSys.Substring(src_text, cur1, cur1 + 1)
                    If vTkn.ContainsKey(str1) Then
                        type1 = vTkn(str1)
                    Else
                        '  ない場合

                        Debug.Print("lex str err [{0}]", TSys.Substring(src_text, cur1, cur1 + 2))
                        Chk(False)
                    End If
                End If

                tkn1 = NewToken(type1, str1, cur1)
                cur1 = cur1 + str1.Length
            End If

            ' Debug.WriteLine("token:{0} {1}", tkn1.StrTkn, tkn1.TypeTkn)
            tkn1.SpcTkn = spc
            v1.Add(tkn1)
            prv_type = tkn1.TypeTkn

            dmp.Write("{0}:{1} ", tkn1.TypeTkn, tkn1.StrTkn)
        Loop

        TFile.WriteAllText("C:\usr\prj\MyIDE\etc\TEST\lex.txt", dmp.ToString())

        Return v1
    End Function

    Public Overrides Sub RegAllClass(prj1 As TProject, src1 As TSourceFile)
        Dim id1 As TToken, k1 As Integer, cla1 As TClass, cla2 As TClass, id2 As TToken
        Dim v As TList(Of TToken) = src1.InputTokenList

        k1 = 0
        Do While k1 < v.Count
            Dim is_abstract As Boolean = False

            If v(k1).TypeTkn = EToken.eAbstract Then
                is_abstract = True
                GetTkn(EToken.eAbstract)
            End If

            Select Case v(k1).TypeTkn
                Case EToken.eDelegate, EToken.eClass, EToken.eStruct, EToken.eInterface, EToken.eEnum

                    k1 += 1
                    Debug.Assert(v(k1).TypeTkn = EToken.eId)

                    id1 = v(k1)

                    If v(k1).TypeTkn = EToken.eDelegate Then
                        Debug.Assert(prj1.GetCla(id1.StrTkn) Is Nothing)

                        cla1 = New TDelegate(prj1, id1.StrTkn)
                        prj1.SimpleParameterizedClassList.Add(cla1)
                        prj1.SimpleParameterizedClassTable.Add(cla1.NameCla(), cla1)
                    Else
                        cla1 = prj1.RegCla(id1.StrTkn)
                    End If

                    If k1 + 2 < v.Count AndAlso v(k1 + 1).TypeTkn = EToken.eLT Then
                        cla1.GenericType = EGeneric.ParameterizedClass

                        cla1.GenCla = New TList(Of TClass)()

                        k1 += 2
                        Do While k1 < v.Count
                            id2 = v(k1)

                            cla2 = New TClass(prj1, id2.StrTkn)
                            cla2.IsParamCla = True
                            cla2.GenericType = EGeneric.ArgumentClass
                            cla1.GenCla.Add(cla2)

                            k1 += 1
                            If v(k1).TypeTkn = EToken.eGT Then
                                Exit Do
                            End If

                            Debug.Assert(v(k1).TypeTkn = EToken.eComma)
                            k1 += 1
                        Loop

                        prj1.dicCmpCla.Add(cla1, New TList(Of TClass)())
                    Else
                        cla1.GenericType = EGeneric.SimpleClass
                    End If

                    Select Case cla1.NameCla()
                        Case "Object"
                            PrjParse.ObjectType = cla1
                        Case "System"
                            PrjParse.SystemType = cla1
                        Case "string"
                            PrjParse.StringType = cla1
                        Case "char"
                            PrjParse.CharType = cla1
                        Case "int"
                            PrjParse.IntType = cla1
                        Case "number"
                            PrjParse.DoubleType = cla1
                        Case "Type"
                            PrjParse.TypeType = cla1
                        Case "boolean"
                            PrjParse.BoolType = cla1
                    End Select

                    Debug.Print("クラス登録 {0}", cla1.LongName())
            End Select

            k1 += 1
        Loop
    End Sub

    Function ArgumentExpressionList(app1 As TApply) As TApply
        Dim trm1 As TTerm, right_token As EToken

        Select Case CurTkn.TypeTkn
            Case EToken.eLP
                GetTkn(EToken.eLP)
                right_token = EToken.eRP

            Case EToken.eLB
                GetTkn(EToken.eLB)
                right_token = EToken.eRB

            Case Else
                Debug.Assert(False)
        End Select

        If CurTkn.TypeTkn = EToken.eOf Then
            GetTkn(EToken.eOf)
        End If
        '                 b_of = true;
        If CurTkn.TypeTkn <> right_token Then
            Do While True
                trm1 = TermExpression()
                app1.AddInArg(trm1)

                If CurTkn.TypeTkn <> EToken.eComma Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If
        GetTkn(right_token)

        Return app1
    End Function

    Function CallExpression(trm1 As TTerm) As TTerm
        Do While CurTkn.TypeTkn = EToken.eLP OrElse CurTkn.TypeTkn = EToken.eLB
            Dim app1 As TApply = TApply.MakeAppCall(trm1)
            ArgumentExpressionList(app1)
            trm1 = app1
        Loop

        Return trm1
    End Function

    '   配列の構文解析
    Function ArrayExpression() As TArray
        Dim arr1 As TArray
        Dim trm1 As TTerm

        arr1 = New TArray()
        GetTkn(EToken.eLB)
        If CurTkn.TypeTkn <> EToken.eRB Then
            Do While True
                trm1 = TermExpression()
                arr1.TrmArr.Add(trm1)
                If CurTkn.TypeTkn = EToken.eRB Then
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If
        GetTkn(EToken.eRB)

        Return arr1
    End Function

    Function NewExpression() As TApply
        Dim tkn1 As TToken
        Dim type1 As TClass
        Dim app1 As TApply

        tkn1 = GetTkn(EToken.eNew)
        type1 = ReadType(True)
        app1 = TApply.MakeAppNew(type1)
        If CurTkn.TypeTkn = EToken.eLP Then
            ArgumentExpressionList(app1)
        End If
        If CurTkn.TypeTkn = EToken.eLB Then
            ' 配列の場合
            app1.IniApp = ArrayExpression()

            ' 配列型に変える
            app1.NewApp = PrjParse.GetArrCla(app1.NewApp, 1)
        End If
        If CurTkn.TypeTkn = EToken.eFrom Then
            GetTkn(EToken.eFrom)

            Debug.Assert(CurTkn.TypeTkn = EToken.eLB)
            app1.IniApp = ArrayExpression()
        End If

        Return app1
    End Function

    ' From i In v1 Where i Mod 2 = 0 Select AA(i)
    Function FromExpression() As TFrom
        Dim from1 As New TFrom

        GetTkn(EToken.eFrom)
        Dim id1 As TToken = GetTkn(EToken.eId)
        from1.VarQry = New TVariable(id1, Nothing)

        GetTkn(EToken.eIn)
        from1.SeqQry = TermExpression()

        If CurTkn.TypeTkn = EToken.eWhere Then

            GetTkn(EToken.eWhere)
            from1.CndQry = TermExpression()
        End If

        If CurTkn.TypeTkn = EToken.eSelect Then

            GetTkn(EToken.eSelect)
            from1.SelFrom = TermExpression()
        End If

        If CurTkn.TypeTkn = EToken.eTake Then

            GetTkn(EToken.eTake)
            from1.TakeFrom = TermExpression()
        End If

        If CurTkn.TypeTkn = EToken.eFrom Then
            from1.InnerFrom = FromExpression()
        End If

        Return from1
    End Function

    ' Aggregate x In v Into Sum(x.Value)
    Function AggregateExpression() As TAggregate
        Dim aggr1 As New TAggregate, id1 As TToken, id2 As TToken

        GetTkn(EToken.eAggregate)
        id1 = GetTkn(EToken.eId)
        aggr1.VarQry = New TVariable(id1, Nothing)

        GetTkn(EToken.eIn)
        aggr1.SeqQry = TermExpression()

        If CurTkn.TypeTkn = EToken.eWhere Then

            GetTkn(EToken.eWhere)
            aggr1.CndQry = TermExpression()
        End If

        GetTkn(EToken.eInto)

        id2 = GetTkn(EToken.eId)
        Select Case id2.StrTkn
            Case "Sum"
                aggr1.FunctionAggr = EAggregateFunction.eSum
            Case "Max"
                aggr1.FunctionAggr = EAggregateFunction.eMax
            Case "Min"
                aggr1.FunctionAggr = EAggregateFunction.eMin
            Case Else
                Debug.Assert(False)
        End Select


        GetTkn(EToken.eLP)

        aggr1.IntoAggr = TermExpression()

        GetTkn(EToken.eRP)

        Return aggr1
    End Function

    Function PrimaryExpression() As TTerm
        Dim ref1 As TReference
        Dim trm1 As TTerm, trm2 As TTerm
        Dim ret1 As TTerm
        Dim id1 As TToken, tkn1 As TToken
        Dim type1 As TClass
        Dim app1 As TApply

        Select Case CurTkn.TypeTkn
            Case EToken.eId
                id1 = GetTkn(EToken.eId)
                If CurTkn.TypeTkn = EToken.eLP AndAlso NxtTkn.TypeTkn = EToken.eOf Then
                    ' ジェネリック型の場合

                    ' ジェネリック型の構文解析
                    type1 = ReadGenType(id1)
                    ref1 = New TReference(type1)
                    Return ref1
                End If

                ref1 = New TReference(id1)
                Return CallExpression(ref1)

            Case EToken.eDot
                trm1 = Nothing
                Do While CurTkn.TypeTkn = EToken.eDot
                    GetTkn(EToken.eDot)
                    id1 = GetTkn(EToken.eId)
                    trm2 = New TDot(trm1, id1.StrTkn)
                    trm1 = CallExpression(trm2)
                Loop

                Return trm1

            Case EToken.eBase
                GetTkn(EToken.eBase)
                GetTkn(EToken.eDot)
                Debug.Assert(CurTkn.TypeTkn = EToken.eNew OrElse CurTkn.TypeTkn = EToken.eId)
                tkn1 = GetTkn(EToken.eUnknown)
                app1 = TApply.MakeAppBase(tkn1)
                ArgumentExpressionList(app1)
                Return app1

            Case EToken.eLP
                GetTkn(EToken.eLP)
                trm1 = New TParenthesis(TermExpression())
                GetTkn(EToken.eRP)
                ret1 = CallExpression(trm1)

            Case EToken.eLB
                Return ArrayExpression()

            Case EToken.eString, EToken.eChar, EToken.eInt, EToken.eHex
                tkn1 = GetTkn(EToken.eUnknown)
                ret1 = New TConstant(tkn1.TypeTkn, tkn1.StrTkn)

            Case EToken.eNew
                Return NewExpression()

            Case EToken.eInstanceof
                GetTkn(EToken.eInstanceof)
                trm1 = AdditiveExpression()
                GetTkn(EToken.eIs)
                type1 = ReadType(False)
                Return TApply.NewTypeOf(trm1, type1)

            Case EToken.eGetType
                GetTkn(EToken.eGetType)
                GetTkn(EToken.eLP)
                type1 = ReadType(False)
                GetTkn(EToken.eRP)
                Return TApply.MakeAppGetType(type1)

            Case EToken.eLT
                GetTkn(EToken.eLT)
                type1 = ReadType(False)
                GetTkn(EToken.eGT)
                trm1 = AdditiveExpression()
                trm1.CastType = type1

                Return trm1

            Case EToken.eAddressOf
                GetTkn(EToken.eAddressOf)
                trm1 = TermExpression()
                Debug.Assert(TypeOf trm1 Is TReference)
                CType(trm1, TReference).IsAddressOf = True
                Return trm1

            Case EToken.eFrom
                Return FromExpression()

            Case EToken.eAggregate
                Return AggregateExpression()

            Case Else
                Chk(False)
                Return Nothing
        End Select

        Return ret1
    End Function

    Function DotExpression() As TTerm
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim id1 As TToken

        trm1 = PrimaryExpression()

        Do While CurTkn.TypeTkn = EToken.eDot
            GetTkn(EToken.eDot)
            id1 = GetTkn(EToken.eId)
            trm2 = New TDot(trm1, id1.StrTkn)
            trm1 = CallExpression(trm2)
        Loop

        Return trm1
    End Function

    Function IncDecExpression() As TTerm
        Dim trm1 As TTerm = DotExpression()

        If CurTkn.TypeTkn = EToken.eINC OrElse CurTkn.TypeTkn = EToken.eDEC Then
            Dim tkn1 As TToken = GetTkn(EToken.eUnknown)

            Return TApply.MakeApp1Opr(tkn1, trm1)
        End If

        Return trm1
    End Function


    Function UnaryExpression() As TTerm
        Dim tkn1 As TToken
        Dim trm1 As TTerm

        If CurTkn.TypeTkn = EToken.eMns Then
            tkn1 = GetTkn(EToken.eMns)
            trm1 = IncDecExpression()

            Return TApply.MakeApp1Opr(tkn1, trm1)
        End If

        Return IncDecExpression()
    End Function

    Function MultiplicativeExpression() As TTerm
        Dim trm1 As TTerm
        Dim tkn1 As TToken
        Dim trm2 As TTerm

        trm1 = UnaryExpression()
        If CurTkn.TypeTkn = EToken.eMUL OrElse CurTkn.TypeTkn = EToken.eDIV OrElse CurTkn.TypeTkn = EToken.eMOD Then
            tkn1 = GetTkn(EToken.eUnknown)
            trm2 = MultiplicativeExpression()

            Return TApply.MakeApp2Opr(tkn1, trm1, trm2)
        End If

        Return trm1
    End Function

    Public Function AdditiveExpression() As TTerm
        Dim trm1 As TTerm
        Dim tkn1 As TToken
        Dim trm2 As TTerm

        trm1 = MultiplicativeExpression()
        If CurTkn.TypeTkn = EToken.eADD OrElse CurTkn.TypeTkn = EToken.eMns Then
            tkn1 = GetTkn(EToken.eUnknown)
            trm2 = AdditiveExpression()

            Return TApply.MakeApp2Opr(tkn1, trm1, trm2)
        End If
        Return trm1
    End Function

    Public Function RelationalExpression() As TTerm
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim type1 As EToken
        '      Dim type2 As TClass
        'Dim par1 As Boolean

        trm1 = AdditiveExpression()
        Select Case CurTkn.TypeTkn
            Case EToken.eEq, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eNE, EToken.eLT, EToken.eGT, EToken.eLE, EToken.eGE, EToken.eIs, EToken.eIsNot, EToken.eInstanceof
                type1 = CurTkn.TypeTkn
                GetTkn(EToken.eUnknown)
                trm2 = AdditiveExpression()
                Return TApply.NewOpr2(type1, trm1, trm2)

            Case Else
                Return trm1
        End Select
    End Function

    Function NotExpression() As TTerm
        Dim trm1 As TTerm
        Dim type1 As EToken
        Dim app1 As TApply

        If CurTkn.TypeTkn = EToken.eNot Then
            type1 = CurTkn.TypeTkn
            GetTkn(type1)
            trm1 = NotExpression()
            Debug.Assert(TypeOf trm1 Is TApply OrElse TypeOf trm1 Is TReference OrElse TypeOf trm1 Is TParenthesis)
            If TypeOf trm1 Is TApply Then
                app1 = CType(trm1, TApply)
                app1.Negation = Not app1.Negation
                Return app1
            Else
                Dim opr1 As TApply = TApply.NewOpr(type1)

                opr1.AddInArg(trm1)
                Return opr1
            End If
        End If

        Return RelationalExpression()
    End Function

    Function BitOrExpression() As TTerm
        Dim trm1 As TTerm
        Dim opr1 As TApply
        Dim type1 As EToken

        trm1 = NotExpression()
        If CurTkn.TypeTkn = EToken.eBitOR Then

            type1 = CurTkn.TypeTkn
            opr1 = TApply.NewOpr(type1)
            opr1.AddInArg(trm1)
            Do While CurTkn.TypeTkn = type1
                GetTkn(type1)
                opr1.AddInArg(NotExpression())
            Loop

            Return opr1
        Else

            Return trm1
        End If
    End Function

    Function AndExpression() As TTerm
        Dim trm1 As TTerm
        Dim opr1 As TApply
        Dim type1 As EToken

        trm1 = BitOrExpression()
        If CurTkn.TypeTkn = EToken.eAnd OrElse CurTkn.TypeTkn = EToken.eAnp Then

            type1 = CurTkn.TypeTkn
            opr1 = TApply.NewOpr(type1)
            opr1.AddInArg(trm1)
            Do While CurTkn.TypeTkn = type1
                GetTkn(type1)
                opr1.AddInArg(BitOrExpression())
            Loop

            Return opr1
        Else

            Return trm1
        End If
    End Function

    Function OrExpression() As TTerm
        Dim trm1 As TTerm
        Dim opr1 As TApply
        Dim type1 As EToken

        trm1 = AndExpression()
        If CurTkn.TypeTkn = EToken.eOR Then

            type1 = CurTkn.TypeTkn
            opr1 = TApply.NewOpr(type1)
            opr1.AddInArg(trm1)
            Do While CurTkn.TypeTkn = type1
                GetTkn(type1)
                opr1.AddInArg(AndExpression())
            Loop

            Return opr1
        Else

            Return trm1
        End If
    End Function

    Public Function TermExpression() As TTerm
        Return OrExpression()
    End Function

    Public Function AssignmentExpression() As TStatement
        Dim trm1 As TTerm
        Dim trm2 As TTerm
        Dim rel1 As TApply
        Dim asn1 As TAssignment
        Dim eq1 As TToken

        trm1 = CType(AdditiveExpression(), TTerm)

        Select Case CurTkn.TypeTkn
            Case EToken.eASN, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ
                eq1 = GetTkn(EToken.eUnknown)
                trm2 = CType(TermExpression(), TTerm)
                rel1 = TApply.NewOpr2(eq1.TypeTkn, trm1, trm2)
                asn1 = New TAssignment(rel1)

                Return asn1
        End Select

        Dim call1 As New TCall(CType(trm1, TApply))

        Return call1
    End Function

    Function ReadReturn(type_tkn As EToken) As TReturn
        Dim ret1 As TReturn

        GetTkn(type_tkn)
        If CurTkn.TypeTkn = EToken.eSM Then

            ret1 = New TReturn(Nothing, type_tkn = EToken.eYield)
        Else
            ret1 = New TReturn(TermExpression(), type_tkn = EToken.eYield)
        End If
        GetTkn(EToken.eSM)

        Return ret1
    End Function

    Function ReadExit() As TStatement
        Dim stmt1 As New TExit

        GetTkn(EToken.eExit)
        Select Case CurTkn.TypeTkn
            Case EToken.eDo
                stmt1.TypeStmt = EToken.eExitDo
            Case EToken.eFor
                stmt1.TypeStmt = EToken.eExitFor
            Case EToken.eSub
                stmt1.TypeStmt = EToken.eExitSub
            Case Else
                Chk(False)
        End Select
        GetTkn(EToken.eUnknown)

        Return stmt1
    End Function

    Function ReadThrow() As TStatement
        Dim stmt1 As TThrow

        GetTkn(EToken.eThrow)
        stmt1 = New TThrow(CType(TermExpression(), TTerm))

        Return stmt1
    End Function

    Function ReadTry() As TStatement
        Dim try1 As New TTry

        GetTkn(EToken.eTry)

        try1.BlcTry = ReadBlock(try1)
        GetTkn(EToken.eCatch)
        try1.VarCatch = New TList(Of TVariable)()
        try1.VarCatch.Add(ReadVariable())
        try1.BlcCatch = ReadBlock(try1)

        Return try1
    End Function

    Function ReadDo() As TStatement
        Dim for1 As New TFor

        for1.IsDo = True

        GetTkn(EToken.eDo)
        GetTkn(EToken.eWhile)
        for1.CndFor = CType(TermExpression(), TTerm)

        for1.BlcFor = ReadBlock(for1)

        Return for1
    End Function

    Function ReadFor() As TStatement
        Dim for1 As New TFor
        Dim id1 As TToken

        GetTkn(EToken.eFor)

        If CurTkn.TypeTkn = EToken.eLP Then

            GetTkn(EToken.eLP)
            GetTkn(EToken.eVar)

            for1.IdxVarFor = ReadVariable()
            GetTkn(EToken.eSM)

            for1.CndFor = TermExpression()
            GetTkn(EToken.eSM)

            for1.StepStmtFor = AssignmentExpression()
            GetTkn(EToken.eRP)
        Else

            GetTkn(EToken.eEach)

            If CurTkn.TypeTkn = EToken.eId Then

                id1 = GetTkn(EToken.eId)
                for1.InVarFor = New TVariable(id1.StrTkn, Nothing)
            End If

            If CurTkn.TypeTkn = EToken.eAt Then

                GetTkn(EToken.eAt)

                Do While True
                    GetTkn(EToken.eId)
                    If CurTkn.TypeTkn <> EToken.eComma Then
                        Exit Do
                    End If
                    GetTkn(EToken.eComma)
                Loop
            End If

            GetTkn(EToken.eIn)

            for1.InTrmFor = CType(TermExpression(), TTerm)
        End If

        for1.BlcFor = ReadBlock(for1)

        Return for1
    End Function

    Function ReadSelect() As TSelect
        Dim sel2 As New TSelect, case2 As TCase

        GetTkn(EToken.eSwitch)
        GetTkn(EToken.eLP)
        sel2.TrmSel = TermExpression()
        GetTkn(EToken.eRP)
        GetTkn(EToken.eLC)

        Do While CurTkn.TypeTkn = EToken.eCase OrElse CurTkn.TypeTkn = EToken.eDefault
            case2 = New TCase()
            case2.DefaultCase = (CurTkn.TypeTkn = EToken.eDefault)
            GetTkn(EToken.eUnknown)

            If Not case2.DefaultCase Then

                Do While True
                    Dim trm1 As TTerm = TermExpression()
                    case2.TrmCase.Add(trm1)
                    If CurTkn.TypeTkn <> EToken.eComma Then
                        Exit Do
                    End If
                    GetTkn(EToken.eComma)
                Loop
            End If
            GetTkn(EToken.eMMB)

            sel2.CaseSel.Add(case2)
            case2.BlcCase = ReadCaseBlock(sel2)

            If case2.DefaultCase Then
                Exit Do
            End If
        Loop

        GetTkn(EToken.eRC)

        Return sel2
    End Function

    Function ReadIf() As TStatement
        Dim if2 As New TIf
        Dim if_cnd As TTerm

        GetTkn(EToken.eIf)
        GetTkn(EToken.eLP)
        if_cnd = CType(TermExpression(), TTerm)
        GetTkn(EToken.eRP)

        Dim if_blc As New TIfBlock(if_cnd, ReadBlock(if2))
        if2.IfBlc.Add(if_blc)

        Do While CurTkn.TypeTkn = EToken.eElse

            GetTkn(EToken.eElse)
            If NxtTkn.TypeTkn = EToken.eIf Then
                GetTkn(EToken.eIf)
                if_blc = New TIfBlock(TermExpression(), ReadBlock(if2))
                if2.IfBlc.Add(if_blc)
            Else
                if_blc = New TIfBlock(Nothing, ReadBlock(if2))
                if2.IfBlc.Add(if_blc)
                Exit Do
            End If
        Loop

        Return if2
    End Function

    Function ReadLocalVariableDeclaration() As TVariableDeclaration
        Dim stmt1 As New TVariableDeclaration
        Dim var1 As TVariable

        GetTkn(EToken.eVar)
        stmt1.TypeStmt = EToken.eVarDecl
        stmt1.ModDecl = New TModifier()
        Do While True
            var1 = ReadVariable()
            stmt1.VarDecl.Add(var1)
            CurBlc.VarBlc.Add(var1)
            If CurTkn.TypeTkn <> EToken.eComma Then
                Exit Do
            End If
            GetTkn(EToken.eComma)
        Loop

        stmt1.TailCom = ReadTailCom()
        GetTkn(EToken.eSM)

        Return stmt1
    End Function

    '  ブロックの構文解析をする
    Function ReadBlockSub(up1 As Object, case_block As Boolean) As TBlock
        Dim blc1 As New TBlock
        Dim blc_sv As TBlock

        blc_sv = CurBlc
        CurBlc = blc1

        If Not case_block Then
            GetTkn(EToken.eLC)
        End If

        Do While CurTkn.TypeTkn <> EToken.eRC AndAlso Not (case_block AndAlso CurTkn.TypeTkn = EToken.eCase)
            Dim stmt1 As TStatement = ReadStatement()
            If stmt1 Is Nothing Then
                Exit Do
            End If
            CurBlc.AddStmtBlc(stmt1)
        Loop

        If CurTkn.TypeTkn = EToken.eRC Then

            GetTkn(EToken.eRC)
        End If

        CurBlc = blc_sv

        Return blc1
    End Function

    '  ブロックの構文解析をする
    Function ReadBlock(up1 As Object) As TBlock
        Return ReadBlockSub(up1, False)
    End Function

    '  ブロックの構文解析をする
    Function ReadCaseBlock(up1 As Object) As TBlock
        Return ReadBlockSub(up1, True)
    End Function

    Function ReadStatement() As TStatement
        Dim mod1 As TModifier, stmt1 As TStatement = Nothing

        '  修飾子を調べる
        mod1 = ReadModifier()

        Select Case CurTkn.TypeTkn
            Case EToken.eVar
                stmt1 = ReadLocalVariableDeclaration()

            Case EToken.eIf
                stmt1 = ReadIf()

            Case EToken.eReturn, EToken.eYield
                stmt1 = ReadReturn(CurTkn.TypeTkn)

            Case EToken.eDo
                stmt1 = ReadDo()

            Case EToken.eSwitch
                stmt1 = ReadSelect()

            Case EToken.eFor
                stmt1 = ReadFor()

            Case EToken.eExit
                stmt1 = ReadExit()

            Case EToken.eId, EToken.eBase, EToken.eCType, EToken.eDot
                stmt1 = AssignmentExpression()
                GetTkn(EToken.eSM)

            Case EToken.eTry
                stmt1 = ReadTry()

            Case EToken.eThrow
                stmt1 = ReadThrow()

            Case EToken.eLineComment
                stmt1 = ReadLineComment()

            Case Else
                Chk(False)
        End Select

        Return stmt1
    End Function

    Function ReadVariableField(is_field As Boolean) As TVariable
        Dim var1 As TVariable
        Dim id1 As TToken
        Dim app1 As TApply

        If is_field Then
            var1 = New TField()
        Else
            var1 = New TVariable()
        End If

        id1 = GetTkn(EToken.eId)
        var1.NameVar = id1.StrTkn

        If CurTkn.TypeTkn = EToken.eMMB Then

            GetTkn(EToken.eMMB)

            If CurTkn.TypeTkn = EToken.eNew Then
                app1 = NewExpression()
                var1.TypeVar = app1.NewApp
                var1.InitVar = app1

                Return var1
            End If

            var1.TypeVar = ReadType(False)
        End If

        If CurTkn.TypeTkn = EToken.eASN Then
            GetTkn(EToken.eASN)

            var1.InitVar = AdditiveExpression()
        End If

        Return var1
    End Function

    Function ReadVariable() As TVariable
        Return ReadVariableField(False)
    End Function

    Function ReadField() As TField
        Dim fld As TField = CType(ReadVariableField(True), TField)
        GetTkn(EToken.eSM)

        Return fld
    End Function

    Sub ReadFunctionArgument(arg_list As TList(Of TVariable))
        GetTkn(EToken.eLP)

        Do While CurTkn.TypeTkn <> EToken.eRP
            Dim by_ref As Boolean, param_array As Boolean
            Dim var1 As TVariable

            by_ref = False
            param_array = False
            Select Case CurTkn.TypeTkn
                Case EToken.eRef
                    by_ref = True
                    GetTkn(EToken.eRef)
                Case EToken.eParamArray
                    param_array = True
                    GetTkn(EToken.eParamArray)
            End Select

            var1 = ReadVariable()
            var1.ByRefVar = by_ref
            var1.ParamArrayVar = param_array
            arg_list.Add(var1)

            If CurTkn.TypeTkn <> EToken.eComma Then
                Exit Do
            End If
            GetTkn(EToken.eComma)
        Loop

        GetTkn(EToken.eRP)
    End Sub

    Function ReadFunction(cla1 As TClass, mod1 As TModifier) As TFunction
        Dim id1 As TToken, id2 As TToken, id3 As TToken
        Dim fnc1 As TFunction = Nothing

        Select Case CurTkn.TypeTkn
            Case EToken.eId
                id1 = GetTkn(EToken.eId)
                fnc1 = New TFunction(id1.StrTkn, Nothing)
                fnc1.TypeFnc = EToken.eFunction

            Case EToken.eConstructor
                id1 = GetTkn(EToken.eConstructor)
                fnc1 = New TFunction("New@" + cla1.NameCla(), Nothing)
                fnc1.TypeFnc = EToken.eNew

            Case EToken.eOperator
                GetTkn(EToken.eOperator)
                fnc1 = New TFunction(CurTkn.StrTkn, Nothing)
                fnc1.TypeFnc = EToken.eOperator
                fnc1.OpFnc = CurTkn.TypeTkn

            Case Else
                Debug.Assert(False)
        End Select

        fnc1.ModVar = mod1
        fnc1.ThisFnc = New TVariable(ThisName, cla1)
        fnc1.IsNew = (fnc1.TypeFnc = EToken.eNew)

        ReadFunctionArgument(fnc1.ArgFnc)

        If CurTkn.TypeTkn = EToken.eMMB Then
            GetTkn(EToken.eMMB)
            fnc1.RetType = ReadType(False)
        End If

        If CurTkn.TypeTkn = EToken.eImplements Then
            GetTkn(EToken.eImplements)

            id2 = GetTkn(EToken.eId)
            GetTkn(EToken.eDot)
            id3 = GetTkn(EToken.eId)

            fnc1.InterfaceFnc = PrjParse.GetCla(id2.StrTkn)
            Debug.Assert(fnc1.InterfaceFnc IsNot Nothing)

            fnc1.ImplFnc = New TReference(id3.StrTkn)
        End If

        If fnc1.ModFnc().isMustOverride Then
            Return fnc1
        End If

        If cla1 Is Nothing OrElse cla1.KndCla <> EClass.eInterfaceCla Then
            ' インターフェイスでない場合

            fnc1.BlcFnc = ReadBlock(fnc1)
        End If

        Return fnc1
    End Function

    Sub ReadClass(mod1 As TModifier)
        Dim cla1 As TClass, tkn1 As TToken
        Dim id1 As TToken

        PrjParse.dicGenCla.Clear()

        tkn1 = CurTkn
        GetTkn(EToken.eUnknown)
        id1 = GetTkn(EToken.eId)
        If tkn1.TypeTkn = EToken.eDelegate Then
            cla1 = PrjParse.GetDelegate(id1.StrTkn)
        Else
            cla1 = PrjParse.GetCla(id1.StrTkn)
        End If
        Debug.Assert(cla1 IsNot Nothing)
        cla1.ModVar = mod1

        PrjParse.CurSrc.ClaSrc.Add(cla1)

        Select Case tkn1.TypeTkn
            Case EToken.eDelegate
                cla1.KndCla = EClass.eDelegateCla
            Case EToken.eClass
                cla1.KndCla = EClass.eClassCla
            Case EToken.eStruct
                cla1.KndCla = EClass.eStructCla
            Case EToken.eInterface
                cla1.KndCla = EClass.eInterfaceCla
        End Select

        If cla1.GenCla IsNot Nothing Then
            ' ジェネリック クラスの場合

            ' for Add
            For Each cla_f In cla1.GenCla
                cla_f.IsParamCla = True
                PrjParse.dicGenCla.Add(cla_f.NameCla(), cla_f)
            Next

            GetTkn(EToken.eLT)

            Do While True
                GetTkn(EToken.eId)
                If CurTkn.TypeTkn = EToken.eGT Then
                    GetTkn(EToken.eGT)
                    Exit Do
                End If
                GetTkn(EToken.eComma)
            Loop
        End If

        If tkn1.TypeTkn = EToken.eDelegate Then
            Dim dlg1 As TDelegate = CType(cla1, TDelegate)

            ReadFunctionArgument(dlg1.ArgDlg)

            GetTkn(EToken.eMMB)
            dlg1.RetDlg = ReadType(False)

            Dim fnc1 As New TFunction("Invoke", dlg1.RetDlg)
            fnc1.SetModFnc(mod1)
            fnc1.ArgFnc = dlg1.ArgDlg
            fnc1.ThisFnc = New TVariable(ThisName, dlg1)
            fnc1.ClaFnc = dlg1
            dlg1.FncCla.Add(fnc1)
        Else
            If CurTkn.TypeTkn = EToken.eExtends Then
                GetTkn(EToken.eExtends)

                Dim spr_cla As TClass = ReadType(False)
                cla1.SuperClassList.Add(spr_cla)
            End If

            If CurTkn.TypeTkn = EToken.eImplements Then
                GetTkn(EToken.eImplements)

                Do While True

                    Dim spr_cla As TClass = ReadType(False)
                    cla1.InterfaceList.Add(spr_cla)

                    If CurTkn.TypeTkn <> EToken.eComma Then
                        Exit Do
                    End If
                    GetTkn(EToken.eComma)
                Loop
            End If

            If PrjParse.ObjectType IsNot cla1 AndAlso cla1.SuperClassList.Count = 0 Then
                cla1.SuperClassList.Add(PrjParse.ObjectType)
            End If

            GetTkn(EToken.eLC)

            Do While CurTkn.TypeTkn <> EToken.eRC
                Dim mod2 As TModifier = ReadModifier()

                If CurTkn.TypeTkn = EToken.eConstructor Then
                    Dim fnc1 As TFunction = ReadFunction(cla1, mod2)
                    fnc1.ClaFnc = cla1
                    cla1.FncCla.Add(fnc1)
                Else

                    Debug.Assert(CurTkn.TypeTkn = EToken.eId)
                    Select Case NxtTkn.TypeTkn
                        Case EToken.eMMB
                            Dim fld1 As TField = ReadField()
                            fld1.ModVar = mod2
                            cla1.AddFld(fld1)

                        Case EToken.eLP
                            Dim fnc1 As TFunction = ReadFunction(cla1, mod2)
                            fnc1.ClaFnc = cla1
                            cla1.FncCla.Add(fnc1)

                        Case Else
                            Debug.Assert(False)
                    End Select
                End If

            Loop
            GetTkn(EToken.eRC)
        End If

        PrjParse.dicGenCla.Clear()
        cla1.Parsed = True
        cla1.SrcCla = PrjParse.CurSrc
    End Sub

    Public Function ReadEnum() As TClass
        Dim cla1 As TClass
        Dim fld1 As TField
        Dim type1 As TClass

        GetTkn(EToken.eEnum)
        Dim id1 As TToken = GetTkn(EToken.eId)

        cla1 = PrjParse.GetCla(id1.StrTkn)
        Debug.Assert(cla1 IsNot Nothing)
        PrjParse.CurSrc.ClaSrc.Add(cla1)

        cla1.KndCla = EClass.eEnumCla
        cla1.SuperClassList.Add(PrjParse.ObjectType)
        type1 = cla1

        Do While CurTkn.TypeTkn = EToken.eId
            Dim ele1 As TToken = GetTkn(EToken.eId)
            fld1 = New TField(ele1.StrTkn, type1)
            cla1.AddFld(fld1)
        Loop
        GetTkn(EToken.eRP)
        cla1.Parsed = True

        Return cla1
    End Function

    Public Function ReadModifier() As TModifier
        Dim mod1 As TModifier

        mod1 = New TModifier()
        mod1.ValidMod = False
        Do While True
            Select Case CurTkn.TypeTkn
                Case EToken.ePartial
                    mod1.isPartial = True
                Case EToken.ePublic
                    mod1.isPublic = True
                Case EToken.eShared
                    mod1.isShared = True
                Case EToken.eConst
                    mod1.isConst = True
                Case EToken.eAbstract
                    mod1.isAbstract = True
                Case EToken.eVirtual
                    mod1.isVirtual = True
                Case EToken.eMustOverride
                    mod1.isMustOverride = True
                Case EToken.eOverride
                    mod1.isOverride = True
                Case EToken.eIterator
                    mod1.isIterator = True
                Case EToken.eProtected, EToken.eFriend, EToken.ePrivate
                Case EToken.Attribute
                    If CurTkn.StrTkn = "@_Weak" Then
                        mod1.isWeak = True
                    ElseIf CurTkn.StrTkn = "@_Invariant" Then
                        mod1.isInvariant = True
                    ElseIf CurTkn.StrTkn = "@_Parent" Then
                        mod1.isParent = True
                    Else
                        Debug.Assert(False)
                    End If

                Case EToken.eLT
                    GetTkn(EToken.eLT)
                    Do While True
                        Dim id1 As TToken

                        id1 = GetTkn(EToken.eId)
                        If id1.StrTkn = "XmlIgnoreAttribute" Then
                            mod1.isXmlIgnore = True
                        ElseIf id1.StrTkn = "_Weak" Then
                            mod1.isWeak = True
                        ElseIf id1.StrTkn = "_Parent" Then
                            mod1.isParent = True
                        ElseIf id1.StrTkn = "_Invariant" Then
                            mod1.isInvariant = True
                        Else
                            Debug.Assert(False)
                        End If
                        GetTkn(EToken.eLP)
                        GetTkn(EToken.eRP)

                        If CurTkn.TypeTkn <> EToken.eComma Then
                            Exit Do
                        End If
                        GetTkn(EToken.eComma)

                    Loop
                    Debug.Assert(CurTkn.TypeTkn = EToken.eGT)

                Case Else
                    Exit Do
            End Select
            GetTkn(EToken.eUnknown)
            mod1.ValidMod = True
        Loop

        Return mod1
    End Function

    Sub ReadImports()
        Dim id1 As TToken
        Dim tkn1 As TToken
        Dim sb1 As TStringWriter

        GetTkn(EToken.eImports)

        sb1 = New TStringWriter()
        Do While True
            id1 = GetTkn(EToken.eId)
            sb1.Append(id1.StrTkn)

            Select Case CurTkn.TypeTkn
                Case EToken.eSM
                    Exit Do
                Case EToken.eDot
                    tkn1 = GetTkn(EToken.eDot)
                    sb1.Append(tkn1.StrTkn)
                Case Else
                    Chk(False)
            End Select
        Loop

        PrjParse.CurSrc.vUsing.Add(sb1.ToString())
    End Sub

    Sub ReadModule(src1 As TSourceFile)
        Dim mod1 As TModifier
        Dim is_module As Boolean = False

        Do While CurTkn.TypeTkn = EToken.eImports
            ReadImports()
        Loop

        If CurTkn.TypeTkn = EToken.eModule Then
            is_module = True
            GetTkn(EToken.eModule)
            GetTkn(EToken.eId)
            GetTkn(EToken.eLP)
        End If

        Do While True
            mod1 = ReadModifier()

            Do While CurTkn.TypeTkn = EToken.eLineComment OrElse CurTkn.TypeTkn = EToken.eBlockComment
                GetTkn(EToken.eUnknown)
            Loop

            Select Case CurTkn.TypeTkn
                Case EToken.eDelegate, EToken.eClass, EToken.eStruct, EToken.eInterface
                    ReadClass(mod1)

                Case EToken.eEnum
                    ReadEnum()

                Case EToken.eEOT
                    Exit Do

                Case EToken.eFunction
                    GetTkn(EToken.eFunction)
                    Dim fnc1 As TFunction = ReadFunction(Nothing, mod1)
                    Debug.Assert(fnc1.NameVar = "_Weak" OrElse fnc1.NameVar = "_Parent" OrElse fnc1.NameVar = "_Invariant")

                Case Else
                    Exit Do
            End Select
        Loop

        If is_module Then
            GetTkn(EToken.eRP)
        End If
    End Sub

    Public Overrides Sub Parse(src1 As TSourceFile)
        CurVTkn = src1.InputTokenList
        CurPos = 0
        CurTkn = CurVTkn(CurPos)
        If CurPos + 1 < CurVTkn.Count Then
            NxtTkn = CurVTkn(CurPos + 1)
        Else
            NxtTkn = EOTTkn
        End If

        ReadModule(src1)
    End Sub
End Class

