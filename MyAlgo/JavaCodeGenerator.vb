Imports System.Diagnostics

Public Class TJavaCodeGenerator
    Inherits TCodeGenerator

    Public Sub New(prj1 As TPrj)
        MyBase.New(prj1)
    End Sub


    Public Shared Function TknAddJava() As Dictionary(Of String, ETkn)
        Dim dic1 As Dictionary(Of String, ETkn)

        dic1 = New Dictionary(Of String, ETkn)()

        dic1.Add("abstract", ETkn.eAbstract)
        dic1.Add("super", ETkn.eBase)
        dic1.Add("break", ETkn.eBreak)
        dic1.Add("byval", ETkn.eByVal)
        dic1.Add("call", ETkn.eCall)
        dic1.Add("case", ETkn.eCase)
        dic1.Add("class", ETkn.eClass)
        dic1.Add("const", ETkn.eConst)
        dic1.Add("default", ETkn.eDefault)
        dic1.Add("dim", ETkn.eDim)
        dic1.Add("do", ETkn.eDo)
        dic1.Add("each", ETkn.eEach)
        dic1.Add("else", ETkn.eElse)
        dic1.Add("elseif", ETkn.eElseIf)
        dic1.Add("endif", ETkn.eEndIf)
        dic1.Add("enum", ETkn.eEnum)
        dic1.Add("exit", ETkn.eExit)
        dic1.Add("extends", ETkn.eExtends)
        dic1.Add("for", ETkn.eFor)
        dic1.Add("foreach", ETkn.eForeach)
        dic1.Add("from", ETkn.eFrom)
        dic1.Add("function", ETkn.eFunction)
        dic1.Add("get", ETkn.eGet)
        dic1.Add("goto", ETkn.eGoto)
        dic1.Add("handles", ETkn.eHandles)
        dic1.Add("if", ETkn.eIf)
        dic1.Add("iif", ETkn.eIIF)
        dic1.Add("implements", ETkn.eImplements)
        dic1.Add("import", ETkn.eImports)
        dic1.Add("in", ETkn.eIn)
        dic1.Add("inherits", ETkn.eInherits)
        dic1.Add("interface", ETkn.eInterface)
        dic1.Add("is", ETkn.eIs)
        dic1.Add("loop", ETkn.eLoop)
        dic1.Add("namespace", ETkn.eNamespace)
        dic1.Add("new", ETkn.eNew)
        dic1.Add("of", ETkn.eOf)
        dic1.Add("out", ETkn.eOut)
        dic1.Add("operator", ETkn.eOperator)
        dic1.Add("override", ETkn.eOverride)
        dic1.Add("ParamArray", ETkn.eParamArray)
        dic1.Add("partial", ETkn.ePartial)
        dic1.Add("private", ETkn.ePrivate)
        dic1.Add("public", ETkn.ePublic)
        dic1.Add("ref", ETkn.eRef)
        dic1.Add("return", ETkn.eReturn)
        dic1.Add("select", ETkn.eSelect)
        dic1.Add("static", ETkn.eShared)
        dic1.Add("struct", ETkn.eStruct)
        dic1.Add("then", ETkn.eThen)
        dic1.Add("typeof", ETkn.eTypeof)
        dic1.Add("var", ETkn.eVar)
        dic1.Add("virtual", ETkn.eVirtual)
        dic1.Add("where", ETkn.eWhere)
        dic1.Add("while", ETkn.eWhile)
        dic1.Add("@id", ETkn.eId)
        dic1.Add("@int", ETkn.eInt)
        dic1.Add("@hex", ETkn.eHex)
        dic1.Add("/*", ETkn.eBlockComment)
        dic1.Add("//", ETkn.eLineComment)
        dic1.Add("=", ETkn.eASN)
        dic1.Add("+=", ETkn.eADDEQ)
        dic1.Add("-=", ETkn.eSUBEQ)
        dic1.Add("*=", ETkn.eMULEQ)
        dic1.Add("/=", ETkn.eDIVEQ)
        dic1.Add("%=", ETkn.eMODEQ)
        dic1.Add("+", ETkn.eADD)
        dic1.Add("-", ETkn.eMns)
        dic1.Add("%", ETkn.eMOD)
        dic1.Add("&", ETkn.eAnp)
        dic1.Add("(", ETkn.eLP)
        dic1.Add(")", ETkn.eRP)
        dic1.Add("*", ETkn.eMUL)
        dic1.Add(",", ETkn.eComma)
        dic1.Add(".", ETkn.eDot)
        dic1.Add("/", ETkn.eDIV)
        dic1.Add(":", ETkn.eMMB)
        dic1.Add(";", ETkn.eSM)
        dic1.Add("?", ETkn.eQUE)
        dic1.Add("[", ETkn.eLB)
        dic1.Add("]", ETkn.eRB)
        dic1.Add("^", ETkn.eHAT)
        dic1.Add("{", ETkn.eLC)
        dic1.Add("|", ETkn.eVLine)
        dic1.Add("}", ETkn.eRC)
        dic1.Add("~", ETkn.eTilde)
        dic1.Add("==", ETkn.eEq)
        dic1.Add("!=", ETkn.eNE)
        dic1.Add("<", ETkn.eLT)
        dic1.Add(">", ETkn.eGT)
        dic1.Add("<=", ETkn.eLE)
        dic1.Add(">=", ETkn.eGE)
        dic1.Add("||", ETkn.eOR)
        dic1.Add("&&", ETkn.eAnd)
        dic1.Add("!", ETkn.eNot)

        Return dic1
    End Function

    Public Shared Function ETknToStringDic(dic1 As Dictionary(Of String, ETkn)) As Dictionary(Of ETkn, String)
        Dim name_dic As Dictionary(Of ETkn, String)

        name_dic = New Dictionary(Of ETkn, String)()
        ' for To
        For Each key1 In dic1.Keys
            name_dic.Add(dic1(key1), key1)
        Next

        Return name_dic
    End Function

    Public Function JavaName(str As String) As String
        If str = "True" Then
            Return "true"
        ElseIf str = "False" Then
            Return "false"
        ElseIf str = "Me" Then
            Return "this"
        ElseIf str = "Single" Then
            Return "float"
        ElseIf str = "Double" Then
            Return "double"
        ElseIf str = "Integer" Then
            Return "int"
        ElseIf str = "Char" Then
            Return "char"
        ElseIf str = "Nothing" Then
            Return "null"
        ElseIf str = "IList" Then
            Return "List<?>"
        ElseIf str = "List" Then
            Return "ArrayList"
        ElseIf str = "Dictionary" Then
            Return "HashMap"
        ElseIf str = "vbCr" Then
            Return "TSys.vbCr"
        ElseIf str = "vbLf" Then
            Return "TSys.vbLf"
        ElseIf str = "vbTab" Then
            Return "TSys.vbTab"
        ElseIf str = "vbNullChar" Then
            Return "TSys.vbNullChar"
        ElseIf str = "AscW" Then
            Return "TSys.AscW"
        ElseIf str = "ChrW" Then
            Return "TSys.ChrW"
        Else
            Return str
        End If
    End Function

    Public Sub MakeDelegateInterfaceJava(dlg1 As TDelegate, cla1 As TCls)
        WordAdd(ETkn.eInterface, EFigType.eResFig, cla1)
        Fmt(cla1)
        WordAdd("{", EFigType.eSymFig, cla1)
        NL()

        Tab(1)
        If dlg1.RetDlg Is Nothing Then
            WordAdd("void", EFigType.eResFig, cla1)
        Else
            TypeSrc(dlg1.RetDlg)
        End If
        WordAdd("Invoke", EFigType.eRefFig, cla1)

        VarListSrc(dlg1.ArgDlg, dlg1)

        WordAdd(";", EFigType.eSymFig, cla1)
        NL()

        WordAdd("}", EFigType.eSymFig, cla1)
        NL()
    End Sub

    Public Sub MakeDelegateClassJava(dlg1 As TDelegatePair, sw As TStringWriter)
        Dim cla1 As TCls, name1 As String, trm_type As String, i1 As Integer

        vLineFig.Clear()

        cla1 = dlg1.ClaDel
        name1 = DelegateName(cla1, dlg1.FncDel)
        trm_type = dlg1.FncDel.ClaFnc.NameCla()

        WordAdd(ETkn.eClass, EFigType.eResFig, cla1)
        WordAdd(name1, EFigType.eClassFig, cla1)
        WordAdd(" implements", EFigType.eResFig, cla1)
        Fmt(cla1)
        WordAdd("{", EFigType.eSymFig, cla1)
        NL()

        If Not dlg1.FncDel.ModFnc().isShared Then

            ' メンバー変数
            Tab(1)
            WordAdd(trm_type, EFigType.eClassFig, cla1)
            WordAdd(" Dst;", EFigType.eVarFig, cla1)
            NL()
            NL()
        End If

        ' コンストラクター
        Tab(1)
        WordAdd(ETkn.ePublic, EFigType.eResFig, cla1)
        WordAdd(name1, EFigType.eResFig, cla1)
        WordAdd("(", EFigType.eSymFig, cla1)
        If Not dlg1.FncDel.ModFnc().isShared Then

            WordAdd(trm_type, EFigType.eClassFig, cla1)
            WordAdd(" dst", EFigType.eVarFig, cla1)
        End If
        WordAdd(")", EFigType.eSymFig, cla1)
        WordAdd("{", EFigType.eSymFig, cla1)
        NL()

        If Not dlg1.FncDel.ModFnc().isShared Then
            Tab(2)
            WordAdd("Dst", EFigType.eVarFig, cla1)
            WordAdd("=", EFigType.eSymFig, cla1)
            WordAdd("dst", EFigType.eVarFig, cla1)
            WordAdd(";", EFigType.eSymFig, cla1)
            NL()
        End If

        Tab(1)
        WordAdd("}", EFigType.eSymFig, cla1)
        NL()
        NL()

        ' Invokeメソッド
        Tab(1)
        WordAdd("public", EFigType.eResFig, cla1)
        If dlg1.FncDel.RetType Is Nothing Then
            WordAdd("void", EFigType.eResFig, cla1)
        Else
            TypeSrc(dlg1.FncDel.RetType)
        End If
        WordAdd("Invoke", EFigType.eRefFig, cla1)
        VarListSrc(dlg1.FncDel.ArgFnc, dlg1.FncDel)
        WordAdd("{", EFigType.eSymFig, cla1)
        NL()

        sw.Append(MakeSrcText())

        sw.Append("" + vbTab + vbTab)
        If dlg1.FncDel.RetType IsNot Nothing Then
            sw.Append("return ")
        End If

        If dlg1.FncDel.ModFnc().isShared Then
            sw.Append(dlg1.FncDel.ClaFnc.NameCla() + ".")
        Else
            sw.Append("Dst.")
        End If

        sw.Append(dlg1.FncDel.NameFnc() + "(")
        For i1 = 0 To dlg1.FncDel.ArgFnc.Count - 1
            If i1 <> 0 Then
                sw.Append(", ")
            End If
            sw.Append(dlg1.FncDel.ArgFnc(i1).NameVar)
        Next
        sw.WriteLine(");")

        sw.WriteLine(vbTab + "}")
        sw.WriteLine("}")
    End Sub

    Public Function DelegateName(cla1 As TCls, fnc1 As TFnc) As String
        Return "_Delegate_" + cla1.NameCla() + "_" + fnc1.ClaFnc.NameCla() + "_" + fnc1.NameFnc()
    End Function

    Public Sub MakeDelegateNewJava(cla1 As TCls, trm1 As TTerm, fnc1 As TFnc)
        WordAdd("new", EFigType.eResFig, cla1)
        WordAdd(DelegateName(cla1, fnc1), EFigType.eClassFig, cla1)
        WordAdd("(", EFigType.eSymFig, cla1)
        If trm1 IsNot Nothing Then
            TrmSrc(trm1)
        End If
        WordAdd(")", EFigType.eSymFig, cla1)
    End Sub

    Public Sub ModifierSrcJava(obj1 As Object, mod1 As TModifier)
        If mod1 IsNot Nothing Then
            If mod1.isPublic Then
                WordAdd(ETkn.ePublic, EFigType.eResFig, obj1)
            End If
            If mod1.isShared Then
                WordAdd(ETkn.eShared, EFigType.eResFig, obj1)
            End If
            If mod1.isConst Then
                WordAdd("static final", EFigType.eResFig, obj1)
            End If
            'If mod1.isVirtual Then
            '    WordAdd(ETkn.eVirtual, EFigType.eResFig, obj1)
            'End If
            'If mod1.isOverride Then
            '    WordAdd(ETkn.eOverride, EFigType.eResFig, obj1)
            'End If
        End If
    End Sub

    '   エスケープ文字を作る
    Public Overrides Function Escape(str1 As String) As String
        Dim sb As TStringWriter
        Dim s As String

        s = "\"
        sb = New TStringWriter()
        For Each ch1 In str1
            Select Case ch1
                Case vbTab
                    sb.Append("\t")
                Case vbLf
                    sb.Append("\n")
                Case vbCr
                    sb.Append("\r")
                Case "\"c
                    sb.Append("\\")
                Case "'"c
                    sb.Append("\'")
                Case """"c
                    sb.Append("\""")
                Case Else
                    sb.Append(ch1)
            End Select
        Next

        Return sb.ToString()
    End Function

    '   セミコロンを追加する
    Public Sub SM(obj1 As Object)
        WordAdd(";", EFigType.eSymFig, obj1)
        NL(obj1)
    End Sub

    Public Overrides Sub AppSrc(app1 As TApp)
        Dim i1 As Integer, asn1 As TAsn, cla1 As TCls, dot1 As TDot, ref1 As TRef, trm1 As TTerm, fnc1 As TFnc, found As Boolean, tp1 As TCls

        Select Case app1.TypeApp
            Case ETkn.eADD, ETkn.eMns, ETkn.eMUL, ETkn.eDIV, ETkn.eMOD
                If app1.ArgApp.Count = 1 AndAlso (app1.TypeApp = ETkn.eADD OrElse app1.TypeApp = ETkn.eMns) Then
                    WordAdd(TPrj.Prj.vTknNamePrj(app1.TypeApp), EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(0))
                Else
                    If TypeOf app1.FncApp Is TRef AndAlso TypeOf CType(app1.FncApp, TRef).VarRef Is TFnc Then
                        ' javaで関数参照の場合

                        fnc1 = CType(CType(app1.FncApp, TRef).VarRef, TFnc)
                        If fnc1.TypeFnc = ETkn.eOperator Then
                            ' 演算子オーバーロードの場合

                            Fmt(fnc1.ClaFnc)
                            WordAdd(".", EFigType.eSymFig, app1)
                            WordAdd(fnc1.OpName(), EFigType.eVarFig, fnc1)
                            WordAdd("(", EFigType.eSymFig, app1)
                            TrmSrc(app1.ArgApp(0))
                            WordAdd(",", EFigType.eSymFig, app1)
                            TrmSrc(app1.ArgApp(1))
                            WordAdd(")", EFigType.eSymFig, app1)
                            Exit Sub
                        End If
                    End If

                    TrmSrc(app1.ArgApp(0))
                    WordAdd(TPrj.Prj.vTknNamePrj(app1.TypeApp), EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(1))
                End If

            Case ETkn.eAppCall
                Select Case app1.KndApp
                    Case EApp.eArrayApp
                        TrmSrc(app1.FncApp)

                        For i1 = 0 To app1.ArgApp.Count - 1
                            WordAdd("[", EFigType.eSymFig, app1)
                            Debug.Assert(PrjMK.GetTermType(app1.ArgApp(i1)) IsNot Nothing)

                            If PrjMK.GetTermType(app1.ArgApp(i1)).KndCla = EClass.eEnumCla Then
                                TrmSrc(app1.ArgApp(i1))
                                WordAdd(".ordinal()", EFigType.eSymFig, app1)
                            Else
                                TrmSrc(app1.ArgApp(i1))
                            End If

                            WordAdd("]", EFigType.eSymFig, app1)
                        Next
                        Exit Sub

                    Case EApp.eStringApp, EApp.eListApp, EApp.eDictionaryApp
                        Debug.Assert(app1.ArgApp.Count = 1)

                        TrmSrc(app1.FncApp)
                        WordAdd(".", EFigType.eSymFig, app1)
                        If app1.KndApp = EApp.eStringApp Then
                            WordAdd("charAt", EFigType.eRefFig, app1)
                        Else
                            WordAdd("get", EFigType.eRefFig, app1)
                        End If
                        WordAdd("(", EFigType.eSymFig, app1)
                        TrmSrc(app1.ArgApp(0))
                        WordAdd(")", EFigType.eSymFig, app1)
                        Exit Sub
                End Select

                If TypeOf app1.FncApp Is TRef AndAlso CType(app1.FncApp, TRef).NameRef = "IIf" Then
                    WordAdd("(", EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(0))
                    WordAdd("?", EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(1))
                    WordAdd(":", EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(2))
                    WordAdd(")", EFigType.eSymFig, app1)
                Else
                    TrmSrc(app1.FncApp)
                    AppArg(app1)
                End If

            Case ETkn.eBaseCall
                WordAdd(ETkn.eBase, EFigType.eResFig, app1)
                WordAdd(".", EFigType.eSymFig, app1)
                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case ETkn.eBaseNew
                WordAdd(ETkn.eBase, EFigType.eResFig, app1)
                AppArg(app1)

            Case ETkn.eNew
                Debug.Assert(app1.NewApp IsNot Nothing)

                If app1.IniApp IsNot Nothing AndAlso app1.NewApp.IsArray() Then
                    WordAdd("new ", EFigType.eRefFig, app1)
                    Debug.Assert(app1.NewApp.DimCla <> 0 AndAlso app1.NewApp.GenCla IsNot Nothing AndAlso app1.NewApp.GenCla.Count = 1)
                    If app1.ArgApp.Count <> 0 Then
                        TypeSrc(app1.NewApp.GenCla(0))
                        WordAdd("[(", EFigType.eSymFig, app1)
                        TrmSrc(app1.ArgApp(0))
                        WordAdd(")+1]", EFigType.eSymFig, app1)
                        Exit Sub
                    End If
                    TypeSrc(app1.NewApp)
                    WordAdd("{", EFigType.eSymFig, app1)
                    For i1 = 0 To app1.IniApp.TrmArr.Count - 1
                        If i1 <> 0 Then
                            WordAdd(",", EFigType.eSymFig, app1)
                        End If
                        TrmSrc(app1.IniApp.TrmArr(i1))
                    Next
                    WordAdd("}", EFigType.eSymFig, app1)
                Else
                    WordAdd(ETkn.eNew, EFigType.eResFig, app1)
                    TypeSrc(app1.NewApp)
                    If app1.IniApp IsNot Nothing Then
                        WordAdd("(", EFigType.eSymFig, app1)
                        WordAdd(")", EFigType.eSymFig, app1)
                        WordAdd("{", EFigType.eSymFig, app1)
                        WordAdd("{", EFigType.eSymFig, app1)
                        For i1 = 0 To app1.IniApp.TrmArr.Count - 1
                            WordAdd("add", EFigType.eRefFig, app1)
                            WordAdd("(", EFigType.eSymFig, app1)
                            TrmSrc(app1.IniApp.TrmArr(i1))
                            WordAdd(")", EFigType.eSymFig, app1)
                            WordAdd(";", EFigType.eSymFig, app1)
                        Next
                        WordAdd("}", EFigType.eSymFig, app1)
                        WordAdd("}", EFigType.eSymFig, app1)
                    Else
                        AppArg(app1)
                    End If
                End If
            Case ETkn.eAs, ETkn.eCast
                If app1.ClassApp.KndCla = EClass.eEnumCla Then
                    ' intからenumに変換する場合

                    TypeSrc(app1.ClassApp)
                    WordAdd(".values()[", EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(0))
                    WordAdd("]", EFigType.eSymFig, app1)
                Else
                    tp1 = PrjMK.GetTermType(app1.ArgApp(0))
                    If tp1.KndCla = EClass.eEnumCla Then
                        ' enumからintに変換する場合

                        WordAdd("(", EFigType.eSymFig, app1)
                        TrmSrc(app1.ArgApp(0))
                        WordAdd(").ordinal()", EFigType.eSymFig, app1)
                    Else
                        WordAdd("((", EFigType.eSymFig, app1)
                        TypeSrc(app1.ClassApp)
                        WordAdd(")(", EFigType.eSymFig, app1)
                        TrmSrc(app1.ArgApp(0))
                        WordAdd("))", EFigType.eSymFig, app1)
                    End If
                End If

            Case ETkn.eGetType
                TypeSrc(app1.ClassApp)
                WordAdd(".", EFigType.eSymFig, app1)
                WordAdd(ETkn.eClass, EFigType.eResFig, app1)

            Case ETkn.eQUE
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd("?", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(1))
                WordAdd(":", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(2))
                WordAdd(")", EFigType.eSymFig, app1)

            Case ETkn.eTypeof
                WordAdd("typeof", EFigType.eResFig, app1)
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(")", EFigType.eSymFig, app1)

            Case ETkn.eAddressOf
                If True Then

                    Return
                End If
                If app1.UpTrm IsNot Nothing AndAlso CType(app1.UpTrm, TTerm).IsLog() AndAlso TypeOf CType(app1.UpTrm, TTerm).UpTrm Is TAsn Then
                Else
                    Debug.WriteLine("Java Address Of 未対応")
                    Return
                End If
                asn1 = CType(CType(app1.UpTrm, TTerm).UpTrm, TAsn)
                cla1 = PrjMK.GetTermType(asn1.RelAsn.ArgApp(0))
                If TypeOf app1.ArgApp(0) Is TDot Then
                    dot1 = CType(app1.ArgApp(0), TDot)
                    Debug.Assert(TypeOf dot1.VarRef Is TFnc)
                    fnc1 = CType(dot1.VarRef, TFnc)
                    trm1 = dot1.TrmDot
                ElseIf TypeOf app1.ArgApp(0) Is TRef Then
                    ref1 = CType(app1.ArgApp(0), TRef)
                    Debug.Assert(TypeOf ref1.VarRef Is TFnc)

                    fnc1 = CType(ref1.VarRef, TFnc)
                    If fnc1.ModFnc().isShared Then
                        trm1 = Nothing
                    Else
                        trm1 = New TRef(fnc1.ThisFnc)
                    End If
                Else
                    Debug.Assert(False)
                    Return
                End If

                found = False
                For Each del In vDelegate
                    If del.ClaDel Is cla1 AndAlso del.FncDel Is fnc1 Then
                        found = True
                        Exit For
                    End If
                Next
                If Not found Then
                    vDelegate.Add(New TDelegatePair(cla1, fnc1))
                End If
                MakeDelegateNewJava(cla1, trm1, fnc1)

            Case Else
                Debug.WriteLine("Err Trm Src2:{0}", app1.TypeApp)
                Debug.Assert(False)
        End Select
    End Sub

    Public Overrides Sub CnsSrc(cns1 As TCns)
        Select Case cns1.TypeAtm
            Case ETkn.eChar
                WordAdd("'" + Escape(cns1.NameRef) + "'", EFigType.eStrFig, cns1)
            Case ETkn.eString
                WordAdd("""" + Escape(cns1.NameRef) + """", EFigType.eStrFig, cns1)
            Case ETkn.eRegEx
                WordAdd(Escape(cns1.NameRef), EFigType.eStrFig, cns1)
            Case ETkn.eInt
                WordAdd(cns1.NameRef, EFigType.eNumFig, cns1)
            Case ETkn.eHex
                Debug.Assert(TSys.Substring(cns1.NameRef, 0, 2) = "&H")
                WordAdd("0x" + cns1.NameRef.Substring(2), EFigType.eNumFig, cns1)
            Case Else
                Debug.Assert(False)
        End Select
    End Sub
    Public Overrides Sub DotSrc(dot1 As TDot)
        Dim is_enum As Boolean, dic1 As Dictionary(Of String, String) = Nothing, mem_name As String = Nothing, class_mem1 As String, class_mem2 As String

        is_enum = dot1.IsEnumDot()
        Debug.Assert(dot1.TrmDot IsNot Nothing AndAlso (is_enum OrElse dot1.TypeDot IsNot Nothing))
        If True Then
            Return
        End If
        If dot1.UpTrm IsNot Nothing AndAlso TypeOf dot1.UpTrm Is TCase AndAlso is_enum Then

            Fmt(dot1)
        Else
            Debug.Assert(dot1.VarRef IsNot Nothing AndAlso (is_enum OrElse dot1.VarRef.ModVar IsNot Nothing))

            If Not is_enum AndAlso dot1.VarRef.ModVar.isShared Then
                ' 列挙型以外のstaticメンバーの参照の場合

                Debug.Assert(TypeOf dot1.TrmDot Is TRef AndAlso TypeOf CType(dot1.TrmDot, TRef).VarRef Is TCls)

                If PrjMK.dicClassMemName IsNot Nothing Then
                    ' クラス名とメンバー名を変換する場合

                    class_mem1 = CType(dot1.TrmDot, TRef).NameRef + "." + dot1.NameRef
                    If PrjMK.dicClassMemName.ContainsKey(class_mem1) Then

                        class_mem2 = PrjMK.dicClassMemName(class_mem1)
                        WordAdd(class_mem2, ETkn.eRef, EFigType.eUnknownFig, dot1)
                        Exit Sub
                    End If
                End If
            End If

            TrmSrc(dot1.TrmDot)
            WordAdd(".", EFigType.eSymFig, dot1)

            If dot1.TypeDot IsNot Nothing AndAlso PrjMK.dicMemName IsNot Nothing Then
                If PrjMK.dicMemName.ContainsKey(dot1.TypeDot.NameCla()) Then
                    dic1 = PrjMK.dicMemName(dot1.TypeDot.NameCla())
                    If dic1.ContainsKey(dot1.NameRef) Then
                        mem_name = dic1(dot1.NameRef)

                        WordAdd(mem_name, ETkn.eRef, EFigType.eRefFig, dot1)
                        Exit Sub
                    End If
                End If
            End If
            If dot1.NameRef = "ToString" Then
                WordAdd("toString", ETkn.eRef, EFigType.eRefFig, dot1)
            Else
                Fmt(dot1)
            End If
        End If
    End Sub

    Public Overrides Sub RefSrc(ref1 As TRef)
        WordAdd(JavaName(ref1.NameRef), ETkn.eRef, EFigType.eRefFig, ref1)
        If ref1.VarRef Is Nothing AndAlso ref1.NameRef <> "true" AndAlso ref1.NameRef <> "false" AndAlso ref1.NameRef <> "null" AndAlso ref1.NameRef <> "undefined" AndAlso ref1.NameRef <> "this" Then
            ' WordAdd("参照未解決", EFigType.eUnknownFig, this);
            ' Debug.WriteLine("参照未解決:{0}", ref1.NameRef);
        End If
    End Sub

    Public Overrides Sub RelSrc(rel1 As TApp)
        Dim fnc_rel As TFnc

        Select Case rel1.TypeApp
            Case ETkn.eEq, ETkn.eNE
                fnc_rel = PrjMK.GetOperatorFunction(rel1.TypeApp, rel1.ArgApp(0))
                If fnc_rel IsNot Nothing Then
                    ' javaで演算子オーバーロードの場合

                    Fmt(fnc_rel.ClaFnc)
                    WordAdd(".", EFigType.eSymFig, rel1)
                    WordAdd(fnc_rel.OpName(), EFigType.eVarFig, fnc_rel)
                    WordAdd("(", EFigType.eSymFig, rel1)
                    TrmSrc(rel1.ArgApp(0))
                    WordAdd(",", EFigType.eSymFig, rel1)
                    TrmSrc(rel1.ArgApp(1))
                    WordAdd(")", EFigType.eSymFig, rel1)
                    Exit Sub
                End If

                TrmSrc(rel1.ArgApp(0))
                WordAdd(rel1.TypeApp, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case ETkn.eASN, ETkn.eLT, ETkn.eGT, ETkn.eADDEQ, ETkn.eSUBEQ, ETkn.eMULEQ, ETkn.eDIVEQ, ETkn.eMODEQ, ETkn.eLE, ETkn.eGE
                TrmSrc(rel1.ArgApp(0))
                WordAdd(rel1.TypeApp, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case ETkn.eIsNot
                TrmSrc(rel1.ArgApp(0))
                WordAdd(ETkn.eNE, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case ETkn.eTypeof
                TrmSrc(rel1.ArgApp(0))
                WordAdd(" instanceof ", EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case ETkn.eIs
                TrmSrc(rel1.ArgApp(0))
                WordAdd(ETkn.eEq, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case Else
                Debug.Assert(False)
        End Select
    End Sub


    Public Overrides Sub ParSrc(par1 As TPar)
        WordAdd("(", EFigType.eSymFig, par1)
        TrmSrc(par1.TrmPar)
        WordAdd(")", EFigType.eSymFig, par1)
    End Sub

    Public Overrides Sub FromSrc(from1 As TFrom)
        Debug.WriteLine("Jave 未実装 From")
    End Sub

    Public Overrides Sub AggregateSrc(aggr1 As TAggregate)
        Debug.WriteLine("Jave 未実装 From")
    End Sub

    '  ifのソースを作る
    Public Overrides Sub IfSrc(if1 As TIf, tab1 As Integer)
        Dim i1 As Integer

        Tab(tab1)
        Fmt(ETkn.eIf, ETkn.eLP)
        TrmSrc(if1.IfBlc(0).CndIf)
        WordAdd(")", EFigType.eSymFig, if1)
        BlcSrc(if1, ETkn.eIf, if1.IfBlc(0).BlcIf, tab1)
        For i1 = 1 To if1.IfBlc.Count - 1
            If if1.IfBlc(i1).CndIf IsNot Nothing Then
                Tab(tab1)
                Fmt(ETkn.eElse, ETkn.eIf, ETkn.eLP)

                TrmSrc(if1.IfBlc(i1).CndIf)
                WordAdd(")", EFigType.eSymFig, if1)
                BlcSrc(if1, ETkn.eElseIf, if1.IfBlc(i1).BlcIf, tab1)
            Else
                Tab(tab1)
                WordAdd("else", EFigType.eResFig, if1)
                BlcSrc(if1, ETkn.eElse, if1.IfBlc(i1).BlcIf, tab1)
            End If
        Next
    End Sub

    '  forのソースを作る
    Public Overrides Sub ForSrc(for1 As TFor, tab1 As Integer)
        Tab(tab1)
        If for1.LabelFor <> 0 Then

            WordAdd(TSys.Format("Label_{0}", for1.LabelFor), EFigType.eLabelFig, for1)
            WordAdd(":", EFigType.eSymFig, for1)
        End If
        If for1.IsDo Then
            WordAdd(ETkn.eWhile, EFigType.eResFig, for1)
            WordAdd("(", EFigType.eSymFig, for1)
            TrmSrc(for1.CndFor)
            WordAdd(")", EFigType.eSymFig, for1)
            BlcSrc(for1, ETkn.eDo, for1.BlcFor, tab1)
        Else
            If for1.InVarFor IsNot Nothing Then
                WordAdd(ETkn.eFor, EFigType.eResFig, for1)
                WordAdd("(", EFigType.eSymFig, for1)
                VarSrc(for1.InVarFor)
                WordAdd(":", EFigType.eSymFig, for1)
                If PrjMK.GetTermType(for1.InTrmFor) Is PrjMK.StringType Then
                    WordAdd("(", EFigType.eSymFig, for1)
                    TrmSrc(for1.InTrmFor)
                    WordAdd(").toCharArray()", EFigType.eSymFig, for1)
                Else
                    TrmSrc(for1.InTrmFor)
                End If
                WordAdd(")", EFigType.eSymFig, for1)
                BlcSrc(for1, ETkn.eEach, for1.BlcFor, tab1)
            ElseIf for1.FromFor IsNot Nothing Then
                WordAdd(ETkn.eFor, EFigType.eResFig, for1)
                WordAdd("(", EFigType.eSymFig, for1)
                Fmt(for1.IdxFor)
                WordAdd("=", EFigType.eSymFig, for1)
                TrmSrc(for1.FromFor)
                WordAdd(";", EFigType.eSymFig, for1)
                Fmt(for1.IdxFor)
                WordAdd("<=", EFigType.eSymFig, for1)
                TrmSrc(for1.ToFor)
                WordAdd(";", EFigType.eSymFig, for1)
                Fmt(for1.IdxFor)
                If for1.StepFor Is Nothing Then
                    WordAdd("++", EFigType.eSymFig, for1)
                Else
                    WordAdd("Java 未対応", EFigType.eUnknownFig, for1)
                End If
                WordAdd(")", EFigType.eSymFig, for1)
                BlcSrc(for1, ETkn.eFor, for1.BlcFor, tab1)
            Else
                Debug.WriteLine("@b")
            End If
        End If
    End Sub

    '  TStmtのソースを作る
    Public Overrides Sub SimpleStmtSrc(stmt1 As TStmt, tab1 As Integer)
        Dim trm1 As TTerm
        Dim asn1 As TAsn, app1 As TApp

        If TypeOf stmt1 Is TAsn Then
            asn1 = CType(stmt1, TAsn)
            Tab(tab1)
            If asn1.RelAsn.ArgApp(0).IsApp() Then
                app1 = CType(asn1.RelAsn.ArgApp(0), TApp)
                Debug.Assert(app1.TypeApp = ETkn.eAppCall)
                If app1.KndApp <> EApp.eArrayApp Then
                    CType(asn1.RelAsn.ArgApp(0), TApp).TrmSrcSet(Me, asn1.RelAsn.ArgApp(1))
                    Exit Sub
                End If
            End If

            TrmSrc(asn1.RelAsn.ArgApp(0))
            If asn1.RelAsn.TypeApp = ETkn.eEq Then
                WordAdd(ETkn.eASN, EFigType.eSymFig, stmt1)
            Else
                WordAdd(asn1.RelAsn.TypeApp, EFigType.eSymFig, stmt1)
            End If

            trm1 = asn1.RelAsn.ArgApp(1)
            TrmSrc(trm1)
        ElseIf TypeOf stmt1 Is TCall Then
            Tab(tab1)
            TrmSrc(CType(stmt1, TCall).AppCall)
        ElseIf TypeOf stmt1 Is TVarDecl Then
            VarDeclSrc(CType(stmt1, TVarDecl), tab1)
        Else
            WordAdd("Simple Stmt Src", EFigType.eSymFig, stmt1)
        End If

        If stmt1.TailCom <> "" Then

            WordAdd(vbTab + stmt1.TailCom, EFigType.eComFig, stmt1)
        End If
    End Sub

    '  TSelectのソースを作る
    Public Overrides Sub SelectSrc(swt1 As TSelect, tab1 As Integer)
        Tab(tab1)
        WordAdd("switch", EFigType.eResFig, swt1)
        WordAdd("(", EFigType.eSymFig, swt1)
        TrmSrc(swt1.TrmSel)
        WordAdd(")", EFigType.eSymFig, swt1)
        WordAdd("{", EFigType.eSymFig, swt1)
        NL(swt1)
        For Each cas1 In swt1.CaseSel
            For Each trm1 In cas1.TrmCase
                Tab(tab1)
                WordAdd("case", EFigType.eResFig, cas1)
                TrmSrc(trm1)
                WordAdd(":", EFigType.eSymFig, cas1)
                NL(cas1)
            Next
            If cas1.DefaultCase Then
                Tab(tab1)
                WordAdd("default", EFigType.eResFig, cas1)
                WordAdd(":", EFigType.eSymFig, cas1)
                NL(cas1)
            End If
            For Each stmt1 In cas1.BlcCase.StmtBlc
                StmtSrc(stmt1, tab1 + 1)
            Next

            If Not cas1.BlcCase.JumpEnd() Then
                ' ジャンプで終わらない場合

                Tab(tab1 + 1)
                WordAdd(ETkn.eBreak, EFigType.eResFig, swt1)
                SM(swt1)
            End If
        Next
        Tab(tab1)
        WordAdd("}", EFigType.eSymFig, swt1)
        NL(swt1)
    End Sub

    Public Overrides Sub TrySrc(try1 As TTry, tab1 As Integer)
        WordAdd("Java Try 未実装", EFigType.eResFig, try1)
    End Sub

    Public Overrides Sub WithSrc(with1 As TWith, tab1 As Integer)
        WordAdd("Java With 未実装", EFigType.eResFig, with1)
    End Sub

    '  TStmtのソースを作る
    Public Overrides Sub StmtSrc(stmt1 As TStmt, tab1 As Integer)
        Dim ret1 As TRet
        Dim red1 As TReDim
        Dim thr1 As TThrow
        Dim ext1 As TExit
        Dim i1 As Integer

        If stmt1 IsNot Nothing AndAlso stmt1.ComStmt IsNot Nothing Then
            For Each tkn_f In stmt1.ComStmt
                Tab(tab1)
                WordAdd(tkn_f.StrTkn, EFigType.eComFig, stmt1)
                NL(stmt1)
            Next
        End If
        If stmt1 Is Nothing Then
            WordAdd("null stmt", EFigType.eResFig, stmt1)
            NL(stmt1)
        ElseIf TypeOf stmt1 Is TAsn OrElse TypeOf stmt1 Is TCall OrElse TypeOf stmt1 Is TVarDecl Then
            SimpleStmtSrc(stmt1, tab1)
            SM(stmt1)
        ElseIf TypeOf stmt1 Is TIf Then
            IfSrc(CType(stmt1, TIf), tab1)
        ElseIf TypeOf stmt1 Is TSelect Then
            SelectSrc(CType(stmt1, TSelect), tab1)

        ElseIf TypeOf stmt1 Is TTry Then
            TrySrc(CType(stmt1, TTry), tab1)

        ElseIf TypeOf stmt1 Is TWith Then
            WithSrc(CType(stmt1, TWith), tab1)

        ElseIf TypeOf stmt1 Is TFor Then
            ForSrc(CType(stmt1, TFor), tab1)

        ElseIf TypeOf stmt1 Is TBlc Then
            BlcSrc(stmt1, ETkn.eUnknown, CType(stmt1, TBlc), tab1)

        ElseIf TypeOf stmt1 Is TReDim Then
            red1 = CType(stmt1, TReDim)
            Tab(tab1)
            WordAdd(ETkn.eReDim, EFigType.eResFig, stmt1)
            TrmSrc(red1.TrmReDim)
            WordAdd("(", EFigType.eSymFig, stmt1)
            For i1 = 0 To red1.DimReDim.Count - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.eSymFig, stmt1)
                End If
                TrmSrc(red1.DimReDim(i1))
            Next
            WordAdd(")", EFigType.eSymFig, stmt1)
            SM(stmt1)
        ElseIf TypeOf stmt1 Is TRet Then
            ret1 = CType(stmt1, TRet)
            Tab(tab1)
            WordAdd(ETkn.eReturn, EFigType.eResFig, stmt1)
            If ret1.TrmRet IsNot Nothing Then
                TrmSrc(ret1.TrmRet)
            End If
            SM(stmt1)

        ElseIf TypeOf stmt1 Is TThrow Then
            thr1 = CType(stmt1, TThrow)
            Tab(tab1)
            WordAdd(ETkn.eThrow, EFigType.eResFig, stmt1)
            TrmSrc(thr1.TrmThrow)
            SM(stmt1)

        ElseIf TypeOf stmt1 Is TComment Then
            ComSrc(CType(stmt1, TComment), tab1, stmt1)
        Else
            Select Case stmt1.TypeStmt
                Case ETkn.eExitDo, ETkn.eExitFor, ETkn.eExitSub
                    Tab(tab1)
                    Select Case stmt1.TypeStmt
                        Case ETkn.eExitDo, ETkn.eExitFor
                            ext1 = CType(stmt1, TExit)
                            WordAdd(ETkn.eBreak, EFigType.eResFig, stmt1)
                            If ext1.LabelExit <> 0 Then

                                WordAdd(TSys.Format(" Label_{0}", ext1.LabelExit), EFigType.eResFig, stmt1)
                            End If

                        Case ETkn.eExitSub
                            WordAdd(ETkn.eReturn, EFigType.eResFig, stmt1)
                    End Select
                    SM(stmt1)
                Case Else
                    Debug.WriteLine("Err Stmt Src:{0}", stmt1)
                    Debug.Assert(False)
            End Select
        End If
    End Sub

    '  ブロックのソースを作る
    Public Overrides Sub BlcSrc(obj1 As Object, type1 As ETkn, blc1 As TBlc, tab1 As Integer)
        WordAdd("{", EFigType.eSymFig, blc1)
        NL(obj1)
        For Each stmt1 In blc1.StmtBlc
            StmtSrc(stmt1, tab1 + 1)
        Next
        Tab(tab1)
        WordAdd("}", EFigType.eSymFig, blc1)
        NL(obj1)
    End Sub

    Public Overrides Sub VarSrc(var1 As TVar)
        TypeSrc(var1.TypeVar)
        WordAdd(" ", EFigType.eSymFig, var1)
        Fmt(var1)

        If var1.InitVar IsNot Nothing Then
            ' 初期値がある場合

            WordAdd("=", EFigType.eSymFig, var1)
            TrmSrc(var1.InitVar)
        End If
    End Sub

    '  関数のソースを作る
    Public Overrides Sub FncSrc(fnc1 As TFnc)
        ComSrc(fnc1.ComVar, 1, fnc1)
        Tab(1)

        ModifierSrcJava(fnc1, fnc1.ModFnc())
        If fnc1.TypeFnc = ETkn.eOperator Then
            TypeSrc(fnc1.RetType)
            WordAdd(" ", EFigType.eSymFig, fnc1)

            WordAdd(fnc1.OpName(), EFigType.eVarFig, fnc1)
        Else
            Select Case fnc1.TypeFnc
                Case ETkn.eFunction
                    TypeSrc(fnc1.RetType)
                Case ETkn.eSub
                    WordAdd("void", EFigType.eResFig, fnc1)
                Case ETkn.eNew
                    Fmt(fnc1.ClaFnc)
            End Select

            WordAdd(" ", EFigType.eSymFig, fnc1)
            WordAdd(fnc1.NameFnc(), EFigType.eVarFig, fnc1)
        End If

        VarListSrc(fnc1.ArgFnc, fnc1)

        If fnc1.BlcFnc Is Nothing Then
            WordAdd(";", EFigType.eSymFig, fnc1)
            NL(fnc1)
        Else
            If fnc1.NameFnc() = "BlcMathML" Then
                Debug.WriteLine("@c")
            End If
            BlcSrc(fnc1, fnc1.TypeFnc, fnc1.BlcFnc, 1)
        End If
        NL(fnc1)
    End Sub

    Public Overrides Function TypeName(name1 As String) As String
        Return JavaName(name1)
    End Function

    '  型のソースを作る
    Public Overrides Sub TypeSrc(type1 As TCls)
        Dim i1 As Integer, cla1 As TCls

        If type1 Is Nothing Then
            WordAdd("型不明", EFigType.eUnknownFig, type1)
            Return
        End If

        If type1.DimCla <> 0 Then
            ' 配列の場合

            Debug.Assert(type1.GenCla IsNot Nothing AndAlso type1.GenCla.Count = 1)
            TypeSrc(type1.GenCla(0))
            WordAdd("[", EFigType.eUnknownFig, type1)
            For i1 = 0 To type1.DimCla - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.eSymFig, type1)
                End If
            Next
            WordAdd("]", EFigType.eSymFig, type1)
        Else
            ' 配列でない場合
            WordAdd(TypeName(type1.NameType()), EFigType.eClassFig, type1)
            If type1.GenCla IsNot Nothing Then
                ' 総称型の場合

                WordAdd("<", EFigType.eUnknownFig, type1)
                For i1 = 0 To type1.GenCla.Count - 1
                    If i1 <> 0 Then
                        WordAdd(",", EFigType.eSymFig, type1)
                    End If
                    cla1 = type1.GenCla(i1)
                    If cla1.NameCla() = "Integer" Then
                        Fmt(cla1)
                    Else
                        TypeSrc(cla1)
                    End If
                Next
                WordAdd(">", EFigType.eSymFig, type1)
            End If
        End If
    End Sub

    '  変数宣言のソースを作る
    Public Overrides Sub VarDeclSrc(dcl1 As TVarDecl, tab1 As Integer)
        Dim i1 As Integer
        Dim var1 As TVar

        Tab(tab1)
        If dcl1.TypeDecl Is Nothing Then

            For i1 = 0 To dcl1.VarDecl.Count - 1
                var1 = dcl1.VarDecl(i1)
                If i1 <> 0 Then
                    WordAdd(";", EFigType.eSymFig, dcl1)
                End If
                VarSrc(var1)
            Next

            Exit Sub
        Else

            TypeSrc(dcl1.TypeDecl)
        End If
    End Sub

    Public Sub MakeJavaSrc(src1 As TSrc, out_dir As String, sw As TStringWriter)
        Dim html_path As String, fname As String, i1 As Integer, dlg1 As TDelegate, src_txt As String

        For Each cla1 In src1.ClaSrc
            ComSrc(CType(cla1.ComCla(), TComment), 0, cla1)
            If cla1.KndCla = EClass.eEnumCla Then
                '  列挙型の場合

                WordAdd(ETkn.eEnum, EFigType.eResFig, cla1)
                Fmt(cla1)
                WordAdd("{", EFigType.eSymFig, cla1)
                NL(cla1)
                '  すべてのフィールドに対し
                For Each fld1 In cla1.FldCla
                    Tab(1)
                    Fmt(fld1)
                    WordAdd(",", EFigType.eSymFig, cla1)
                    NL(fld1)
                Next
                WordAdd("}", EFigType.eSymFig, cla1)
                NL(cla1)
            ElseIf cla1.KndCla = EClass.eDelegateCla Then
                ' デリゲートの場合

                dlg1 = CType(cla1, TDelegate)
                MakeDelegateInterfaceJava(dlg1, cla1)
            Else
                '  クラスの場合

                If cla1.ModCla().isAbstract Then
                    WordAdd(ETkn.eAbstract, EFigType.eResFig, cla1)
                End If

                Select Case cla1.KndCla
                    Case EClass.eClassCla
                        WordAdd(ETkn.eClass, EFigType.eResFig, cla1)
                    Case EClass.eStructCla
                        WordAdd(ETkn.eClass, EFigType.eResFig, cla1)
                    Case EClass.eInterfaceCla
                        WordAdd(ETkn.eInterface, EFigType.eResFig, cla1)
                End Select
                Fmt(cla1)

                If cla1.GenCla IsNot Nothing Then
                    ' ジェネリック型の場合

                    WordAdd("<", EFigType.eSymFig, cla1)

                    For i1 = 0 To cla1.GenCla.Count - 1
                        If i1 <> 0 Then
                            WordAdd(",", EFigType.eSymFig, cla1)
                        End If
                        Fmt(cla1.GenCla(i1))
                    Next

                    WordAdd(">", EFigType.eSymFig, cla1)
                End If

                If cla1.SuperCla.Count <> 0 AndAlso cla1.SuperCla(0) IsNot PrjMK.ObjectType Then
                    WordAdd(" ", EFigType.eUnknownFig, cla1)
                    WordAdd(ETkn.eExtends, EFigType.eResFig, cla1)
                    Fmt(cla1.SuperCla(0))
                End If

                If cla1.InterfacesCls.Count <> 0 Then
                    WordAdd(" ", EFigType.eSymFig, cla1)
                    WordAdd(ETkn.eImplements, EFigType.eResFig, cla1)
                    For i1 = 0 To cla1.InterfacesCls.Count - 1
                        If i1 <> 0 Then
                            WordAdd(",", EFigType.eSymFig, cla1)
                        End If
                        Fmt(cla1.InterfacesCls(i1))
                    Next
                End If
                Fmt(ETkn.eLC)
                NL(cla1)

                '  すべてのフィールドに対し
                For Each fld1 In cla1.FldCla
                    ComSrc(fld1.ComVar, 1, fld1)
                    Tab(1)
                    ModifierSrcJava(fld1, fld1.ModVar)
                    TypeSrc(fld1.TypeVar)
                    WordAdd(" ", EFigType.eSymFig, fld1)
                    Fmt(fld1)
                    If fld1.InitVar IsNot Nothing Then
                        '  初期値がある場合
                        WordAdd("=", EFigType.eSymFig, fld1)
                        TrmSrc(fld1.InitVar)
                    End If

                    If fld1.TailCom <> "" Then

                        WordAdd(vbTab + fld1.TailCom, EFigType.eComFig, fld1)
                    End If

                    SM(fld1)
                Next

                If cla1.KndCla = EClass.eStructCla Then
                    ' javaの構造体の場合

                    ' デフォルトコンストラクターを作る
                    Tab(1)
                    Fmt(ETkn.ePublic, cla1)
                    WordAdd("(){}", EFigType.eUnknownFig, cla1)
                    NL()
                End If

                '  すべてのメソッドに対し
                For Each fnc1 In cla1.FncCla
                    FncSrc(fnc1)
                Next

                WordAdd(ETkn.eRC, EFigType.eResFig, cla1)
                NL(cla1)
            End If
        Next

        src_txt = MakeSrcText()

        If sw IsNot Nothing Then
            sw.Write(src_txt)
        Else
            fname = TPath.GetFileNameWithoutExtension(src1.FileSrc)
            html_path = out_dir + "html\" + fname + ".html"
            TFile.WriteAllText(html_path, MakeSrcHTML(src1))
            Return
        End If
    End Sub

    Public Overrides Function MakeSrcText() As String
        Dim sw As TStringWriter
        sw = New TStringWriter()
        For Each line1 In vLineFig
            For Each txt1 In line1.TextLine
                If txt1.TabTxt <> 0 Then
                    sw.Write(TSys.StringRepeat(vbTab, txt1.TabTxt))
                End If
                Select Case txt1.TypeFig
                    Case EFigType.eSymFig
                        If txt1.TextTxt.Length = 1 Then
                            Select Case txt1.TextTxt(0)
                                Case "("c, ")"c, "["c, "]"c, "{"c, "}"c, "."c
                                    sw.Write(txt1.TextTxt)
                                Case Else
                                    sw.Write(" " + txt1.TextTxt + " ")
                            End Select
                        Else
                            sw.Write(" " + txt1.TextTxt + " ")
                        End If
                    Case EFigType.eResFig
                        Select Case txt1.TknTxt
                            Case ETkn.eAs, ETkn.eTo, ETkn.eIs, ETkn.eIsNot
                                sw.Write(" " + txt1.TextTxt + " ")
                            Case ETkn.eThen
                                sw.Write(" " + txt1.TextTxt)
                            Case Else
                                sw.Write(txt1.TextTxt + " ")
                        End Select
                    Case EFigType.eRefFig
                        Select Case txt1.TknTxt
                            Case ETkn.eRef
                                sw.Write(txt1.TextTxt)
                            Case Else
                                sw.Write(txt1.TextTxt)
                        End Select
                    Case Else
                        sw.Write(txt1.TextTxt)
                End Select
            Next
            sw.WriteLine("")
        Next
        Return sw.ToString()
    End Function
End Class
