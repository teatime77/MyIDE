Imports System.Diagnostics

Public Class TJavaCodeGenerator
    Inherits TCodeGenerator

    Public Sub New(prj1 As TProject, parser As TSourceParser)
        MyBase.New(prj1, parser)
    End Sub


    Public Shared Function TknAddJava() As Dictionary(Of String, EToken)
        Dim dic1 As Dictionary(Of String, EToken)

        dic1 = New Dictionary(Of String, EToken)()

        dic1.Add("abstract", EToken.Abstract)
        dic1.Add("super", EToken.Base)
        dic1.Add("break", EToken.Break_)
        dic1.Add("byval", EToken.ByVal_)
        dic1.Add("call", EToken.Call_)
        dic1.Add("case", EToken.Case_)
        dic1.Add("class", EToken.Class_)
        dic1.Add("const", EToken.Const_)
        dic1.Add("default", EToken.Default_)
        dic1.Add("dim", EToken.Var)
        dic1.Add("do", EToken.Do_)
        dic1.Add("each", EToken.Each_)
        dic1.Add("else", EToken.Else_)
        dic1.Add("elseif", EToken.ElseIf_)
        dic1.Add("endif", EToken.EndIf_)
        dic1.Add("enum", EToken.Enum_)
        dic1.Add("exit", EToken.Exit_)
        dic1.Add("extends", EToken.Extends)
        dic1.Add("for", EToken.For_)
        dic1.Add("foreach", EToken.Foreach_)
        dic1.Add("from", EToken.From_)
        dic1.Add("function", EToken.Function_)
        dic1.Add("get", EToken.Get_)
        dic1.Add("goto", EToken.Goto_)
        dic1.Add("handles", EToken.Handles_)
        dic1.Add("if", EToken.If_)
        dic1.Add("implements", EToken.Implements_)
        dic1.Add("import", EToken.Imports_)
        dic1.Add("in", EToken.In_)
        dic1.Add("interface", EToken.Interface_)
        dic1.Add("is", EToken.Is_)
        dic1.Add("loop", EToken.Loop_)
        dic1.Add("namespace", EToken.Namespace_)
        dic1.Add("new", EToken.New_)
        dic1.Add("of", EToken.Of_)
        dic1.Add("out", EToken.Out_)
        dic1.Add("operator", EToken.Operator_)
        dic1.Add("override", EToken.Override)
        dic1.Add("ParamArray", EToken.ParamArray_)
        dic1.Add("partial", EToken.Partial_)
        dic1.Add("private", EToken.Private_)
        dic1.Add("public", EToken.Public_)
        dic1.Add("ref", EToken.Ref)
        dic1.Add("return", EToken.Return_)
        dic1.Add("select", EToken.Select_)
        dic1.Add("static", EToken.Shared_)
        dic1.Add("struct", EToken.Struct)
        dic1.Add("then", EToken.Then_)
        dic1.Add("typeof", EToken.Instanceof)
        dic1.Add("virtual", EToken.Virtual)
        dic1.Add("where", EToken.Where_)
        dic1.Add("while", EToken.While_)
        dic1.Add("@id", EToken.Id)
        dic1.Add("@int", EToken.Int)
        dic1.Add("@hex", EToken.Hex)
        dic1.Add("/*", EToken.BlockComment)
        dic1.Add("//", EToken.LineComment)
        dic1.Add("=", EToken.ASN)
        dic1.Add("+=", EToken.ADDEQ)
        dic1.Add("-=", EToken.SUBEQ)
        dic1.Add("*=", EToken.MULEQ)
        dic1.Add("/=", EToken.DIVEQ)
        dic1.Add("%=", EToken.MODEQ)
        dic1.Add("+", EToken.ADD)
        dic1.Add("-", EToken.Mns)
        dic1.Add("%", EToken.MOD_)
        dic1.Add("&", EToken.Anp)
        dic1.Add("(", EToken.LP)
        dic1.Add(")", EToken.RP)
        dic1.Add("*", EToken.MUL)
        dic1.Add(",", EToken.Comma)
        dic1.Add(".", EToken.Dot)
        dic1.Add("/", EToken.DIV)
        dic1.Add(":", EToken.MMB)
        dic1.Add(";", EToken.SM)
        dic1.Add("?", EToken.Question)
        dic1.Add("[", EToken.LB)
        dic1.Add("]", EToken.RB)
        dic1.Add("^", EToken.HAT)
        dic1.Add("{", EToken.LC)
        dic1.Add("|", EToken.BitOR)
        dic1.Add("}", EToken.RC)
        dic1.Add("~", EToken.Tilde)
        dic1.Add("==", EToken.Eq)
        dic1.Add("!=", EToken.NE)
        dic1.Add("<", EToken.LT)
        dic1.Add(">", EToken.GT)
        dic1.Add("<=", EToken.LE)
        dic1.Add(">=", EToken.GE)
        dic1.Add("||", EToken.OR_)
        dic1.Add("&&", EToken.And_)
        dic1.Add("!", EToken.Not_)

        Return dic1
    End Function

    Public Shared Function ETknToStringDic(dic1 As Dictionary(Of String, EToken)) As Dictionary(Of EToken, String)
        Dim name_dic As Dictionary(Of EToken, String)

        name_dic = New Dictionary(Of EToken, String)()
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

    Public Sub MakeDelegateInterfaceJava(dlg1 As TDelegate, cla1 As TClass)
        WordAdd(EToken.Interface_, EFigType.eResFig, cla1)
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
        Dim cla1 As TClass, name1 As String, trm_type As String, i1 As Integer

        vLineFig.Clear()

        cla1 = dlg1.ClaDel
        name1 = DelegateName(cla1, dlg1.FncDel)
        trm_type = dlg1.FncDel.ClaFnc.NameCla()

        WordAdd(EToken.Class_, EFigType.eResFig, cla1)
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
        WordAdd(EToken.Public_, EFigType.eResFig, cla1)
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

    Public Function DelegateName(cla1 As TClass, fnc1 As TFunction) As String
        Return "_Delegate_" + cla1.NameCla() + "_" + fnc1.ClaFnc.NameCla() + "_" + fnc1.NameFnc()
    End Function

    Public Sub MakeDelegateNewJava(cla1 As TClass, trm1 As TTerm, fnc1 As TFunction)
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
                WordAdd(EToken.Public_, EFigType.eResFig, obj1)
            End If
            If mod1.isShared Then
                WordAdd(EToken.Shared_, EFigType.eResFig, obj1)
            End If
            If mod1.isConst Then
                WordAdd("static final", EFigType.eResFig, obj1)
            End If
            'If mod1.isVirtual Then
            '    WordAdd(EToken.Virtual, EFigType.eResFig, obj1)
            'End If
            'If mod1.isOverride Then
            '    WordAdd(EToken.Override, EFigType.eResFig, obj1)
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

    Public Overrides Sub AppSrc(app1 As TApply)
        Dim i1 As Integer, asn1 As TAssignment, cla1 As TClass, dot1 As TDot, ref1 As TReference, trm1 As TTerm, fnc1 As TFunction, found As Boolean, tp1 As TClass

        Select Case app1.TypeApp
            Case EToken.ADD, EToken.Mns, EToken.MUL, EToken.DIV, EToken.MOD_, EToken.BitOR
                If app1.ArgApp.Count = 1 AndAlso (app1.TypeApp = EToken.ADD OrElse app1.TypeApp = EToken.Mns) Then
                    WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(0))
                Else
                    If TypeOf app1.FncApp Is TReference AndAlso TypeOf CType(app1.FncApp, TReference).VarRef Is TFunction Then
                        ' javaで関数参照の場合

                        fnc1 = CType(CType(app1.FncApp, TReference).VarRef, TFunction)
                        If fnc1.TypeFnc = EToken.Operator_ Then
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
                    WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(1))
                End If

            Case EToken.INC, EToken.DEC
                TrmSrc(app1.ArgApp(0))
                If ParserCG.vTknName.ContainsKey(app1.TypeApp) Then
                    WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.eSymFig, app1)
                Else
                    Select Case app1.TypeApp
                        Case EToken.INC
                            WordAdd(ParserCG.vTknName(EToken.ADDEQ), EFigType.eSymFig, app1)
                        Case EToken.DEC
                            WordAdd(ParserCG.vTknName(EToken.SUBEQ), EFigType.eSymFig, app1)
                    End Select
                    WordAdd("1", EFigType.eNumFig, app1)
                End If

            Case EToken.AppCall
                Select Case app1.KndApp
                    Case EApply.eArrayApp
                        TrmSrc(app1.FncApp)

                        For i1 = 0 To app1.ArgApp.Count - 1
                            WordAdd("[", EFigType.eSymFig, app1)
                            Debug.Assert(app1.ArgApp(i1).TypeTrm IsNot Nothing)

                            If app1.ArgApp(i1).TypeTrm.KndCla = EClass.eEnumCla Then
                                TrmSrc(app1.ArgApp(i1))
                                WordAdd(".ordinal()", EFigType.eSymFig, app1)
                            Else
                                TrmSrc(app1.ArgApp(i1))
                            End If

                            WordAdd("]", EFigType.eSymFig, app1)
                        Next
                        Exit Sub

                    Case EApply.eStringApp, EApply.eListApp, EApply.eDictionaryApp
                        Debug.Assert(app1.ArgApp.Count = 1)

                        TrmSrc(app1.FncApp)
                        WordAdd(".", EFigType.eSymFig, app1)
                        If app1.KndApp = EApply.eStringApp Then
                            WordAdd("charAt", EFigType.eRefFig, app1)
                        Else
                            WordAdd("get", EFigType.eRefFig, app1)
                        End If
                        WordAdd("(", EFigType.eSymFig, app1)
                        TrmSrc(app1.ArgApp(0))
                        WordAdd(")", EFigType.eSymFig, app1)
                        Exit Sub
                End Select

                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case EToken.BaseCall
                WordAdd(EToken.Base, EFigType.eResFig, app1)
                WordAdd(".", EFigType.eSymFig, app1)
                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case EToken.BaseNew
                WordAdd(EToken.Base, EFigType.eResFig, app1)
                AppArg(app1)

            Case EToken.New_
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
                    WordAdd(EToken.New_, EFigType.eResFig, app1)
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
            Case EToken.As_, EToken.Cast
                If app1.ClassApp.KndCla = EClass.eEnumCla Then
                    ' intからenumに変換する場合

                    TypeSrc(app1.ClassApp)
                    WordAdd(".values()[", EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(0))
                    WordAdd("]", EFigType.eSymFig, app1)
                Else
                    tp1 = app1.ArgApp(0).TypeTrm
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

            Case EToken.GetType_
                TypeSrc(app1.ClassApp)
                WordAdd(".", EFigType.eSymFig, app1)
                WordAdd(EToken.Class_, EFigType.eResFig, app1)

            Case EToken.Question
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd("?", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(1))
                WordAdd(":", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(2))
                WordAdd(")", EFigType.eSymFig, app1)

            Case EToken.Instanceof
                WordAdd("typeof", EFigType.eResFig, app1)
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(")", EFigType.eSymFig, app1)

            Case EToken.AddressOf_
                If True Then

                    Return
                End If
                If app1.UpTrm IsNot Nothing AndAlso CType(app1.UpTrm, TTerm).IsLog() AndAlso TypeOf CType(app1.UpTrm, TTerm).UpTrm Is TAssignment Then
                Else
                    Debug.WriteLine("Java Address Of 未対応")
                    Return
                End If
                asn1 = CType(CType(app1.UpTrm, TTerm).UpTrm, TAssignment)
                cla1 = asn1.RelAsn.ArgApp(0).TypeTrm
                If TypeOf app1.ArgApp(0) Is TDot Then
                    dot1 = CType(app1.ArgApp(0), TDot)
                    Debug.Assert(TypeOf dot1.VarRef Is TFunction)
                    fnc1 = CType(dot1.VarRef, TFunction)
                    trm1 = dot1.TrmDot
                ElseIf TypeOf app1.ArgApp(0) Is TReference Then
                    ref1 = CType(app1.ArgApp(0), TReference)
                    Debug.Assert(TypeOf ref1.VarRef Is TFunction)

                    fnc1 = CType(ref1.VarRef, TFunction)
                    If fnc1.ModFnc().isShared Then
                        trm1 = Nothing
                    Else
                        trm1 = New TReference(fnc1.ThisFnc)
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

    Public Overrides Sub CnsSrc(cns1 As TConstant)
        Select Case cns1.TypeAtm
            Case EToken.Char_
                WordAdd("'" + Escape(cns1.NameRef) + "'", EFigType.eStrFig, cns1)
            Case EToken.String_
                WordAdd("""" + Escape(cns1.NameRef) + """", EFigType.eStrFig, cns1)
            Case EToken.RegEx
                WordAdd(Escape(cns1.NameRef), EFigType.eStrFig, cns1)
            Case EToken.Int
                WordAdd(cns1.NameRef, EFigType.eNumFig, cns1)
            Case EToken.Hex
                Debug.Assert(TSys.Substring(cns1.NameRef, 0, 2) = "&H")
                WordAdd("0x" + cns1.NameRef.Substring(2), EFigType.eNumFig, cns1)
            Case Else
                Debug.Assert(False)
        End Select
    End Sub
    Public Overrides Sub DotSrc(dot1 As TDot)
        Dim is_enum As Boolean, dic1 As Dictionary(Of String, String) = Nothing, mem_name As String = Nothing, class_mem1 As String, class_mem2 As String

        is_enum = dot1.IsEnumDot()
        If True Then
            Return
        End If
        Debug.Assert(dot1.TrmDot IsNot Nothing AndAlso (is_enum OrElse dot1.TypeDot IsNot Nothing))
        If dot1.UpTrm IsNot Nothing AndAlso TypeOf dot1.UpTrm Is TCase AndAlso is_enum Then

            Fmt(dot1)
        Else
            Debug.Assert(dot1.VarRef IsNot Nothing AndAlso (is_enum OrElse dot1.VarRef.ModVar IsNot Nothing))

            If Not is_enum AndAlso dot1.VarRef.ModVar.isShared Then
                ' 列挙型以外のstaticメンバーの参照の場合

                Debug.Assert(TypeOf dot1.TrmDot Is TReference AndAlso TypeOf CType(dot1.TrmDot, TReference).VarRef Is TClass)

                If PrjMK.dicClassMemName IsNot Nothing Then
                    ' クラス名とメンバー名を変換する場合

                    class_mem1 = CType(dot1.TrmDot, TReference).NameRef + "." + dot1.NameRef
                    If PrjMK.dicClassMemName.ContainsKey(class_mem1) Then

                        class_mem2 = PrjMK.dicClassMemName(class_mem1)
                        WordAdd(class_mem2, EToken.Ref, EFigType.eUnknownFig, dot1)
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

                        WordAdd(mem_name, EToken.Ref, EFigType.eRefFig, dot1)
                        Exit Sub
                    End If
                End If
            End If
            If dot1.NameRef = "ToString" Then
                WordAdd("toString", EToken.Ref, EFigType.eRefFig, dot1)
            Else
                Fmt(dot1)
            End If
        End If
    End Sub

    Public Overrides Sub RefSrc(ref1 As TReference)
        WordAdd(JavaName(ref1.NameRef), EToken.Ref, EFigType.eRefFig, ref1)
        If ref1.VarRef Is Nothing AndAlso ref1.NameRef <> "true" AndAlso ref1.NameRef <> "false" AndAlso ref1.NameRef <> "null" AndAlso ref1.NameRef <> "undefined" AndAlso ref1.NameRef <> "this" Then
            ' WordAdd("参照未解決", EFigType.eUnknownFig, this);
            ' Debug.WriteLine("参照未解決:{0}", ref1.NameRef);
        End If
    End Sub

    Public Overrides Sub RelSrc(rel1 As TApply)
        Dim fnc_rel As TFunction

        Select Case rel1.TypeApp
            Case EToken.Eq, EToken.NE
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
            Case EToken.ASN, EToken.LT, EToken.GT, EToken.ADDEQ, EToken.SUBEQ, EToken.MULEQ, EToken.DIVEQ, EToken.MODEQ, EToken.LE, EToken.GE, EToken.Instanceof
                TrmSrc(rel1.ArgApp(0))
                WordAdd(rel1.TypeApp, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case EToken.IsNot_
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.NE, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case EToken.Instanceof
                TrmSrc(rel1.ArgApp(0))
                WordAdd(" instanceof ", EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case EToken.Is_
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.Eq, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case Else
                Debug.Assert(False)
        End Select
    End Sub


    Public Overrides Sub ParSrc(par1 As TParenthesis)
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
        Fmt(EToken.If_, EToken.LP)
        TrmSrc(if1.IfBlc(0).CndIf)
        WordAdd(")", EFigType.eSymFig, if1)
        BlcSrc(if1, EToken.If_, if1.IfBlc(0).BlcIf, tab1)
        For i1 = 1 To if1.IfBlc.Count - 1
            If if1.IfBlc(i1).CndIf IsNot Nothing Then
                Tab(tab1)
                Fmt(EToken.Else_, EToken.If_, EToken.LP)

                TrmSrc(if1.IfBlc(i1).CndIf)
                WordAdd(")", EFigType.eSymFig, if1)
                BlcSrc(if1, EToken.ElseIf_, if1.IfBlc(i1).BlcIf, tab1)
            Else
                Tab(tab1)
                WordAdd("else", EFigType.eResFig, if1)
                BlcSrc(if1, EToken.Else_, if1.IfBlc(i1).BlcIf, tab1)
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
            WordAdd(EToken.While_, EFigType.eResFig, for1)
            WordAdd("(", EFigType.eSymFig, for1)
            TrmSrc(for1.CndFor)
            WordAdd(")", EFigType.eSymFig, for1)
            BlcSrc(for1, EToken.Do_, for1.BlcFor, tab1)
        Else
            If for1.InVarFor IsNot Nothing Then
                WordAdd(EToken.For_, EFigType.eResFig, for1)
                WordAdd("(", EFigType.eSymFig, for1)
                VarSrc(for1.InVarFor)
                WordAdd(":", EFigType.eSymFig, for1)
                If for1.InTrmFor.TypeTrm Is PrjMK.StringType Then
                    WordAdd("(", EFigType.eSymFig, for1)
                    TrmSrc(for1.InTrmFor)
                    WordAdd(").toCharArray()", EFigType.eSymFig, for1)
                Else
                    TrmSrc(for1.InTrmFor)
                End If
                WordAdd(")", EFigType.eSymFig, for1)
                BlcSrc(for1, EToken.Each_, for1.BlcFor, tab1)
            ElseIf for1.FromFor IsNot Nothing Then
                WordAdd(EToken.For_, EFigType.eResFig, for1)
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
                BlcSrc(for1, EToken.For_, for1.BlcFor, tab1)
            Else
                Debug.WriteLine("@b")
            End If
        End If
    End Sub

    '  TStatementのソースを作る
    Public Overrides Sub SimpleStmtSrc(stmt1 As TStatement, tab1 As Integer)
        Dim trm1 As TTerm
        Dim asn1 As TAssignment, app1 As TApply

        If TypeOf stmt1 Is TAssignment Then
            asn1 = CType(stmt1, TAssignment)
            Tab(tab1)
            If asn1.RelAsn.ArgApp(0).IsApp() Then
                app1 = CType(asn1.RelAsn.ArgApp(0), TApply)
                Debug.Assert(app1.TypeApp = EToken.AppCall)
                If app1.KndApp <> EApply.eArrayApp Then
                    CType(asn1.RelAsn.ArgApp(0), TApply).TrmSrcSet(Me, asn1.RelAsn.ArgApp(1))
                    Exit Sub
                End If
            End If

            TrmSrc(asn1.RelAsn.ArgApp(0))
            If asn1.RelAsn.TypeApp = EToken.Eq Then
                WordAdd(EToken.ASN, EFigType.eSymFig, stmt1)
            Else
                WordAdd(asn1.RelAsn.TypeApp, EFigType.eSymFig, stmt1)
            End If

            trm1 = asn1.RelAsn.ArgApp(1)
            TrmSrc(trm1)
        ElseIf TypeOf stmt1 Is TCall Then
            Tab(tab1)
            TrmSrc(CType(stmt1, TCall).AppCall)
        ElseIf TypeOf stmt1 Is TVariableDeclaration Then
            VarDeclSrc(CType(stmt1, TVariableDeclaration), tab1)
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
                WordAdd(EToken.Break_, EFigType.eResFig, swt1)
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

    '  TStatementのソースを作る
    Public Overrides Sub StmtSrc(stmt1 As TStatement, tab1 As Integer)
        Dim ret1 As TReturn
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
        ElseIf TypeOf stmt1 Is TAssignment OrElse TypeOf stmt1 Is TCall OrElse TypeOf stmt1 Is TVariableDeclaration Then
            SimpleStmtSrc(stmt1, tab1)
            SM(stmt1)
        ElseIf TypeOf stmt1 Is TIf Then
            IfSrc(CType(stmt1, TIf), tab1)
        ElseIf TypeOf stmt1 Is TSelect Then
            SelectSrc(CType(stmt1, TSelect), tab1)

        ElseIf TypeOf stmt1 Is TTry Then
            TrySrc(CType(stmt1, TTry), tab1)

        ElseIf TypeOf stmt1 Is TFor Then
            ForSrc(CType(stmt1, TFor), tab1)

        ElseIf TypeOf stmt1 Is TBlock Then
            BlcSrc(stmt1, EToken.Unknown, CType(stmt1, TBlock), tab1)

        ElseIf TypeOf stmt1 Is TReDim Then
            red1 = CType(stmt1, TReDim)
            Tab(tab1)
            WordAdd(EToken.ReDim_, EFigType.eResFig, stmt1)
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
        ElseIf TypeOf stmt1 Is TReturn Then
            ret1 = CType(stmt1, TReturn)
            Tab(tab1)
            WordAdd(EToken.Return_, EFigType.eResFig, stmt1)
            If ret1.TrmRet IsNot Nothing Then
                TrmSrc(ret1.TrmRet)
            End If
            SM(stmt1)

        ElseIf TypeOf stmt1 Is TThrow Then
            thr1 = CType(stmt1, TThrow)
            Tab(tab1)
            WordAdd(EToken.Throw_, EFigType.eResFig, stmt1)
            TrmSrc(thr1.TrmThrow)
            SM(stmt1)

        ElseIf TypeOf stmt1 Is TComment Then
            ComSrc(CType(stmt1, TComment), tab1, stmt1)
        Else
            Select Case stmt1.TypeStmt
                Case EToken.ExitDo, EToken.ExitFor, EToken.ExitSub
                    Tab(tab1)
                    Select Case stmt1.TypeStmt
                        Case EToken.ExitDo, EToken.ExitFor
                            ext1 = CType(stmt1, TExit)
                            WordAdd(EToken.Break_, EFigType.eResFig, stmt1)
                            If ext1.LabelExit <> 0 Then

                                WordAdd(TSys.Format(" Label_{0}", ext1.LabelExit), EFigType.eResFig, stmt1)
                            End If

                        Case EToken.ExitSub
                            WordAdd(EToken.Return_, EFigType.eResFig, stmt1)
                    End Select
                    SM(stmt1)
                Case Else
                    Debug.WriteLine("Err Stmt Src:{0}", stmt1)
                    Debug.Assert(False)
            End Select
        End If
    End Sub

    '  ブロックのソースを作る
    Public Overrides Sub BlcSrc(obj1 As Object, type1 As EToken, blc1 As TBlock, tab1 As Integer)
        WordAdd("{", EFigType.eSymFig, blc1)
        NL(obj1)
        For Each stmt1 In blc1.StmtBlc
            StmtSrc(stmt1, tab1 + 1)
        Next
        Tab(tab1)
        WordAdd("}", EFigType.eSymFig, blc1)
        NL(obj1)
    End Sub

    Public Overrides Sub VarSrc(var1 As TVariable)
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
    Public Overrides Sub FncSrc(fnc1 As TFunction)
        ComSrc(fnc1.ComVar, 1, fnc1)
        Tab(1)

        ModifierSrcJava(fnc1, fnc1.ModFnc())
        If fnc1.TypeFnc = EToken.Operator_ Then
            TypeSrc(fnc1.RetType)
            WordAdd(" ", EFigType.eSymFig, fnc1)

            WordAdd(fnc1.OpName(), EFigType.eVarFig, fnc1)
        Else
            Select Case fnc1.TypeFnc
                Case EToken.Function_
                    TypeSrc(fnc1.RetType)
                Case EToken.Sub_
                    WordAdd("void", EFigType.eResFig, fnc1)
                Case EToken.New_
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
    Public Overrides Sub TypeSrc(type1 As TClass)
        Dim i1 As Integer, cla1 As TClass

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
    Public Overrides Sub VarDeclSrc(dcl1 As TVariableDeclaration, tab1 As Integer)
        Dim i1 As Integer
        Dim var1 As TVariable

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

    Public Sub MakeJavaSrc(src1 As TSourceFile, out_dir As String, sw As TStringWriter)
        Dim html_path As String, fname As String, i1 As Integer, dlg1 As TDelegate, src_txt As String

        For Each cla1 In src1.ClaSrc
            ComSrc(CType(cla1.ComCla(), TComment), 0, cla1)
            If cla1.KndCla = EClass.eEnumCla Then
                '  列挙型の場合

                WordAdd(EToken.Enum_, EFigType.eResFig, cla1)
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
                    WordAdd(EToken.Abstract, EFigType.eResFig, cla1)
                End If

                Select Case cla1.KndCla
                    Case EClass.eClassCla
                        WordAdd(EToken.Class_, EFigType.eResFig, cla1)
                    Case EClass.eStructCla
                        WordAdd(EToken.Class_, EFigType.eResFig, cla1)
                    Case EClass.eInterfaceCla
                        WordAdd(EToken.Interface_, EFigType.eResFig, cla1)
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

                If cla1.SuperClassList.Count <> 0 AndAlso cla1.SuperClassList(0) IsNot PrjMK.ObjectType Then
                    WordAdd(" ", EFigType.eUnknownFig, cla1)
                    WordAdd(EToken.Extends, EFigType.eResFig, cla1)
                    Fmt(cla1.SuperClassList(0))
                End If

                If cla1.InterfaceList.Count <> 0 Then
                    WordAdd(" ", EFigType.eSymFig, cla1)
                    WordAdd(EToken.Implements_, EFigType.eResFig, cla1)
                    For i1 = 0 To cla1.InterfaceList.Count - 1
                        If i1 <> 0 Then
                            WordAdd(",", EFigType.eSymFig, cla1)
                        End If
                        Fmt(cla1.InterfaceList(i1))
                    Next
                End If
                Fmt(EToken.LC)
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
                    Fmt(EToken.Public_, cla1)
                    WordAdd("(){}", EFigType.eUnknownFig, cla1)
                    NL()
                End If

                '  すべてのメソッドに対し
                For Each fnc1 In cla1.FncCla
                    FncSrc(fnc1)
                Next

                WordAdd(EToken.RC, EFigType.eResFig, cla1)
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
                            Case EToken.As_, EToken.To_, EToken.Is_, EToken.IsNot_
                                sw.Write(" " + txt1.TextTxt + " ")
                            Case EToken.Then_
                                sw.Write(" " + txt1.TextTxt)
                            Case Else
                                sw.Write(txt1.TextTxt + " ")
                        End Select
                    Case EFigType.eRefFig
                        Select Case txt1.TknTxt
                            Case EToken.Ref
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
