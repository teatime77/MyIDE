Imports System.Diagnostics

Public Class TTokenWriter
    Public ObjTlm As Object
    Public ParserTW As TSourceParser
    Public TokenListTW As New List(Of TToken)

    Public Sub New(obj As Object, parser As TSourceParser)
        ObjTlm = obj
        ParserTW = parser
    End Sub

    Sub AddToken(obj As Object)
        TokenListTW.Add(New TToken(obj))
    End Sub

    Public Sub AddToken(type1 As EToken, obj As Object)
        TokenListTW.Add(New TToken(type1, obj))
    End Sub

    Public Sub TAB(n As Integer)
        Dim tab1 As New TToken

        tab1.TypeTkn = EToken.eTab
        tab1.TabTkn = n
        TokenListTW.Add(tab1)
    End Sub

    Public Sub Fmt(ParamArray args As Object())
        For Each o1 In args

            If TypeOf o1 Is String Then
                TokenListTW.Add(New TToken(CType(o1, String), o1))

            ElseIf TypeOf o1 Is Integer Then
                TokenListTW.Add(New TToken(EToken.eInt, o1.ToString()))

            ElseIf TypeOf o1 Is TDot Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TReference Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TClass Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TVariable Then
                AddToken(o1)

            ElseIf TypeOf o1 Is TToken Then
                TokenListTW.Add(CType(o1, TToken))

            ElseIf TypeOf o1 Is EToken Then
                Dim type1 As EToken = CType(o1, EToken)

                If type1 = EToken.eNL OrElse type1 = EToken.eEOL Then
                    Dim new_line As New TToken

                    If type1 = EToken.eEOL Then

                        Select Case ParserTW.LanguageSP
                            Case ELanguage.Basic
                            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                AddToken(EToken.eSM, ObjTlm)
                        End Select
                    End If

                    new_line.TypeTkn = EToken.eNL
                    TokenListTW.Add(new_line)
                Else
                    If ParserTW.vTknName.ContainsKey(type1) Then

                        Dim s As String = ParserTW.vTknName(type1)
                        If s <> "" Then
                            AddToken(type1, ObjTlm)
                        End If
                    End If
                End If

            ElseIf TypeOf o1 Is List(Of TToken) Then
                TokenListTW.AddRange(CType(o1, List(Of TToken)))

            Else
                Debug.Assert(False)
            End If
        Next

    End Sub

    Public Function GetTokenList() As List(Of TToken)
        Return TokenListTW
    End Function
End Class

Public Class TNaviMakeSourceCode
    Inherits TDeclarative
    Public PrjMK As TProject
    Public ParserMK As TSourceParser

    Public Sub New(prj1 As TProject, parser As TSourceParser)
        PrjMK = prj1
        ParserMK = parser
    End Sub


    Public Overrides Sub StartCondition(self As Object)
        If TypeOf self Is TFunction Then

        End If
    End Sub

    Public Sub VariableTypeInitializer(self As Object, tw As TTokenWriter)
        If TypeOf self Is TVariable Then
            With CType(self, TVariable)
                Dim as_new As Boolean = False, app1 As TApply

                If .InitVar IsNot Nothing AndAlso .InitVar.TokenList Is Nothing Then
                    ' 生成された初期化関数が、まだ呼ばれていない場合

                    NaviTerm(.InitVar)
                End If

                If .TypeVar IsNot Nothing AndAlso Not .NoType AndAlso ParserMK.LanguageSP <> ELanguage.JavaScript Then
                    tw.Fmt(EToken.eAs)
                    If .InitVar IsNot Nothing AndAlso .InitVar.IsApp() AndAlso CType(.InitVar, TApply).TypeApp = EToken.eNew Then
                        as_new = True

                        app1 = CType(.InitVar, TApply)
                        If app1.ArgApp.Count = 0 Then
                            ' 引数がない場合

                            tw.Fmt(EToken.eNew)
                            tw.Fmt(app1.NewApp.TokenListVar)
                        Else
                            ' 引数がある場合
                            tw.Fmt(app1.TokenList)
                        End If

                        If app1.IniApp IsNot Nothing Then

                            tw.Fmt(EToken.eFrom, app1.IniApp.TokenList)
                        End If
                    Else
                        PrjMK.SetClassNameList(.TypeVar, ParserMK)
                        tw.Fmt(.TypeVar.TokenListVar)
                    End If
                End If

                If Not as_new AndAlso .InitVar IsNot Nothing Then
                    tw.Fmt(EToken.eASN, .InitVar.TokenList)
                End If

            End With
        End If
    End Sub

    ' コメントのソースを作る
    Public Sub ComSrc(com1 As TComment, tab1 As Integer, tw As TTokenWriter)
        If com1 IsNot Nothing Then
            For Each s In com1.LineCom
                If s <> "" Then
                    tw.TAB(tab1)
                    tw.Fmt(New TToken(EToken.eComment, s))
                End If
                tw.Fmt(EToken.eNL)
            Next
        End If
    End Sub

    Public Sub JavaScriptClass(self As Object, tw As TTokenWriter)
        If TypeOf self Is TClass Then
            With CType(self, TClass)
                Select Case .KndCla
                    Case EClass.eEnumCla
                        '  列挙型の場合

                        tw.Fmt(EToken.eVar, .NameVar, EToken.eASN, EToken.eFunction, EToken.eLP, EToken.eRP, EToken.eLC, EToken.eRC, EToken.eNL)

                        Dim idx As Integer = 0
                        For Each fld1 In .FldCla
                            tw.Fmt(.NameVar, EToken.eDot, fld1.NameVar, EToken.eASN, New TToken(EToken.eInt, idx.ToString()), EToken.eEOL)
                            idx += 1
                        Next

                    Case EClass.eStructCla, EClass.eClassCla
                        ' 構造体かクラスの場合

                        If .SuperClassList.Count <> 0 AndAlso .SuperClassList(0).NameVar = "Attribute" Then
                        Else
                            '  すべてのメソッドに対し
                            For Each fnc1 In .FncCla
                                tw.Fmt(fnc1.TokenListVar)
                            Next
                        End If

                End Select
            End With
        End If
    End Sub

    Public Overrides Sub EndCondition(self As Object)
        Dim tw As New TTokenWriter(self, ParserMK)

        If self Is Nothing Then
            tw.Fmt("null stmt", EToken.eNL)
            Exit Sub

        End If

        If TypeOf self Is TVariable Then
            With CType(self, TVariable)
                If TypeOf self Is TClass Then
                    With CType(self, TClass)
                        Dim gen_tw As New TTokenWriter(self, ParserMK)

                        ComSrc(CType(.ComCla(), TComment), 0, tw)
                        If ParserMK.LanguageSP = ELanguage.JavaScript Then

                            JavaScriptClass(self, tw)
                        Else
                            Select Case .KndCla
                                Case EClass.eEnumCla
                                    '  列挙型の場合

                                    Select Case ParserMK.LanguageSP
                                        Case ELanguage.Basic
                                            tw.Fmt(EToken.ePublic, EToken.eEnum, self, EToken.eNL)

                                            For Each fld1 In .FldCla
                                                tw.Fmt(fld1, EToken.eNL)
                                            Next

                                            tw.Fmt(EToken.eEnd, EToken.eEnum, EToken.eNL)

                                        Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                            tw.Fmt(EToken.ePublic, EToken.eEnum, self, EToken.eLC, EToken.eNL)

                                            For Each fld1 In .FldCla
                                                tw.Fmt(fld1, EToken.eComma, EToken.eNL)
                                            Next

                                            tw.Fmt(EToken.eRC, EToken.eNL)
                                    End Select

                                Case EClass.eDelegateCla
                                    ' デリゲートの場合

                                    If TypeOf self Is TDelegate Then
                                        With CType(self, TDelegate)
                                            tw.Fmt(EToken.ePublic, EToken.eDelegate)

                                            Select Case ParserMK.LanguageSP
                                                Case ELanguage.Basic
                                                    If .RetDlg Is Nothing Then
                                                        tw.Fmt(EToken.eSub)
                                                    Else
                                                        tw.Fmt(EToken.eFunction)
                                                    End If
                                                Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                            End Select

                                            tw.Fmt(.NameVar)

                                            tw.Fmt(EToken.eLP)
                                            tw.Fmt(Laminate((From var1 In .ArgDlg Select var1.TokenListVar), New TToken(EToken.eComma, self)))
                                            tw.Fmt(EToken.eRP)

                                            If .RetDlg IsNot Nothing Then
                                                PrjMK.SetClassNameList(.RetDlg, ParserMK)
                                                tw.Fmt(EToken.eAs, .RetDlg.TokenListVar)
                                            End If

                                            tw.Fmt(EToken.eEOL)
                                        End With
                                    End If
                                Case Else
                                    '  クラスの場合

                                    If .ModCla().isPartial Then
                                        tw.Fmt(EToken.ePartial)
                                    End If

                                    tw.Fmt(EToken.ePublic)

                                    If .ModCla().isAbstract Then
                                        tw.Fmt(EToken.eAbstract)
                                    End If

                                    Select Case .KndCla
                                        Case EClass.eClassCla
                                            tw.Fmt(EToken.eClass)
                                        Case EClass.eStructCla
                                            tw.Fmt(EToken.eStruct)
                                        Case EClass.eInterfaceCla
                                            tw.Fmt(EToken.eInterface)
                                        Case Else
                                            Debug.Assert(False)
                                    End Select


                                    tw.Fmt(.TokenListVar)

                                    If ParserMK.LanguageSP = ELanguage.Basic Then
                                        tw.Fmt(EToken.eNL)
                                    End If

                                    If .SuperClassList.Count <> 0 AndAlso .SuperClassList(0) IsNot PrjMK.ObjectType Then
                                        tw.Fmt(EToken.eExtends, .SuperClassList(0).TokenListVar)

                                        If ParserMK.LanguageSP = ELanguage.Basic Then
                                            tw.Fmt(EToken.eNL)
                                        End If
                                    End If

                                    If .InterfaceList.Count <> 0 AndAlso .InterfaceList(0) IsNot PrjMK.ObjectType Then
                                        tw.Fmt(EToken.eImplements)
                                        tw.Fmt(Laminate((From cls1 In .InterfaceList Select cls1.TokenListVar), New TToken(EToken.eComma, self)))

                                        If ParserMK.LanguageSP = ELanguage.Basic Then
                                            tw.Fmt(EToken.eNL)
                                        End If
                                    End If

                                    If ParserMK.LanguageSP <> ELanguage.Basic Then
                                        tw.Fmt(EToken.eLC, EToken.eNL)
                                    End If

                                    '  すべてのフィールドに対し
                                    For Each fld1 In .FldCla
                                        tw.Fmt(fld1.TokenListVar)
                                    Next

                                    '  すべてのメソッドに対し
                                    For Each fnc1 In .FncCla
                                        If fnc1.IsTreeWalker OrElse fnc1.IsInitializer() Then
                                            gen_tw.Fmt(fnc1.TokenListVar)
                                        Else
                                            tw.Fmt(fnc1.TokenListVar)
                                        End If
                                    Next

                                    If ParserMK.LanguageSP = ELanguage.Basic Then
                                        Select Case .KndCla
                                            Case EClass.eClassCla
                                                tw.Fmt(EToken.eEndClass, EToken.eNL)
                                            Case EClass.eStructCla
                                                tw.Fmt(EToken.eEndStruct, EToken.eNL)
                                            Case EClass.eInterfaceCla
                                                tw.Fmt(EToken.eEndInterface, EToken.eNL)
                                        End Select
                                    Else
                                        tw.Fmt(EToken.eRC, EToken.eNL)
                                    End If
                            End Select
                        End If

                        .TokenListCls = tw.GetTokenList()

                        If gen_tw.TokenListTW.Count <> 0 Then
                            .GenTokenListCls = gen_tw.GetTokenList()
                        End If
                    End With

                ElseIf TypeOf self Is TFunction Then
                    With CType(self, TFunction)

                        If Not .IsInitializer() OrElse ParserMK.LanguageSP = ELanguage.JavaScript Then
                            If .ComVar IsNot Nothing Then

                                tw.Fmt(.ComVar.TokenListStmt)
                            End If

                            If ParserMK.LanguageSP <> ELanguage.JavaScript Then

                                tw.Fmt(.ModVar.TokenListMod)
                            End If

                            Select Case ParserMK.LanguageSP
                                Case ELanguage.Basic
                                    Select Case .TypeFnc
                                        Case EToken.eFunction
                                            tw.Fmt(EToken.eFunction, self)
                                        Case EToken.eSub
                                            tw.Fmt(EToken.eSub, self)
                                        Case EToken.eNew
                                            tw.Fmt(EToken.eSub, EToken.eNew)

                                        Case EToken.eOperator
                                            tw.Fmt(EToken.eOperator, self)

                                        Case Else
                                            Debug.WriteLine("")
                                    End Select

                                Case ELanguage.TypeScript
                                    Select Case .TypeFnc
                                        Case EToken.eFunction, EToken.eSub
                                            tw.Fmt(self)

                                        Case EToken.eNew
                                            tw.Fmt(EToken.eConstructor)

                                        Case EToken.eOperator
                                            tw.Fmt(EToken.eOperator, self)

                                        Case Else
                                            Debug.WriteLine("関数のタイプが不明:" + .NameVar)
                                    End Select

                                Case ELanguage.JavaScript
                                    Select Case .TypeFnc
                                        Case EToken.eFunction, EToken.eSub
                                            tw.Fmt(.ClaFnc.NameVar, EToken.eDot, "prototype", EToken.eDot, .NameVar, EToken.eASN, EToken.eFunction)

                                        Case EToken.eNew
                                            tw.Fmt(EToken.eVar, .ClaFnc.NameVar, EToken.eASN, EToken.eFunction)
                                        Case Else
                                            Debug.Assert(False)
                                    End Select

                                Case ELanguage.CSharp, ELanguage.Java
                            End Select

                            tw.Fmt(EToken.eLP)
                            tw.Fmt(Laminate((From var1 In .ArgFnc Select var1.TokenListVar), New TToken(EToken.eComma, self)))
                            tw.Fmt(EToken.eRP)

                            If ParserMK.LanguageSP <> ELanguage.JavaScript Then

                                If .RetType IsNot Nothing Then
                                    PrjMK.SetClassNameList(.RetType, ParserMK)
                                    tw.Fmt(EToken.eAs, .RetType.TokenListVar)

                                End If
                            End If

                            If .InterfaceFnc IsNot Nothing Then
                                PrjMK.SetClassNameList(.InterfaceFnc, ParserMK)
                                tw.Fmt(EToken.eImplements, .InterfaceFnc.TokenListVar, EToken.eDot, .ImplFnc)
                            End If

                            If ParserMK.LanguageSP = ELanguage.Basic Then

                                tw.Fmt(EToken.eNL)
                                If .BlcFnc IsNot Nothing Then

                                    If .WithFnc IsNot Nothing Then
                                        If .WithFnc Is .ArgFnc(0).TypeVar Then
                                            tw.Fmt(EToken.eWith, .ArgFnc(0).NameVar, EToken.eNL)
                                        Else
                                            tw.Fmt(EToken.eWith, EToken.eCType, EToken.eLP, .ArgFnc(0).NameVar, EToken.eComma, .WithFnc.TokenListVar, EToken.eRP, EToken.eNL)
                                        End If
                                        tw.Fmt(.BlcFnc.TokenListStmt)
                                        tw.Fmt(EToken.eEndWith, EToken.eNL)
                                    Else
                                        tw.Fmt(.BlcFnc.TokenListStmt)
                                    End If

                                    Select Case .TypeFnc
                                        Case EToken.eOperator
                                            tw.Fmt(EToken.eEndOperator)
                                        Case EToken.eSub, EToken.eNew
                                            tw.Fmt(EToken.eEndSub)
                                        Case EToken.eFunction
                                            tw.Fmt(EToken.eEndFunction)
                                    End Select

                                    tw.Fmt(EToken.eNL)
                                End If
                            Else

                                If .BlcFnc Is Nothing Then
                                    tw.Fmt(EToken.eEOL)
                                Else
                                    tw.Fmt(EToken.eLC, EToken.eNL)

                                    If ParserMK.LanguageSP = ELanguage.JavaScript AndAlso .TypeFnc = EToken.eNew Then
                                        ' JavaScriptのコンストラクタの場合

                                        ' InstanceInitializerを再帰的に呼ぶ。CallInstanceInitializer(this, this);
                                        tw.Fmt("CallInstanceInitializer", EToken.eLP, ParserMK.ThisName, EToken.eComma, ParserMK.ThisName, EToken.eRP, EToken.eEOL)
                                    End If

                                    tw.Fmt(.BlcFnc.TokenListStmt)

                                    tw.Fmt(EToken.eRC, EToken.eNL)
                                End If
                            End If

                        End If

                        If ParserMK.LanguageSP = ELanguage.JavaScript AndAlso .TypeFnc = EToken.eNew Then
                            ' JavaScriptのコンストラクタの場合

                            If .ClaFnc.SuperClassList.Count <> 0 AndAlso .ClaFnc.SuperClassList(0) IsNot PrjMK.ObjectType Then
                                ' 親クラスがObjectでない場合

                                ' 親クラスを継承する。
                                tw.Fmt("Inherits", EToken.eLP, .ClaFnc.NameVar, EToken.eComma, .ClaFnc.SuperClassList(0).NameVar, EToken.eRP, EToken.eEOL)
                            End If
                        End If

                        .TokenListVar = tw.GetTokenList()
                    End With

                ElseIf TypeOf self Is TField Then
                    With CType(self, TField)

                        If .ComVar IsNot Nothing Then
                            tw.Fmt(.ComVar.TokenListStmt)
                        End If

                        Select Case ParserMK.LanguageSP
                            Case ELanguage.Basic
                                If .ModVar IsNot Nothing Then
                                    tw.Fmt(.ModVar.TokenListMod)
                                End If
                                If .ModVar Is Nothing OrElse Not .ModVar.isPublic AndAlso Not .ModVar.isShared Then
                                    tw.Fmt(EToken.eVar)
                                End If

                            Case ELanguage.TypeScript

                            Case ELanguage.JavaScript
                                tw.Fmt(EToken.eVar)

                            Case ELanguage.CSharp, ELanguage.Java
                        End Select

                        tw.Fmt(self)
                        VariableTypeInitializer(self, tw)

                        If .TailCom <> "" Then
                            tw.Fmt(New TToken(EToken.eComment, .TailCom))
                        End If

                        tw.Fmt(EToken.eEOL)

                        .TokenListVar = tw.GetTokenList()
                    End With

                Else

                    If False AndAlso ParserMK.LanguageSP = ELanguage.JavaScript Then
                        tw.Fmt(self)
                    Else
                        If .ByRefVar Then
                            tw.Fmt(EToken.eRef)
                        End If
                        If .ParamArrayVar Then
                            tw.Fmt(EToken.eParamArray)
                        End If

                        tw.Fmt(self)
                        VariableTypeInitializer(self, tw)
                    End If

                    .TokenListVar = tw.GetTokenList()
                End If

            End With
        End If

        If TypeOf self Is TTerm Then
            With CType(self, TTerm)

                If .CastType IsNot Nothing Then

                    tw.Fmt(EToken.eCType, EToken.eLP)
                End If

                If TypeOf self Is TConstant Then
                    With CType(self, TConstant)
                        Select Case .TypeAtm
                            Case EToken.eChar
                                tw.Fmt("""" + Escape(.NameRef) + """c")

                            Case EToken.eString
                                tw.Fmt("""" + Escape(.NameRef) + """")

                            Case EToken.eRegEx
                                tw.Fmt(Escape(.NameRef))

                            Case EToken.eInt
                                tw.Fmt(.NameRef)

                            Case EToken.eHex
                                tw.Fmt(.NameRef)
                                Debug.Assert(TSys.Substring(.NameRef, 0, 2) = "&H")

                            Case Else
                                Debug.Assert(False)
                        End Select

                    End With

                ElseIf TypeOf self Is TArray Then
                    With CType(self, TArray)
                        tw.Fmt(EToken.eLC, Laminate((From trm In .TrmArr Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eRC)
                    End With

                ElseIf TypeOf self Is TDot Then
                    With CType(self, TDot)
                        If .IsAddressOf Then
                            tw.Fmt(EToken.eAddressOf)
                        End If

                        If .TrmDot Is Nothing Then
                            If ParserMK.LanguageSP = ELanguage.JavaScript Then

                                tw.Fmt(.FunctionTrm.ArgFnc(0).NameVar)
                            End If
                        Else
                            tw.Fmt(.TrmDot.TokenList)
                        End If

                        tw.Fmt(EToken.eDot, ParserMK.TranslageReferenceName(CType(self, TDot)))
                    End With

                ElseIf TypeOf self Is TReference Then
                    With CType(self, TReference)
                        If .IsAddressOf Then
                            tw.Fmt(EToken.eAddressOf)
                        End If

                        If ParserMK.LanguageSP = ELanguage.JavaScript Then
                            ' JavaScriptの場合

                            If .NameRef = "Nothing" Then

                                tw.Fmt("undefined")

                            ElseIf TypeOf .VarRef Is TField AndAlso CType(.VarRef, TField).ClaFld IsNot ParserMK.PrjParse.SystemType OrElse TypeOf .VarRef Is TFunction AndAlso CType(.VarRef, TFunction).ClaFnc IsNot ParserMK.PrjParse.SystemType Then
                                ' Systemクラス以外のフィールドの参照の場合

                                tw.Fmt(ParserMK.ThisName, EToken.eDot, self)
                            Else

                                tw.Fmt(self)
                            End If

                        Else
                            tw.Fmt(self)
                        End If

                    End With

                ElseIf TypeOf self Is TApply Then
                    With CType(self, TApply)

                        If .Negation Then

                            tw.Fmt(EToken.eNot)
                        End If

                        Select Case .TypeApp
                            Case EToken.eOR, EToken.eAnd, EToken.eAnp, EToken.eBitOR
                                tw.Fmt(Laminate((From trm In .ArgApp Select trm.TokenList), New TToken(.TypeApp, self)))
                            Case EToken.eNot
                                tw.Fmt(EToken.eNot, .ArgApp(0).TokenList)

                            '--------------------------------------------------------------------------------------
                            Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD, EToken.eBitOR
                                If .ArgApp.Count = 1 AndAlso (.TypeApp = EToken.eADD OrElse .TypeApp = EToken.eMns) Then
                                    tw.Fmt(.TypeApp, .ArgApp(0).TokenList)
                                Else

                                    tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                                End If

                            Case EToken.eINC, EToken.eDEC
                                tw.Fmt(.ArgApp(0).TokenList, .TypeApp)

                            Case EToken.eAppCall
                                tw.Fmt(.FncApp.TokenList, AppArgTokenList(self))

                            Case EToken.eBaseCall

                                tw.Fmt(EToken.eBase, EToken.eDot, .FncApp.TokenList, AppArgTokenList(self))

                            Case EToken.eBaseNew

                                tw.Fmt(EToken.eBase, EToken.eDot, EToken.eNew, AppArgTokenList(self))

                            Case EToken.eNew
                                Debug.Assert(.NewApp IsNot Nothing)

                                If ParserMK.LanguageSP = ELanguage.JavaScript AndAlso .NewApp.OrgCla IsNot Nothing AndAlso .NewApp.OrgCla.NameVar = "TList" Then
                                    ' IEnumerableから作るリストは未対応
                                    Debug.Assert(.ArgApp.Count = 0)
                                    tw.Fmt(EToken.eLB, EToken.eRB)
                                Else

                                    tw.Fmt(EToken.eNew)

                                    If .IniApp Is Nothing Then
                                        ' 初期値がない場合

                                        tw.Fmt(.NewApp.TokenListVar, AppArgTokenList(self))
                                    Else
                                        ' 初期値がある場合

                                        If .NewApp.IsArray() Then
                                            ' 配列の場合

                                            tw.Fmt(.NewApp.GenCla(0).TokenListVar, AppArgTokenList(self))
                                        Else
                                            ' 配列でない場合

                                            tw.Fmt(.NewApp.TokenListVar, AppArgTokenList(self), EToken.eFrom)
                                        End If

                                        tw.Fmt(.IniApp.TokenList)
                                    End If
                                End If


                            Case EToken.eAs, EToken.eCast

                                tw.Fmt(EToken.eCType, EToken.eLP, .ArgApp(0).TokenList, EToken.eComma, .ClassApp.TokenListVar, EToken.eRP)

                            Case EToken.eGetType
                                tw.Fmt(EToken.eGetType, EToken.eLP, .ClassApp.TokenListVar, EToken.eRP)

                            Case EToken.Question
                                tw.Fmt(EToken.eIf, EToken.eLP, .ArgApp(0).TokenList, EToken.eComma, .ArgApp(1).TokenList, EToken.eComma, .ArgApp(2).TokenList, EToken.eRP)

                            Case EToken.eInstanceof
                                If ParserMK.LanguageSP = ELanguage.Basic Then
                                    tw.Fmt(EToken.eInstanceof, .ArgApp(0).TokenList, EToken.eIs, CType(.ArgApp(1), TReference).VarRef.TokenListVar)
                                Else
                                    tw.Fmt(.ArgApp(0).TokenList, EToken.eInstanceof, CType(.ArgApp(1), TReference).VarRef.TokenListVar)
                                End If

                            '--------------------------------------------------------------------------------------
                            Case EToken.eEq, EToken.eNE
                                Dim tp1 As TClass, tp2 As TClass

                                tw.Fmt(.ArgApp(0).TokenList)
                                tp1 = .ArgApp(0).TypeTrm
                                tp2 = .ArgApp(1).TypeTrm
                                If tp1 Is Nothing OrElse tp2 Is Nothing Then
                                    ' Debug.WriteLine("");
                                    ' tp1 = .ArgApp[0].TypeTrm;
                                    ' tp2 = .ArgApp[1].TypeTrm;
                                End If
                                If ParserMK.LanguageSP <> ELanguage.Basic OrElse tp1 IsNot Nothing AndAlso (tp1.IsAtomType() OrElse tp1.KndCla = EClass.eStructCla) OrElse tp2 IsNot Nothing AndAlso (tp2.IsAtomType() OrElse tp2.KndCla = EClass.eStructCla) Then
                                    tw.Fmt(.TypeApp)
                                Else
                                    If .TypeApp = EToken.eNE Then
                                        tw.Fmt(EToken.eIsNot)
                                    Else
                                        tw.Fmt(EToken.eIs)
                                    End If
                                End If
                                tw.Fmt(.ArgApp(1).TokenList)
                            Case EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE
                                tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                            Case EToken.eIsNot
                                tw.Fmt(.ArgApp(0).TokenList, EToken.eIsNot, .ArgApp(1).TokenList)

                            Case EToken.eInstanceof
                                If ParserMK.LanguageSP = ELanguage.Basic Then
                                    tw.Fmt(.ArgApp(0).TokenList, .TypeApp, .ArgApp(1).TokenList)
                                Else
                                    tw.Fmt(EToken.eInstanceof, .ArgApp(0).TokenList, EToken.eIs, .ArgApp(1).TokenList)
                                End If

                            Case EToken.eIs
                                tw.Fmt(.ArgApp(0).TokenList, EToken.eIs, .ArgApp(1).TokenList)

                            Case Else
                                Debug.Assert(False)
                        End Select
                    End With

                ElseIf TypeOf self Is TParenthesis Then
                    With CType(self, TParenthesis)
                        If .TrmPar.IsApp() AndAlso CType(.TrmPar, TApply).TypeApp = EToken.eCast Then

                            tw.Fmt(.TrmPar.TokenList)
                        Else

                            tw.Fmt(EToken.eLP, .TrmPar.TokenList, EToken.eRP)
                        End If

                    End With
                ElseIf TypeOf self Is TFrom Then
                    With CType(self, TFrom)
                        If ParserMK.LanguageSP = ELanguage.JavaScript Then
                            '            var vx = $Query(self.Children, function (x) { return true; }, function (x) { return x.BoundingRectangle.Position.X });
                            tw.Fmt("$Query(", .SeqQry.TokenList)

                            If .CndQry IsNot Nothing Then

                                tw.Fmt(", function(", .VarQry.NameVar, "){ return ", .CndQry.TokenList, "; }")
                            Else
                                tw.Fmt(", undefined")
                            End If

                            If .SelFrom IsNot Nothing Then

                                tw.Fmt(", function(", .VarQry.NameVar, "){ return ", .SelFrom.TokenList, "; }")
                            Else
                                tw.Fmt(", undefined")
                            End If

                            tw.Fmt(")")
                        Else
                            tw.Fmt(EToken.eFrom, .VarQry.NameVar, EToken.eIn, .SeqQry.TokenList)

                            If .CndQry IsNot Nothing Then

                                tw.Fmt(EToken.eWhere, .CndQry.TokenList)
                            End If

                            If .SelFrom IsNot Nothing Then

                                tw.Fmt(EToken.eSelect, .SelFrom.TokenList)
                            End If

                            If .TakeFrom IsNot Nothing Then

                                tw.Fmt(EToken.eTake, .TakeFrom.TokenList)
                            End If

                            If .InnerFrom IsNot Nothing Then

                                tw.Fmt(.InnerFrom.TokenList)
                            End If
                        End If
                    End With

                ElseIf TypeOf self Is TAggregate Then
                    With CType(self, TAggregate)
                        If ParserMK.LanguageSP = ELanguage.JavaScript Then
                            '            var vx = $Query(self.Children, function (x) { return true; }, function (x) { return x.BoundingRectangle.Position.X });
                            tw.Fmt("$Query(", .SeqQry.TokenList)

                            If .CndQry IsNot Nothing Then

                                tw.Fmt(", function(", .VarQry.NameVar, "){ return ", .CndQry.TokenList, "; }")
                            Else
                                tw.Fmt(", undefined")
                            End If

                            tw.Fmt(")")

                            Select Case .FunctionAggr
                                Case EAggregateFunction.eSum
                                    tw.Fmt(".Sum()")
                                Case EAggregateFunction.eMax
                                    tw.Fmt(".Max()")
                                Case EAggregateFunction.eMin
                                    tw.Fmt(".Min()")
                                Case Else
                                    Debug.Assert(False)
                            End Select

                        Else
                            tw.Fmt(EToken.eAggregate, .VarQry.NameVar, EToken.eIn, .SeqQry.TokenList)

                            If .CndQry IsNot Nothing Then

                                tw.Fmt(EToken.eWhere, .CndQry.TokenList)
                            End If

                            tw.Fmt(EToken.eInto)

                            Select Case .FunctionAggr
                                Case EAggregateFunction.eSum
                                    tw.Fmt("Sum")
                                Case EAggregateFunction.eMax
                                    tw.Fmt("Max")
                                Case EAggregateFunction.eMin
                                    tw.Fmt("Min")
                                Case Else
                                    Debug.Assert(False)
                            End Select
                            tw.Fmt(EToken.eLP, .IntoAggr.TokenList, EToken.eRP)
                        End If

                    End With
                Else
                    Debug.Assert(False)
                End If

                If .CastType IsNot Nothing Then

                    tw.Fmt(EToken.eComma, .CastType.TokenListVar, EToken.eRP)
                End If

                .TokenList = tw.GetTokenList()
            End With

        ElseIf TypeOf self Is TStatement Then
            With CType(self, TStatement)

                If .BeforeSrc IsNot Nothing Then
                    Dim v = .BeforeSrc.Replace(vbCr, "").Split(New Char() {vbLf(0)})
                    For Each s In v
                        tw.Fmt(s, EToken.eNL)
                    Next
                End If

                If .ComStmt IsNot Nothing Then
                    For Each tkn_f In .ComStmt
                        tw.TAB(.TabStmt)
                        tw.Fmt(tkn_f.StrTkn, EToken.eNL)
                    Next
                End If
                If .IsGenerated Then

                ElseIf TypeOf self Is TAssignment OrElse TypeOf self Is TCall OrElse TypeOf self Is TVariableDeclaration Then
                    tw.TAB(.TabStmt)
                    If TypeOf self Is TAssignment Then
                        With CType(self, TAssignment)
                            Dim asn_op As EToken

                            If ParserMK.LanguageSP <> ELanguage.Basic AndAlso .RelAsn.TypeApp = EToken.eEq Then
                                asn_op = EToken.eASN
                            Else
                                asn_op = .RelAsn.TypeApp
                            End If

                            tw.Fmt(.RelAsn.ArgApp(0).TokenList, asn_op, .RelAsn.ArgApp(1).TokenList)
                        End With

                    ElseIf TypeOf self Is TCall Then
                        With CType(self, TCall)
                            tw.Fmt(.AppCall.TokenList)
                        End With

                    ElseIf TypeOf self Is TVariableDeclaration Then
                        With CType(self, TVariableDeclaration)

                            tw.Fmt(.ModDecl.TokenListMod)
                            If .ModDecl Is Nothing OrElse Not .ModDecl.isPublic AndAlso Not .ModDecl.isShared Then
                                tw.Fmt(EToken.eVar)
                            End If

                            tw.Fmt(Laminate((From var1 In .VarDecl Select var1.TokenListVar), New TToken(EToken.eComma, self)))

                        End With
                    End If

                    If .TailCom <> "" Then

                        tw.Fmt(New TToken(EToken.eComment, .TailCom))
                    End If
                    tw.Fmt(EToken.eEOL)

                ElseIf TypeOf self Is TIfBlock Then
                    With CType(self, TIfBlock)
                        Dim if1 As TIf, i1 As Integer

                        if1 = CType(.ParentStmt, TIf)
                        i1 = if1.IfBlc.IndexOf(CType(self, TIfBlock))
                        Debug.Assert(i1 <> -1)

                        Select Case ParserMK.LanguageSP
                            Case ELanguage.Basic
                                If i1 = 0 Then
                                    tw.Fmt(EToken.eIf, .CndIf.TokenList, EToken.eThen, EToken.eNL)
                                Else
                                    If .CndIf IsNot Nothing Then
                                        tw.Fmt(EToken.eElseIf, .CndIf.TokenList, EToken.eThen, EToken.eNL)
                                    Else
                                        tw.Fmt(EToken.eElse, EToken.eNL)
                                    End If
                                End If

                            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                If i1 = 0 Then
                                    tw.Fmt(EToken.eIf, EToken.eLP, .CndIf.TokenList, EToken.eRP, EToken.eLC, EToken.eNL)
                                Else
                                    If .CndIf IsNot Nothing Then
                                        tw.Fmt(EToken.eElse, EToken.eIf, EToken.eLP, .CndIf.TokenList, EToken.eRP, EToken.eLC, EToken.eNL)
                                    Else
                                        tw.Fmt(EToken.eElse, EToken.eLC, EToken.eNL)
                                    End If
                                End If
                        End Select

                        If ParserMK.LanguageSP = ELanguage.Basic AndAlso .WithIf IsNot Nothing Then
                            tw.Fmt(EToken.eWith, .WithIf.TokenList, EToken.eNL)
                            tw.Fmt(.BlcIf.TokenListStmt)
                            tw.Fmt(EToken.eEndWith, EToken.eNL)
                        Else
                            tw.Fmt(.BlcIf.TokenListStmt)
                        End If


                        If ParserMK.LanguageSP <> ELanguage.Basic Then
                            tw.Fmt(EToken.eRC, EToken.eNL)
                        End If

                    End With

                ElseIf TypeOf self Is TIf Then
                    With CType(self, TIf)
                        For Each if_blc In .IfBlc
                            tw.Fmt(if_blc.TokenListStmt)
                        Next

                        If ParserMK.LanguageSP = ELanguage.Basic Then
                            tw.Fmt(EToken.eEndIf, EToken.eNL)
                        End If
                    End With

                ElseIf TypeOf self Is TCase Then
                    With CType(self, TCase)
                        Select Case ParserMK.LanguageSP
                            Case ELanguage.Basic
                                If Not .DefaultCase Then
                                    tw.Fmt(EToken.eCase, Laminate((From trm In .TrmCase Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eNL)
                                Else
                                    tw.Fmt(EToken.eCase, EToken.eElse, EToken.eNL)
                                End If

                                tw.Fmt(.BlcCase.TokenListStmt)

                            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                If Not .DefaultCase Then
                                    tw.Fmt(EToken.eCase, Laminate((From trm In .TrmCase Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eMMB, EToken.eNL)
                                Else
                                    tw.Fmt(EToken.eDefault, EToken.eNL)
                                End If

                                tw.Fmt(.BlcCase.TokenListStmt)

                                tw.Fmt(EToken.eBreak, EToken.eEOL)
                        End Select

                    End With

                ElseIf TypeOf self Is TSelect Then
                    With CType(self, TSelect)
                        Select Case ParserMK.LanguageSP
                            Case ELanguage.Basic
                                tw.Fmt(EToken.eSelect, EToken.eCase, .TrmSel.TokenList, EToken.eNL)

                            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                tw.Fmt(EToken.eSwitch, EToken.eLP, .TrmSel.TokenList, EToken.eRP, EToken.eLC, EToken.eNL)
                        End Select


                        For Each cas1 In .CaseSel
                            tw.Fmt(cas1.TokenListStmt)
                        Next

                        Select Case ParserMK.LanguageSP
                            Case ELanguage.Basic
                                tw.Fmt(EToken.eEndSelect, EToken.eNL)
                            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                tw.Fmt(EToken.eRC, EToken.eNL)
                        End Select
                    End With

                ElseIf TypeOf self Is TTry Then
                    With CType(self, TTry)

                        Select Case ParserMK.LanguageSP
                            Case ELanguage.Basic
                                tw.Fmt(EToken.eTry, EToken.eNL)
                                tw.Fmt(.BlcTry.TokenListStmt)
                                tw.Fmt(EToken.eCatch, Laminate((From var1 In .VarCatch Select var1.TokenListVar), New TToken(EToken.eComma, self)), EToken.eNL)
                                tw.Fmt(.BlcCatch.TokenListStmt)
                                tw.Fmt(EToken.eEndTry, EToken.eNL)

                            Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                tw.Fmt(EToken.eTry, EToken.eLC, EToken.eNL)
                                tw.Fmt(.BlcTry.TokenListStmt)
                                tw.Fmt(EToken.eRC, EToken.eNL)
                                tw.Fmt(EToken.eCatch, EToken.eLP, Laminate((From var1 In .VarCatch Select var1.TokenListVar), New TToken(EToken.eComma, self)), EToken.eRP, EToken.eLC, EToken.eNL)
                                tw.Fmt(.BlcCatch.TokenListStmt)
                                tw.Fmt(EToken.eRC, EToken.eNL)
                        End Select

                    End With

                ElseIf TypeOf self Is TFor Then
                    With CType(self, TFor)
                        If .IsDo Then

                            If ParserMK.LanguageSP = ELanguage.Basic Then
                                tw.Fmt(EToken.eDo, EToken.eWhile, .CndFor.TokenList, EToken.eNL)
                                tw.Fmt(.BlcFor.TokenListStmt)
                                tw.Fmt(EToken.eLoop, EToken.eNL)
                            Else
                                tw.Fmt(EToken.eWhile, EToken.eLP, .CndFor.TokenList, EToken.eRP, EToken.eLC, EToken.eNL)
                                tw.Fmt(.BlcFor.TokenListStmt)
                                tw.Fmt(EToken.eRC, EToken.eNL)
                            End If


                        ElseIf .InVarFor IsNot Nothing Then
                            Select Case ParserMK.LanguageSP
                                Case ELanguage.Basic
                                    tw.Fmt(EToken.eFor, EToken.eEach, .InVarFor.NameVar, EToken.eIn, .InTrmFor.TokenList, EToken.eNL)
                                    tw.Fmt(.BlcFor.TokenListStmt)
                                    tw.Fmt(EToken.eNext, EToken.eNL)

                                Case ELanguage.TypeScript, ELanguage.CSharp, ELanguage.Java
                                    tw.Fmt(EToken.eFor, EToken.eLP, EToken.eVar, .InVarFor.NameVar, EToken.eIn, .InTrmFor.TokenList, EToken.eRP, EToken.eLC, EToken.eNL)
                                    tw.Fmt(.BlcFor.TokenListStmt)
                                    tw.Fmt(EToken.eRC, EToken.eNL)

                                Case ELanguage.JavaScript
                                    ' for(var $i = 0; $i < v.length; $i++)
                                    ' var x = v[$i];
                                    tw.Fmt(EToken.eFor, EToken.eLP, EToken.eVar, "$i", EToken.eASN, 0, EToken.eSM, "$i", EToken.eLT, .InTrmFor.TokenList, EToken.eDot, "length", EToken.eSM, "$i++", EToken.eRP, EToken.eLC, EToken.eNL)
                                    tw.Fmt(EToken.eVar, .InVarFor.NameVar, EToken.eASN, .InTrmFor.TokenList, EToken.eLB, "$i", EToken.eRB, EToken.eEOL)
                                    tw.Fmt(.BlcFor.TokenListStmt)
                                    tw.Fmt(EToken.eRC, EToken.eNL)
                            End Select

                        ElseIf .IdxVarFor IsNot Nothing Then
                            Select Case ParserMK.LanguageSP
                                Case ELanguage.Basic

                                Case ELanguage.TypeScript, ELanguage.JavaScript, ELanguage.CSharp, ELanguage.Java
                                    tw.Fmt(EToken.eFor, EToken.eLP, EToken.eVar, .IdxVarFor, EToken.eSM, .CndFor.TokenList, EToken.eSM, .StepStmtFor.TokenListStmt, EToken.eRP, EToken.eLC, EToken.eNL)
                                    tw.Fmt(.BlcFor.TokenListStmt)
                                    tw.Fmt(EToken.eRC, EToken.eNL)
                            End Select

                        ElseIf .FromFor IsNot Nothing Then
                            tw.Fmt(EToken.eFor, .IdxFor.TokenList, EToken.eEq, .FromFor.TokenList, EToken.eTo, .ToFor.TokenList)

                            If .StepFor IsNot Nothing Then
                                tw.Fmt(EToken.eStep, .StepFor.TokenList)
                            End If
                            tw.Fmt(EToken.eNL)

                            tw.Fmt(.BlcFor.TokenListStmt)
                            tw.Fmt(EToken.eNext, EToken.eNL)
                        Else
                            Debug.Assert(False, "For Src Bas")
                        End If
                    End With

                ElseIf TypeOf self Is TReDim Then
                    With CType(self, TReDim)
                        tw.Fmt(EToken.eReDim, .TrmReDim.TokenList, EToken.eLP, Laminate((From trm In .DimReDim Select trm.TokenList), New TToken(EToken.eComma, self)), EToken.eRP, EToken.eNL)
                    End With

                ElseIf TypeOf self Is TBlock Then
                    With CType(self, TBlock)
                        For Each stmt In .StmtBlc
                            tw.Fmt(stmt.TokenListStmt)
                        Next
                    End With

                ElseIf TypeOf self Is TReturn Then
                    With CType(self, TReturn)
                        If .YieldRet Then
                            tw.Fmt(EToken.eYield)
                        Else
                            tw.Fmt(EToken.eReturn)
                        End If
                        If .TrmRet IsNot Nothing Then
                            tw.Fmt(.TrmRet.TokenList)
                        End If
                        tw.Fmt(EToken.eEOL)

                    End With

                ElseIf TypeOf self Is TThrow Then
                    With CType(self, TThrow)
                        tw.Fmt(EToken.eThrow, .TrmThrow.TokenList, EToken.eEOL)
                    End With

                ElseIf TypeOf self Is TComment Then
                    With CType(self, TComment)
                        ComSrc(CType(self, TComment), 0, tw)
                    End With

                Else
                    Select Case .TypeStmt
                        Case EToken.eExitDo, EToken.eExitFor, EToken.eExitSub
                            Select Case .TypeStmt
                                Case EToken.eExitDo
                                    tw.Fmt(EToken.eExitDo, EToken.eNL)
                                Case EToken.eExitFor
                                    tw.Fmt(EToken.eExitFor, EToken.eNL)
                                Case EToken.eExitSub
                                    tw.Fmt(EToken.eExitSub, EToken.eNL)
                                Case Else
                                    Debug.Assert(False)
                            End Select

                        Case Else
                            Debug.WriteLine("Err Stmt Src:{0}", self)
                            Debug.Assert(False)
                    End Select
                End If

                If .AfterSrc <> "" Then
                    Dim v = .AfterSrc.Trim().Replace(vbCr, "").Split(New Char() {vbLf(0)})
                    For Each s In v
                        tw.Fmt(s, EToken.eNL)
                    Next
                End If

                .TokenListStmt = tw.TokenListTW
            End With

        ElseIf TypeOf self Is TModifier Then
            With CType(self, TModifier)
                If .isXmlIgnore OrElse .isWeak OrElse .isParent OrElse .isInvariant Then
                    tw.Fmt(EToken.eLT)

                    If .isXmlIgnore Then
                        tw.Fmt("XmlIgnoreAttribute", EToken.eLP, EToken.eRP)
                    End If

                    If .isWeak Then
                        If .isXmlIgnore Then
                            tw.Fmt(EToken.eComma)
                        End If

                        tw.Fmt("_Weak", EToken.eLP, EToken.eRP)
                    End If

                    If .isParent Then
                        If .isXmlIgnore OrElse .isWeak Then
                            tw.Fmt(EToken.eComma)
                        End If

                        tw.Fmt("_Parent", EToken.eLP, EToken.eRP)
                    End If

                    If .isInvariant Then
                        tw.Fmt("_Invariant", EToken.eLP, EToken.eRP)
                    End If

                    tw.Fmt(EToken.eGT)
                End If
                If .isPublic Then
                    tw.Fmt(EToken.ePublic)
                End If
                If .isShared Then
                    tw.Fmt(EToken.eShared)
                End If
                If .isIterator Then
                    tw.Fmt(EToken.eIterator)
                End If
                If .isConst Then
                    tw.Fmt(EToken.eConst)
                End If
                If .isVirtual Then
                    tw.Fmt(EToken.eVirtual)
                End If
                If .isMustOverride Then
                    tw.Fmt(EToken.eMustOverride)
                End If
                If .isOverride Then
                    tw.Fmt(EToken.eOverride)
                End If

                .TokenListMod = tw.GetTokenList()
            End With

        ElseIf TypeOf self Is TSourceFile Then
            With CType(self, TSourceFile)

                Select Case ParserMK.LanguageSP
                    Case ELanguage.Basic, ELanguage.CSharp
                        For Each str_f In .vUsing
                            tw.Fmt(EToken.eImports, str_f, EToken.eNL)
                        Next
                End Select

                For Each cla1 In .ClaSrc
                    'If PrjMK.OutputNotUsed OrElse cla1.UsedVar OrElse cla1.KndCla = EClass.eDelegateCla Then

                    'End If
                    If cla1.TokenListCls Is Nothing Then
                        tw.Fmt(cla1.TokenListVar)
                    Else
                        tw.Fmt(cla1.TokenListCls)
                    End If
                Next

                .TokenListSrc = tw.GetTokenList()
            End With

        End If

    End Sub

    '   エスケープ文字を作る
    Public Function Escape(str1 As String) As String
        Dim sb As New TStringWriter

        For Each ch1 In str1
            If ch1 = """"c Then
                sb.Append("""""")
            Else
                sb.Append(ch1)
            End If
        Next

        Return sb.ToString()
    End Function

    Public Function Laminate(vvtkn As IEnumerable(Of List(Of TToken)), sep As TToken) As List(Of TToken)
        Dim i As Integer = 0
        Dim vtkn As New List(Of TToken)

        For Each tkns In vvtkn
            If i <> 0 Then
                vtkn.Add(sep)
            End If

            vtkn.AddRange(tkns)
            i += 1
        Next

        Return vtkn
    End Function

    Public Function AppArgTokenList(self As Object) As List(Of TToken)
        If TypeOf self Is TApply Then
            With CType(self, TApply)
                Dim vtkn As New List(Of TToken)

                vtkn.Add(New TToken(EToken.eLP, self))
                vtkn.AddRange(Laminate((From trm In .ArgApp Select trm.TokenList), New TToken(EToken.eComma, self)))
                vtkn.Add(New TToken(EToken.eRP, self))

                Return vtkn
            End With
        End If
        Debug.Assert(False)
        Return Nothing
    End Function

    Public Sub ModifierSrc(mod1 As TModifier)
        Dim tw As New TTokenWriter(Nothing, ParserMK)

        If mod1 IsNot Nothing Then
            If mod1.isPublic Then
                tw.Fmt(EToken.ePublic)
            End If
            If mod1.isShared Then
                tw.Fmt(EToken.eShared)
            End If
            If mod1.isConst Then
                tw.Fmt(EToken.eConst)
            End If
            If mod1.isVirtual Then
                tw.Fmt(EToken.eVirtual)
            End If
            If mod1.isMustOverride Then
                tw.Fmt(EToken.eMustOverride)
            End If
            If mod1.isOverride Then
                tw.Fmt(EToken.eOverride)
            End If

            mod1.TokenListMod = tw.GetTokenList()
        End If
    End Sub
End Class
