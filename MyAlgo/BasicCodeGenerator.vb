Imports System.Diagnostics

Public Class TBasicCodeGenerator
    Inherits TCodeGenerator

    Public Sub New(prj1 As TProject, parser As TSourceParser)
        MyBase.New(prj1, parser)
    End Sub

    '   エスケープ文字を作る
    Public Overrides Function Escape(str1 As String) As String
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

    Public Overrides Sub AppSrc(app1 As TApply)
        Select Case app1.TypeApp
            Case EToken.eADD, EToken.eMns, EToken.eMUL, EToken.eDIV, EToken.eMOD, EToken.eINC, EToken.eDEC
                If app1.ArgApp.Count = 1 AndAlso (app1.TypeApp = EToken.eADD OrElse app1.TypeApp = EToken.eMns) Then
                    WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(0))
                Else

                    TrmSrc(app1.ArgApp(0))
                    WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.eSymFig, app1)
                    TrmSrc(app1.ArgApp(1))
                End If

            Case EToken.eAppCall

                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case EToken.eBaseCall
                WordAdd(EToken.eBase, EFigType.eResFig, app1)
                WordAdd(".", EFigType.eSymFig, app1)
                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case EToken.eBaseNew
                Fmt(EToken.eBase, EToken.eDot, EToken.eNew)
                AppArg(app1)

            Case EToken.eNew
                Debug.Assert(app1.NewApp IsNot Nothing)

                WordAdd(EToken.eNew, EFigType.eResFig, app1)

                If app1.IniApp Is Nothing Then
                    ' 初期値がない場合

                    TypeSrc(app1.NewApp)
                    AppArg(app1)
                Else
                    ' 初期値がある場合

                    If app1.NewApp.IsArray() Then
                        ' 配列の場合

                        TypeSrc(app1.NewApp.GenCla(0))
                        AppArg(app1)
                    Else
                        ' 配列でない場合

                        TypeSrc(app1.NewApp)
                        AppArg(app1)
                        WordAdd(EToken.eFrom, EFigType.eResFig, app1)
                    End If
                    TrmSrc(app1.IniApp)
                End If
            Case EToken.eAs, EToken.eCast
                WordAdd("CType", EFigType.eResFig, app1)
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(",", EFigType.eSymFig, app1)
                TypeSrc(app1.ClassApp)
                WordAdd(")", EFigType.eSymFig, app1)

            Case EToken.eGetType
                WordAdd(EToken.eGetType, EFigType.eResFig, app1)
                WordAdd("(", EFigType.eSymFig, app1)
                TypeSrc(app1.ClassApp)
                WordAdd(")", EFigType.eSymFig, app1)

            Case EToken.eQUE
                WordAdd(EToken.eIIF, EFigType.eResFig, app1)
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(",", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(1))
                WordAdd(",", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(2))
                WordAdd(")", EFigType.eSymFig, app1)

            Case EToken.eTypeof
                WordAdd("typeof", EFigType.eResFig, app1)
                WordAdd("(", EFigType.eSymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(")", EFigType.eSymFig, app1)

            Case Else
                Debug.WriteLine("Err Trm Src2:{0}", app1.TypeApp)
                Debug.Assert(False)
        End Select
    End Sub

    Public Overrides Sub CnsSrc(cns1 As TConstant)
        Select Case cns1.TypeAtm
            Case EToken.eChar
                WordAdd("""" + Escape(cns1.NameRef) + """c", EFigType.eStrFig, cns1)
            Case EToken.eString
                WordAdd("""" + Escape(cns1.NameRef) + """", EFigType.eStrFig, cns1)
            Case EToken.eRegEx
                WordAdd(Escape(cns1.NameRef), EFigType.eStrFig, cns1)
            Case EToken.eInt
                WordAdd(cns1.NameRef, EFigType.eNumFig, cns1)
            Case EToken.eHex
                Debug.Assert(TSys.Substring(cns1.NameRef, 0, 2) = "&H")
                WordAdd(cns1.NameRef, EFigType.eNumFig, cns1)
            Case Else
                Debug.Assert(False)
        End Select
    End Sub

    Public Overrides Sub DotSrc(dot1 As TDot)
        Dim is_enum As Boolean, dic1 As Dictionary(Of String, String) = Nothing, mem_name As String = Nothing, class_mem1 As String, class_mem2 As String

        is_enum = dot1.IsEnumDot()


        Debug.Assert(is_enum OrElse dot1.TypeDot IsNot Nothing)
        Debug.Assert(dot1.VarRef IsNot Nothing AndAlso (is_enum OrElse dot1.VarRef.ModVar IsNot Nothing))

        If dot1.IsAddressOf Then
            WordAdd(EToken.eAddressOf, EFigType.eResFig, dot1)
        End If

        If dot1.TrmDot Is Nothing Then

        Else
            If Not is_enum AndAlso dot1.VarRef.ModVar.isShared Then
                ' 列挙型以外のstaticメンバーの参照の場合

                Debug.Assert(TypeOf dot1.TrmDot Is TReference AndAlso TypeOf CType(dot1.TrmDot, TReference).VarRef Is TClass)

                If PrjMK.dicClassMemName IsNot Nothing Then
                    ' クラス名とメンバー名を変換する場合

                    class_mem1 = CType(dot1.TrmDot, TReference).NameRef + "." + dot1.NameRef
                    If PrjMK.dicClassMemName.ContainsKey(class_mem1) Then

                        class_mem2 = PrjMK.dicClassMemName(class_mem1)
                        WordAdd(class_mem2, EToken.eRef, EFigType.eUnknownFig, dot1)
                        Exit Sub
                    End If
                End If
            End If

            TrmSrc(dot1.TrmDot)
        End If

        WordAdd(".", EFigType.eSymFig, dot1)

        If dot1.TypeDot IsNot Nothing AndAlso PrjMK.dicMemName IsNot Nothing Then
            If PrjMK.dicMemName.ContainsKey(dot1.TypeDot.NameCla()) Then
                dic1 = PrjMK.dicMemName(dot1.TypeDot.NameCla())
                If dic1.ContainsKey(dot1.NameRef) Then
                    mem_name = dic1(dot1.NameRef)

                    Fmt(dot1)
                    '                    WordAdd(mem_name, EToken.eRef, EFigType.eRefFig, dot1)
                    Exit Sub
                End If
            End If
        End If

        Fmt(dot1)
    End Sub

    Public Overrides Sub RefSrc(ref1 As TReference)
        PrjMK.CheckRefVar(ref1)

        If ref1.IsAddressOf Then
            WordAdd(EToken.eAddressOf, EFigType.eResFig, ref1)
        End If

        Fmt(ref1)
        If ref1.VarRef Is Nothing AndAlso ref1.NameRef <> "true" AndAlso ref1.NameRef <> "false" AndAlso ref1.NameRef <> "null" AndAlso ref1.NameRef <> "undefined" AndAlso ref1.NameRef <> PrjMK.ParsePrj.ThisName Then
            ' WordAdd("参照未解決", EFigType.eUnknownFig, this);
            ' Debug.WriteLine("参照未解決:{0}", ref1.NameRef);
        End If
    End Sub

    Public Overrides Sub RelSrc(rel1 As TApply)
        Dim tp1 As TClass, tp2 As TClass

        Select Case rel1.TypeApp
            Case EToken.eEq, EToken.eNE
                TrmSrc(rel1.ArgApp(0))
                tp1 = rel1.ArgApp(0).TypeTrm
                tp2 = rel1.ArgApp(1).TypeTrm
                If tp1 Is Nothing OrElse tp2 Is Nothing Then
                    ' Debug.WriteLine("");
                    ' tp1 = rel1.ArgApp[0].TypeTrm;
                    ' tp2 = rel1.ArgApp[1].TypeTrm;
                End If
                If tp1 IsNot Nothing AndAlso (tp1.IsAtomType() OrElse tp1.KndCla = EClass.eStructCla) OrElse tp2 IsNot Nothing AndAlso (tp2.IsAtomType() OrElse tp2.KndCla = EClass.eStructCla) Then
                    WordAdd(rel1.TypeApp, EFigType.eSymFig, rel1)
                Else
                    If rel1.TypeApp = EToken.eNE Then
                        WordAdd(EToken.eIsNot, EFigType.eResFig, rel1)
                    Else
                        WordAdd(EToken.eIs, EFigType.eResFig, rel1)
                    End If
                End If
                TrmSrc(rel1.ArgApp(1))
            Case EToken.eASN, EToken.eLT, EToken.eGT, EToken.eADDEQ, EToken.eSUBEQ, EToken.eMULEQ, EToken.eDIVEQ, EToken.eMODEQ, EToken.eLE, EToken.eGE, EToken.eInstanceof
                TrmSrc(rel1.ArgApp(0))
                WordAdd(rel1.TypeApp, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case EToken.eIsNot
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.eIsNot, EFigType.eSymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case EToken.eTypeof
                WordAdd(EToken.eTypeof, EFigType.eResFig, rel1)
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.eIs, EFigType.eResFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case EToken.eIs
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.eIs, EFigType.eResFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case Else
                Debug.Assert(False)
        End Select
    End Sub


    Public Overrides Sub ParSrc(par1 As TParenthesis)
        If par1.TrmPar.IsApp() AndAlso CType(par1.TrmPar, TApply).TypeApp = EToken.eCast Then
            TrmSrc(par1.TrmPar)
        Else
            WordAdd("(", EFigType.eSymFig, par1)
            TrmSrc(par1.TrmPar)
            WordAdd(")", EFigType.eSymFig, par1)
        End If
    End Sub

    ' From i In v1 Where i Mod 2 = 0 Select AA(i)
    Public Overrides Sub FromSrc(from1 As TFrom)
        WordAdd(EToken.eFrom, EFigType.eResFig, from1)
        Fmt(from1.VarFrom)
        WordAdd(EToken.eIn, EFigType.eResFig, from1)
        TrmSrc(from1.SeqFrom)

        If from1.CndFrom IsNot Nothing Then

            WordAdd(EToken.eWhere, EFigType.eResFig, from1)
            TrmSrc(from1.CndFrom)
        End If

        If from1.SelFrom IsNot Nothing Then

            WordAdd(" Select", EFigType.eResFig, from1)
            TrmSrc(from1.SelFrom)
        End If

        If from1.TakeFrom IsNot Nothing Then

            WordAdd(EToken.eTake, EFigType.eResFig, from1)
            TrmSrc(from1.TakeFrom)
        End If

        If from1.InnerFrom IsNot Nothing Then
            FromSrc(from1.InnerFrom)
        End If
    End Sub

    ' Aggregate x In v Into Sum(x.Value)
    Public Overrides Sub AggregateSrc(aggr1 As TAggregate)
        WordAdd(EToken.eAggregate, EFigType.eResFig, aggr1)
        Fmt(aggr1.VarAggr)
        WordAdd(EToken.eIn, EFigType.eResFig, aggr1)
        TrmSrc(aggr1.SeqAggr)

        WordAdd(EToken.eInto, EFigType.eResFig, aggr1)

        Select Case aggr1.FunctionAggr
            Case EAggregateFunction.eSum
                WordAdd("Sum", EFigType.eResFig, aggr1)
            Case EAggregateFunction.eMax
                WordAdd("Max", EFigType.eResFig, aggr1)
            Case EAggregateFunction.eMin
                WordAdd("Min", EFigType.eResFig, aggr1)
            Case Else
                Debug.Assert(False)
        End Select
        Fmt(EToken.eLP)


        TrmSrc(aggr1.IntoAggr)

        Fmt(EToken.eRP)
    End Sub

    Public Sub IfBlcHeadSrc(if_blc As TIfBlock, tab1 As Integer)
        Dim if1 As TIf, i1 As Integer

        if1 = CType(if_blc.ParentStmt, TIf)
        i1 = if1.IfBlc.IndexOf(if_blc)
        Debug.Assert(i1 <> -1)

        If i1 = 0 Then
            Tab(tab1)
            WordAdd("If", EFigType.eResFig, if1)
            TrmSrc(if1.IfBlc(0).CndIf)
            WordAdd(EToken.eThen, EFigType.eResFig, if1)
        Else
            If if1.IfBlc(i1).CndIf IsNot Nothing Then
                Tab(tab1)
                WordAdd("ElseIf", EFigType.eResFig, if1)
                TrmSrc(if1.IfBlc(i1).CndIf)
                WordAdd(EToken.eThen, EFigType.eResFig, if1)
            Else
                Tab(tab1)
                WordAdd("Else", EFigType.eResFig, if1)
            End If
        End If
    End Sub

    Public Sub IfBlcSrc(if_blc As TIfBlock, tab1 As Integer)
        IfBlcHeadSrc(if_blc, tab1)
        BlcSrc(if_blc, EToken.eUnknown, if_blc.BlcIf, tab1)
    End Sub

    '  ifのソースを作る
    Public Overrides Sub IfSrc(if1 As TIf, tab1 As Integer)
        For Each if_blc In if1.IfBlc
            StmtSrc(if_blc, tab1)
        Next

        Tab(tab1)
        WordAdd("End If", EFigType.eResFig, if1)
        NL(if1)
    End Sub

    '  forのソースを作る
    Public Overrides Sub ForSrc(for1 As TFor, tab1 As Integer)
        Tab(tab1)
        If for1.IsDo Then
            WordAdd(EToken.eDo, EFigType.eResFig, for1)
            WordAdd(EToken.eWhile, EFigType.eResFig, for1)
            TrmSrc(for1.CndFor)
            BlcSrc(for1, EToken.eDo, for1.BlcFor, tab1)
        ElseIf for1.InVarFor IsNot Nothing Then
            WordAdd(EToken.eFor, EFigType.eResFig, for1)
            WordAdd(EToken.eEach, EFigType.eResFig, for1)
            Fmt(for1.InVarFor)
            WordAdd(EToken.eIn, EFigType.eResFig, for1)
            TrmSrc(for1.InTrmFor)
            BlcSrc(for1, EToken.eEach, for1.BlcFor, tab1)
        ElseIf for1.IdxVarFor IsNot Nothing Then

        ElseIf for1.FromFor IsNot Nothing Then
            WordAdd(EToken.eFor, EFigType.eResFig, for1)
            Fmt(for1.IdxFor)
            WordAdd("=", EFigType.eSymFig, for1)
            TrmSrc(for1.FromFor)
            WordAdd(EToken.eTo, EFigType.eResFig, for1)
            TrmSrc(for1.ToFor)
            If for1.StepFor IsNot Nothing Then

                WordAdd(EToken.eStep, EFigType.eResFig, for1)
                TrmSrc(for1.StepFor)
            End If
            BlcSrc(for1, EToken.eFor, for1.BlcFor, tab1)
        Else
            Debug.Assert(False, "For Src Bas")
        End If
    End Sub

    '  TStatementのソースを作る
    Public Overrides Sub SimpleStmtSrc(stmt1 As TStatement, tab1 As Integer)
        Dim trm1 As TTerm
        Dim asn1 As TAssignment

        If TypeOf stmt1 Is TAssignment Then
            asn1 = CType(stmt1, TAssignment)
            Tab(tab1)

            TrmSrc(asn1.RelAsn.ArgApp(0))
            WordAdd(asn1.RelAsn.TypeApp, EFigType.eSymFig, stmt1)

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

    Public Sub SelectHeaderSrc(swt1 As TSelect, tab1 As Integer)
        Tab(tab1)
        WordAdd("Select Case", EFigType.eResFig, swt1)
        TrmSrc(swt1.TrmSel)
        NL(swt1)
    End Sub

    Public Sub CaseSrc(cas1 As TCase, tab1 As Integer)
        If Not cas1.DefaultCase Then
            Tab(tab1)
            WordAdd("Case", EFigType.eResFig, cas1)
            For Each trm1 In cas1.TrmCase
                If trm1 IsNot cas1.TrmCase(0) Then
                    '  最初でない場合
                    WordAdd(",", EFigType.eSymFig, cas1)
                End If
                TrmSrc(trm1)
            Next
            NL(cas1)
        Else
            Tab(tab1)
            WordAdd("Case Else", EFigType.eResFig, cas1)
            NL(cas1)
        End If
        For Each stmt1 In cas1.BlcCase.StmtBlc
            StmtSrc(stmt1, tab1 + 1)
        Next

    End Sub

    '  TSelectのソースを作る
    Public Overrides Sub SelectSrc(swt1 As TSelect, tab1 As Integer)
        SelectHeaderSrc(swt1, tab1)
        For Each cas1 In swt1.CaseSel
            StmtSrc(cas1, tab1)
        Next
        Tab(tab1)
        WordAdd("End Select", EFigType.eResFig, swt1)
        NL(swt1)
    End Sub

    '  TTryのソースを作る
    Public Overrides Sub TrySrc(try1 As TTry, tab1 As Integer)
        Tab(tab1)
        WordAdd(EToken.eTry, EFigType.eResFig, try1)
        BlcSrc(try1, EToken.eTry, try1.BlcTry, tab1)

        Tab(tab1)
        WordAdd(EToken.eCatch, EFigType.eResFig, try1)
        VarSrc(try1.VarCatch(0))

        BlcSrc(try1, EToken.eCatch, try1.BlcCatch, tab1)

        Tab(tab1)
        WordAdd("End Try", EFigType.eResFig, try1)
        NL(try1)
    End Sub

    '  TWithのソースを作る
    Public Overrides Sub WithSrc(with1 As TWith, tab1 As Integer)
        Tab(tab1)
        WordAdd(EToken.eWith, EFigType.eResFig, with1)
        TrmSrc(with1.TermWith)

        BlcSrc(with1, EToken.eWith, with1.BlcWith, tab1)

        Tab(tab1)
        WordAdd("End With", EFigType.eResFig, with1)
        NL(with1)
    End Sub

    '  TStatementのソースを作る
    Public Overrides Sub StmtSrc(stmt1 As TStatement, tab1 As Integer)
        Dim ret1 As TReturn
        Dim thr1 As TThrow
        Dim red1 As TReDim
        Dim i1 As Integer

        If stmt1 Is Nothing Then
            WordAdd("null stmt", EFigType.eResFig, stmt1)
            NL(stmt1)
            Exit Sub

        End If

        If stmt1.BeforeSrc IsNot Nothing Then
            Dim v = stmt1.BeforeSrc.Replace(vbCr, "").Split(New Char() {vbLf(0)})
            For Each s In v
                WordAdd(s, EFigType.eUnknownFig, stmt1)
                NL(stmt1)
            Next
        End If

        If Not stmt1.ValidStmt Then

            If TypeOf stmt1 Is TIfBlock Then
                IfBlcHeadSrc(CType(stmt1, TIfBlock), tab1)
                NL(stmt1)
            End If

            Exit Sub
        End If

        If stmt1.ComStmt IsNot Nothing Then
            For Each tkn_f In stmt1.ComStmt
                Tab(tab1)
                WordAdd(tkn_f.StrTkn, EFigType.eComFig, stmt1)
                NL(stmt1)
            Next
        End If
        If TypeOf stmt1 Is TAssignment OrElse TypeOf stmt1 Is TCall OrElse TypeOf stmt1 Is TVariableDeclaration Then
            SimpleStmtSrc(stmt1, tab1)
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TIfBlock Then
            IfBlcSrc(CType(stmt1, TIfBlock), tab1)

        ElseIf TypeOf stmt1 Is TIf Then
            IfSrc(CType(stmt1, TIf), tab1)

        ElseIf TypeOf stmt1 Is TCase Then
            CaseSrc(CType(stmt1, TCase), tab1)

        ElseIf TypeOf stmt1 Is TSelect Then
            SelectSrc(CType(stmt1, TSelect), tab1)

        ElseIf TypeOf stmt1 Is TTry Then
            TrySrc(CType(stmt1, TTry), tab1)

        ElseIf TypeOf stmt1 Is TWith Then
            WithSrc(CType(stmt1, TWith), tab1)

        ElseIf TypeOf stmt1 Is TFor Then
            ForSrc(CType(stmt1, TFor), tab1)
        ElseIf TypeOf stmt1 Is TReDim Then
            red1 = CType(stmt1, TReDim)
            Tab(tab1)
            WordAdd(EToken.eReDim, EFigType.eResFig, stmt1)
            TrmSrc(red1.TrmReDim)
            WordAdd("(", EFigType.eSymFig, stmt1)
            For i1 = 0 To red1.DimReDim.Count - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.eSymFig, stmt1)
                End If
                TrmSrc(red1.DimReDim(i1))
            Next
            WordAdd(")", EFigType.eSymFig, stmt1)
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TBlock Then
            BlcSrc(stmt1, EToken.eUnknown, CType(stmt1, TBlock), tab1)

        ElseIf TypeOf stmt1 Is TReturn Then
            ret1 = CType(stmt1, TReturn)
            Tab(tab1)
            If ret1.YieldRet Then
                WordAdd(EToken.eYield, EFigType.eResFig, stmt1)
            Else
                WordAdd(EToken.eReturn, EFigType.eResFig, stmt1)
            End If
            If ret1.TrmRet IsNot Nothing Then
                TrmSrc(ret1.TrmRet)
            End If
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TThrow Then
            thr1 = CType(stmt1, TThrow)
            Tab(tab1)
            WordAdd(EToken.eThrow, EFigType.eResFig, stmt1)
            TrmSrc(thr1.TrmThrow)
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TComment Then



            ComSrc(CType(stmt1, TComment), tab1, stmt1)
        Else
            Select Case stmt1.TypeStmt
                Case EToken.eExitDo, EToken.eExitFor, EToken.eExitSub
                    Tab(tab1)
                    Select Case stmt1.TypeStmt
                        Case EToken.eExitDo
                            WordAdd("Exit Do", EFigType.eResFig, stmt1)
                        Case EToken.eExitFor
                            WordAdd("Exit For", EFigType.eResFig, stmt1)
                        Case EToken.eExitSub
                            WordAdd("Exit Sub", EFigType.eResFig, stmt1)
                        Case Else
                            Debug.Assert(False)
                    End Select
                    NL(stmt1)
                Case Else
                    Debug.WriteLine("Err Stmt Src:{0}", stmt1)
                    Debug.Assert(False)
            End Select
        End If

        If stmt1.AfterSrc <> "" Then
            Dim v = stmt1.AfterSrc.Trim().Replace(vbCr, "").Split(New Char() {vbLf(0)})
            For Each s In v
                WordAdd(s, EFigType.eUnknownFig, stmt1)
                NL(stmt1)
            Next
        End If
    End Sub

    '  ブロックのソースを作る
    Public Overrides Sub BlcSrc(obj1 As Object, type1 As EToken, blc1 As TBlock, tab1 As Integer)
        NL(obj1)
        For Each stmt1 In blc1.StmtBlc
            If Not stmt1.IsGenerated Then
                StmtSrc(stmt1, tab1 + 1)
            End If
        Next
        Select Case type1
            Case EToken.eOperator
                Tab(tab1)
                WordAdd(EToken.eEnd, EFigType.eResFig, obj1)
                WordAdd(EToken.eOperator, EFigType.eResFig, obj1)
                NL(obj1)
            Case EToken.eSub, EToken.eNew
                Tab(tab1)
                WordAdd(EToken.eEnd, EFigType.eResFig, obj1)
                WordAdd(EToken.eSub, EFigType.eResFig, obj1)
                NL(obj1)
            Case EToken.eFunction
                Tab(tab1)
                WordAdd(EToken.eEnd, EFigType.eResFig, obj1)
                WordAdd(EToken.eFunction, EFigType.eResFig, obj1)
                NL(obj1)
            Case EToken.eFor, EToken.eEach
                Tab(tab1)
                WordAdd(EToken.eNext, EFigType.eResFig, obj1)
                NL(obj1)
            Case EToken.eDo
                Tab(tab1)
                WordAdd(EToken.eLoop, EFigType.eResFig, obj1)
                NL(obj1)
        End Select
    End Sub

    Public Overrides Sub VarSrc(var1 As TVariable)
        Dim as_new As Boolean, app1 As TApply

        as_new = False
        If var1.ByRefVar Then
            WordAdd(EToken.eRef, EFigType.eResFig, var1)
        End If
        If var1.ParamArrayVar Then
            Fmt(EToken.eParamArray)
        End If

        Fmt(var1)
        If var1.TypeVar IsNot Nothing AndAlso Not var1.NoType Then
            WordAdd(EToken.eAs, EFigType.eResFig, var1)
            If var1.InitVar IsNot Nothing AndAlso var1.InitVar.IsApp() AndAlso CType(var1.InitVar, TApply).TypeApp = EToken.eNew Then
                as_new = True
                app1 = CType(var1.InitVar, TApply)
                If app1.ArgApp.Count = 0 Then
                    ' 引数がない場合

                    WordAdd(EToken.eNew, EFigType.eResFig, var1)
                    TypeSrc(app1.NewApp)
                Else
                    ' 引数がある場合
                    TrmSrc(app1)
                End If

                If app1.IniApp IsNot Nothing Then

                    WordAdd(EToken.eFrom, EFigType.eResFig, var1)
                    TrmSrc(app1.IniApp)
                End If
            Else
                TypeSrc(var1.TypeVar)
            End If
        End If
        If Not as_new AndAlso var1.InitVar IsNot Nothing Then
            WordAdd(EToken.eASN, EFigType.eSymFig, var1)
            TrmSrc(var1.InitVar)
        End If
    End Sub

    '  関数のソースを作る
    Public Overrides Sub FncSrc(fnc1 As TFunction)
        ComSrc(fnc1.ComVar, 1, fnc1)
        Tab(1)

        ModifierSrc(fnc1, fnc1.ModFnc())
        Select Case fnc1.TypeFnc
            Case EToken.eFunction
                WordAdd("Function", EFigType.eResFig, fnc1)
                Fmt(fnc1)
            Case EToken.eSub
                WordAdd("Sub", EFigType.eResFig, fnc1)
                Fmt(fnc1)
            Case EToken.eNew
                WordAdd("Sub", EFigType.eResFig, fnc1)
                WordAdd("New", EFigType.eResFig, fnc1)
            Case EToken.eOperator
                WordAdd(EToken.eOperator, EFigType.eResFig, fnc1)
                WordAdd(fnc1.NameFnc(), EFigType.eVarFig, fnc1)
            Case Else
                Debug.WriteLine("")
        End Select


        VarListSrc(fnc1.ArgFnc, fnc1)

        If fnc1.RetType IsNot Nothing Then
            WordAdd(EToken.eAs, EFigType.eResFig, fnc1)
            TypeSrc(fnc1.RetType)
        End If

        If fnc1.InterfaceFnc IsNot Nothing Then
            WordAdd(EToken.eImplements, EFigType.eResFig, fnc1)
            Fmt(fnc1.InterfaceFnc)
            WordAdd(".", EFigType.eSymFig, fnc1)
            Fmt(fnc1.ImplFnc)
        End If

        If fnc1.BlcFnc Is Nothing Then
            NL(fnc1)
        Else
            BlcSrc(fnc1, fnc1.TypeFnc, fnc1.BlcFnc, 1)
        End If
        NL(fnc1)
    End Sub

    Public Overrides Function TypeName(name1 As String) As String
        If name1 = "int" Then
            Return "Integer"
        ElseIf name1 = "bool" Then
            Return "Boolean"
        End If

        If PrjMK.ClassNameTable IsNot Nothing AndAlso PrjMK.ClassNameTable.ContainsKey(name1) Then
            Return PrjMK.ClassNameTable(name1)
        End If

        Return name1
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
            WordAdd("(", EFigType.eUnknownFig, type1)
            For i1 = 0 To type1.DimCla - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.eSymFig, type1)
                End If
            Next
            WordAdd(")", EFigType.eSymFig, type1)
        Else
            ' 配列でない場合
            WordAdd(TypeName(type1.NameType()), EFigType.eClassFig, type1)
            If type1.GenCla IsNot Nothing Then
                ' 総称型の場合

                WordAdd("(", EFigType.eSymFig, type1)
                WordAdd("Of", EFigType.eResFig, type1)
                For i1 = 0 To type1.GenCla.Count - 1
                    If i1 <> 0 Then
                        WordAdd(",", EFigType.eSymFig, type1)
                    End If
                    cla1 = type1.GenCla(i1)
                    TypeSrc(cla1)
                Next
                WordAdd(")", EFigType.eSymFig, type1)
            End If
        End If
    End Sub

    '  変数宣言のソースを作る
    Public Overrides Sub VarDeclSrc(dcl1 As TVariableDeclaration, tab1 As Integer)
        Dim i1 As Integer
        Dim var1 As TVariable

        Tab(tab1)
        ModifierSrc(dcl1, dcl1.ModDecl)
        If dcl1.ModDecl Is Nothing OrElse Not dcl1.ModDecl.isPublic AndAlso Not dcl1.ModDecl.isShared Then
            WordAdd(EToken.eVar, EFigType.eResFig, dcl1)
        End If
        '             sw.Write('\t');
        For i1 = 0 To dcl1.VarDecl.Count - 1
            var1 = dcl1.VarDecl(i1)
            If i1 <> 0 Then
                WordAdd(",", EFigType.eSymFig, dcl1)
            End If
            VarSrc(var1)
        Next
    End Sub

    Public Sub GenericSrc(cla1 As TClass)
        Dim i1 As Integer

        If cla1.GenCla IsNot Nothing Then
            ' ジェネリック型の場合

            WordAdd("(", EFigType.eSymFig, cla1)
            WordAdd(EToken.eOf, EFigType.eResFig, cla1)

            For i1 = 0 To cla1.GenCla.Count - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.eSymFig, cla1)
                End If
                Fmt(cla1.GenCla(i1))
            Next

            WordAdd(")", EFigType.eSymFig, cla1)
        End If
    End Sub

    Public Sub ClsSrc(cla1 As TClass)
        Dim i1 As Integer

        If cla1.ModCla().isPartial Then
            WordAdd(EToken.ePartial, EFigType.eResFig, cla1)
        End If

        WordAdd(EToken.ePublic, EFigType.eResFig, cla1)

        If cla1.ModCla().isAbstract Then
            WordAdd(EToken.eAbstract, EFigType.eResFig, cla1)
        End If

        Select Case cla1.KndCla
            Case EClass.eClassCla
                WordAdd(EToken.eClass, EFigType.eResFig, cla1)
            Case EClass.eStructCla
                WordAdd(EToken.eStruct, EFigType.eResFig, cla1)
            Case EClass.eInterfaceCla
                WordAdd(EToken.eInterface, EFigType.eResFig, cla1)
        End Select
        Fmt(cla1)

        GenericSrc(cla1)

        If cla1.SuperClassList.Count <> 0 AndAlso cla1.SuperClassList(0) IsNot PrjMK.ObjectType Then
            NL(cla1)
            Tab(1)
            WordAdd("Inherits", EFigType.eResFig, cla1)
            TypeSrc(cla1.SuperClassList(0))
        End If

        If cla1.InterfaceList.Count <> 0 Then
            If cla1.InterfaceList(0) IsNot PrjMK.ObjectType Then
                NL(cla1)
                Tab(1)
                WordAdd(EToken.eImplements, EFigType.eResFig, cla1)
                For i1 = 0 To cla1.InterfaceList.Count - 1
                    If i1 <> 0 Then
                        WordAdd(",", EFigType.eSymFig, cla1)
                    End If
                    TypeSrc(cla1.InterfaceList(i1))
                Next
            End If
        End If
        NL(cla1)

        '  すべてのフィールドに対し
        For Each fld1 In cla1.FldCla
            If PrjMK.OutputNotUsed OrElse fld1.UsedVar Then

                ComSrc(fld1.ComVar, 1, fld1)
                Tab(1)
                ModifierSrc(fld1, fld1.ModVar)
                If fld1.ModVar Is Nothing OrElse Not fld1.ModVar.isPublic AndAlso Not fld1.ModVar.isShared Then
                    WordAdd(EToken.eVar, EFigType.eResFig, fld1)
                End If
                VarSrc(fld1)

                If fld1.TailCom <> "" Then

                    WordAdd(vbTab + fld1.TailCom, EFigType.eComFig, fld1)
                End If

                NL(fld1)
            End If
        Next

        '  すべてのメソッドに対し
        For Each fnc1 In cla1.FncCla
            If Not fnc1.IsGenerated() AndAlso (PrjMK.OutputNotUsed OrElse (fnc1.Reachable OrElse fnc1.ModFnc().isMustOverride) OrElse fnc1.NameVar = "New@TList") Then

                FncSrc(fnc1)
            End If
        Next

        WordAdd(EToken.eEnd, EFigType.eResFig, cla1)

        Select Case cla1.KndCla
            Case EClass.eClassCla
                WordAdd(EToken.eClass, EFigType.eResFig, cla1)
            Case EClass.eStructCla
                WordAdd(EToken.eStruct, EFigType.eResFig, cla1)
            Case EClass.eInterfaceCla
                WordAdd(EToken.eInterface, EFigType.eResFig, cla1)
        End Select
        NL(cla1)
    End Sub

    Public Sub MakeBasicSrc(src1 As TSourceFile)
        Dim dlg1 As TDelegate

        For Each str_f In src1.vUsing
            WordAdd(EToken.eImports, EFigType.eResFig, src1)
            WordAdd(str_f, EFigType.eUnknownFig, src1)
            NL(src1)
        Next

        For Each cla1 In src1.ClaSrc
            'If PrjMK.OutputNotUsed OrElse cla1.UsedVar OrElse cla1.KndCla = EClass.eDelegateCla Then

            'End If
            ComSrc(CType(cla1.ComCla(), TComment), 0, cla1)
            Select Case cla1.KndCla
                Case EClass.eEnumCla
                    '  列挙型の場合
                    WordAdd(EToken.ePublic, EFigType.eResFig, cla1)
                    WordAdd(EToken.eEnum, EFigType.eResFig, cla1)
                    Fmt(cla1)
                    NL(cla1)
                    '  すべてのフィールドに対し
                    For Each fld1 In cla1.FldCla
                        'If PrjMK.OutputNotUsed OrElse fld1.UsedVar Then
                        'End If
                        Tab(1)
                        Fmt(fld1)
                        NL(fld1)
                    Next
                    WordAdd(EToken.eEnd, EFigType.eResFig, cla1)
                    WordAdd(EToken.eEnum, EFigType.eResFig, cla1)
                    NL(cla1)

                Case EClass.eDelegateCla
                    ' デリゲートの場合

                    dlg1 = CType(cla1, TDelegate)
                    WordAdd(EToken.ePublic, EFigType.eResFig, cla1)
                    WordAdd(EToken.eDelegate, EFigType.eResFig, cla1)
                    If dlg1.RetDlg Is Nothing Then
                        WordAdd(EToken.eSub, EFigType.eResFig, cla1)
                    Else
                        WordAdd(EToken.eFunction, EFigType.eResFig, cla1)
                    End If
                    Fmt(cla1)

                    GenericSrc(cla1)

                    VarListSrc(dlg1.ArgDlg, dlg1)
                    If dlg1.RetDlg IsNot Nothing Then
                        Fmt(EToken.eAs)
                        TypeSrc(dlg1.RetDlg)
                    End If
                    NL(dlg1)
                Case Else

                    '  クラスの場合
                    ClsSrc(cla1)
            End Select
        Next

    End Sub

    Public Sub OutputBasicSrc(src1 As TSourceFile, out_dir As String)
        Dim src_dir As String, html_path As String, fname As String, ext1 As String, src_txt As String

        src_txt = MakeSrcText()

        fname = TPath.GetFileNameWithoutExtension(src1.FileSrc)

        src_dir = out_dir + "html\_src"
        TDirectory.CreateDirectory(src_dir)

        html_path = src_dir + "\" + fname + ".html"
        TFile.WriteAllText(html_path, MakeSrcHTML(src1))
        ext1 = ".vb"
        TFile.WriteAllText(out_dir + fname + ext1, src_txt)
    End Sub

    Public Overrides Function MakeSrcText() As String
        Dim sw As New TStringWriter

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
                            Case EToken.eAs, EToken.eTo, EToken.eIs, EToken.eIsNot, EToken.eIn, EToken.eInto, EToken.eWhere, EToken.eTake, EToken.eStep, EToken.eImplements, EToken.eParamArray
                                sw.Write(" " + txt1.TextTxt + " ")
                            Case EToken.eThen
                                sw.Write(" " + txt1.TextTxt)
                            Case Else
                                sw.Write(txt1.TextTxt + " ")
                        End Select
                    Case EFigType.eRefFig
                        Select Case txt1.TknTxt
                            Case EToken.eRef
                                If txt1.TextTxt = "null" Then
                                    sw.Write("Nothing")
                                ElseIf txt1.TextTxt = PrjMK.ParsePrj.ThisName Then
                                    sw.Write(PrjMK.ParsePrj.ThisName)
                                Else
                                    sw.Write(txt1.TextTxt)
                                End If
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
