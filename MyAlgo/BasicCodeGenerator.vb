﻿Imports System.Diagnostics

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
            Case EToken.ADD, EToken.Mns, EToken.MUL, EToken.DIV, EToken.MOD_, EToken.INC, EToken.DEC, EToken.BitOR
                If app1.ArgApp.Count = 1 Then
                    If app1.TypeApp = EToken.ADD OrElse app1.TypeApp = EToken.Mns Then
                        WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.SymFig, app1)
                        TrmSrc(app1.ArgApp(0))
                    Else
                        TrmSrc(app1.ArgApp(0))
                        WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.SymFig, app1)
                    End If
                Else

                    TrmSrc(app1.ArgApp(0))
                    WordAdd(ParserCG.vTknName(app1.TypeApp), EFigType.SymFig, app1)
                    TrmSrc(app1.ArgApp(1))
                End If

            Case EToken.AppCall

                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case EToken.BaseCall
                WordAdd(EToken.Base, EFigType.ResFig, app1)
                WordAdd(".", EFigType.SymFig, app1)
                TrmSrc(app1.FncApp)
                AppArg(app1)

            Case EToken.BaseNew
                Fmt(EToken.Base, EToken.Dot, EToken.New_)
                AppArg(app1)

            Case EToken.New_
                Debug.Assert(app1.NewApp IsNot Nothing)

                WordAdd(EToken.New_, EFigType.ResFig, app1)

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
                        WordAdd(EToken.From_, EFigType.ResFig, app1)
                    End If
                    TrmSrc(app1.IniApp)
                End If

            Case EToken.As_, EToken.Cast
                WordAdd("CType", EFigType.ResFig, app1)
                WordAdd("(", EFigType.SymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(",", EFigType.SymFig, app1)
                TypeSrc(app1.ClassApp)
                WordAdd(")", EFigType.SymFig, app1)

            Case EToken.GetType_
                WordAdd(EToken.GetType_, EFigType.ResFig, app1)
                WordAdd("(", EFigType.SymFig, app1)
                TypeSrc(app1.ClassApp)
                WordAdd(")", EFigType.SymFig, app1)

            Case EToken.Question
                WordAdd(EToken.If_, EFigType.ResFig, app1)
                WordAdd("(", EFigType.SymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(",", EFigType.SymFig, app1)
                TrmSrc(app1.ArgApp(1))
                WordAdd(",", EFigType.SymFig, app1)
                TrmSrc(app1.ArgApp(2))
                WordAdd(")", EFigType.SymFig, app1)

            Case EToken.Instanceof
                WordAdd("typeof", EFigType.ResFig, app1)
                WordAdd("(", EFigType.SymFig, app1)
                TrmSrc(app1.ArgApp(0))
                WordAdd(")", EFigType.SymFig, app1)

            Case Else
                Debug.WriteLine("Err Trm Src2:{0}", app1.TypeApp)
                Debug.Assert(False)
        End Select
    End Sub

    Public Overrides Sub CnsSrc(cns1 As TConstant)
        Select Case cns1.TypeAtm
            Case EToken.Char_
                WordAdd("""" + Escape(cns1.NameRef) + """c", EFigType.StrFig, cns1)
            Case EToken.String_
                WordAdd("""" + Escape(cns1.NameRef) + """", EFigType.StrFig, cns1)
            Case EToken.RegEx
                WordAdd(Escape(cns1.NameRef), EFigType.StrFig, cns1)
            Case EToken.Int
                WordAdd(cns1.NameRef, EFigType.NumFig, cns1)
            Case EToken.Hex
                Debug.Assert(TSys.Substring(cns1.NameRef, 0, 2) = "&H")
                WordAdd(cns1.NameRef, EFigType.NumFig, cns1)
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
            WordAdd(EToken.AddressOf_, EFigType.ResFig, dot1)
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
                        WordAdd(class_mem2, EToken.Ref, EFigType.UnknownFig, dot1)
                        Exit Sub
                    End If
                End If
            End If

            TrmSrc(dot1.TrmDot)
        End If

        WordAdd(".", EFigType.SymFig, dot1)

        If dot1.TypeDot IsNot Nothing AndAlso PrjMK.dicMemName IsNot Nothing Then
            If PrjMK.dicMemName.ContainsKey(dot1.TypeDot.NameCla()) Then
                dic1 = PrjMK.dicMemName(dot1.TypeDot.NameCla())
                If dic1.ContainsKey(dot1.NameRef) Then
                    mem_name = dic1(dot1.NameRef)

                    Fmt(dot1)
                    '                    WordAdd(mem_name, EToken.Ref, EFigType.RefFig, dot1)
                    Exit Sub
                End If
            End If
        End If

        Fmt(dot1)
    End Sub

    Public Overrides Sub RefSrc(ref1 As TReference)
        PrjMK.CheckRefVar(ref1)

        If ref1.IsAddressOf Then
            WordAdd(EToken.AddressOf_, EFigType.ResFig, ref1)
        End If

        Fmt(ref1)
        If ref1.VarRef Is Nothing AndAlso ref1.NameRef <> "true" AndAlso ref1.NameRef <> "false" AndAlso ref1.NameRef <> "null" AndAlso ref1.NameRef <> "undefined" AndAlso ref1.NameRef <> PrjMK.ParsePrj.ThisName Then
            ' WordAdd("参照未解決", EFigType.UnknownFig, this);
            ' Debug.WriteLine("参照未解決:{0}", ref1.NameRef);
        End If
    End Sub

    Public Overrides Sub RelSrc(rel1 As TApply)
        Dim tp1 As TClass, tp2 As TClass

        Select Case rel1.TypeApp
            Case EToken.Eq, EToken.NE
                TrmSrc(rel1.ArgApp(0))
                tp1 = rel1.ArgApp(0).TypeTrm
                tp2 = rel1.ArgApp(1).TypeTrm
                If tp1 Is Nothing OrElse tp2 Is Nothing Then
                    ' Debug.WriteLine("");
                    ' tp1 = rel1.ArgApp[0].TypeTrm;
                    ' tp2 = rel1.ArgApp[1].TypeTrm;
                End If
                If tp1 IsNot Nothing AndAlso (tp1.IsAtomType() OrElse tp1.KndCla = EClass.StructCla) OrElse tp2 IsNot Nothing AndAlso (tp2.IsAtomType() OrElse tp2.KndCla = EClass.StructCla) Then
                    WordAdd(rel1.TypeApp, EFigType.SymFig, rel1)
                Else
                    If rel1.TypeApp = EToken.NE Then
                        WordAdd(EToken.IsNot_, EFigType.ResFig, rel1)
                    Else
                        WordAdd(EToken.Is_, EFigType.ResFig, rel1)
                    End If
                End If
                TrmSrc(rel1.ArgApp(1))
            Case EToken.ASN, EToken.LT, EToken.GT, EToken.ADDEQ, EToken.SUBEQ, EToken.MULEQ, EToken.DIVEQ, EToken.MODEQ, EToken.LE, EToken.GE
                TrmSrc(rel1.ArgApp(0))
                WordAdd(rel1.TypeApp, EFigType.SymFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case EToken.IsNot_
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.IsNot_, EFigType.SymFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case EToken.Instanceof
                WordAdd(EToken.Instanceof, EFigType.ResFig, rel1)
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.Is_, EFigType.ResFig, rel1)
                TrmSrc(rel1.ArgApp(1))

            Case EToken.Is_
                TrmSrc(rel1.ArgApp(0))
                WordAdd(EToken.Is_, EFigType.ResFig, rel1)
                TrmSrc(rel1.ArgApp(1))
            Case Else
                Debug.Assert(False)
        End Select
    End Sub


    Public Overrides Sub ParSrc(par1 As TParenthesis)
        If par1.TrmPar.IsApp() AndAlso CType(par1.TrmPar, TApply).TypeApp = EToken.Cast Then
            TrmSrc(par1.TrmPar)
        Else
            WordAdd("(", EFigType.SymFig, par1)
            TrmSrc(par1.TrmPar)
            WordAdd(")", EFigType.SymFig, par1)
        End If
    End Sub

    ' From i In v1 Where i Mod 2 = 0 Select AA(i)
    Public Overrides Sub FromSrc(from1 As TFrom)
        WordAdd(EToken.From_, EFigType.ResFig, from1)
        Fmt(from1.VarQry)
        WordAdd(EToken.In_, EFigType.ResFig, from1)
        TrmSrc(from1.SeqQry)

        If from1.CndQry IsNot Nothing Then

            WordAdd(EToken.Where_, EFigType.ResFig, from1)
            TrmSrc(from1.CndQry)
        End If

        If from1.SelFrom IsNot Nothing Then

            WordAdd(" Select", EFigType.ResFig, from1)
            TrmSrc(from1.SelFrom)
        End If

        If from1.TakeFrom IsNot Nothing Then

            WordAdd(EToken.Take_, EFigType.ResFig, from1)
            TrmSrc(from1.TakeFrom)
        End If

        If from1.InnerFrom IsNot Nothing Then
            FromSrc(from1.InnerFrom)
        End If
    End Sub

    ' Aggregate x In v Into Sum(x.Value)
    Public Overrides Sub AggregateSrc(aggr1 As TAggregate)
        WordAdd(EToken.Aggregate_, EFigType.ResFig, aggr1)
        Fmt(aggr1.VarQry)
        WordAdd(EToken.In_, EFigType.ResFig, aggr1)
        TrmSrc(aggr1.SeqQry)

        If aggr1.CndQry IsNot Nothing Then

            WordAdd(EToken.Where_, EFigType.ResFig, aggr1)
            TrmSrc(aggr1.CndQry)
        End If

        WordAdd(EToken.Into_, EFigType.ResFig, aggr1)

        Select Case aggr1.FunctionAggr
            Case EAggregateFunction.Sum
                WordAdd("Sum", EFigType.ResFig, aggr1)
            Case EAggregateFunction.Max
                WordAdd("Max", EFigType.ResFig, aggr1)
            Case EAggregateFunction.Min
                WordAdd("Min", EFigType.ResFig, aggr1)
            Case EAggregateFunction.Average
                WordAdd("Average", EFigType.ResFig, aggr1)
            Case Else
                Debug.Assert(False)
        End Select
        Fmt(EToken.LP)


        TrmSrc(aggr1.IntoAggr)

        Fmt(EToken.RP)
    End Sub

    Public Sub IfBlcHeadSrc(if_blc As TIfBlock, tab1 As Integer)
        Dim if1 As TIf, i1 As Integer

        if1 = CType(if_blc.UpTrm, TIf)
        i1 = if1.IfBlc.IndexOf(if_blc)
        Debug.Assert(i1 <> -1)

        If i1 = 0 Then
            Tab(tab1)
            WordAdd("If", EFigType.ResFig, if1)
            TrmSrc(if1.IfBlc(0).CndIf)
            WordAdd(EToken.Then_, EFigType.ResFig, if1)
        Else
            If if1.IfBlc(i1).CndIf IsNot Nothing Then
                Tab(tab1)
                WordAdd("ElseIf", EFigType.ResFig, if1)
                TrmSrc(if1.IfBlc(i1).CndIf)
                WordAdd(EToken.Then_, EFigType.ResFig, if1)
            Else
                Tab(tab1)
                WordAdd("Else", EFigType.ResFig, if1)
            End If
        End If
    End Sub

    Public Sub IfBlcSrc(if_blc As TIfBlock, tab1 As Integer)
        IfBlcHeadSrc(if_blc, tab1)
        If if_blc.WithIf IsNot Nothing Then
            NL(if_blc)
            Tab(tab1)
            WordAdd(EToken.With_, EFigType.ResFig, if_blc)
            TrmSrc(if_blc.WithIf)

            BlcSrc(if_blc, EToken.Unknown, if_blc.BlcIf, tab1)

            Tab(tab1)
            WordAdd("End With", EFigType.ResFig, if_blc)
            NL(if_blc)
        Else
            BlcSrc(if_blc, EToken.Unknown, if_blc.BlcIf, tab1)
        End If
    End Sub

    '  ifのソースを作る
    Public Overrides Sub IfSrc(if1 As TIf, tab1 As Integer)
        For Each if_blc In if1.IfBlc
            StmtSrc(if_blc, tab1)
        Next

        Tab(tab1)
        WordAdd("End If", EFigType.ResFig, if1)
        NL(if1)
    End Sub

    '  forのソースを作る
    Public Overrides Sub ForSrc(for1 As TFor, tab1 As Integer)
        Tab(tab1)
        If for1.IsDo Then
            WordAdd(EToken.Do_, EFigType.ResFig, for1)
            WordAdd(EToken.While_, EFigType.ResFig, for1)
            TrmSrc(for1.CndFor)
            BlcSrc(for1, EToken.Do_, for1.BlcFor, tab1)
        ElseIf for1.InVarFor IsNot Nothing Then
            WordAdd(EToken.For_, EFigType.ResFig, for1)
            WordAdd(EToken.Each_, EFigType.ResFig, for1)
            Fmt(for1.InVarFor)
            WordAdd(EToken.In_, EFigType.ResFig, for1)
            TrmSrc(for1.InTrmFor)
            BlcSrc(for1, EToken.Each_, for1.BlcFor, tab1)
        ElseIf for1.IdxVarFor IsNot Nothing Then

        ElseIf for1.FromFor IsNot Nothing Then
            WordAdd(EToken.For_, EFigType.ResFig, for1)
            Fmt(for1.IdxFor)
            WordAdd("=", EFigType.SymFig, for1)
            TrmSrc(for1.FromFor)
            WordAdd(EToken.To_, EFigType.ResFig, for1)
            TrmSrc(for1.ToFor)
            If for1.StepFor IsNot Nothing Then

                WordAdd(EToken.Step_, EFigType.ResFig, for1)
                TrmSrc(for1.StepFor)
            End If
            BlcSrc(for1, EToken.For_, for1.BlcFor, tab1)
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
            WordAdd(asn1.RelAsn.TypeApp, EFigType.SymFig, stmt1)

            trm1 = asn1.RelAsn.ArgApp(1)
            TrmSrc(trm1)
        ElseIf TypeOf stmt1 Is TCall Then
            Tab(tab1)
            TrmSrc(CType(stmt1, TCall).AppCall)
        ElseIf TypeOf stmt1 Is TVariableDeclaration Then
            VarDeclSrc(CType(stmt1, TVariableDeclaration), tab1)
        Else
            WordAdd("Simple Stmt Src", EFigType.SymFig, stmt1)
        End If

        If stmt1.TailCom <> "" Then

            WordAdd(vbTab + stmt1.TailCom, EFigType.ComFig, stmt1)
        End If
    End Sub

    Public Sub SelectHeaderSrc(swt1 As TSelect, tab1 As Integer)
        Tab(tab1)
        WordAdd("Select Case", EFigType.ResFig, swt1)
        TrmSrc(swt1.TrmSel)
        NL(swt1)
    End Sub

    Public Sub CaseSrc(cas1 As TCase, tab1 As Integer)
        If Not cas1.DefaultCase Then
            Tab(tab1)
            WordAdd("Case", EFigType.ResFig, cas1)
            For Each trm1 In cas1.TrmCase
                If trm1 IsNot cas1.TrmCase(0) Then
                    '  最初でない場合
                    WordAdd(",", EFigType.SymFig, cas1)
                End If
                TrmSrc(trm1)
            Next
            NL(cas1)
        Else
            Tab(tab1)
            WordAdd("Case Else", EFigType.ResFig, cas1)
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
        WordAdd("End Select", EFigType.ResFig, swt1)
        NL(swt1)
    End Sub

    '  TTryのソースを作る
    Public Overrides Sub TrySrc(try1 As TTry, tab1 As Integer)
        Tab(tab1)
        WordAdd(EToken.Try_, EFigType.ResFig, try1)
        BlcSrc(try1, EToken.Try_, try1.BlcTry, tab1)

        Tab(tab1)
        WordAdd(EToken.Catch_, EFigType.ResFig, try1)
        VarSrc(try1.VarCatch(0))

        BlcSrc(try1, EToken.Catch_, try1.BlcCatch, tab1)

        Tab(tab1)
        WordAdd("End Try", EFigType.ResFig, try1)
        NL(try1)
    End Sub

    '  TStatementのソースを作る
    Public Overrides Sub StmtSrc(stmt1 As TStatement, tab1 As Integer)
        Dim ret1 As TReturn
        Dim thr1 As TThrow
        Dim red1 As TReDim
        Dim i1 As Integer

        If stmt1 Is Nothing Then
            WordAdd("null stmt", EFigType.ResFig, stmt1)
            NL(stmt1)
            Exit Sub

        End If

        If stmt1.BeforeSrc IsNot Nothing Then
            Dim v = stmt1.BeforeSrc.Replace(vbCr, "").Split(New Char() {vbLf(0)})
            For Each s In v
                WordAdd(s, EFigType.UnknownFig, stmt1)
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
                WordAdd(tkn_f.StrTkn, EFigType.ComFig, stmt1)
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

        ElseIf TypeOf stmt1 Is TFor Then
            ForSrc(CType(stmt1, TFor), tab1)
        ElseIf TypeOf stmt1 Is TReDim Then
            red1 = CType(stmt1, TReDim)
            Tab(tab1)
            WordAdd(EToken.ReDim_, EFigType.ResFig, stmt1)
            TrmSrc(red1.TrmReDim)
            WordAdd("(", EFigType.SymFig, stmt1)
            For i1 = 0 To red1.DimReDim.Count - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.SymFig, stmt1)
                End If
                TrmSrc(red1.DimReDim(i1))
            Next
            WordAdd(")", EFigType.SymFig, stmt1)
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TBlock Then
            BlcSrc(stmt1, EToken.Unknown, CType(stmt1, TBlock), tab1)

        ElseIf TypeOf stmt1 Is TReturn Then
            ret1 = CType(stmt1, TReturn)
            Tab(tab1)
            If ret1.YieldRet Then
                WordAdd(EToken.Yield_, EFigType.ResFig, stmt1)
            Else
                WordAdd(EToken.Return_, EFigType.ResFig, stmt1)
            End If
            If ret1.TrmRet IsNot Nothing Then
                TrmSrc(ret1.TrmRet)
            End If
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TThrow Then
            thr1 = CType(stmt1, TThrow)
            Tab(tab1)
            WordAdd(EToken.Throw_, EFigType.ResFig, stmt1)
            TrmSrc(thr1.TrmThrow)
            NL(stmt1)

        ElseIf TypeOf stmt1 Is TComment Then



            ComSrc(CType(stmt1, TComment), tab1, stmt1)
        Else
            Select Case stmt1.TypeStmt
                Case EToken.ExitDo, EToken.ExitFor, EToken.ExitSub
                    Tab(tab1)
                    Select Case stmt1.TypeStmt
                        Case EToken.ExitDo
                            WordAdd("Exit Do", EFigType.ResFig, stmt1)
                        Case EToken.ExitFor
                            WordAdd("Exit For", EFigType.ResFig, stmt1)
                        Case EToken.ExitSub
                            WordAdd("Exit Sub", EFigType.ResFig, stmt1)
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
                WordAdd(s, EFigType.UnknownFig, stmt1)
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
            Case EToken.Operator_
                Tab(tab1)
                WordAdd(EToken.End_, EFigType.ResFig, obj1)
                WordAdd(EToken.Operator_, EFigType.ResFig, obj1)
                NL(obj1)
            Case EToken.Sub_, EToken.New_
                Tab(tab1)
                WordAdd(EToken.End_, EFigType.ResFig, obj1)
                WordAdd(EToken.Sub_, EFigType.ResFig, obj1)
                NL(obj1)
            Case EToken.Function_
                Tab(tab1)
                WordAdd(EToken.End_, EFigType.ResFig, obj1)
                WordAdd(EToken.Function_, EFigType.ResFig, obj1)
                NL(obj1)
            Case EToken.For_, EToken.Each_
                Tab(tab1)
                WordAdd(EToken.Next_, EFigType.ResFig, obj1)
                NL(obj1)
            Case EToken.Do_
                Tab(tab1)
                WordAdd(EToken.Loop_, EFigType.ResFig, obj1)
                NL(obj1)
        End Select
    End Sub

    Public Overrides Sub VarSrc(var1 As TVariable)
        Dim as_new As Boolean, app1 As TApply

        as_new = False
        If var1.ByRefVar Then
            WordAdd(EToken.Ref, EFigType.ResFig, var1)
        End If
        If var1.ParamArrayVar Then
            Fmt(EToken.ParamArray_)
        End If

        Fmt(var1)
        If var1.TypeVar IsNot Nothing AndAlso Not var1.NoType Then
            WordAdd(EToken.As_, EFigType.ResFig, var1)
            If var1.InitVar IsNot Nothing AndAlso var1.InitVar.IsApp() AndAlso CType(var1.InitVar, TApply).TypeApp = EToken.New_ Then
                as_new = True
                app1 = CType(var1.InitVar, TApply)
                If app1.ArgApp.Count = 0 Then
                    ' 引数がない場合

                    WordAdd(EToken.New_, EFigType.ResFig, var1)
                    TypeSrc(app1.NewApp)
                Else
                    ' 引数がある場合
                    TrmSrc(app1)
                End If

                If app1.IniApp IsNot Nothing Then

                    WordAdd(EToken.From_, EFigType.ResFig, var1)
                    TrmSrc(app1.IniApp)
                End If
            Else
                TypeSrc(var1.TypeVar)
            End If
        End If
        If Not as_new AndAlso var1.InitVar IsNot Nothing Then
            WordAdd(EToken.ASN, EFigType.SymFig, var1)
            TrmSrc(var1.InitVar)
        End If
    End Sub

    '  関数のソースを作る
    Public Overrides Sub FncSrc(fnc1 As TFunction)
        ComSrc(fnc1.ComVar, 1, fnc1)
        Tab(1)

        ModifierSrc(fnc1, fnc1.ModFnc())
        Select Case fnc1.TypeFnc
            Case EToken.Function_
                WordAdd("Function", EFigType.ResFig, fnc1)
                Fmt(fnc1)
            Case EToken.Sub_
                WordAdd("Sub", EFigType.ResFig, fnc1)
                Fmt(fnc1)
            Case EToken.New_
                WordAdd("Sub", EFigType.ResFig, fnc1)
                WordAdd("New", EFigType.ResFig, fnc1)
            Case EToken.Operator_
                WordAdd(EToken.Operator_, EFigType.ResFig, fnc1)
                WordAdd(fnc1.NameFnc(), EFigType.VarFig, fnc1)
            Case Else
                Debug.WriteLine("")
        End Select


        VarListSrc(fnc1.ArgFnc, fnc1)

        If fnc1.RetType IsNot Nothing Then
            WordAdd(EToken.As_, EFigType.ResFig, fnc1)
            TypeSrc(fnc1.RetType)
        End If

        If fnc1.InterfaceFnc IsNot Nothing Then
            WordAdd(EToken.Implements_, EFigType.ResFig, fnc1)
            Fmt(fnc1.InterfaceFnc)
            WordAdd(".", EFigType.SymFig, fnc1)
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
            WordAdd("型不明", EFigType.UnknownFig, type1)
            Return
        End If

        If type1.DimCla <> 0 Then
            ' 配列の場合

            Debug.Assert(type1.GenCla IsNot Nothing AndAlso type1.GenCla.Count = 1)
            TypeSrc(type1.GenCla(0))
            WordAdd("(", EFigType.UnknownFig, type1)
            For i1 = 0 To type1.DimCla - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.SymFig, type1)
                End If
            Next
            WordAdd(")", EFigType.SymFig, type1)
        Else
            ' 配列でない場合
            WordAdd(TypeName(type1.NameType()), EFigType.ClassFig, type1)
            If type1.GenCla IsNot Nothing Then
                ' 総称型の場合

                WordAdd("(", EFigType.SymFig, type1)
                WordAdd("Of", EFigType.ResFig, type1)
                For i1 = 0 To type1.GenCla.Count - 1
                    If i1 <> 0 Then
                        WordAdd(",", EFigType.SymFig, type1)
                    End If
                    cla1 = type1.GenCla(i1)
                    TypeSrc(cla1)
                Next
                WordAdd(")", EFigType.SymFig, type1)
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
            WordAdd(EToken.Var, EFigType.ResFig, dcl1)
        End If
        '             sw.Write('\t');
        For i1 = 0 To dcl1.VarDecl.Count - 1
            var1 = dcl1.VarDecl(i1)
            If i1 <> 0 Then
                WordAdd(",", EFigType.SymFig, dcl1)
            End If
            VarSrc(var1)
        Next
    End Sub

    Public Sub GenericSrc(cla1 As TClass)
        Dim i1 As Integer

        If cla1.GenCla IsNot Nothing Then
            ' ジェネリック型の場合

            WordAdd("(", EFigType.SymFig, cla1)
            WordAdd(EToken.Of_, EFigType.ResFig, cla1)

            For i1 = 0 To cla1.GenCla.Count - 1
                If i1 <> 0 Then
                    WordAdd(",", EFigType.SymFig, cla1)
                End If
                Fmt(cla1.GenCla(i1))
            Next

            WordAdd(")", EFigType.SymFig, cla1)
        End If
    End Sub

    Public Sub ClsSrc(cla1 As TClass)
        Dim i1 As Integer

        If cla1.ModCla().isPartial Then
            WordAdd(EToken.Partial_, EFigType.ResFig, cla1)
        End If

        WordAdd(EToken.Public_, EFigType.ResFig, cla1)

        If cla1.ModCla().isAbstract Then
            WordAdd(EToken.Abstract, EFigType.ResFig, cla1)
        End If

        Select Case cla1.KndCla
            Case EClass.ClassCla
                WordAdd(EToken.Class_, EFigType.ResFig, cla1)
            Case EClass.StructCla
                WordAdd(EToken.Struct, EFigType.ResFig, cla1)
            Case EClass.InterfaceCla
                WordAdd(EToken.Interface_, EFigType.ResFig, cla1)
        End Select
        Fmt(cla1)

        GenericSrc(cla1)

        If cla1.DirectSuperClassList.Count <> 0 AndAlso cla1.DirectSuperClassList(0) IsNot PrjMK.ObjectType Then
            NL(cla1)
            Tab(1)
            WordAdd("Inherits", EFigType.ResFig, cla1)
            TypeSrc(cla1.DirectSuperClassList(0))
        End If

        If cla1.InterfaceList.Count <> 0 Then
            If cla1.InterfaceList(0) IsNot PrjMK.ObjectType Then
                NL(cla1)
                Tab(1)
                WordAdd(EToken.Implements_, EFigType.ResFig, cla1)
                For i1 = 0 To cla1.InterfaceList.Count - 1
                    If i1 <> 0 Then
                        WordAdd(",", EFigType.SymFig, cla1)
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
                    WordAdd(EToken.Var, EFigType.ResFig, fld1)
                End If
                VarSrc(fld1)

                If fld1.TailCom <> "" Then

                    WordAdd(vbTab + fld1.TailCom, EFigType.ComFig, fld1)
                End If

                NL(fld1)
            End If
        Next

        '  すべてのメソッドに対し
        For Each fnc1 In cla1.FncCla
            If Not fnc1.IsInitializer() AndAlso (PrjMK.OutputNotUsed OrElse (fnc1.Reachable OrElse fnc1.ModFnc().isMustOverride) OrElse fnc1.NameVar = "New@TList") Then

                FncSrc(fnc1)
            End If
        Next

        WordAdd(EToken.End_, EFigType.ResFig, cla1)

        Select Case cla1.KndCla
            Case EClass.ClassCla
                WordAdd(EToken.Class_, EFigType.ResFig, cla1)
            Case EClass.StructCla
                WordAdd(EToken.Struct, EFigType.ResFig, cla1)
            Case EClass.InterfaceCla
                WordAdd(EToken.Interface_, EFigType.ResFig, cla1)
        End Select
        NL(cla1)
    End Sub

    Public Sub MakeBasicSrc(src1 As TSourceFile)
        Dim dlg1 As TDelegate

        For Each str_f In src1.vUsing
            WordAdd(EToken.Imports_, EFigType.ResFig, src1)
            WordAdd(str_f, EFigType.UnknownFig, src1)
            NL(src1)
        Next

        For Each cla1 In src1.ClaSrc
            'If PrjMK.OutputNotUsed OrElse cla1.UsedVar OrElse cla1.KndCla = EClass.DelegateCla Then

            'End If
            ComSrc(CType(cla1.ComCla(), TComment), 0, cla1)
            Select Case cla1.KndCla
                Case EClass.EnumCla
                    '  列挙型の場合
                    WordAdd(EToken.Public_, EFigType.ResFig, cla1)
                    WordAdd(EToken.Enum_, EFigType.ResFig, cla1)
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
                    WordAdd(EToken.End_, EFigType.ResFig, cla1)
                    WordAdd(EToken.Enum_, EFigType.ResFig, cla1)
                    NL(cla1)

                Case EClass.DelegateCla
                    ' デリゲートの場合

                    dlg1 = CType(cla1, TDelegate)
                    WordAdd(EToken.Public_, EFigType.ResFig, cla1)
                    WordAdd(EToken.Delegate_, EFigType.ResFig, cla1)
                    If dlg1.RetDlg Is Nothing Then
                        WordAdd(EToken.Sub_, EFigType.ResFig, cla1)
                    Else
                        WordAdd(EToken.Function_, EFigType.ResFig, cla1)
                    End If
                    Fmt(cla1)

                    GenericSrc(cla1)

                    VarListSrc(dlg1.ArgDlg, dlg1)
                    If dlg1.RetDlg IsNot Nothing Then
                        Fmt(EToken.As_)
                        TypeSrc(dlg1.RetDlg)
                    End If
                    NL(dlg1)
                Case Else

                    '  クラスの場合
                    ClsSrc(cla1)
            End Select
        Next

    End Sub

    Public Sub OutputBasicTextHTML(src1 As TSourceFile, out_dir As String)
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
                    Case EFigType.SymFig
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
                    Case EFigType.ResFig
                        Select Case txt1.TknTxt
                            Case EToken.As_, EToken.To_, EToken.Is_, EToken.IsNot_, EToken.In_, EToken.Into_, EToken.Where_, EToken.Take_, EToken.Step_, EToken.Implements_, EToken.ParamArray_
                                sw.Write(" " + txt1.TextTxt + " ")
                            Case EToken.Then_
                                sw.Write(" " + txt1.TextTxt)
                            Case Else
                                sw.Write(txt1.TextTxt + " ")
                        End Select
                    Case EFigType.RefFig
                        Select Case txt1.TknTxt
                            Case EToken.Ref
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
