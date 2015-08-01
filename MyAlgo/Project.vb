﻿Imports System.IO
Imports System.Text
Imports System.Diagnostics
'Imports System.Web

' -------------------------------------------------------------------------------- TPrj
Public Class TPrj
    Public Shared Prj As TPrj
    Public SrcDir As String
    Public OutDir As String
    Public SrcFileNames As String()
    Public vCla As New TList(Of TCls)
    Public vGenCla As New TList(Of TCls)
    Public vGenTmpCla As New TList(Of TCls)
    Public vArrCla As New TList(Of TCls)
    Public vAllClass As New TList(Of TCls)
    Public vAllFnc As New TList(Of TFnc)
    Public vAllFld As New TList(Of TFld)    ' すべてのフィールド
    Public CurSrc As TSrc ' 現在のソース
    Public TypeType As TCls
    Public SystemType As TCls
    Public BoolType As TCls
    Public ObjectType As TCls
    Public DoubleType As TCls
    Public CharType As TCls
    Public IntType As TCls
    Public StringType As TCls
    Public ArrayType As TCls
    Public WaitHandleType As TCls
    Public dicCla As New Dictionary(Of String, TCls) ' クラス辞書
    Public dicGenCla As New Dictionary(Of String, TCls)
    Public dicCmpCla As New Dictionary(Of TCls, TList(Of TCls))
    Public dicArrCla As New Dictionary(Of TCls, TList(Of TCls))
    Public dicMemName As Dictionary(Of String, Dictionary(Of String, String))
    Public dicClassMemName As Dictionary(Of String, String)
    Public ClassNameTable As Dictionary(Of String, String)
    Public SrcPrj As New TList(Of TSrc)
    Public vTknNamePrj As Dictionary(Of ETkn, String)
    Public ParsePrj As TParseBas
    Public MainClassName As String
    Public MainFunctionName As String
    Public theMain As TFnc
    Public ArrayMaker As TFnc
    Public OutputNotUsed As Boolean = True

    Public Sub New()
        Prj = CType(Me, TPrj)
    End Sub

    Public Sub chk(b As Boolean)
        If Not b Then
            Debug.Assert(False)
        End If
    End Sub

    Public Function GetCla(name1 As String) As TCls
        Dim cla1 As TCls = Nothing

        If dicGenCla.ContainsKey(name1) Then
            cla1 = dicGenCla(name1)
            Return cla1
        End If

        If dicCla.ContainsKey(name1) Then
            cla1 = dicCla(name1)
            Return cla1
        Else
            Return Nothing
        End If
    End Function

    Public Function RegCla(name1 As String) As TCls
        Dim cla1 As TCls

        Debug.Assert(GetCla(name1) Is Nothing)

        cla1 = New TCls(Nothing, name1)
        vCla.Add(cla1)
        dicCla.Add(cla1.NameCla(), cla1)

        Return cla1
    End Function

    Public Function RegDelegate(name1 As String) As TDelegate
        Dim cla1 As TDelegate

        Debug.Assert(GetCla(name1) Is Nothing)

        cla1 = New TDelegate(name1)
        vCla.Add(cla1)
        dicCla.Add(cla1.NameCla(), cla1)

        Return cla1
    End Function

    Public Function GetDelegate(name1 As String) As TDelegate
        Dim cla1 As TDelegate

        cla1 = CType(GetCla(name1), TDelegate)
        Debug.Assert(cla1 IsNot Nothing)

        Return cla1
    End Function

    Public Function GetGenCla(name1 As String, vtp As TList(Of TCls)) As TCls
        Dim i1 As Integer, cla1 As TCls, v As TList(Of TCls) = Nothing, ok As Boolean

        cla1 = GetCla(name1)
        Debug.Assert(cla1 IsNot Nothing AndAlso cla1.GenCla IsNot Nothing AndAlso cla1.GenCla.Count = vtp.Count)

        If dicCmpCla.ContainsKey(cla1) Then
            v = dicCmpCla(cla1)
            ' for Find
            For Each cla2 In v
                ok = True
                ' for Find
                For i1 = 0 To cla2.GenCla.Count - 1
                    If cla2.GenCla(i1) IsNot vtp(i1) Then
                        ' 型が違う場合

                        ok = False
                        Exit For
                    End If
                Next

                If ok Then
                    ' すべての型が一致した場合

                    Return cla2
                End If
            Next
        End If

        Return Nothing
    End Function

    ' ジェネリック型のクラスを作る
    Public Function RegGenCla(name1 As String, vtp As TList(Of TCls)) As TCls
        Dim cla1 As TCls, cla3 As TCls, v As TList(Of TCls) = Nothing

        cla1 = GetCla(name1)
        Debug.Assert(cla1 IsNot Nothing AndAlso cla1.GenCla IsNot Nothing AndAlso cla1.GenCla.Count = vtp.Count)

        If dicCmpCla.ContainsKey(cla1) Then
            v = dicCmpCla(cla1)
        Else
            v = New TList(Of TCls)()
            dicCmpCla.Add(cla1, v)
        End If

        ' 新しくジェネリック型のクラスを作る
        If TypeOf cla1 Is TDelegate Then
            cla3 = New TDelegate(cla1.NameCla())
        Else
            cla3 = New TCls(cla1.NameCla())
        End If
        cla3.OrgCla = cla1

        cla3.GenCla = New TList(Of TCls)(From tp In vtp)
        For Each tp In vtp
            Debug.Assert(tp IsNot Nothing)
        Next

        'Debug.WriteLine("ジェネリック {0}", cla3.NameVar)
        ' for Add
        For Each tp In cla3.GenCla
            'Debug.WriteLine("    {0}", tp.NameVar)
        Next

        If Not cla3.GenCla(0).IsParamCla Then

            If cla3.GenCla(0).NameCla() = "T" Then
                Debug.WriteLine("@@@@@@@@@@@@@@")
            End If
            For Each cla4 In vGenCla
                If cla4.LongName() = cla3.LongName() Then
                    Debug.WriteLine("DUP {0} {1} {2}", cla3.NameVar, cla3.GenCla(0).ToString(), cla4.GenCla(0).ToString())
                End If
            Next
            vGenCla.Add(cla3)
            vGenTmpCla.Add(cla3)
        End If

        v.Add(cla3)

        Return cla3
    End Function

    Public Function GetRegGenCla(name1 As String, vtp As TList(Of TCls)) As TCls
        Dim cla1 As TCls

        cla1 = GetGenCla(name1, vtp)
        If cla1 IsNot Nothing Then
            Return cla1
        End If

        cla1 = RegGenCla(name1, vtp)
        Return cla1
    End Function

    '  配列型を得る
    Public Function GetArrCla(cla1 As TCls, dim_cnt As Integer) As TCls
        Dim cla3 As TCls, v As TList(Of TCls) = Nothing

        Debug.Assert(dim_cnt <> 0)
        If dicArrCla.ContainsKey(cla1) Then
            v = dicArrCla(cla1)
            ' for Find
            For Each cla2 In v
                If cla2.DimCla = dim_cnt Then
                    ' 次元が同じ場合

                    Return cla2
                End If
            Next
        Else
            v = New TList(Of TCls)()
            dicArrCla.Add(cla1, v)
        End If

        '  新たに型を作る
        cla3 = TCls.MakeArr(cla1, dim_cnt)
        v.Add(cla3)
        vCla.Add(cla3)
        vArrCla.Add(cla3)

        Return cla3
    End Function

    Public Function GetIEnumerableCla(cla1 As TCls) As TCls
        Dim vtp As New TList(Of TCls), cla2 As TCls

        vtp.Add(cla1)
        cla2 = GetRegGenCla("IEnumerable", vtp)
        CopyGenClaAll()

        Return cla2
    End Function

    Public Function GetTListCla(cla1 As TCls) As TCls
        Dim vtp As New TList(Of TCls), cla2 As TCls

        vtp.Add(cla1)
        cla2 = GetRegGenCla("TList", vtp)
        CopyGenClaAll()

        Return cla2
    End Function

    ' ジェネリック型を変換する
    Public Function RepGenType(tp As TCls, dic As Dictionary(Of String, TCls)) As TCls
        Dim name1 As String, vtp As TList(Of TCls), tp1 As TCls = Nothing, tp2 As TCls, changed As Boolean = False, cla1 As TCls

        If tp.DimCla <> 0 Then
            ' 配列型の場合

            ' 配列型を返す
            Debug.Assert(tp.NameCla() = "Array" AndAlso tp.GenCla IsNot Nothing AndAlso tp.GenCla.Count = 1)

            tp1 = tp.GenCla(0)
            tp2 = RepGenType(tp1, dic)
            If tp2 Is tp1 Then
                Return tp
            Else
                Return GetArrCla(tp2, tp.DimCla)
            End If
        End If

        If dic.ContainsKey(tp.NameCla()) Then
            tp1 = dic(tp.NameCla())
            changed = True
            name1 = tp1.NameType()
        Else
            name1 = tp.NameType()
        End If

        If tp.GenCla Is Nothing Then
            ' ジェネリック型でない場合

            Debug.Assert(tp.DimCla = 0)
            If Not changed Then
                Return tp
            Else
                Return tp1
            End If
        Else
            ' ジェネリック型の場合

            vtp = New TList(Of TCls)()
            ' for Add Find
            For Each tp_f In tp.GenCla
                tp2 = RepGenType(tp_f, dic)
                If tp2 IsNot tp_f Then
                    changed = True
                End If
                vtp.Add(tp2)
            Next

            If changed Then
                ' 変換した場合

                ' ジェネリック型のクラスを得る
                cla1 = GetGenCla(name1, vtp)
                If cla1 Is Nothing Then
                    ' ない場合

                    cla1 = RegGenCla(name1, vtp)
                    CopyGenCla(cla1)
                End If

                Return cla1
            Else
                ' 変換しなかった場合

                Return tp
            End If
        End If
    End Function

    Public Function CopyVariable(var_src As TVar, dic As Dictionary(Of String, TCls)) As TVar
        Dim var1 As TVar

        If TypeOf var_src Is TFld Then
            var1 = New TFld()
        Else
            var1 = New TVar()
        End If

        var_src.CopyVarMem(var1)
        var1.TypeVar = RepGenType(var_src.TypeVar, dic)

        Return var1
    End Function


    Public Function CopyField(cla1 As TCls, fld_src As TFld, dic As Dictionary(Of String, TCls)) As TFld
        Dim fld1 As New TFld

        fld_src.CopyVarMem(fld1)

        fld1.ClaFld = cla1
        fld1.OrgFld = fld_src
        fld1.TypeVar = RepGenType(fld_src.TypeVar, dic)

        Return fld1
    End Function

    Public Function CopyFunction(cla1 As TCls, fnc_src As TFnc, dic As Dictionary(Of String, TCls)) As TFnc
        Dim fnc1 As New TFnc(fnc_src.NameVar, fnc_src.TypeVar)

        fnc_src.CopyFncMem(fnc1)

        fnc1.ClaFnc = cla1
        fnc1.OrgFnc = fnc_src
        fnc1.ArgFnc = New TList(Of TVar)(From var_f In fnc_src.ArgFnc Select CopyVariable(var_f, dic))

        If fnc_src.RetType Is Nothing Then
            fnc1.RetType = Nothing
        Else
            fnc1.RetType = TPrj.Prj.RepGenType(fnc_src.RetType, dic)
        End If

        Return fnc1
    End Function

    Public Sub CopyGenClaSub(cla1 As TCls, dic As Dictionary(Of String, TCls))
        If cla1 IsNot Nothing AndAlso cla1.OrgCla IsNot Nothing Then
            cla1.SuperCla = New TList(Of TCls)(From spr1 In cla1.OrgCla.SuperCla Select TPrj.Prj.RepGenType(spr1, dic))
            cla1.InterfacesCls = New TList(Of TCls)(From spr1 In cla1.OrgCla.InterfacesCls Select TPrj.Prj.RepGenType(spr1, dic))

            cla1.FldCla = New TList(Of TFld)(From fld1 In cla1.OrgCla.FldCla Select CopyField(cla1, fld1, dic))

            cla1.FncCla = New TList(Of TFnc)(From fnc1 In cla1.OrgCla.FncCla Select CopyFunction(cla1, fnc1, dic))
        End If
    End Sub

    Public Sub CopyGenCla(cla1 As TCls)
        Dim dic As Dictionary(Of String, TCls), i1 As Integer, dlg1 As TDelegate, dlg_org As TDelegate

        dic = New Dictionary(Of String, TCls)()
        ' for Each c In OrgCla.GenCla Add (c.NameCla, GenCla(@Idx)) To dic
        For i1 = 0 To cla1.OrgCla.GenCla.Count - 1
            dic.Add(cla1.OrgCla.GenCla(i1).NameCla(), cla1.GenCla(i1))
        Next

        CopyGenClaSub(cla1, dic)

        If TypeOf cla1 Is TDelegate Then
            dlg1 = CType(cla1, TDelegate)
            dlg_org = CType(dlg1.OrgCla, TDelegate)

            dlg1.ArgDlg = New TList(Of TVar)(From var_f In dlg_org.ArgDlg Select CopyVariable(var_f, dic))
            dlg1.RetDlg = TPrj.Prj.RepGenType(dlg_org.RetDlg, dic)
        End If
    End Sub

    Public Sub CopyGenCla2(cla1 As TCls)
        Dim dic As Dictionary(Of String, TCls)

        dic = New Dictionary(Of String, TCls)()
        ' for Each c In OrgCla.GenCla Add (c.NameCla, GenCla(@Idx)) To dic
        For Each cla2 In cla1.GenCla
            dic.Add(cla2.NameCla(), cla2)
        Next

        For Each fld In cla1.FldCla
            CopyGenClaSub(fld.TypeVar, dic)
        Next

        For Each fnc In cla1.FncCla
            CopyGenClaSub(fnc.RetType, dic)

            For Each var1 In fnc.ArgFnc
                CopyGenClaSub(var1.TypeVar, dic)
            Next
        Next
    End Sub

    Public Sub CopyGenClaAll()
        Dim gen_cla As TCls

        Do While vGenTmpCla.Count <> 0
            gen_cla = vGenTmpCla(0)
            vGenTmpCla.RemoveAt(0)

            ' CopyGenClaの中でvGenTmpClaが変化する
            CopyGenCla(gen_cla)
        Loop

    End Sub

    Public Function FindVariable(name1 As String, vvvar As TList(Of TList(Of TVar))) As TVar
        Dim cla1 As TCls = Nothing

        ' for Find
        For Each vvar In vvvar
            ' for Find
            For Each var1 In vvar
                If var1.NameVar = name1 Then
                    '  名前が一致する場合
                    ' Debug.WriteLine("変数定義:{0}", ref1.NameRef);
                    Return var1
                End If
            Next
        Next

        If dicGenCla.ContainsKey(name1) Then
            cla1 = dicGenCla(name1)
            Return cla1
        End If

        If dicCla.ContainsKey(name1) Then
            cla1 = dicCla(name1)
            Return cla1
        End If
        Return Nothing
    End Function

    Public Function CanCnvCla(dst_cla As TCls, src_trm As TTerm, src_cla As TCls) As Boolean
        Dim dst_dlg As TDelegate, src_dlg As TDelegate, i1 As Integer

        If src_trm IsNot Nothing AndAlso TypeOf src_trm Is TRef AndAlso CType(src_trm, TRef).NameRef = "Nothing" Then
            Return Not dst_cla.IsSubcla(DoubleType)
        End If

        If TypeOf dst_cla Is TDelegate Then
            If Not TypeOf src_cla Is TDelegate Then
                Return False
            End If

            dst_dlg = CType(dst_cla, TDelegate)
            src_dlg = CType(src_cla, TDelegate)

            If Not CanCnvCla(dst_dlg.RetDlg, Nothing, src_dlg.RetDlg) Then
                Return False
            End If

            If dst_dlg.ArgDlg.Count <> src_dlg.ArgDlg.Count Then
                Return False
            End If

            For i1 = 0 To dst_dlg.ArgDlg.Count - 1
                If Not CanCnvCla(dst_dlg.ArgDlg(i1).TypeVar, Nothing, src_dlg.ArgDlg(i1).TypeVar) Then
                    Return False
                End If
            Next

            Return True
        End If

        If dst_cla Is StringType AndAlso src_cla Is CharType Then
            Return True
        End If

        Return dst_cla Is src_cla OrElse dst_cla Is ObjectType OrElse src_cla.IsSubcla(dst_cla)
    End Function

    Public Function MatchFncArg(fnc1 As TFnc, varg As TList(Of TTerm)) As Boolean
        Dim i1 As Integer, param_array As Boolean, var1 As TVar, trm1 As TTerm, tp1 As TCls

        If varg Is Nothing Then
            Return True
        Else
            param_array = (fnc1.ArgFnc.Count <> 0 AndAlso fnc1.ArgFnc(fnc1.ArgFnc.Count - 1).ParamArrayVar)

            If fnc1.ArgFnc.Count = varg.Count OrElse param_array AndAlso fnc1.ArgFnc.Count <= varg.Count Then
                For i1 = 0 To fnc1.ArgFnc.Count - 1
                    If param_array AndAlso i1 = fnc1.ArgFnc.Count - 1 Then
                        Return True
                    End If

                    var1 = fnc1.ArgFnc(i1)
                    trm1 = varg(i1)
                    tp1 = GetTermType(trm1)
                    If var1.TypeVar Is Nothing Then
                    ElseIf tp1 Is Nothing Then
                    Else
                        If Not CanCnvCla(var1.TypeVar, trm1, tp1) Then
                            Return False
                        End If
                    End If
                Next
                Return True
            End If

            Return False
        End If
    End Function

    Public Function MatchFunction(fnc1 As TFnc, name1 As String, varg As TList(Of TTerm)) As Boolean
        Return fnc1.NameFnc() = name1 AndAlso Not fnc1.IsNew AndAlso Prj.MatchFncArg(fnc1, varg)
    End Function

    Public Shared Function FindFieldFunction(cla1 As TCls, name1 As String, varg As TList(Of TTerm)) As TVar
        Dim var1 As TVar

        ' for Find
        For Each fld2 In cla1.FldCla
            If fld2.NameVar = name1 Then
                Return fld2
            End If
        Next

        ' for Find
        For Each fnc2 In cla1.FncCla
            If Prj.MatchFunction(fnc2, name1, varg) Then
                Return fnc2
            End If
        Next

        ' for Find
        For Each cla_f In cla1.SuperCla
            var1 = FindFieldFunction(cla_f, name1, varg)
            If var1 IsNot Nothing Then
                Return var1
            End If
        Next

        ' for Find
        For Each cla_f In cla1.InterfacesCls
            var1 = FindFieldFunction(cla_f, name1, varg)
            If var1 IsNot Nothing Then
                Return var1
            End If
        Next

        Return Nothing
    End Function

    Public Shared Function FindFieldByName(cla1 As TCls, name1 As String) As TFld
        Dim fld1 As TFld

        ' for Find
        For Each fld2 In cla1.FldCla
            If fld2.NameVar = name1 Then
                Return fld2
            End If
        Next

        ' for Find
        For Each cla_f In cla1.SuperCla
            fld1 = FindFieldByName(cla_f, name1)
            If fld1 IsNot Nothing Then
                Return fld1
            End If
        Next

        Return Nothing
    End Function

    Public Function FindFunctionByName(class_name As String, fnc_name As String) As TFnc
        For Each cls1 In vCla
            For Each fnc1 In cls1.FncCla
                If cls1.NameCla() = class_name AndAlso fnc1.NameFnc() = fnc_name Then
                    Return fnc1
                End If
            Next
        Next

        Return Nothing
    End Function

    Public Function FindFieldByName(class_name As String, field_name As String) As TFld
        For Each cls1 In vCla
            For Each fld1 In cls1.FldCla
                If cls1.NameCla() = class_name AndAlso fld1.NameVar = field_name Then
                    Return fld1
                End If
            Next
        Next

        Return Nothing
    End Function

    Public Shared Function FindNew(cla1 As TCls, varg As TList(Of TTerm)) As TVar
        Dim var1 As TVar

        ' for Find
        For Each fnc2 In cla1.FncCla
            If fnc2.IsNew AndAlso Prj.MatchFncArg(fnc2, varg) Then
                Return fnc2
            End If
        Next

        ' for Find
        For Each cla_f In cla1.SuperCla
            var1 = FindNew(cla_f, varg)
            If var1 IsNot Nothing Then
                Return var1
            End If
        Next

        Return Nothing
    End Function

    Public Sub DumpClass(cla1 As TCls, sw As TStringWriter)
        Dim i As Integer

        sw.Write("{0} {1} {2} ", cla1.ToString(), cla1.FldCla.Count, cla1.FncCla.Count)
        If cla1.GenCla IsNot Nothing Then
            sw.Write("<")
            For i = 0 To cla1.GenCla.Count - 1
                If i <> 0 Then
                    sw.Write(",")
                End If
                sw.Write(cla1.GenCla(i).ToString())
            Next
            sw.Write(">")
        End If
        sw.WriteLine("")

        If cla1.SuperCla.Count <> 0 Then
            sw.WriteLine("  super:{0}", cla1.SuperCla(0).ToString())
        End If
        If cla1.InterfacesCls.Count <> 0 Then
            sw.Write("  impl:")
            For Each cla2 In cla1.InterfacesCls
                sw.Write(" {0}", cla2.ToString())
            Next
            sw.WriteLine("")
        End If

    End Sub

    Public Sub MakeInstanceClassInitializer()
        Dim idx As Integer, is_shared As Boolean, new_fnc As TFnc, ini_fnc As TFnc, asn1 As TAsn, call_ini As TCall, app_ini As TApp, dot1 As TDot, ref1 As TRef


        For Each cls1 In vCla
            Dim vnew = (From fnc1 In cls1.FncCla Where fnc1.IsNew).ToList()
            If vnew.Count = 0 Then

                new_fnc = New TFnc("Implicit@New", Nothing)

                new_fnc.ClaFnc = cls1
                new_fnc.ModVar = New TModifier()
                new_fnc.TypeFnc = ETkn.eNew
                new_fnc.ThisFnc = New TVar("Me", cls1)
                new_fnc.BlcFnc = New TBlc()
                new_fnc.IsNew = True

                cls1.FncCla.Add(new_fnc)
                vnew.Add(new_fnc)
            End If

            For idx = 0 To 1
                is_shared = (idx = 0)
                Dim vfld = (From fld1 In cls1.FldCla Where fld1.InitVar IsNot Nothing AndAlso fld1.ModVar.isShared = is_shared).ToList()
                If vfld.Count <> 0 Then

                    If is_shared Then
                        ini_fnc = New TFnc("Class@Initializer", Nothing)
                    Else
                        ini_fnc = New TFnc("Instance@Initializer", Nothing)
                    End If

                    ini_fnc.ClaFnc = cls1
                    ini_fnc.ModVar = New TModifier()
                    ini_fnc.TypeFnc = ETkn.eSub
                    ini_fnc.ThisFnc = New TVar("Me", cls1)
                    ini_fnc.BlcFnc = New TBlc()

                    For Each fld1 In vfld

                        ref1 = New TRef(fld1)
                        asn1 = New TAsn(ref1, fld1.InitVar)
                        ini_fnc.BlcFnc.AddStmtBlc(asn1)
                    Next

                    cls1.FncCla.Add(ini_fnc)

                    If is_shared Then

                        dot1 = New TDot(New TRef(cls1), ini_fnc)
                        app_ini = TApp.MakeAppCall(dot1)
                        call_ini = New TCall(app_ini)
                        call_ini.IsGenerated = True
                        theMain.BlcFnc.StmtBlc.Insert(0, call_ini)
                        call_ini.BlcStmt = theMain.BlcFnc

                        theMain.CallTo.Add(ini_fnc)
                        ini_fnc.CallFrom.Add(theMain)
                    Else

                        For Each new1 In vnew

                            app_ini = TApp.MakeAppCall(New TRef(ini_fnc))
                            call_ini = New TCall(app_ini)
                            call_ini.IsGenerated = True
                            new1.BlcFnc.StmtBlc.Insert(0, call_ini)
                            call_ini.BlcStmt = new1.BlcFnc

                            new1.CallTo.Add(ini_fnc)
                            ini_fnc.CallFrom.Add(new1)
                        Next
                    End If
                End If
            Next
        Next
    End Sub

    Public Sub MakeSrcPrj()
        SrcPrj = New TList(Of TSrc)(From fname In SrcFileNames Select New TSrc(fname))
    End Sub

    Public Sub Compile()
        Dim set_ref As TNavSetRef, set_ref_fnc As TNavSetRefFnc, set_var_ref As TNavSetVarRef
        Dim i1 As Integer, cla2 As TCls
        Dim set_call As TNavSetCall, nav_test As TNavTest, set_parent_stmt As TNavSetParentStmt, set_up_trm As TNavSetUpTrm

        ParsePrj = New TParseBas(Me)
        ' for ???
        For Each src_f In SrcPrj
            ParsePrj.ParseAllLines(src_f)
            ParsePrj.RegAllClass(src_f)
        Next

        For Each src_f In SrcPrj
            Debug.WriteLine("ソース:{0}", src_f.FileSrc)
            CurSrc = src_f
            ParsePrj.ClearParse()

            '            ParsePrj.ParseAllLines(src_f)
            ParsePrj.ReadAllStatement()
            CurSrc = Nothing
        Next

        ' for Call
        For Each src_f In SrcPrj
            CurSrc = src_f
            ParsePrj.Parse(src_f)
            CurSrc = Nothing
        Next

        For i1 = 0 To vArrCla.Count - 1
            cla2 = vArrCla(i1)
            If cla2.InterfacesCls.Count = 0 Then
                cla2.InterfacesCls.Add(GetIEnumerableCla(cla2.GenCla(0)))
            End If
        Next

        For Each cls1 In vCla
            For Each fnc1 In cls1.FncCla
                If cls1.NameCla() = MainClassName AndAlso fnc1.NameFnc() = MainFunctionName Then
                    theMain = fnc1
                End If
                If cls1.NameCla() = "Array" AndAlso fnc1.NameFnc() = "CreateInstance" Then
                    ArrayMaker = fnc1
                End If
            Next
        Next
        Debug.Assert(theMain IsNot Nothing)

        For Each cla1 In vCla
            If cla1.GenCla IsNot Nothing Then
                ' 総称型の場合

                CopyGenCla2(cla1)
            End If
        Next

        CopyGenClaAll()

        Dim sw As New TStringWriter
        For Each cla1 In vCla
            DumpClass(cla1, sw)
            cla1.SetAllSuperCla()
        Next
        sw.WriteLine("--------------------------------------------------------------------------------------------")
        For Each cla1 In vGenCla
            DumpClass(cla1, sw)
            cla1.SetAllSuperCla()
        Next
        TFile.WriteAllText("C:\usr\prj\MyIDE\MyAlgo\a.txt", sw.ToString())

        MakeInstanceClassInitializer()

        ' 変数参照を解決する
        set_ref = New TNavSetRef()
        set_ref.PrjSetRef = Me
        set_ref.NavPrj(Me, Nothing)
        Debug.Assert(Not set_ref.ErrNav)

        set_var_ref = New TNavSetVarRef()
        set_var_ref.NavPrj(Me, Nothing)
        Debug.Assert(set_ref.RefCnt = set_var_ref.RefCnt)

        set_call = New TNavSetCall()
        set_call.NavPrj(Me, Nothing)

        Debug.Assert(set_ref.RefCnt = set_call.RefCnt)

        ' 関数内の参照をセットする
        set_ref_fnc = New TNavSetRefFnc()
        set_ref_fnc.NavPrj(Me, Nothing)
        Debug.Assert(set_ref.RefCnt = set_ref_fnc.RefCnt)

        nav_test = New TNavTest()
        nav_test.NavPrj(Me, Nothing)
        Debug.Assert(set_ref.RefCnt = nav_test.RefCnt)


        For Each cla1 In vCla
            Debug.Assert(Not vGenCla.Contains(cla1))
        Next
        For Each cla1 In vGenCla
            Debug.Assert(Not vCla.Contains(cla1))
        Next
        vAllClass = New TList(Of TCls)(vCla)
        vAllClass.AddRange(vGenCla)

        ' サブクラスをセットする
        For Each cls1 In vAllClass
            For Each super_class In cls1.SuperCla
                super_class.SubClasses.Add(cls1)
            Next
        Next

        set_parent_stmt = New TNavSetParentStmt()
        set_parent_stmt.NavPrj(Me, Nothing)

        set_up_trm = New TNavSetUpTrm()
        set_up_trm.NavPrj(Me, Nothing)


        ' オーバーロード関数をセットする
        SetOvrFnc()

        ' 間接呼び出しをセットする
        SetCallAll()
    End Sub

    Public Sub MakeSrc()
        MakeAllBasicCode(OutDir + "\")
        Debug.WriteLine("Basic ソース 生成")

        MakeAllHtml(OutDir + "\")
        Debug.WriteLine("HTML 生成")
    End Sub

    Public Sub CodeAnalysis()

        ' 間接呼び出しをCallToAll.htmlに書く
        DmpCallToAll()

        ''''
        For Each cla1 In vCla
            For Each fld1 In cla1.FldCla
                If fld1.RefVar.Count = 0 Then
                    'Debug.WriteLine("未使用 {0}.{1} {2}", cla1.NameVar, fld1.NameVar, cla1.SrcCla.FileSrc)
                End If
            Next

            For Each fnc1 In cla1.FncCla
                If Not fnc1.Reachable Then
                    'Debug.WriteLine("未使用 {0}.{1} {2}", cla1.NameVar, fnc1.NameVar, cla1.SrcCla.FileSrc)
                End If
            Next
        Next
        ''''

        Debug.WriteLine("Build Bas 終了")

        MakePrjAllSrc()

    End Sub

    Public Function GetTermType(trm1 As TTerm) As TCls
        Dim ref1 As TRef
        Dim app1 As TApp
        Dim cns1 As TCns
        Dim fnc1 As TFnc
        Dim cla1 As TCls, cla2 As TCls
        Dim frm1 As TFrom

        If trm1 IsNot Nothing Then
            If TypeOf trm1 Is TCns Then
                cns1 = CType(trm1, TCns)
                Select Case cns1.TypeAtm
                    Case ETkn.eString
                        Return StringType
                    Case ETkn.eInt, ETkn.eHex
                        Return IntType
                    Case ETkn.eChar
                        Return CharType
                    Case Else
                        Debug.WriteLine("@h")
                        Return Nothing
                End Select
            ElseIf TypeOf trm1 Is TArray Then
            ElseIf TypeOf trm1 Is TRef Then
                ref1 = CType(trm1, TRef)
                If ref1.VarRef IsNot Nothing Then
                    If TypeOf ref1.VarRef Is TFnc Then
                        Return CType(ref1.VarRef, TFnc).RetType
                    ElseIf TypeOf ref1.VarRef Is TCls Then
                        Return CType(ref1.VarRef, TCls)
                    Else
                        Return ref1.VarRef.TypeVar
                    End If
                End If
            ElseIf trm1.IsApp() Then
                app1 = CType(trm1, TApp)
                Select Case app1.TypeApp
                    Case ETkn.eADD, ETkn.eMns, ETkn.eMUL, ETkn.eDIV, ETkn.eMOD
                        Return GetTermType(app1.ArgApp(0))
                    Case ETkn.eAppCall
                        If TypeOf app1.FncApp Is TRef Then
                            ref1 = CType(app1.FncApp, TRef)
                            If TypeOf ref1.VarRef Is TFnc Then
                                fnc1 = CType(ref1.VarRef, TFnc)
                                Return fnc1.RetType
                            Else
                                Select Case app1.KndApp
                                    Case EApp.eArrayApp
                                        Debug.Assert(ref1.VarRef.TypeVar.GenCla IsNot Nothing AndAlso ref1.VarRef.TypeVar.GenCla.Count = 1)
                                        Return ref1.VarRef.TypeVar.GenCla(0)
                                    Case EApp.eStringApp
                                        Return CharType
                                    Case EApp.eListApp
                                        Debug.Assert(ref1.VarRef.TypeVar.GenCla IsNot Nothing AndAlso ref1.VarRef.TypeVar.GenCla.Count = 1)
                                        Return ref1.VarRef.TypeVar.GenCla(0)
                                    Case EApp.eDictionaryApp
                                        Debug.Assert(ref1.VarRef.TypeVar.GenCla IsNot Nothing AndAlso ref1.VarRef.TypeVar.GenCla.Count = 2)
                                        Return ref1.VarRef.TypeVar.GenCla(1)
                                    Case Else
                                        Debug.Assert(False)
                                        Return Nothing
                                End Select
                            End If
                        End If
                        cla1 = GetTermType(app1.FncApp)
                        If cla1 Is StringType Then
                            Return CharType
                        End If
                        Return cla1
                    Case ETkn.eBaseCall
                        Return Nothing
                    Case ETkn.eBaseNew
                        Return Nothing
                    Case ETkn.eAs, ETkn.eCast
                        Return app1.ClassApp
                    Case ETkn.eQUE
                        Return GetTermType(app1.ArgApp(1))
                    Case ETkn.eTypeof
                        Return BoolType
                    Case ETkn.eNew
                        Return app1.NewApp

                    Case ETkn.eAddressOf
                        ref1 = CType(app1.ArgApp(0), TRef)
                        Debug.Assert(ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TFnc)
                        Return New TDelegate(CType(ref1.VarRef, TFnc))

                    Case ETkn.eGetType
                        Return TypeType

                    Case Else
                        Debug.WriteLine("Err Trm Src2:{0}", app1.TypeApp)
                        Debug.Assert(False)
                End Select
            ElseIf trm1.IsLog() Then
                If CType(trm1, TApp).IsRel() Then
                    Return BoolType
                End If
            ElseIf TypeOf trm1 Is TPar Then
                Return GetTermType(CType(trm1, TPar).TrmPar)
            ElseIf TypeOf trm1 Is TFrom Then

                frm1 = CType(trm1, TFrom)
                If frm1.SelFrom Is Nothing Then
                    cla1 = GetTermType(frm1.SeqFrom)
                    Debug.Assert(cla1 IsNot Nothing)
                    cla2 = ElementType(cla1)
                    Return GetIEnumerableCla(cla2)
                Else
                    cla1 = GetTermType(frm1.SelFrom)
                    Debug.Assert(cla1 IsNot Nothing)
                    Return GetIEnumerableCla(cla1)
                End If

            Else
                Debug.Assert(False)
            End If
        End If
        Return Nothing
    End Function

    Public Function ElementType(type1 As TCls) As TCls
        chk(type1 IsNot Nothing)
        If type1.DimCla <> 0 OrElse type1.NameType() = "List" OrElse type1.NameType() = "TList" OrElse type1.NameType() = "IEnumerable" Then
            Debug.Assert(type1.GenCla IsNot Nothing AndAlso type1.GenCla.Count = 1)
            Return type1.GenCla(0)
        ElseIf type1.NameType() = "Dictionary" Then
            Debug.Assert(type1.GenCla IsNot Nothing AndAlso type1.GenCla.Count = 2)
            Return type1.GenCla(0)
        ElseIf type1.NameType().ToLower() = "string" Then
            Return CharType
        ElseIf type1.NameType() = "IList" Then
            Return ObjectType
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Function GetOperatorFunction(type1 As ETkn, trm1 As TTerm) As TFnc
        Dim tp1 As TCls, name1 As String

        Select Case type1
            Case ETkn.eADD
                name1 = "+"
            Case ETkn.eMns
                name1 = "-"
            Case ETkn.eMUL
                name1 = "*"
            Case ETkn.eDIV
                name1 = "/"
            Case ETkn.eMOD
                name1 = "Mod"
            Case ETkn.eEq
                name1 = "="
            Case ETkn.eNE
                name1 = "<>"
            Case Else
                Return Nothing
        End Select

        ' 最初の引数の型を得る
        tp1 = GetTermType(trm1)
        Debug.Assert(tp1 IsNot Nothing)

        ' すべてのメソッドに対し
        ' for Find
        For Each fnc In tp1.FncCla

            If fnc.TypeFnc = ETkn.eOperator AndAlso fnc.NameFnc() = name1 Then
                ' 演算子オーバーロード関数で名前が同じ場合

                Return fnc
            End If
        Next

        Return Nothing
    End Function

    Sub Pop(vvvar As TList(Of TList(Of TVar)))
        vvvar.RemoveAt(vvvar.Count - 1)
    End Sub

    ' オーバーロードしているメソッド(OvrFnc)とオーバーロードされているメソッド(OvredFnc)をセットする
    Public Sub SetOvrFncSub(cla1 As TCls, fnc1 As TFnc)
        Dim i1 As Integer, tp1 As TCls, tp2 As TCls, all_eq As Boolean

        ' すべてのスーパークラスに対し
        For Each cla2 In cla1.SuperCla

            ' すべてのメソッドに対し
            For Each fnc2 In cla2.FncCla
                If fnc2.NameFnc() = fnc1.NameFnc() AndAlso fnc2.ArgFnc.Count = fnc1.ArgFnc.Count Then
                    ' 名前と引数の数が同じ場合

                    all_eq = True
                    For i1 = 0 To fnc1.ArgFnc.Count - 1
                        tp1 = fnc1.ArgFnc(i1).TypeVar
                        tp2 = fnc2.ArgFnc(i1).TypeVar
                        If tp1 IsNot tp2 Then
                            ' 引数の型が違う場合

                            Debug.WriteLine("引数の型が違う {0} {1} {2}", cla1.NameCla(), fnc1.NameFnc(), cla2.NameCla())
                            all_eq = False
                            Exit For
                        End If
                    Next

                    If all_eq Then
                        ' すべての引数の型が同じ場合

                        ' fnc1のオーバーロード関数を設定する
                        fnc1.OvrFnc = fnc2
                        fnc2.OvredFnc.Add(fnc1)
                        Exit Sub
                    End If
                End If
            Next

            If fnc1.OvrFnc Is Nothing Then
                ' cla2のメソッドにfnc1のオーバーロード関数がない場合

                ' cla2のスーパークラスの中でfnc1のオーバーロード関数を探す
                SetOvrFncSub(cla2, fnc1)
                If fnc1.OvrFnc IsNot Nothing Then
                    ' cla2のスーパークラスの中にfnc1のオーバーロード関数があった場合
                    Exit Sub
                End If
            End If
        Next
    End Sub

    ' fnc1をオーバーロードしているメソッド(OvredFnc)の子孫をセットする
    Public Sub SetEqOvredFncAll(fnc1 As TFnc, vfnc As TList(Of TFnc))
        For Each fnc2 In fnc1.OvredFnc
            vfnc.Add(fnc2)

            ' fnc2のオーバーロード関数の子孫をセットする
            SetEqOvredFncAll(fnc2, vfnc)
        Next
    End Sub

    ' オーバーロード関数をセットする
    Public Sub SetOvrFnc()
        '  すべてのクラスに対し
        For Each cla1 In vCla
            For Each fnc1 In cla1.FncCla
                If Not vAllFnc.Contains(fnc1) Then
                    vAllFnc.Add(fnc1)
                End If

                For Each fnc2 In fnc1.CallTo
                    If Not vAllFnc.Contains(fnc2) Then
                        vAllFnc.Add(fnc2)
                    End If
                Next
            Next
        Next

        For Each cla1 In vCla
            For Each fld1 In cla1.FldCla
                If vAllFld.Contains(fld1) Then
                    Debug.Assert(False)
                Else
                    vAllFld.Add(fld1)
                End If
            Next
        Next
        For Each cla1 In vGenCla
            For Each fld1 In cla1.FldCla
                If vAllFld.Contains(fld1) Then
                    Debug.Assert(False)
                Else
                    vAllFld.Add(fld1)
                End If
            Next
        Next

        '  すべてのクラスに対し
        For Each fnc1 In vAllFnc
            If fnc1.ModFnc().isOverride Then

                ' オーバーロードしているメソッド(OvrFnc)とオーバーロードされているメソッド(OvredFnc)をセットする
                SetOvrFncSub(fnc1.ClaFnc, fnc1)
                Debug.Assert(fnc1.OvrFnc IsNot Nothing)
            End If
        Next

        '  すべてのクラスに対し
        For Each fnc1 In vAllFnc
            Debug.Assert(fnc1.EqOvredFncAll.Count = 0)

            ' fnc1自身をEqOvredFncAllに追加する
            fnc1.EqOvredFncAll.Add(fnc1)

            ' fnc1をオーバーロードをしているメソッドの子孫をEqOvredFncAllに追加する
            SetEqOvredFncAll(fnc1, fnc1.EqOvredFncAll)
        Next
    End Sub

    ' 間接呼び出しをセットする
    Public Sub SetCallAll()
        Dim changed As Boolean, v As New TList(Of TFnc), i1 As Integer

        '  すべてのクラスに対し
        For Each cla1 In vCla

            '  すべてのメソッドに対し
            For Each fnc1 In cla1.FncCla

                Debug.Assert(fnc1.CallToAll.Count = 0)

                ' fnc1から呼んでいるすべてのメソッドに対し
                For Each fnc2 In fnc1.CallTo

                    ' fnc2とfnc2をオーバーロードをしているメソッドの子孫(EqOvredFncAll)をCallToAllにセットする
                    fnc1.CallToAll.AddRange(fnc2.EqOvredFncAll)
                Next

                For Each fnc2 In fnc1.CallToAll
                    'Debug.WriteLine("call to All {0} - {1}", fnc1.FullName(), fnc2.FullName())
                Next
            Next
        Next

        i1 = 1
        Do While True
            changed = False


            '  すべてのクラスに対し
            For Each cla1 In vCla

                '  すべてのメソッドに対し
                For Each fnc1 In cla1.FncCla

                    v.Clear()

                    ' fnc1から直接/間接に呼んでいるすべてのメソッドに対し
                    For Each fnc2 In fnc1.CallToAll

                        ' fnc2から直接/間接に呼んでいるすべてのメソッドに対し
                        For Each fnc3 In fnc2.CallToAll

                            If Not fnc1.CallToAll.Contains(fnc3) AndAlso Not v.Contains(fnc3) Then
                                v.Add(fnc3)
                            End If
                        Next
                    Next

                    If v.Count <> 0 Then
                        fnc1.CallToAll.AddRange(v)
                        changed = True
                    End If
                Next
            Next

            Debug.WriteLine("間接呼び出し {0}", i1.ToString())
            i1 = i1 + 1

            If Not changed Then
                Exit Do
            End If
        Loop

        '  すべてのクラスのすべてのメソッドに対し、Reachableをセットする
        For Each fnc1 In vAllFnc
            fnc1.Reachable = fnc1 Is theMain OrElse theMain.CallToAll.Contains(fnc1)
            If fnc1.OrgFnc IsNot Nothing Then
                fnc1.OrgFnc.Reachable = (fnc1.OrgFnc.Reachable OrElse fnc1.Reachable)
                Debug.Assert(fnc1.OrgFnc.OrgFnc Is Nothing)
            End If

            Debug.Assert(fnc1.ClaFnc IsNot Nothing)
            If fnc1.Reachable Then
                fnc1.ClaFnc.UsedVar = True
                If fnc1.OrgFnc IsNot Nothing Then
                    fnc1.OrgFnc.ClaFnc.UsedVar = True
                End If
            End If
        Next

        '  すべてのフィールドに対し
        For Each fld1 In vAllFld
            For Each ref1 In fld1.RefVar
                If ref1.FncRef.Reachable Then
                    fld1.UsedVar = True
                    If fld1.OrgFld IsNot Nothing Then
                        fld1.OrgFld.UsedVar = (fld1.OrgFld.UsedVar OrElse fld1.UsedVar)
                        Debug.Assert(fld1.OrgFld.OrgFld Is Nothing)
                    End If

                    Exit For
                End If
            Next
            Debug.Assert(fld1.ClaFld IsNot Nothing)
            If fld1.UsedVar Then
                fld1.ClaFld.UsedVar = True
                If fld1.OrgFld IsNot Nothing Then
                    fld1.OrgFld.ClaFld.UsedVar = True
                End If
            End If
        Next

        ' すべてのクラスに対し
        For Each cls1 In vAllClass
            If cls1.UsedVar Then
                For Each cls2 In cls1.AllSuperCla
                    cls2.UsedVar = True
                Next
            End If
        Next
    End Sub

    Sub FncHtml(sw As TStringWriter, fnc1 As TFnc)
        Dim fname As String

        If fnc1.ClaFnc IsNot Nothing AndAlso fnc1.ClaFnc.SrcCla IsNot Nothing Then
            fname = TPath.GetFileNameWithoutExtension(fnc1.ClaFnc.SrcCla.FileSrc)
            sw.Write(" <a href=""./{0}.html#var{1}"" >{2}</a>", fname, fnc1.IdxVar, fnc1.FullName())
        Else
            sw.Write(" {0}", fnc1.FullName())
        End If

    End Sub

    ' 間接呼び出しをCallToAll.htmlに書く
    Sub DmpCallToAll()
        Dim v1 As New TList(Of TFnc), v2 As New TList(Of TFnc), fnc1 As TFnc
        Dim sw As TStringWriter

        sw = New TStringWriter()
        sw.WriteLine(TCodeGenerator.HTMLHead)

        For Each cla1 In vCla
            For Each fnc1 In cla1.FncCla
                FncHtml(sw, fnc1)
                sw.Write("&nbsp; : オーバーロード&nbsp;")
                For Each fnc2 In fnc1.EqOvredFncAll
                    sw.Write("&nbsp;")
                    FncHtml(sw, fnc2)
                Next
                sw.WriteLine("<br/>")
            Next
        Next

        v1.Add(theMain)
        Do While v1.Count <> 0

            fnc1 = v1(0)
            v1.RemoveAt(0)

            v2.Add(fnc1)

            FncHtml(sw, fnc1)

            sw.Write("&nbsp; : 直接&nbsp;")
            sw.Write("[")
            For Each fnc2 In fnc1.CallTo
                sw.Write("&nbsp;")
                FncHtml(sw, fnc2)
            Next
            sw.Write("]")

            For Each fnc2 In fnc1.CallTo
                For Each fnc3 In fnc2.EqOvredFncAll
                    sw.Write("&nbsp;")
                    FncHtml(sw, fnc3)
                Next
            Next
            sw.WriteLine("<br/>")

            sw.Write("&nbsp; : 間接&nbsp;")
            For Each fnc2 In fnc1.CallToAll
                sw.Write("&nbsp;")
                FncHtml(sw, fnc2)

                If Not v2.Contains(fnc2) Then
                    v1.Add(fnc2)
                End If
            Next
            sw.WriteLine("<br/>")
        Loop

        sw.WriteLine("</body></html>")
        TFile.WriteAllText(OutDir + "\html\CallToAll.html", sw.ToString())
    End Sub

    ' 関数のノードをグラフに追加する
    Public Function AddFncGraph(dic1 As Dictionary(Of Object, TFlowNode), fnc1 As TFnc) As TFncNode
        Dim fncnd As TFncNode, fncnd2 As TFncNode, fnc2 As TFnc

        ' 関数のノードを作る。
        fncnd = New TFncNode(fnc1)

        ' 辞書に追加する。
        dic1.Add(fnc1, fncnd)

        ' fnc1がどこから呼ばれ得るかを調べる。
        ' fnc1とそのすべてのスーパークラスの関数についてループする。
        fnc2 = fnc1
        Do While fnc2 IsNot Nothing

            If fnc2 IsNot fnc1 AndAlso dic1.ContainsKey(fnc2) Then
                ' 辞書に登録済みの場合

                'Debug.WriteLine("処理済み {0}", fnc2.FullName())
            Else
                ' 辞書に登録済みでない場合

                ' すべての関数呼び出しに対し
                For Each ref2 In fnc2.RefVar

                    If ref2.FncRef.Reachable Then
                        ' 到達可能の関数内で参照されている場合

                        If dic1.ContainsKey(ref2.FncRef) Then
                            ' 関数が辞書にある場合

                            fncnd2 = dic1(ref2.FncRef)
                        Else
                            ' 関数が辞書にない場合

                            ' 関数のノードをグラフに追加する
                            fncnd2 = AddFncGraph(dic1, ref2.FncRef)
                        End If

                        ' 関数から関数への矢印を追加する
                        If Not fncnd2.ToNd.ContainsKey(fnc1) Then
                            fncnd2.ToNd.Add(fnc1, fncnd)
                        End If
                    End If
                Next
            End If

            ' コード上はfnc1のスーパークラスの関数fnc2の呼び出しでも、実際はfnc1が呼ばれる場合がある
            fnc2 = fnc2.OvrFnc
        Loop

        Return fncnd
    End Function

    ' 変数参照のノードをグラフに追加する
    Public Function AddRefGraph(dic1 As Dictionary(Of Object, TFlowNode), ref1 As TRef) As TRefNode
        Dim refnd As TRefNode, fncnd As TFncNode

        ' 変数参照のノードを作る。
        refnd = New TRefNode(ref1)

        ' 辞書に追加する。
        Debug.Assert(Not dic1.ContainsKey(ref1))
        dic1.Add(ref1, refnd)

        If dic1.ContainsKey(ref1.FncRef) Then
            ' 変数参照を含む関数のノードが辞書にある場合

            fncnd = CType(dic1(ref1.FncRef), TFncNode)
        Else
            ' 変数参照を含む関数のノードが辞書にない場合

            ' 関数のノードをグラフに追加する
            fncnd = AddFncGraph(dic1, ref1.FncRef)
        End If

        ' 関数から変数参照への矢印を追加する。
        If Not fncnd.ToNd.ContainsKey(ref1) Then

            fncnd.ToNd.Add(ref1, refnd)
        End If

        Return refnd
    End Function

    ' 変数参照のグラフを作る
    Public Sub MakeRefGraph()
        Dim dic1 As New Dictionary(Of Object, TFlowNode), vnd As TList(Of TNode), vfnc As List(Of TFnc)
        Dim dgr As TDrawGraph, dot_dir As String, dot_path As String, idx As Integer, def_ref As Boolean
        Dim sw As New TStringWriter, file_name As String

        dot_dir = OutDir + "\html\_dot"

        ' すべてのクラスに対し
        For Each cla1 In vCla

            If vCla.IndexOf(cla1) Mod 25 = 0 Then
                Debug.WriteLine("Make Ref Graph {0}", vCla.IndexOf(cla1))
            End If

            ' すべてのフィールドに対し
            For Each fld1 In cla1.FldCla

                If True OrElse cla1.NameVar = "TApp" AndAlso fld1.NameVar = "KndApp" Then

                    For idx = 0 To 1

                        def_ref = IIf(idx = 0, True, False)

                        ' ノードの辞書を初期化する
                        TFlowNode.CntNd = 0
                        dic1 = New Dictionary(Of Object, TFlowNode)()
                        vfnc = New List(Of TFnc)()

                        ' すべてのフィールド参照に対し
                        For Each ref1 In fld1.RefVar

                            If ref1.DefRef = def_ref AndAlso ref1.FncRef.Reachable Then
                                ' 到達可能の関数内で参照されている場合

                                If Not vfnc.Contains(ref1.FncRef) Then
                                    ' 同一関数内での変数参照が未処理の場合

                                    vfnc.Add(ref1.FncRef)

                                    ' 変数参照のノードをグラフに追加する
                                    AddRefGraph(dic1, ref1)
                                End If
                            End If
                        Next

                        ' ノードの集合からグラフを作る
                        vnd = TGraph.Node2Graph(New TList(Of TFlowNode)(dic1.Values))

                        dgr = New TDrawGraph(vnd)
                        TDrawGraph.CheckGraph(dgr.AllNode)

                        TDirectory.CreateDirectory(dot_dir)

                        file_name = GetHtmlFileName(cla1) + "_" + GetHtmlFileName(fld1) + "_" + IIf(def_ref, "define", "use")
                        dot_path = dot_dir + "\" + file_name + ".dot"

                        ' dotファイルに書く
                        TGraph.WriteDotFile("オリジナル", dgr.AllNode, Nothing, dot_path)

                        sw.WriteLine("dot -Tsvg {0}.dot -o {0}.svg", file_name)
                    Next
                End If
            Next
        Next

        TFile.WriteAllText(dot_dir + "\DotToSvg.bat", sw.ToString(), Encoding.GetEncoding("Shift_JIS"))
    End Sub

    Public Sub SetDicMemNameJava()
        Dim dic1 As Dictionary(Of String, String)

        dicMemName = New Dictionary(Of String, Dictionary(Of String, String))()

        dic1 = New Dictionary(Of String, String)()
        dic1.Add("Length", "length")
        dic1.Add("Clone", "clone")
        dicMemName.Add("Array", dic1)

        dic1 = New Dictionary(Of String, String)()
        dic1.Add("Add", "add")
        dic1.Add("AddRange", "addAll")
        dic1.Add("Contains", "contains")
        dic1.Add("Count", "size()")
        dic1.Add("IndexOf", "indexOf")
        dic1.Add("Insert", "add")
        dic1.Add("Remove", "remove")
        dic1.Add("RemoveAt", "remove")
        dic1.Add("Clear", "clear")
        dicMemName.Add("List", dic1)

        dic1 = New Dictionary(Of String, String)()
        dic1.Add("Count", "size()")
        dic1.Add("Peek", "peek")
        dic1.Add("Pop", "pop")
        dic1.Add("Push", "push")
        dicMemName.Add("Stack", dic1)

        dic1 = New Dictionary(Of String, String)()
        dic1.Add("Clear", "clear")
        dic1.Add("ContainsKey", "containsKey")
        dic1.Add("Count", "length()")
        dic1.Add("Length", "length()")
        dic1.Add("IndexOf", "indexOf")
        dic1.Add("Replace", "replace")
        dic1.Add("Substring", "substring")
        dic1.Add("ToLower", "toLowerCase")
        dicMemName.Add("String", dic1)

        dic1 = New Dictionary(Of String, String)()
        dic1.Add("ContainsKey", "containsKey")
        dic1.Add("Add", "put")
        dic1.Add("Keys", "keySet()")
        dic1.Add("Clear", "clear")
        dic1.Add("Remove", "remove")
        dic1.Add("Values", "values()")
        dicMemName.Add("Dictionary", dic1)

        dic1 = New Dictionary(Of String, String)()
        dic1.Add("Abs", "abs")
        dic1.Add("Ceiling", "ceil")
        dic1.Add("Floor", "floor")
        dic1.Add("Max", "max")
        dic1.Add("Min", "min")
        dic1.Add("Round", "round")
        dic1.Add("Sqrt", "sqrt")
        dicMemName.Add("Math", dic1)

        dicClassMemName = New Dictionary(Of String, String)()
        dicClassMemName.Add("Double.NaN", "Double.NaN")
        dicClassMemName.Add("Double.IsNaN", "Double.isNaN")
        dicClassMemName.Add("Double.MaxValue", "Double.MAX_VALUE")
        dicClassMemName.Add("Double.MinValue", "Double.MIN_VALUE")
        dicClassMemName.Add("Char.IsDigit", "Character.isDigit")
        dicClassMemName.Add("Char.IsWhiteSpace", "Character.isWhitespace")
        dicClassMemName.Add("Char.IsLetter", "Character.isLetter")
        dicClassMemName.Add("Char.IsLetterOrDigit", "Character.isLetterOrDigit")
        dicClassMemName.Add("Integer.Parse", "Integer.parseInt")
        '        dicClassMemName.Add("", "")
    End Sub

    ' Basicのソースを作る
    Public Sub MakeAllBasicCode(out_dir As String)
        '  すべてのソースに対し
        For Each src_f In SrcPrj
            src_f.FigSrc = New TBasicCodeGenerator(Me)
            CurSrc = src_f
            src_f.FigSrc.MakeBasicSrc(src_f)
            src_f.FigSrc.OutputBasicSrc(src_f, out_dir)
            CurSrc = Nothing
        Next
    End Sub

    Public Function WriteInheritanceHierarchy(class_sw As StringWriter, cls1 As TCls) As Integer
        Dim indent As Integer

        If cls1.SuperCla.Count <> 0 Then
            indent = WriteInheritanceHierarchy(class_sw, cls1.SuperCla(0))
        Else
            indent = 0
        End If

        class_sw.WriteLine("<p style=""text-indent:{0}em""><a href=""../{1}/{1}.html"">{1}</a></p>", indent * 2, cls1.NameVar)

        Return indent + 1
    End Function

    Public Sub MakeAllHtml(out_dir As String)
        Dim all_class_dir As String, class_dir As String, paht1 As String, sw As TStringWriter, index_sw As New StringWriter, class_sw As StringWriter
        Dim indent As Integer, com_str As String, fname As String, html_file_name As String, html_class_file_name As String, svg_file_name As String
        Dim vfnc As List(Of TFnc), idx As Integer, def_ref As Boolean, vref As List(Of TRef), vfld As TList(Of TFld), fld2 As TFld

        all_class_dir = out_dir + "html"
        index_sw.WriteLine(TCodeGenerator.HTMLHead)
        index_sw.WriteLine("<ul>")

        '  すべてのソースに対し
        For Each src1 In SrcPrj
            src1.FigSrc = New TBasicCodeGenerator(Me)

            ' ソース内のすべてのクラスに対し
            For Each cls1 In src1.ClaSrc
                If cls1.GenCla Is Nothing Then
                    ' 総称型でない場合

                    html_class_file_name = GetHtmlFileName(cls1)
                    index_sw.WriteLine(vbTab + "<li><a href=""{0}/{0}.html"">{1}</a></li>", html_class_file_name, cls1.NameCla())

                    class_dir = all_class_dir + "\" + html_class_file_name
                    TDirectory.CreateDirectory(class_dir)

                    class_sw = New StringWriter()

                    class_sw.WriteLine(TCodeGenerator.HTMLHead)
                    class_sw.WriteLine("<h1>{0} クラス</h1>", cls1.NameVar)

                    class_sw.WriteLine("<h2>継承階層</h2>")
                    If cls1.SuperCla.Count <> 0 Then

                        indent = WriteInheritanceHierarchy(class_sw, cls1.SuperCla(0))
                    End If

                    class_sw.WriteLine("<p style=""text-indent:{0}em"">{1}</p>", indent * 2, cls1.NameVar)

                    For Each sub_class In cls1.SubClasses
                        If vCla.Contains(sub_class) Then
                            class_sw.WriteLine("<p style=""text-indent:{0}em""><a href=""../{1}/{1}.html"">{1}</a></p>", (indent + 1) * 2, sub_class.NameVar)
                        End If
                    Next

                    Select Case cls1.KndCla
                        Case EClass.eEnumCla
                        Case EClass.eDelegateCla
                        Case Else

                            class_sw.WriteLine("<h2>フィールド</h2>")

                            class_sw.WriteLine("<table border=""1""><thead><tr><th>名前</th><th>説明</th></tr></thead><tbody>")

                            ' クラス内のすべてのフィールドに対し
                            For Each fld1 In cls1.FldCla
                                html_file_name = GetHtmlFileName(fld1)
                                class_sw.WriteLine("<tr>")
                                class_sw.WriteLine("<td><a href=""{0}.html"">{1}</a></td><td>{2}</td>", html_file_name, fld1.NameVar, fld1.TailCom)
                                class_sw.WriteLine("</tr>")

                                paht1 = class_dir + "\" + html_file_name + ".html"
                                sw = New TStringWriter()

                                sw.WriteLine(TCodeGenerator.HTMLHead)
                                sw.WriteLine("<h2>{0} フィールド</h2>", fld1.NameVar)

                                ' フィールドに値を代入している関数とフィールドの値を参照している関数
                                For idx = 0 To 1
                                    def_ref = IIf(idx = 0, True, False)
                                    sw.WriteLine("<h2>{0}</h2>", IIf(def_ref, "このフィールドに値を代入している関数", "このフィールドの値を参照している関数"))

                                    vref = New TList(Of TRef)(From ref1 In fld1.RefVar Where ref1.DefRef = def_ref)
                                    If vref.Count = 0 Then
                                        sw.WriteLine("<h3>なし</h3>")
                                    Else
                                        vfnc = New List(Of TFnc)()
                                        sw.WriteLine("<ul>")
                                        For Each ref1 In vref
                                            If Not vfnc.Contains(ref1.FncRef) Then
                                                ' 未処理の場合

                                                vfnc.Add(ref1.FncRef)
                                                sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(ref1.FncRef.GetClassVar()), GetHtmlFileName(ref1.FncRef), ref1.FncRef.FullName())
                                            End If
                                        Next
                                        sw.WriteLine("</ul>")
                                    End If
                                Next

                                If fld1.ClaFld IsNot Nothing AndAlso fld1.ClaFld.SrcCla IsNot Nothing Then

                                    fname = TPath.GetFileNameWithoutExtension(fld1.ClaFld.SrcCla.FileSrc)
                                    sw.WriteLine(" <a href=""../_src/{0}.html#var{1}"" >ソース</a><br/>", fname, fld1.IdxVar)
                                End If

                                svg_file_name = GetHtmlFileName(cls1) + "_" + GetHtmlFileName(fld1)
                                sw.WriteLine("<a href=""../_dot/{0}_define.svg"" >値の代入への経路</a><br/>", svg_file_name)
                                sw.WriteLine("<a href=""../_dot/{0}_use.svg"" >値の使用への経路</a><br/>", svg_file_name)

                                sw.WriteLine("</body></html>")

                                TFile.WriteAllText(paht1, sw.ToString())
                            Next

                            class_sw.WriteLine("</tbody></table>")

                            class_sw.WriteLine("<h2>関数</h2>")
                            class_sw.WriteLine("<table border=""1""><thead><tr><th>名前</th><th>説明</th></tr></thead><tbody>")

                            ' クラス内のすべての関数に対し
                            For Each fnc1 In cls1.FncCla
                                If fnc1.ComVar IsNot Nothing Then
                                    com_str = fnc1.ComVar.GetFirstLine()
                                Else
                                    com_str = ""
                                End If

                                html_file_name = GetHtmlFileName(fnc1)

                                class_sw.WriteLine("<tr>")
                                class_sw.WriteLine("<td><a href=""{0}.html"">{1}</a></td><td>{2}</td>", html_file_name, fnc1.NameVar, com_str)
                                class_sw.WriteLine("</tr>")

                                paht1 = class_dir + "\" + html_file_name + ".html"

                                sw = New TStringWriter()
                                sw.WriteLine(TCodeGenerator.HTMLHead)
                                sw.WriteLine("<h1>{0} 関数</h1>", fnc1.NameVar)

                                If fnc1.ComVar IsNot Nothing Then

                                    sw.WriteLine("<h2>説明</h2>")
                                    For Each s In fnc1.ComVar.LineCom
                                        sw.WriteLine("{0}<br/>", s)
                                    Next
                                End If

                                sw.WriteLine("<h2>この関数から呼ぶ関数</h2>")
                                If fnc1.CallTo.Count = 0 Then
                                    sw.WriteLine("<h3>なし</h3>")
                                Else
                                    sw.WriteLine("<ul>")
                                    For Each fnc2 In fnc1.CallTo
                                        sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(fnc2.GetClassVar()), GetHtmlFileName(fnc2), fnc2.FullName())
                                    Next
                                    sw.WriteLine("</ul>")
                                End If

                                sw.WriteLine("<h2>この関数を呼ぶ関数</h2>")
                                If fnc1.CallFrom.Count = 0 Then
                                    sw.WriteLine("<h3>なし</h3>")
                                Else
                                    sw.WriteLine("<ul>")
                                    For Each fnc2 In fnc1.CallFrom
                                        sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(fnc2.GetClassVar()), GetHtmlFileName(fnc2), fnc2.FullName())
                                    Next
                                    sw.WriteLine("</ul>")
                                End If

                                ' 関数が値を代入しているフィールドと、関数が値を参照しているフィールド
                                For idx = 0 To 1
                                    def_ref = IIf(idx = 0, True, False)
                                    sw.WriteLine("<h2>{0}</h2>", IIf(def_ref, "この関数が値を代入しているフィールド", "この関数が値を参照しているフィールド"))

                                    vref = New TList(Of TRef)(From ref1 In fnc1.RefFnc Where ref1.DefRef = def_ref AndAlso ref1.VarRef IsNot Nothing AndAlso TypeOf ref1.VarRef Is TFld)
                                    If vref.Count = 0 Then
                                        sw.WriteLine("<h3>なし</h3>")
                                    Else
                                        vfld = New TList(Of TFld)()
                                        sw.WriteLine("<ul>")
                                        For Each ref1 In vref
                                            fld2 = CType(ref1.VarRef, TFld)
                                            If Not vfld.Contains(fld2) Then
                                                ' 未処理の場合

                                                vfld.Add(fld2)
                                                sw.Write("<li><a href=""../{0}/{1}.html"" >{2}</a></li>", GetHtmlFileName(fld2.ClaFld), GetHtmlFileName(fld2), fld2.ToString())
                                            End If
                                        Next
                                        sw.WriteLine("</ul>")
                                    End If
                                Next

                                If fnc1.ClaFnc IsNot Nothing AndAlso fnc1.ClaFnc.SrcCla IsNot Nothing Then

                                    fname = TPath.GetFileNameWithoutExtension(fnc1.ClaFnc.SrcCla.FileSrc)
                                    sw.WriteLine(" <a href=""../_src/{0}.html#var{1}"" >ソース</a>", fname, fnc1.IdxVar)
                                End If

                                sw.WriteLine("</body></html>")

                                TFile.WriteAllText(paht1, sw.ToString())
                            Next

                            class_sw.WriteLine("</tbody></table>")
                    End Select

                    class_sw.WriteLine("</body></html>")
                    TFile.WriteAllText(class_dir + "\" + html_class_file_name + ".html", class_sw.ToString())
                End If
            Next
        Next

        index_sw.WriteLine("</ul>")
        index_sw.WriteLine("</body></html>")

        TFile.WriteAllText(all_class_dir + "\class_index.html", index_sw.ToString())
    End Sub

    Public Sub MakePrjAllSrc()
        Dim out_dir As String
        Dim java_code As TJavaCodeGenerator, sw As TStringWriter
        Dim dic1 As Dictionary(Of String, ETkn), vtkn_name As Dictionary(Of ETkn, String)
        Dim fname As String

        out_dir = OutDir + "\"

        '-------------------------------------------------- Javaのソースを作る
        dic1 = TJavaCodeGenerator.TknAddJava()
        vtkn_name = TJavaCodeGenerator.ETknToStringDic(dic1)
        SetDicMemNameJava()

        sw = New TStringWriter()
        sw.WriteLine("package info.skyrecipe.kitchencabinet;")
        sw.WriteLine("import java.util.List;")
        sw.WriteLine("import java.util.ArrayList;")
        sw.WriteLine("import java.util.Stack;")
        sw.WriteLine("import java.util.HashMap;")
        sw.WriteLine("import java.io.StringWriter;")
        sw.WriteLine("import java.lang.reflect.Type;")
        sw.WriteLine("public class TTest {}")
        ' for ???
        For Each src_f In SrcPrj
            '  すべてのソースに対し

            fname = TPath.GetFileName(src_f.FileSrc)
            If fname <> "@lib.vb" AndAlso fname <> "Sys.vb" AndAlso fname <> "WindowsForms.vb" Then

                java_code = New TJavaCodeGenerator(Me)
                java_code.vTknNameMK = vtkn_name
                java_code.MakeJavaSrc(src_f, out_dir, sw)

                ' for ???
                For Each del In java_code.vDelegate
                    java_code.MakeDelegateClassJava(del, sw)
                Next
            End If
        Next
        TFile.WriteAllText("C:\usr\prj\workspace\KitchenCabinet\src\info\skyrecipe\kitchencabinet\TTest.java", sw.ToString())

        dicMemName = Nothing
        dicClassMemName = Nothing
    End Sub

    Public Shared Function TrmStr(fm As TBasicCodeGenerator, trm1 As TTerm) As String
        Dim s As String

        fm.ClearFM()
        fm.TrmSrc(trm1)
        fm.NL()
        s = fm.MakeSrcText()

        Return s.Substring(0, s.IndexOf(vbCr))
    End Function

    Public Shared Function HexString(s As String) As String
        Dim sw As New StringWriter

        For Each c In s
            If AscW(c) < 256 Then
                sw.Write(c)
            Else
                sw.Write(String.Format("_{0:X4}", AscW(c)))
            End If
        Next

        Return sw.ToString()
    End Function

    Public Shared Function GetHtmlFileName(cls_fld_fnc As TVar) As String
        If TypeOf cls_fld_fnc Is TCls OrElse TypeOf cls_fld_fnc Is TFld Then
            Return HexString(cls_fld_fnc.NameVar)
        ElseIf TypeOf cls_fld_fnc Is TFnc Then
            Return cls_fld_fnc.IdxVar.ToString()
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Sub CheckRefVar(ref1 As TRef)
        Dim fname As String

        If ref1.VarRef Is Nothing Then
            Debug.Assert(CurSrc IsNot Nothing)
            fname = TPath.GetFileNameWithoutExtension(CurSrc.FileSrc)
            Debug.Assert(fname = "@lib" OrElse fname = "System")
        End If
    End Sub
End Class

Public Class TDelegatePair
    Public ClaDel As TCls
    Public FncDel As TFnc

    Public Sub New(cla1 As TCls, fnc1 As TFnc)
        ClaDel = cla1
        FncDel = fnc1
    End Sub
End Class


Public Class TFlowNode
    Public Shared CntNd As Integer
    Public IdxNd As Integer
    Public ToNd As New Dictionary(Of Object, TFlowNode)
End Class


Public Class TRefNode
    Inherits TFlowNode
    Public RefNode As TRef

    Public Sub New(ref1 As TRef)
        CntNd = CntNd + 1
        IdxNd = CntNd
        RefNode = ref1
    End Sub
End Class

Public Class TFncNode
    Inherits TFlowNode
    Public FncNode As TFnc

    Public Sub New(fnc1 As TFnc)
        CntNd = CntNd + 1
        IdxNd = CntNd
        FncNode = fnc1
    End Sub
End Class