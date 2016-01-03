Imports System.IO
Imports System.Xml.Serialization
Imports System.Text
Imports System.Diagnostics

Public Class _Weak
    Inherits Attribute

    Public Sub New()
    End Sub
End Class

Public Class _Invariant
    Inherits Attribute

    Public Sub New()
    End Sub
End Class

Public Class _Parent
    Inherits Attribute

    Public Sub New()
    End Sub
End Class

Public Class TLibrary
    Public LibraryDirectory As String
    Public SourceFileNameList As String()
End Class

' -------------------------------------------------------------------------------- TProject
Public Class TProject
    <XmlIgnoreAttribute(), _Weak()> Public Shared Prj As TProject

    Public Language As ELanguage = ELanguage.Basic
    Public OutputDirectory As String
    Public MainClassName As String
    Public MainFunctionName As String
    Public OutputNotUsed As Boolean = True
    Public UseReferenceGraph As Boolean = False
    Public ClassNameTablePath As String = ""
    Public Dataflow As Boolean = False

    <_Weak()> Public OutputLanguageList As New List(Of ELanguage)
    <_Weak()> Public LibraryList As TLibrary()

    <XmlIgnoreAttribute()> Public SimpleParameterizedClassList As New TList(Of TClass)

    <XmlIgnoreAttribute(), _Weak()> Public ApplicationClassList As TList(Of TClass)
    <XmlIgnoreAttribute(), _Weak()> Public SrcPrj As New TList(Of TSourceFile)
    <XmlIgnoreAttribute(), _Weak()> Public ClassNameTable As Dictionary(Of String, String)
    <XmlIgnoreAttribute(), _Weak()> Public SpecializedClassList As New TList(Of TClass)
    <XmlIgnoreAttribute(), _Weak()> Public PendingSpecializedClassList As New TList(Of TClass)
    <XmlIgnoreAttribute(), _Weak()> Public SimpleParameterizedSpecializedClassList As New TList(Of TClass)
    <XmlIgnoreAttribute(), _Weak()> Public SimpleFieldList As List(Of TField)    ' 単純クラスのフィールドのリスト
    <XmlIgnoreAttribute(), _Weak()> Public vAllFnc As New TList(Of TFunction)
    <XmlIgnoreAttribute(), _Weak()> Public vAllFld As New TList(Of TField)    ' すべてのフィールド
    <XmlIgnoreAttribute(), _Weak()> Public CurSrc As TSourceFile ' 現在のソース
    <XmlIgnoreAttribute(), _Weak()> Public TypeType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public SystemType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public BoolType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public ObjectType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public DoubleType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public CharType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public IntType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public StringType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public WaitHandleType As TClass
    <XmlIgnoreAttribute(), _Weak()> Public MainClass As TClass
    <XmlIgnoreAttribute(), _Weak()> Public SimpleParameterizedClassTable As New Dictionary(Of String, TClass) ' クラス辞書
    <XmlIgnoreAttribute(), _Weak()> Public dicGenCla As New Dictionary(Of String, TClass)
    <XmlIgnoreAttribute(), _Weak()> Public dicCmpCla As New Dictionary(Of TClass, TList(Of TClass))
    <XmlIgnoreAttribute(), _Weak()> Public dicArrCla As New Dictionary(Of TClass, TList(Of TClass))
    <XmlIgnoreAttribute(), _Weak()> Public dicMemName As Dictionary(Of String, Dictionary(Of String, String))
    <XmlIgnoreAttribute(), _Weak()> Public dicClassMemName As Dictionary(Of String, String)
    <XmlIgnoreAttribute(), _Weak()> Public ParsePrj As TSourceParser
    <XmlIgnoreAttribute(), _Weak()> Public theMain As TFunction
    <XmlIgnoreAttribute(), _Weak()> Public ArrayMaker As TFunction

    Public Sub New()
        Prj = CType(Me, TProject)
    End Sub

    Public Sub OutputSourceFile()
        For Each lang In OutputLanguageList
            Debug.WriteLine("ソース 生成 {0} --------------------------------------------------------------------------------------------", lang)
            Select Case lang
                Case ELanguage.Basic
                    Dim basic_parser As New TBasicParser(Me)
                    MakeAllSourceCode(basic_parser)

                Case ELanguage.JavaScript, ELanguage.TypeScript
                    Dim script_parser As New TScriptParser(Me, lang)
                    MakeAllSourceCode(script_parser)

                Case Else
                    Debug.Assert(False)
            End Select
        Next

    End Sub

    Public Sub chk(b As Boolean)
        If Not b Then
            Debug.Assert(False)
        End If
    End Sub

    Public Function GetCla(name1 As String) As TClass
        Dim cla1 As TClass = Nothing

        If dicGenCla.ContainsKey(name1) Then
            cla1 = dicGenCla(name1)
            Return cla1
        End If

        If SimpleParameterizedClassTable.ContainsKey(name1) Then
            cla1 = SimpleParameterizedClassTable(name1)
            Return cla1
        Else
            Return Nothing
        End If
    End Function

    Public Function RegCla(name1 As String) As TClass
        Dim cla1 As TClass

        Debug.Assert(GetCla(name1) Is Nothing)

        cla1 = New TClass(Me, Nothing, name1)
        SimpleParameterizedClassList.Add(cla1)
        SimpleParameterizedClassTable.Add(cla1.NameCla(), cla1)

        Return cla1
    End Function

    Public Function GetDelegate(name1 As String) As TDelegate
        Dim cla1 As TDelegate

        cla1 = CType(GetCla(name1), TDelegate)
        Debug.Assert(cla1 IsNot Nothing)

        Return cla1
    End Function

    Public Function GetSpecializedClass(name1 As String, vtp As TList(Of TClass)) As TClass
        Dim cla1 As TClass, v As TList(Of TClass) = Nothing

        cla1 = GetCla(name1)
        Debug.Assert(cla1 IsNot Nothing AndAlso cla1.GenCla IsNot Nothing AndAlso cla1.GenCla.Count = vtp.Count AndAlso cla1.GenericType = EGeneric.ParameterizedClass)

        If dicCmpCla.ContainsKey(cla1) Then
            v = dicCmpCla(cla1)
            ' for Find
            For Each cla2 In v
                Debug.Assert(cla2.GenericType = EGeneric.SpecializedClass)

                ' 一致しない引数があるか調べる。
                Dim vidx = From idx In TNaviUp.IndexList(cla2.GenCla) Where cla2.GenCla(idx) IsNot vtp(idx)
                If Not vidx.Any() Then
                    ' すべて一致する場合

                    Return cla2
                End If
            Next
        Else
            Debug.Print("")
        End If

        Return Nothing
    End Function

    ' 仮引数クラスを含む場合はtrue
    Public Function ContainsArgumentClass(cla1 As TClass) As Boolean
        Select Case cla1.GenericType
            Case EGeneric.SimpleClass
                Return False
            Case EGeneric.ParameterizedClass
                Return True
            Case EGeneric.ArgumentClass
                Return True
            Case EGeneric.SpecializedClass
                For Each cla2 In cla1.GenCla
                    If ContainsArgumentClass(cla2) Then
                        Return True
                    End If
                Next
                Return False
            Case Else
                Debug.Assert(False)
                Return False
        End Select
    End Function

    ' ジェネリック型のクラスを作る
    Public Function AddSpecializedClass(name1 As String, vtp As TList(Of TClass), dim_len As Integer) As TClass
        Dim cla1 As TClass, cla3 As TClass, v As TList(Of TClass) = Nothing

        cla1 = GetCla(name1)
        Debug.Assert(cla1 IsNot Nothing AndAlso cla1.GenCla IsNot Nothing AndAlso cla1.GenCla.Count = vtp.Count)

        Debug.Assert(dicCmpCla.ContainsKey(cla1))
        v = dicCmpCla(cla1)

        ' 新しくジェネリック型のクラスを作る
        If TypeOf cla1 Is TDelegate Then
            cla3 = New TDelegate(Me, cla1.NameCla())
        Else
            cla3 = New TClass(Me, cla1.NameCla())
        End If
        cla3.KndCla = cla1.KndCla
        cla3.GenericType = EGeneric.SpecializedClass
        cla3.OrgCla = cla1
        cla3.DimCla = dim_len

        cla3.GenCla = New TList(Of TClass)(From tp In vtp)
        For Each tp In vtp
            Debug.Assert(tp IsNot Nothing)
        Next

        v.Add(cla3)

        If cla3.GenCla(0).IsParamCla Then
            'Debug.Print("")
        Else

            If cla3.GenCla(0).NameCla() = "T" Then
                Debug.WriteLine("@@@@@@@@@@@@@@")
            End If
            For Each cla4 In SpecializedClassList
                Debug.Assert(cla4.LongName() <> cla3.LongName())
            Next
            SpecializedClassList.Add(cla3)
            If cla1.Parsed AndAlso Not ContainsArgumentClass(cla3) Then
                SetMemberOfSpecializedClass(cla3)
            Else
                PendingSpecializedClassList.Add(cla3)
            End If
        End If

        Return cla3
    End Function

    Public Function GetAddSpecializedClass(name1 As String, vtp As TList(Of TClass)) As TClass
        Dim cla1 As TClass

        cla1 = GetSpecializedClass(name1, vtp)
        If cla1 IsNot Nothing Then
            Return cla1
        End If

        cla1 = AddSpecializedClass(name1, vtp, 0)
        Return cla1
    End Function

    '  配列型を得る
    Public Function GetArrCla(cla1 As TClass, dim_cnt As Integer) As TClass
        Dim cla3 As TClass, v As TList(Of TClass) = Nothing

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
            v = New TList(Of TClass)()
            dicArrCla.Add(cla1, v)
        End If

        '  新たに型を作る
        Dim vtp As New TList(Of TClass)
        vtp.Add(cla1)
        cla3 = AddSpecializedClass("Array", vtp, dim_cnt)
        v.Add(cla3)

        Return cla3
    End Function

    Public Function GetIEnumerableClass(cla1 As TClass) As TClass
        Dim vtp As New TList(Of TClass), cla2 As TClass

        vtp.Add(cla1)
        cla2 = GetAddSpecializedClass("IEnumerable", vtp)

        Return cla2
    End Function

    ' ジェネリック型を変換する
    Public Function SubstituteArgumentClass(tp As TClass, dic As Dictionary(Of String, TClass)) As TClass
        Dim name1 As String, vtp As TList(Of TClass), tp1 As TClass = Nothing, tp2 As TClass, changed As Boolean = False, cla1 As TClass

        If tp.GenericType = EGeneric.ArgumentClass AndAlso Not dic.ContainsKey(tp.NameCla()) Then
            Debug.Print("")
        End If

        If tp.DimCla <> 0 Then
            ' 配列型の場合

            ' 配列型を返す
            Debug.Assert(tp.NameCla() = "Array" AndAlso tp.GenCla IsNot Nothing AndAlso tp.GenCla.Count = 1)

            tp1 = tp.GenCla(0)
            tp2 = SubstituteArgumentClass(tp1, dic)
            If tp2 Is tp1 Then
                Return tp
            Else
                Return GetArrCla(tp2, tp.DimCla)
            End If
        End If

        If dic.ContainsKey(tp.NameCla()) Then
            If tp.GenericType <> EGeneric.ArgumentClass Then
                Debug.Print("")
            End If

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

            vtp = New TList(Of TClass)()
            ' for Add Find
            For Each tp_f In tp.GenCla
                tp2 = SubstituteArgumentClass(tp_f, dic)
                If tp2 IsNot tp_f Then
                    changed = True
                End If
                vtp.Add(tp2)
            Next

            If changed Then
                ' 変換した場合

                ' ジェネリック型のクラスを得る
                cla1 = GetSpecializedClass(name1, vtp)
                If cla1 Is Nothing Then
                    ' ない場合

                    cla1 = AddSpecializedClass(name1, vtp, 0)
                    SetMemberOfSpecializedClass(cla1)
                End If

                Return cla1
            Else
                ' 変換しなかった場合

                Return tp
            End If
        End If
    End Function

    Public Function CopyVariable(var_src As TVariable, dic As Dictionary(Of String, TClass)) As TVariable
        Dim var1 As TVariable

        If TypeOf var_src Is TField Then
            var1 = New TField()
        Else
            var1 = New TVariable()
        End If

        var_src.CopyVarMem(var1)
        var1.TypeVar = SubstituteArgumentClass(var_src.TypeVar, dic)

        Return var1
    End Function


    Public Function CopyField(cla1 As TClass, fld_src As TField, dic As Dictionary(Of String, TClass)) As TField
        Dim fld1 As New TField

        fld_src.CopyVarMem(fld1)

        fld1.ClaFld = cla1
        fld1.OrgFld = fld_src
        fld1.TypeVar = SubstituteArgumentClass(fld_src.TypeVar, dic)

        Return fld1
    End Function

    Public Function CopyFunctionDeclaration(cla1 As TClass, fnc_src As TFunction, dic As Dictionary(Of String, TClass)) As TFunction
        Dim fnc1 As New TFunction(fnc_src.NameVar, fnc_src.TypeVar)

        fnc_src.CopyFncMem(fnc1)

        fnc1.ClaFnc = cla1
        fnc1.OrgFnc = fnc_src
        fnc1.ArgFnc = New TList(Of TVariable)(From var_f In fnc_src.ArgFnc Select CopyVariable(var_f, dic))

        If fnc_src.RetType Is Nothing Then
            fnc1.RetType = Nothing
        Else
            fnc1.RetType = TProject.Prj.SubstituteArgumentClass(fnc_src.RetType, dic)
        End If

        Return fnc1
    End Function

    Public Sub SetMemberOfSpecializedClass(cla1 As TClass)
        Dim dic As Dictionary(Of String, TClass), i1 As Integer, dlg1 As TDelegate, dlg_org As TDelegate

        dic = New Dictionary(Of String, TClass)()
        ' for Each c In OrgCla.GenCla Add (c.NameCla, GenCla(@Idx)) To dic
        For i1 = 0 To cla1.OrgCla.GenCla.Count - 1
            dic.Add(cla1.OrgCla.GenCla(i1).NameCla(), cla1.GenCla(i1))
        Next

        cla1.SuperClassList = New TList(Of TClass)(From spr1 In cla1.OrgCla.SuperClassList Select TProject.Prj.SubstituteArgumentClass(spr1, dic))
        cla1.InterfaceList = New TList(Of TClass)(From spr1 In cla1.OrgCla.InterfaceList Select TProject.Prj.SubstituteArgumentClass(spr1, dic))
        cla1.FldCla = New TList(Of TField)(From fld1 In cla1.OrgCla.FldCla Select CopyField(cla1, fld1, dic))
        cla1.FncCla = New TList(Of TFunction)(From fnc1 In cla1.OrgCla.FncCla Select CopyFunctionDeclaration(cla1, fnc1, dic))

        If TypeOf cla1 Is TDelegate Then
            dlg1 = CType(cla1, TDelegate)
            dlg_org = CType(dlg1.OrgCla, TDelegate)

            dlg1.ArgDlg = New TList(Of TVariable)(From var_f In dlg_org.ArgDlg Select CopyVariable(var_f, dic))
            dlg1.RetDlg = TProject.Prj.SubstituteArgumentClass(dlg_org.RetDlg, dic)
        End If
    End Sub

    Public Function FindVariable(term As TTerm, name1 As String) As TVariable
        Dim cla1 As TClass = Nothing, var1 As TVariable

        Dim vfld = From fld In SystemType.FldCla Where fld.NameVar = name1
        If vfld.Any() Then

            Return vfld.First()
        End If

        For Each obj In TNaviUp.AncestorList(term)
            If TypeOf obj Is TFrom Then
                With CType(obj, TFrom)
                    If .VarQry.NameVar = name1 Then
                        Return .VarQry
                    End If
                End With

            ElseIf TypeOf obj Is TAggregate Then
                With CType(obj, TAggregate)
                    If .VarQry.NameVar = name1 Then
                        Return .VarQry
                    End If
                End With

            ElseIf TypeOf obj Is TFor Then
                With CType(obj, TFor)
                    If .InVarFor IsNot Nothing AndAlso .InVarFor.NameVar = name1 Then
                        Return .InVarFor
                    End If
                    If .IdxVarFor IsNot Nothing AndAlso .IdxVarFor.NameVar = name1 Then
                        Return .IdxVarFor
                    End If
                End With

            ElseIf TypeOf obj Is TTry Then
                With CType(obj, TTry)
                    For Each var1 In .VarCatch
                        If var1.NameVar = name1 Then
                            Return var1
                        End If
                    Next
                End With

            ElseIf TypeOf obj Is TBlock Then
                With CType(obj, TBlock)
                    For Each var1 In .VarBlc
                        If var1.NameVar = name1 Then
                            Return var1
                        End If
                    Next

                End With

            ElseIf TypeOf obj Is TFunction Then
                With CType(obj, TFunction)
                    For Each var1 In .ArgFnc
                        If var1.NameVar = name1 Then
                            Return var1
                        End If
                    Next

                    If .ThisFnc.NameVar = name1 Then
                        Return .ThisFnc
                    End If
                End With
            End If
        Next

        If dicGenCla.ContainsKey(name1) Then
            cla1 = dicGenCla(name1)
            Return cla1
        End If

        If SimpleParameterizedClassTable.ContainsKey(name1) Then
            cla1 = SimpleParameterizedClassTable(name1)
            Return cla1
        End If

        Return Nothing
    End Function

    Public Function CanCnvCla(dst_cla As TClass, src_trm As TTerm, src_cla As TClass) As Boolean
        Dim dst_dlg As TDelegate, src_dlg As TDelegate, i1 As Integer

        If src_trm IsNot Nothing AndAlso TypeOf src_trm Is TReference AndAlso CType(src_trm, TReference).NameRef = "Nothing" Then
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

        If dst_cla.ContainsArgumentClass AndAlso dst_cla.OrgCla Is src_cla.OrgCla Then
            Return True
        End If

        Return dst_cla Is src_cla OrElse dst_cla Is ObjectType OrElse src_cla.IsSubcla(dst_cla)
    End Function

    Public Function MatchFncArg(fnc1 As TFunction, varg As TList(Of TTerm)) As Boolean
        Dim i1 As Integer, param_array As Boolean, var1 As TVariable, trm1 As TTerm, tp1 As TClass

        If varg Is Nothing Then
            Return True
        Else
            param_array = (fnc1.ArgFnc.Count <> 0 AndAlso fnc1.ArgFnc(fnc1.ArgFnc.Count - 1).ParamArrayVar)

            If fnc1.ArgFnc.Count = varg.Count OrElse param_array AndAlso fnc1.ArgFnc.Count - 1 <= varg.Count Then
                For i1 = 0 To fnc1.ArgFnc.Count - 1
                    If fnc1.ArgFnc(i1).ParamArrayVar Then
                        Return True
                    End If

                    var1 = fnc1.ArgFnc(i1)
                    trm1 = varg(i1)
                    tp1 = trm1.TypeTrm
                    If var1.TypeVar Is Nothing Then
                    ElseIf tp1 Is Nothing Then
                    ElseIf TypeOf trm1 Is TReference AndAlso CType(trm1, TReference).IsAddressOf Then
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

    Public Function MatchFunction(fnc1 As TFunction, name1 As String, varg As TList(Of TTerm)) As Boolean
        Return fnc1.NameFnc() = name1 AndAlso Not fnc1.IsNew AndAlso Prj.MatchFncArg(fnc1, varg)
    End Function

    Public Shared Function FindFieldFunctionSub(cla1 As TClass, name1 As String, varg As TList(Of TTerm)) As TVariable
        Dim field_list = From fld2 In cla1.FldCla Where fld2.NameVar = name1
        If field_list.Any() Then
            Return field_list.First()
        End If

        Dim function_list = From fnc2 In cla1.FncCla Where Prj.MatchFunction(fnc2, name1, varg)
        If function_list.Any() Then
            Return function_list.First()
        End If

        Return Nothing
    End Function

    Public Shared Function FindFieldFunction(cla1 As TClass, name1 As String, varg As TList(Of TTerm)) As TVariable
        Dim variable_list = From var1 In (From cla2 In Concatenate(Prj.SystemType, cla1, TNaviUp.AncestorSuperClassList(cla1), TNaviUp.AncestorInterfaceList(cla1)) Select FindFieldFunctionSub(CType(cla2, TClass), name1, varg)) Where var1 IsNot Nothing

        If variable_list.Any() _
            Then
            Return variable_list.First()
        Else
            Return Nothing
        End If
    End Function

    Public Shared Iterator Function Concatenate(ParamArray args As Object()) As IEnumerable(Of Object)
        For Each arg In args
            If TypeOf arg Is IEnumerable Then
                For Each o In CType(arg, IEnumerable)
                    Yield o
                Next
            Else
                Yield arg
            End If
        Next
    End Function

    Public Shared Function FindFieldByName(cla1 As TClass, name1 As String) As TField
        Dim fld1 As TField

        ' for Find
        For Each fld2 In cla1.FldCla
            If fld2.NameVar = name1 Then
                Return fld2
            End If
        Next

        ' for Find
        For Each cla_f In cla1.SuperClassList
            fld1 = FindFieldByName(cla_f, name1)
            If fld1 IsNot Nothing Then
                Return fld1
            End If
        Next

        Return Nothing
    End Function

    Public Function FindFunctionByName(class_name As String, fnc_name As String) As TFunction
        For Each cls1 In SimpleParameterizedClassList
            For Each fnc1 In cls1.FncCla
                If cls1.NameCla() = class_name AndAlso fnc1.NameFnc() = fnc_name Then
                    Return fnc1
                End If
            Next
        Next

        Return Nothing
    End Function

    Public Function FindFieldByName(class_name As String, field_name As String) As TField
        For Each cls1 In SimpleParameterizedClassList
            For Each fld1 In cls1.FldCla
                If cls1.NameCla() = class_name AndAlso fld1.NameVar = field_name Then
                    Return fld1
                End If
            Next
        Next

        Return Nothing
    End Function

    Public Shared Function FindNew(cla1 As TClass, varg As TList(Of TTerm)) As TVariable
        For Each cla2 In Concatenate(cla1, TNaviUp.AncestorSuperClassList(cla1))
            Dim vfnc = From fnc In CType(cla2, TClass).FncCla Where fnc.IsNew AndAlso Prj.MatchFncArg(fnc, varg)
            If vfnc.Any() Then
                Return vfnc.First()
            End If
        Next

        Return Nothing
    End Function

    Public Sub DumpClass(cla1 As TClass, sw As TStringWriter)
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

        If cla1.SuperClassList.Count <> 0 Then
            sw.WriteLine("  super:{0}", cla1.SuperClassList(0).ToString())
        End If
        If cla1.InterfaceList.Count <> 0 Then
            sw.Write("  impl:")
            For Each cla2 In cla1.InterfaceList
                sw.Write(" {0}", cla2.ToString())
            Next
            sw.WriteLine("")
        End If

    End Sub

    ' 指定されたクラスのクラスの初期化メソッドまたはインスタンスの初期化メソッドを作る。
    Public Sub MakeInstanceClassInitializerSub(cls1 As TClass, vnew As List(Of TFunction), is_shared As Boolean)
        Dim ini_fnc As TFunction, asn1 As TAssignment, call_ini As TCall, app_ini As TApply, dot1 As TDot, ref1 As TReference

        ' 初期化の式があるフィールドのリストを得る。
        Dim vfld = (From fld1 In cls1.FldCla Where fld1.InitVar IsNot Nothing AndAlso fld1.ModVar.isShared = is_shared).ToList()
        If vfld.Count <> 0 Then

            ' フィールドの初期化式から作った代入文を集めたメソッドを作る。
            If is_shared Then
                ' クラスの初期化の場合

                ini_fnc = New TFunction(TFunction.ClassInitializerName, Nothing)
            Else
                ' インスタンスの初期化の場合

                ini_fnc = New TFunction(TFunction.InstanceInitializerName, Nothing)
            End If
            cls1.FncCla.Add(ini_fnc)

            ini_fnc.ClaFnc = cls1
            ini_fnc.ModVar = New TModifier()
            ini_fnc.TypeFnc = EToken.eSub
            ini_fnc.ThisFnc = New TVariable(ParsePrj.ThisName, cls1)
            ini_fnc.BlcFnc = New TBlock()

            ' フィールドの初期化式から作った代入文をメソッドで定義する。
            For Each fld1 In vfld

                ref1 = New TReference(fld1)
                asn1 = New TAssignment(ref1, fld1.InitVar)
                ini_fnc.BlcFnc.AddStmtBlc(asn1)
            Next


            If is_shared Then
                ' クラスの初期化の場合

                ' クラスの初期化メソッドから、フィールドの初期化式から作った代入文を集めたメソッドを呼ぶ。
                dot1 = New TDot(New TReference(cls1), ini_fnc)
                app_ini = TApply.MakeAppCall(dot1)
                call_ini = New TCall(app_ini)
                call_ini.IsGenerated = True

                If theMain IsNot Nothing Then

                    theMain.BlcFnc.StmtBlc.Insert(0, call_ini)

                    theMain.CallTo.Add(ini_fnc)
                    ini_fnc.CallFrom.Add(theMain)
                End If
            Else
                ' インスタンスの初期化の場合

                ' すべてのコンストラクターに対し
                For Each new1 In vnew

                    ' インスタンスの初期化メソッドから、フィールドの初期化式から作った代入文を集めたメソッドを呼ぶ。
                    app_ini = TApply.MakeAppCall(New TReference(ini_fnc))
                    call_ini = New TCall(app_ini)
                    call_ini.IsGenerated = True
                    new1.BlcFnc.StmtBlc.Insert(0, call_ini)

                    new1.CallTo.Add(ini_fnc)
                    ini_fnc.CallFrom.Add(new1)
                Next
            End If
        End If
    End Sub

    ' すべての単純クラスとパラメータ化クラスに対し、クラスの初期化メソッドとインスタンスの初期化メソッドを作る。
    Public Sub MakeInstanceClassInitializer()
        ' すべての単純クラスとパラメータ化クラスに対し
        For Each cls1 In SimpleParameterizedClassList
            Dim vnew = (From fnc1 In cls1.FncCla Where fnc1.IsNew).ToList()

            If vnew.Count = 0 Then
                ' コンストラクターが１つもない場合

                ' 暗黙のコンストラクターを作る。
                Dim new_fnc As New TFunction(TFunction.ImplicitNewName, Nothing)

                new_fnc.ClaFnc = cls1
                new_fnc.ModVar = New TModifier()
                new_fnc.TypeFnc = EToken.eNew
                new_fnc.ThisFnc = New TVariable(ParsePrj.ThisName, cls1)
                new_fnc.BlcFnc = New TBlock()
                new_fnc.IsNew = True

                ' コンストラクターが最初になるようにする。
                cls1.FncCla.Insert(0, new_fnc)

                vnew.Add(new_fnc)
            End If

            ' クラスの初期化メソッドを作る。
            MakeInstanceClassInitializerSub(cls1, vnew, True)

            ' インスタンスの初期化メソッドを作る。
            MakeInstanceClassInitializerSub(cls1, vnew, False)
        Next
    End Sub

    Public Function ElementType(type1 As TClass) As TClass
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

    ' フィールドがリストなどの場合は要素の型を返し、それ以外ならフィールドの型を返す。
    Public Function FieldElementType(fld As TField) As TClass
        Return CType(If(fld.TypeVar.OrgCla Is Nothing, fld.TypeVar, ElementType(fld.TypeVar)), TClass)
    End Function

    Public Function GetOperatorFunction(type1 As EToken, trm1 As TTerm) As TFunction
        Dim tp1 As TClass, name1 As String

        Select Case type1
            Case EToken.eADD
                name1 = "+"
            Case EToken.eMns
                name1 = "-"
            Case EToken.eMUL
                name1 = "*"
            Case EToken.eDIV
                name1 = "/"
            Case EToken.eMOD
                name1 = "Mod"
            Case EToken.eEq
                name1 = "="
            Case EToken.eNE
                name1 = "<>"
            Case EToken.eINC
                name1 = "++"
            Case EToken.eDEC
                name1 = "--"
            Case EToken.eBitOR
                name1 = "|"
            Case Else
                Return Nothing
        End Select

        ' 最初の引数の型を得る
        tp1 = trm1.TypeTrm
        Debug.Assert(tp1 IsNot Nothing)

        ' 名前が同じの演算子オーバーロード関数を探す
        Dim vfnc = From fnc In tp1.FncCla Where fnc.TypeFnc = EToken.eOperator AndAlso fnc.NameFnc() = name1
        If vfnc.Any() Then
            Return vfnc.First()
        End If

        Return Nothing
    End Function

    ' オーバーロードしているメソッド(OvrFnc)とオーバーロードされているメソッド(OvredFnc)をセットする
    Public Sub SetOvrFncSub(cla1 As TClass, fnc1 As TFunction)
        Dim i1 As Integer, tp1 As TClass, tp2 As TClass, all_eq As Boolean

        ' すべてのスーパークラスに対し
        For Each cla2 In cla1.SuperClassList

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
    Public Sub SetEqOvredFncAll(fnc1 As TFunction, vfnc As TList(Of TFunction))
        For Each fnc2 In fnc1.OvredFnc
            vfnc.Add(fnc2)

            ' fnc2のオーバーロード関数の子孫をセットする
            SetEqOvredFncAll(fnc2, vfnc)
        Next
    End Sub

    ' オーバーロード関数をセットする
    Public Sub SetOvrFnc()
        '  すべてのクラスに対し
        For Each cla1 In SimpleParameterizedClassList
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

        For Each cla1 In SimpleParameterizedClassList
            For Each fld1 In cla1.FldCla
                If vAllFld.Contains(fld1) Then
                    Debug.Assert(False)
                Else
                    vAllFld.Add(fld1)
                End If
            Next
        Next
        For Each cla1 In SpecializedClassList
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
        Dim changed As Boolean, v As New TList(Of TFunction), i1 As Integer

        '  すべてのクラスに対し
        For Each cla1 In SimpleParameterizedClassList

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
            For Each cla1 In SimpleParameterizedClassList

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
                If ref1.FunctionTrm.Reachable Then
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
        For Each cls1 In SimpleParameterizedSpecializedClassList
            If cls1.UsedVar Then
                For Each cls2 In cls1.AllSuperClassList
                    cls2.UsedVar = True
                Next
            End If
        Next
    End Sub

    ' 関数のノードをグラフに追加する
    Public Function AddFncGraph(dic1 As Dictionary(Of Object, TFlowNode), fnc1 As TFunction) As TFncNode
        Dim fncnd As TFncNode, fncnd2 As TFncNode, fnc2 As TFunction

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

                    If ref2.FunctionTrm.Reachable Then
                        ' 到達可能の関数内で参照されている場合

                        If dic1.ContainsKey(ref2.FunctionTrm) Then
                            ' 関数が辞書にある場合

                            fncnd2 = dic1(ref2.FunctionTrm)
                        Else
                            ' 関数が辞書にない場合

                            ' 関数のノードをグラフに追加する
                            fncnd2 = AddFncGraph(dic1, ref2.FunctionTrm)
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
    Public Function AddRefGraph(dic1 As Dictionary(Of Object, TFlowNode), ref1 As TReference) As TRefNode
        Dim refnd As TRefNode, fncnd As TFncNode

        ' 変数参照のノードを作る。
        refnd = New TRefNode(ref1)

        ' 辞書に追加する。
        Debug.Assert(Not dic1.ContainsKey(ref1))
        dic1.Add(ref1, refnd)

        If dic1.ContainsKey(ref1.FunctionTrm) Then
            ' 変数参照を含む関数のノードが辞書にある場合

            fncnd = CType(dic1(ref1.FunctionTrm), TFncNode)
        Else
            ' 変数参照を含む関数のノードが辞書にない場合

            ' 関数のノードをグラフに追加する
            fncnd = AddFncGraph(dic1, ref1.FunctionTrm)
        End If

        ' 関数から変数参照への矢印を追加する。
        If Not fncnd.ToNd.ContainsKey(ref1) Then

            fncnd.ToNd.Add(ref1, refnd)
        End If

        Return refnd
    End Function

    ' 変数参照のグラフを作る
    Public Sub MakeReferenceGraph()
        Dim dic1 As New Dictionary(Of Object, TFlowNode), vnd As TList(Of TNode), vfnc As List(Of TFunction)
        Dim dgr As TDrawGraph, dot_dir As String, dot_path As String, idx As Integer, def_ref As Boolean
        Dim sw As New TStringWriter, file_name As String

        dot_dir = OutputDirectory + "\html\_dot"

        ' すべてのクラスに対し
        For Each cla1 In SimpleParameterizedClassList

            If SimpleParameterizedClassList.IndexOf(cla1) Mod 25 = 0 Then
                Debug.WriteLine("Make Ref Graph {0}", SimpleParameterizedClassList.IndexOf(cla1))
            End If

            ' すべてのフィールドに対し
            For Each fld1 In cla1.FldCla

                If True OrElse cla1.NameVar = "TApply" AndAlso fld1.NameVar = "KndApp" Then

                    For idx = 0 To 1

                        def_ref = If(idx = 0, True, False)

                        ' ノードの辞書を初期化する
                        TFlowNode.CntNd = 0
                        dic1 = New Dictionary(Of Object, TFlowNode)()
                        vfnc = New List(Of TFunction)()

                        ' すべてのフィールド参照に対し
                        For Each ref1 In fld1.RefVar

                            If ref1.DefRef = def_ref AndAlso ref1.FunctionTrm.Reachable Then
                                ' 到達可能の関数内で参照されている場合

                                If Not vfnc.Contains(ref1.FunctionTrm) Then
                                    ' 同一関数内での変数参照が未処理の場合

                                    vfnc.Add(ref1.FunctionTrm)

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

                        file_name = GetHtmlFileName(cla1) + "_" + GetHtmlFileName(fld1) + "_" + If(def_ref, "define", "use")
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

    Public Function TypeName(name1 As String) As String
        If name1 = "int" Then
            Return "Integer"
        ElseIf name1 = "bool" Then
            Return "Boolean"
        End If

        If ClassNameTable IsNot Nothing AndAlso ClassNameTable.ContainsKey(name1) Then
            Return ClassNameTable(name1)
        End If

        Return name1
    End Function

    Public Sub SetClassNameList(cla1 As TClass, parser As TSourceParser)
        Dim tw As New TTokenWriter(cla1, parser)
        Dim i1 As Integer

        If cla1.TokenListVar IsNot Nothing Then
            Exit Sub
        End If

        If cla1 Is Nothing Then
            tw.Fmt("型不明")
        Else

            If cla1.DimCla <> 0 Then
                ' 配列の場合

                Debug.Assert(cla1.GenCla IsNot Nothing AndAlso cla1.GenCla.Count = 1)
                SetClassNameList(cla1.GenCla(0), parser)
                tw.Fmt(cla1.GenCla(0).TokenListVar, EToken.eLP)

                For i1 = 0 To cla1.DimCla - 1
                    If i1 <> 0 Then
                        tw.Fmt(EToken.eComma)
                    End If
                Next
                tw.Fmt(EToken.eRP)
            Else
                ' 配列でない場合
                tw.Fmt(cla1.NameVar)
                If cla1.GenCla IsNot Nothing Then
                    ' 総称型の場合

                    tw.Fmt(EToken.eLP, EToken.eOf)
                    For i1 = 0 To cla1.GenCla.Count - 1
                        If i1 <> 0 Then
                            tw.Fmt(EToken.eComma)
                        End If

                        SetClassNameList(cla1.GenCla(i1), parser)
                        tw.Fmt(cla1.GenCla(i1).TokenListVar)
                    Next
                    tw.Fmt(EToken.eRP)
                End If
            End If
        End If

        cla1.TokenListVar = tw.GetTokenList()

    End Sub

    Public Sub SetTokenListClsAll(parser As TSourceParser)
        For Each cla1 In SimpleParameterizedSpecializedClassList
            SetClassNameList(cla1, parser)
        Next
    End Sub

    Public Function TokenListToString(parser As TSourceParser, v As List(Of TToken)) As String
        Dim sw As New TStringWriter

        For Each tkn In v
            Dim txt As String = ""

            Select Case tkn.TypeTkn
                Case EToken.eInt
                Case EToken.eNL
                Case EToken.eUnknown
                Case EToken.eComment
                Case EToken.eTab
                Case Else
                    If parser.vTknName.ContainsKey(tkn.TypeTkn) Then
                        txt = parser.vTknName(tkn.TypeTkn)
                    Else
                        txt = "未登録語 : " + tkn.TypeTkn.ToString()
                        parser.vTknName.Add(tkn.TypeTkn, txt)
                        Debug.Print(txt)
                        '                        Debug.Assert(False)
                    End If

            End Select

            Select Case tkn.TypeTkn
                Case EToken.eInt
                    sw.Write(tkn.StrTkn)

                Case EToken.eNL
                    sw.WriteLine("")

                Case EToken.eComment
                    If parser.LanguageSP = ELanguage.Basic Then
                        sw.Write("'" + tkn.StrTkn)
                    Else
                        sw.Write("//" + tkn.StrTkn)
                    End If

                Case EToken.eAs, EToken.eTo, EToken.eIs, EToken.eIsNot, EToken.eIn, EToken.eInto, EToken.eWhere, EToken.eTake, EToken.eStep, EToken.eImplements, EToken.eParamArray
                    sw.Write(" " + txt + " ")

                Case EToken.eThen
                    sw.Write(" " + txt)

                Case EToken.eUnknown
                    If TypeOf tkn.ObjTkn Is TDot Then
                        With CType(tkn.ObjTkn, TDot)
                            sw.Write(".{0}", parser.TranslageReferenceName(CType(tkn.ObjTkn, TDot)))

                        End With

                    ElseIf TypeOf tkn.ObjTkn Is TReference Then
                        With CType(tkn.ObjTkn, TReference)
                            sw.Write(parser.TranslageReferenceName(CType(tkn.ObjTkn, TReference)))
                        End With

                    ElseIf TypeOf tkn.ObjTkn Is TClass Then
                        With CType(tkn.ObjTkn, TClass)
                            sw.Write(.NameVar)

                        End With

                    ElseIf TypeOf tkn.ObjTkn Is TVariable Then
                        With CType(tkn.ObjTkn, TVariable)
                            sw.Write(.NameVar)

                        End With

                    ElseIf TypeOf tkn.ObjTkn Is String Then
                        sw.Write(CType(tkn.ObjTkn, String))

                    Else
                        Debug.Print("{0}", tkn.ObjTkn.GetType())

                    End If

                Case Else
                    If txt.Length = 1 Then
                        Select Case txt(0)
                            Case "("c, ")"c, "["c, "]"c, "{"c, "}"c, "."c
                                sw.Write(txt)
                            Case Else
                                sw.Write(" " + txt + " ")
                        End Select
                    Else
                        sw.Write(" " + txt + " ")
                    End If

            End Select

        Next

        Return sw.ToString()
    End Function

    Public Shared Function FileExtension(lang As ELanguage) As String
        Select Case lang
            Case ELanguage.Basic
                Return ".vb"
            Case ELanguage.TypeScript
                Return ".ts"
            Case ELanguage.JavaScript
                Return ".js"
            Case ELanguage.Java
                Return ".java"
            Case Else
                Debug.Assert(False)
                Return Nothing
        End Select
    End Function

    ' Basicのソースを作る
    Public Sub MakeAllSourceCode(parser As TSourceParser)
        SetTokenListClsAll(parser)

        Dim out_dir2 As String = String.Format("{0}\{1}", OutputDirectory, parser.LanguageSP)
        TDirectory.CreateDirectory(out_dir2)

        '  すべてのソースに対し
        For Each src_f In SrcPrj
            CurSrc = src_f

            Dim navi_make_source_code As New TNaviMakeSourceCode(Me, parser)
            navi_make_source_code.NaviSourceFile(src_f)

            Dim src_path As String = String.Format("{0}\{1}{2}", out_dir2, TPath.GetFileNameWithoutExtension(src_f.FileSrc), FileExtension(parser.LanguageSP))
            Dim src_txt2 As String = TokenListToString(parser, src_f.TokenListSrc)
            TFile.WriteAllText(src_path, src_txt2)


            CurSrc = Nothing
        Next

        If ApplicationClassList IsNot Nothing Then

            Dim class_list = From c In ApplicationClassList Where c.GenTokenListCls IsNot Nothing
            If class_list.Any() Then

                Dim sw As New StringWriter

                For Each cls1 In class_list
                    Select Case parser.LanguageSP
                        Case ELanguage.Basic
                            sw.WriteLine("Partial Public Class {0}", cls1.NameVar)
                            sw.WriteLine(TokenListToString(parser, cls1.GenTokenListCls))
                            sw.WriteLine("End Class")
                            sw.WriteLine()

                        Case ELanguage.JavaScript
                            sw.WriteLine(TokenListToString(parser, cls1.GenTokenListCls))

                    End Select
                Next

                Dim src_path As String = String.Format("{0}\{1}{2}", out_dir2, "Generated", FileExtension(parser.LanguageSP))
                TFile.WriteAllText(src_path, sw.ToString())
            End If
        End If

    End Sub

    Public Function WriteInheritanceHierarchy(class_sw As StringWriter, cls1 As TClass) As Integer
        Dim indent As Integer

        If cls1.SuperClassList.Count <> 0 Then
            indent = WriteInheritanceHierarchy(class_sw, cls1.SuperClassList(0))
        Else
            indent = 0
        End If

        class_sw.WriteLine("<p style=""text-indent:{0}em""><a href=""../{1}/{1}.html"">{1}</a></p>", indent * 2, cls1.NameVar)

        Return indent + 1
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

    Public Shared Function GetHtmlFileName(cls_fld_fnc As TVariable) As String
        If TypeOf cls_fld_fnc Is TClass OrElse TypeOf cls_fld_fnc Is TField Then
            Return HexString(cls_fld_fnc.NameVar)
        ElseIf TypeOf cls_fld_fnc Is TFunction Then
            Return cls_fld_fnc.IdxVar.ToString()
        Else
            Debug.Assert(False)
            Return Nothing
        End If
    End Function

    Public Sub CheckRefVar(ref1 As TReference)
        Dim fname As String

        If ref1.VarRef Is Nothing Then
            Debug.Assert(CurSrc IsNot Nothing)
            fname = TPath.GetFileNameWithoutExtension(CurSrc.FileSrc)
            Debug.Assert(fname = "@lib" OrElse fname = "System" OrElse fname = "sys" OrElse fname = "web")
        End If
    End Sub
End Class

Public Class TDelegatePair
    Public ClaDel As TClass
    Public FncDel As TFunction

    Public Sub New(cla1 As TClass, fnc1 As TFunction)
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
    Public RefNode As TReference

    Public Sub New(ref1 As TReference)
        CntNd = CntNd + 1
        IdxNd = CntNd
        RefNode = ref1
    End Sub
End Class

Public Class TFncNode
    Inherits TFlowNode
    Public FncNode As TFunction

    Public Sub New(fnc1 As TFunction)
        CntNd = CntNd + 1
        IdxNd = CntNd
        FncNode = fnc1
    End Sub
End Class
