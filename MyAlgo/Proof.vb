Imports System.IO
Imports System.Diagnostics


' -------------------------------------------------------------------------------- TDataflow
Public Enum EAnalyzeChangeableFld
    Init
    ChangeableCondition
    Consistency
    MayBeAffectedStmtList
    ValueChangePropagation
    ValidStmt
    LocalVariableAssignment
End Enum


' -------------------------------------------------------------------------------- TDataflow
Public Class TDataflow
    Public Const ParentPrefix As String = "_Parent"
    Public Const PrevPrefix As String = "_Prev"
    Public Const CurrentPrefix As String = "_current"
    Public Const ChildPrefix As String = "_child"
    Public ProjectDtf As TProject
    Public Changed As Boolean
    Public RuleFnc As TFunction
    Public DoneVar As New TList(Of TVariable)
    Public ChangeableFldList As TList(Of TField)
    Public PrjDF As TProject
    Public GlobalRule As TFunction
    Public RuleCp As TFunction
    Public ChangeableFld As TField
    Public Change As TChange
    Public RefChangeableUpStmt As TStatement
    Public NormalizedCondition As TApply
    Public PreCondition As TApply
    Public ChangePropagation As TChange
    Public LocalVariableAssignment As TAssignment
    Public SyncFldList As New TList(Of TField)
    Public SyncClassName As String
    Public SyncClassSrc As String
    Public RuleSW As New StringWriter
    Public MinLevel As Integer
    Public MaxLevel As Integer
    Public dicAffectedRefLevelSelf As Dictionary(Of Integer, TList(Of TAffectedRef))
    Public dicAffectedRefLevel As Dictionary(Of Integer, TList(Of TAffectedRef))

    Public Sub New(prj1 As TProject)
        ProjectDtf = prj1
    End Sub

    Public Shared Function UpStmt(obj1 As Object) As Object
        If obj1 Is Nothing Then
            '            Debug.WriteLine("")
            Return Nothing
        End If

        If TypeOf obj1 Is TTerm Then
            Return UpStmt(CType(obj1, TTerm).UpTrm)
        ElseIf TypeOf obj1 Is IUpList Then
            Return UpStmt(CType(obj1, IUpList).GetUpList())
        ElseIf TypeOf obj1 Is TBlock Then
            Return UpStmt(CType(obj1, TBlock).ParentStmt)
        ElseIf TypeOf obj1 Is TStatement Then
            Return CType(obj1, TStatement)
        ElseIf TypeOf obj1 Is TFunction Then
            Return obj1
        ElseIf TypeOf obj1 Is TVariable Then
            Return UpStmt(CType(obj1, TVariable).UpVar)
        Else
            Debug.WriteLine("@i")
            Return Nothing
        End If
    End Function

    Public Shared Function UpBlock(stmt1 As TStatement) As TBlock
        Return CType(CType(stmt1.ParentStmt, IUpList).GetUpList(), TBlock)
    End Function

    Public Shared Function UpStmtProper(obj1 As Object) As TStatement
        Dim up_obj As Object

        up_obj = UpStmt(obj1)
        If TypeOf up_obj Is TStatement Then
            Return CType(up_obj, TStatement)
        Else
            Return Nothing
        End If
    End Function

    Public Shared Function UpFor(obj1 As Object) As TFor
        Dim stmt1 As TStatement

        stmt1 = UpStmtProper(obj1)
        Do While stmt1 IsNot Nothing
            If TypeOf stmt1 Is TFor Then
                Return CType(stmt1, TFor)
            End If
            stmt1 = UpStmtProper(stmt1.ParentStmt)
        Loop

        Return Nothing
    End Function

    ' 値が変化したときの条件を正規化する。
    Public Function NormalizeValueChangedCondition(change As TChange, ref1 As TReference, ref_type As ERefPathType) As TApply
        Dim J As TApply
        Dim trans As TTransRelative

        Select Case ref_type
            Case ERefPathType.SelfField
                J = Sys.CopyTrm(change.ConditionChn, Nothing)
            Case ERefPathType.ParentField, ERefPathType.PrevField
                If ref_type = ERefPathType.ParentField Then
                    trans = New TTransRelative(E相対位置.親)
                Else
                    trans = New TTransRelative(E相対位置.直前)
                End If

                J = TApply.NewOpr(EToken.eAnd)
                For Each trm In change.ConditionChn.ArgApp
                    Dim trm_trans As TTerm

                    trans.無効 = False
                    trm_trans = trans.TransTrm(trm, Nothing)
                    If Not trans.無効 Then
                        J.AddInArg(trm_trans)
                    End If
                Next

            Case ERefPathType.SelfFieldChild
                J = Nothing
            Case ERefPathType.局所変数
                J = Sys.CopyTrm(change.ConditionChn, Nothing)
            Case Else
                J = Nothing
        End Select

        If J IsNot Nothing Then

            ' 余分な条件を取り除く
            CleanCondition(J)
        End If

        Return J
    End Function

    ' 影響され得る文のリストを得る
    Public Function GetMayBeAffectedStmtList(stmt As TStatement) As TList(Of TStatement)
        Dim may_be_affected_stmt As New TList(Of TStatement)

        If TypeOf stmt Is TAssignment Then
            ' 代入文の場合

            ' 代入先の変数の値の変化を波及させる。
            may_be_affected_stmt.Add(CType(stmt, TAssignment))

        ElseIf TypeOf stmt Is TCall Then
            ' メソッド呼び出しの場合

            Dim call1 As TCall, ref_assert As TReference

            call1 = CType(stmt, TCall)
            ref_assert = CType(call1.AppCall.FncApp, TReference)

            ' メソッド呼び出しはAssertのみ
            Debug.Assert(ref_assert.NameRef = "Assert")

        ElseIf TypeOf stmt Is TIfBlock Then
            ' Ifブロックの場合

            Dim up_if As TIf, eq_after As Boolean = False

            ' Ifブロックを含むIf文を得る
            up_if = CType(UpStmt(stmt.ParentStmt), TIf)

            ' If文は影響され得る
            may_be_affected_stmt.Add(up_if)

            ' 手前のIfブロックは条件判定のみ有効とする。
            For Each if_blc In up_if.IfBlc
                If if_blc Is stmt Then
                    ' 以降のIfブロック内部の文は無条件に有効とする。

                    eq_after = True
                End If

                If eq_after Then
                    ' このIfブロックとそれ以降のIfブロックの場合

                    ' Ifブロックと内部の文を列挙する。
                    Dim enum_stmt As New TEnumInnerStmt
                    enum_stmt.NaviStatement(if_blc, Nothing)

                    ' Ifブロックと内部の文は影響され得る
                    may_be_affected_stmt.AddRange(enum_stmt.vStmt)
                End If
            Next

        ElseIf TypeOf stmt Is TSelect Then
            ' Select文の場合

            ' Select文と内部の文を列挙する。
            Dim enum_stmt As New TEnumInnerStmt
            enum_stmt.NaviStatement(stmt, Nothing)

            ' Select文と内部の文は影響され得る
            may_be_affected_stmt.AddRange(enum_stmt.vStmt)
        Else
            Debug.Assert(False)
        End If

        Return may_be_affected_stmt
    End Function

    ' 有効な文を波及させる。
    Public Sub PropagateValidStmt(valid_stmt As Dictionary(Of TStatement, TStatement))

        Dim vwork_stmt As TList(Of TStatement)
        Dim enum_localRef As TEnumInnerLocalRef

        enum_localRef = New TEnumInnerLocalRef()
        vwork_stmt = New TList(Of TStatement)(valid_stmt.Values)
        Do While vwork_stmt.Count <> 0
            Dim wk_stmt As TStatement, up_stmt As TStatement, asn_stmt As TAssignment

            wk_stmt = vwork_stmt(0)
            vwork_stmt.RemoveAt(0)

            ' 有効な文を囲む条件文,For文は有効とする。
            up_stmt = UpStmtProper(wk_stmt.ParentStmt)
            Do While up_stmt IsNot Nothing

                If Not valid_stmt.ContainsKey(up_stmt) Then
                    ' 未処理の場合

                    valid_stmt.Add(up_stmt, up_stmt)
                    vwork_stmt.Add(up_stmt)

                    '-------------------------------------------------- 有効な文を表示する。
                    'Yield EAnalyzeChangeableFld.ValidStmt
                End If

                up_stmt = UpStmtProper(up_stmt.ParentStmt)
            Loop

            ' 局所変数の参照を列挙する
            enum_localRef.NaviStatement(wk_stmt, Nothing)
            For Each var_f In enum_localRef.vNewVar
                For Each ref_f In var_f.RefVar
                    If ref_f.DefRef Then
                        ' 代入の場合

                        ' 有効な文が局所変数の値を参照する場合、局所変数への代入文も有効とする。
                        asn_stmt = CType(UpStmt(ref_f), TAssignment)
                        If Not valid_stmt.ContainsKey(asn_stmt) Then
                            ' 未処理の場合

                            valid_stmt.Add(asn_stmt, asn_stmt)
                            vwork_stmt.Add(asn_stmt)

                            ' 局所変数の宣言文も有効とする。
                            Dim decl_stmt = UpStmt(var_f)
                            Debug.Assert(decl_stmt IsNot Nothing AndAlso TypeOf decl_stmt Is TVariableDeclaration)
                            If Not valid_stmt.ContainsKey(CType(decl_stmt, TVariableDeclaration)) Then

                                valid_stmt.Add(CType(decl_stmt, TVariableDeclaration), CType(decl_stmt, TVariableDeclaration))
                            End If

                            '-------------------------------------------------- 局所変数への代入文を表示する。
                            LocalVariableAssignment = asn_stmt
                            'Yield EAnalyzeChangeableFld.LocalVariableAssignment
                        End If
                    End If
                Next
            Next
        Loop
    End Sub

    ' 値が変化し得るフィールドを解析する
    Public Sub AnalyzeChangeableFld()
        Dim prj1 As TProject = PrjDF
        Dim rule As TFunction = GlobalRule
        Dim root_change As TChange
        Dim valid_stmt As Dictionary(Of TStatement, TStatement)
        Dim me_ref As TReference, typeof_app As TApply
        Dim Unprocessed As New TList(Of TChange)      ' 未処理の(条件, 変数)のペアのリスト Unprocessed
        Dim vchange_all As New TList(Of TChange)    ' 処理済みの(条件, 変数)のペアのリスト Processed
        Dim dic_may_be_affected_stmt_list As New Dictionary(Of TStatement, TList(Of TStatement))

        SyncClassName = "TSync_" + ChangeableFld.FullFldName()

        '-------------------------------------------------- 処理対象のフィールドを表示する。

        valid_stmt = New Dictionary(Of TStatement, TStatement)()

        RuleCp = Sys.CopyFnc(rule)
        RuleCp.NameVar = RuleCp.NameVar + "_" + ChangeableFld.FullFldName()
        rule.ClaFnc.FncCla.Add(RuleCp)

        '            text_block_class = prj1.GetCla("TTextBlock")
        me_ref = New TReference(RuleCp.ArgFnc(0))
        typeof_app = TApply.NewTypeOf(me_ref, ChangeableFld.TypeVar)

        ' クラスTViewのオブジェクトのフィールドMarginLeftの値が変化した場合
        root_change = New TChange(Nothing, ChangeableFld, Nothing, typeof_app)
        Change = root_change

        ' (Me ∊ C, F) を Unprocessedに積む
        Unprocessed.Add(Change)
        vchange_all.Add(Change)

        Do While Unprocessed.Count <> 0
            ' 未処理の変化リストが空でない場合

            ' 未処理の変化を１つ取り出す
            Change = Unprocessed.Last()
            Unprocessed.RemoveAt(Unprocessed.Count - 1)

            '-------------------------------------------------- 値が変化した変数、代入された場所、代入時の条件を表示する。

            For Each ref1 In Change.VariableChn.RefVar
                ' すべての変数参照に対し

                If ref1.FunctionTrm Is RuleCp AndAlso Not ref1.DefRef Then
                    ' 規則内の変数参照で、値の代入ではない場合

                    ' 参照のされ方を得る。
                    Dim ref_type As ERefPathType = GetRefType(ref1)

                    ' 代入時の条件を自身の条件に直す。
                    NormalizedCondition = NormalizeValueChangedCondition(Change, ref1, ref_type)

                    ' 変数参照を含む文
                    RefChangeableUpStmt = UpStmt(ref1)

                    If NormalizedCondition Is Nothing Then
                        ' 代入時の条件がない場合

                        ' 前提条件は求めない
                        PreCondition = Nothing
                    Else
                        ' 代入時の条件がある場合

                        ' 文を実行する前提条件を得る
                        PreCondition = GetPreConditionClean(RefChangeableUpStmt)
                    End If

                    '-------------------------------------------------- 代入時の条件と、文の実行の条件の無矛盾の判定を表示する。

                    If NormalizedCondition Is Nothing OrElse Consistent(NormalizedCondition, PreCondition) Then
                        ' 代入時の条件と、文の実行の条件が矛盾しない場合

                        Dim afr As New TAffectedRef(Change, ref1, ref_type, RefChangeableUpStmt)

                        ' タスクの起動のコードを作る
                        MakeInvokeCode(Change, ref_type)

                        ' 影響され得る文のリストを得る
                        Dim may_be_affected_stmt_list As TList(Of TStatement)

                        If dic_may_be_affected_stmt_list.ContainsKey(RefChangeableUpStmt) Then

                            may_be_affected_stmt_list = dic_may_be_affected_stmt_list(RefChangeableUpStmt)
                        Else

                            may_be_affected_stmt_list = GetMayBeAffectedStmtList(RefChangeableUpStmt)
                            dic_may_be_affected_stmt_list.Add(RefChangeableUpStmt, may_be_affected_stmt_list)
                        End If


                        '-------------------------------------------------- 影響され得る文のリストを表示する。

                        ' 影響され得る代入文から代入先の変数の値の変化を波及させる。
                        For Each asn1 In (From x In may_be_affected_stmt_list Where TypeOf x Is TAssignment Select CType(x, TAssignment))
                            Dim ref_asn As TReference

                            ' 代入先の変数参照を得る。
                            ref_asn = CType(asn1.RelAsn.ArgApp(0), TReference)

                            ' 代入先の変数参照が処理済みか調べる
                            Dim vchn = (From c In vchange_all Where c.RefChn Is ref_asn Select c).ToList()

                            If vchn.Count = 0 Then
                                ' 未処理の場合

                                ' 未処理のリストに追加する。
                                ChangePropagation = New TChange(asn1, ref_asn.VarRef, ref_asn, GetPreConditionClean(asn1))
                                afr.DstChangeList.Add(ChangePropagation)

                                Unprocessed.Add(ChangePropagation)
                                vchange_all.Add(ChangePropagation)

                                '-------------------------------------------------- 値が変化した変数、代入された場所、代入時の条件を表示する。
                            End If
                        Next
                    End If
                End If
            Next
        Loop

        ' 影響され得る文を有効な文のリストに入れる
        For Each stmt_list In dic_may_be_affected_stmt_list.Values
            For Each stmt_f In stmt_list
                If Not valid_stmt.ContainsKey(stmt_f) Then
                    ' 有効な文のリストにない場合

                    valid_stmt.Add(stmt_f, stmt_f)
                End If
            Next
        Next

        ' 有効な文を波及させる。
        PropagateValidStmt(valid_stmt)

        ' データフローのソースを作る。
        MakeDataflowSrc(prj1, valid_stmt)
    End Sub

    ' 参照のされ方から相対レベルを得る。
    Public Function RelLevelByRefType(ref_type As ERefPathType) As Integer
        Select Case ref_type
            Case ERefPathType.SelfField, ERefPathType.PrevField, ERefPathType.局所変数
                Return 0
            Case ERefPathType.ParentField
                Return 1
            Case ERefPathType.SelfFieldChild
                Return -1
            Case Else
                Debug.WriteLine("")
                Return 0
        End Select

    End Function

    ' レベルごとに影響され得る参照をセットする。
    Public Sub MakeAffectedRefLevel(root_change As TChange)
        Dim level1 As Integer, afr_list As TList(Of TAffectedRef)

        dicAffectedRefLevelSelf = New Dictionary(Of Integer, TList(Of TAffectedRef))()
        dicAffectedRefLevel = New Dictionary(Of Integer, TList(Of TAffectedRef))()


        For Each afr In root_change.AffectedRefList
            level1 = RelLevelByRefType(afr.RefTypeAfr)

            If dicAffectedRefLevelSelf.ContainsKey(level1) Then

                afr_list = dicAffectedRefLevelSelf(level1)
                If Not afr_list.Contains(afr) Then
                    afr_list.Add(afr)
                End If
            Else

                afr_list = New TList(Of TAffectedRef)()
                afr_list.Add(afr)
                dicAffectedRefLevelSelf.Add(level1, afr_list)
            End If
        Next

        Dim idx As Integer

        For idx = 1 To 2
            Dim changed As Boolean
            Dim cur_level As Integer, dst_level As Integer
            Dim dic_affected_ref_level As Dictionary(Of Integer, TList(Of TAffectedRef))

            If idx = 1 Then
                dic_affected_ref_level = dicAffectedRefLevelSelf

                MinLevel = 0
                MaxLevel = 0
            Else
                dic_affected_ref_level = dicAffectedRefLevel
                dic_affected_ref_level.Add(1, New TList(Of TAffectedRef)(dicAffectedRefLevelSelf(1)))

                MinLevel = 1
                MaxLevel = 1
            End If

            Do While True

                changed = False

                ' すべてのレベルに対し
                For cur_level = MinLevel To MaxLevel

                    ' このレベルの影響され得る参照のリストを得る。
                    afr_list = dic_affected_ref_level(cur_level)

                    ' このレベルの影響され得るすべての参照に対し
                    For Each afr1 In afr_list

                        ' この参照から影響され得るすべての代入に対し
                        For Each change2 In afr1.DstChangeList

                            ' この代入から影響され得るすべての参照に対し
                            For Each afr2 In change2.AffectedRefList

                                If UpFor(afr2.StmtAfr) Is Nothing Then
                                    ' For文の中にある場合

                                    dst_level = cur_level - 1 + RelLevelByRefType(afr2.RefTypeAfr)
                                Else
                                    ' For文の中にない場合

                                    dst_level = cur_level + RelLevelByRefType(afr2.RefTypeAfr)
                                End If

                                If idx = 1 OrElse dst_level <= 1 Then

                                    If dic_affected_ref_level.ContainsKey(dst_level) Then
                                        ' このレベルの影響され得る参照のリストがある場合

                                        afr_list = dic_affected_ref_level(dst_level)
                                        If Not afr_list.Contains(afr2) Then
                                            ' このレベルの影響され得る参照のリストに含まれない場合

                                            changed = True
                                            afr_list.Add(afr2)
                                        End If
                                    Else
                                        ' このレベルの影響され得る参照のリストがない場合

                                        changed = True

                                        ' このレベルの影響され得る参照のリストを作る
                                        afr_list = New TList(Of TAffectedRef)()
                                        afr_list.Add(afr2)
                                        dic_affected_ref_level.Add(level1, afr_list)
                                    End If
                                End If
                            Next
                        Next
                    Next
                Next

                MinLevel = Aggregate x In dic_affected_ref_level.Keys Into Min(x)
                MaxLevel = Aggregate x In dic_affected_ref_level.Keys Into Max(x)

                If Not changed Then
                    ' 変化しなかった場合

                    Exit Do
                End If
            Loop
        Next

    End Sub

    ' データフローのソースを作る。
    Public Sub MakeDataflowSrc(prj1 As TProject, valid_stmt As Dictionary(Of TStatement, TStatement))
        ' 同期が必要なフィールドを探す
        Dim find_sync_field As New TFindSyncField

        find_sync_field.ValidStmt = valid_stmt
        find_sync_field.NaviFunction(RuleCp, Nothing)
        SyncFldList = find_sync_field.SyncFldList

        ' 並列処理の文を探す
        Dim nav_parallel_for_each As New TNaviParallelForEach
        nav_parallel_for_each.NaviFunction(RuleCp, Nothing)

        For Each call_f In nav_parallel_for_each.ParallelForEachList
            Dim children_fld As TField

            children_fld = CType(CType(call_f.AppCall.ArgApp(0), TDot).VarRef, TField)
            MakeParallelForEach(prj1, RuleCp, ChangeableFld, SyncFldList, call_f, children_fld)
        Next

        ' 規則内のすべての文を得る
        Dim nav_all_stmt As New TNaviAllStmt
        nav_all_stmt.NaviFunction(RuleCp, Nothing)

        ' _Set_* の呼び出しのソースを作る。
        Dim vasn = From x In nav_all_stmt.AllStmts Where TypeOf (x) Is TAssignment Select CType(x, TAssignment)
        For Each x In vasn
            If TypeOf x.RelAsn.ArgApp(0) Is TDot Then
                Dim dot1 As TDot = CType(x.RelAsn.ArgApp(0), TDot)

                If dot1.TrmDot Is Nothing Then
                    ' 自身のフィールドへの代入の場合

                    Dim fld1 As TField = CType(dot1.VarRef, TField)

                    Dim if_blc = CType((From o In TNaviUp.AncestorList(x) Where TypeOf o Is TIfBlock AndAlso CType(o, TIfBlock).WithIf IsNot Nothing).First(), TIfBlock)

                    Dim with_cls As TClass = if_blc.WithIf.TypeTrm
                    Dim set_fnc As TFunction = TProject.FindFieldFunction(with_cls, "_Set_" + fld1.NameVar, New TList(Of TTerm)())
                    If set_fnc IsNot Nothing Then

                        x.AfterSrc += "." + set_fnc.NameVar + "()" + vbCrLf
                    End If

                Else
                    ' 特定の子のフィールドへの代入の場合
                End If
            End If
        Next

        For Each stmt_f In nav_all_stmt.AllStmts
            If TypeOf stmt_f Is TVariableDeclaration Then
                stmt_f.ValidStmt = True
            Else
                stmt_f.ValidStmt = valid_stmt.ContainsKey(stmt_f)
            End If
        Next

        ' 親の文が無効な文は無効とする。
        Dim nav_set_valid_stmt As New TNaviSetValidStmt
        nav_set_valid_stmt.NaviFunction(RuleCp, Nothing)

        ' 同期用のクラスのソースを作る
        SyncClassSrc = MakeSyncClassSrc(prj1, ChangeableFld, SyncFldList)

        ' 規則のソースを作る
        MakeRuleSrc(prj1, ChangeableFld)
    End Sub

    ' タスクの起動のコードを作る
    Public Sub MakeInvokeCode(change As TChange, ref_type As ERefPathType)
        Dim invoke_str As String = Nothing

        If change.AsnChn Is Nothing Then
            Exit Sub
        End If

        Select Case ref_type
            Case ERefPathType.ParentField
                ' 親のフィールドを参照している場合

                ' 子のタスクが起動していなければ、起動する。
                invoke_str = "' 子のタスクが起動していなければ、起動する。"

            Case ERefPathType.SelfFieldChild
                ' 不特定の子のフィールドを参照している場合

                ' 親のタスクが起動していなければ、起動する。
                invoke_str = "' 親のタスクが起動していなければ、起動する。"

            Case ERefPathType.PrevField
                ' 直前のフィールドを参照している場合

                ' 次のタスクが起動していなければ、起動する。
                invoke_str = "' 次のタスクが起動していなければ、起動する。"
        End Select

        If invoke_str IsNot Nothing AndAlso change.AsnChn.AfterSrc.IndexOf(invoke_str) = -1 Then

            change.AsnChn.AfterSrc += invoke_str + vbCrLf
        End If
    End Sub

    ' 規則のソースを作る
    Public Sub MakeRuleSrc(prj1 As TProject, changeable_fld As TField)

        Dim self_var As TVariable, sync_var As TVariable

        self_var = RuleCp.ArgFnc(0)
        sync_var = New TVariable("_sync", New TClass(ProjectDtf, SyncClassName))

        RuleCp.BlcFnc.StmtBlc(0).BeforeSrc = String.Format("Dim {0} As {1} = CType(_sync.SelfSync,{1})", self_var.NameVar, self_var.TypeVar.NameVar)

        RuleCp.ArgFnc.RemoveAt(0)
        RuleCp.ArgFnc.Add(sync_var)

        For Each sync_fld In SyncFldList
            Dim signal_name As String = "Wait_" + sync_fld.FullFldName()

            For Each ref1 In sync_fld.RefVar
                If ref1.FunctionTrm Is RuleCp Then
                    Dim stmt1 As TStatement = UpStmt(ref1)
                    If ref1.DefRef Then
                        Debug.Assert(TypeOf stmt1 Is TAssignment)

                        Dim sync_set As String = String.Format("CType(.Sync, {0}).{1}.Set()", SyncClassName, signal_name)
                        If stmt1.ValidStmt Then
                            ' 有効な文の場合

                            stmt1.AfterSrc += sync_set + vbCrLf
                        Else
                            ' 無効な文の場合

                            Dim stmt2 As TStatement = stmt1, up_stmt As TStatement

                            up_stmt = UpStmtProper(stmt2.ParentStmt)
                            Do While up_stmt IsNot Nothing

                                If up_stmt.ValidStmt Then
                                    ' 有効の場合

                                    If Not (TypeOf up_stmt Is TIf OrElse TypeOf up_stmt Is TIfBlock OrElse TypeOf up_stmt Is TSelect OrElse TypeOf up_stmt Is TCase) Then

                                        Debug.WriteLine("sync set {0}", up_stmt.GetType())
                                        Debug.Assert(False)
                                    End If
                                    If up_stmt.AfterSrc = "" OrElse up_stmt.AfterSrc.IndexOf(sync_set) = -1 Then
                                        up_stmt.AfterSrc += sync_set + vbCrLf
                                    End If
                                    Exit Do
                                End If
                                up_stmt = UpStmtProper(up_stmt.ParentStmt)
                            Loop
                        End If
                    Else
                        Debug.Assert(TypeOf stmt1 Is TAssignment)

                        Dim ref_type = TDataflow.GetRefType(ref1)
                        Select Case ref_type
                            Case ERefPathType.ParentField
                                stmt1.BeforeSrc = String.Format("CType(._ParentControl.Sync, {0}).{1}.WaitOne()", SyncClassName, signal_name)

                            Case ERefPathType.SelfFieldChild
                                Dim sw1 As New StringWriter

                                Dim children_fld As TField = AsnAggregateChildrenFld(CType(stmt1, TAssignment))

                                sw1.WriteLine("WaitAll(From _x In .{0} Select CType(_x.Sync, {1}).{2})", children_fld.NameVar, SyncClassName, WaitSyncFldName(sync_fld))

                                stmt1.BeforeSrc = sw1.ToString()

                            Case ERefPathType.PrevField
                                stmt1.BeforeSrc = String.Format("CType(._Prev.Sync, {0}).{1}.WaitOne()", SyncClassName, signal_name)

                            Case ERefPathType.その他
                                stmt1.BeforeSrc = "'その他の待ち"

                        End Select
                    End If
                End If
            Next
        Next

        'Dim RuleCodeGen As TBasicCodeGenerator = New TBasicCodeGenerator(prj1, prj1.ParsePrj)
        'RuleCodeGen.FncSrc(RuleCp)
        'RuleSW.WriteLine(RuleCodeGen.MakeSrcText())

        RuleCp.ArgFnc.RemoveAt(0)
        RuleCp.ArgFnc.Add(self_var)

    End Sub

    ' 値が変化し得るフィールドのリストを得る
    Public Sub SetChangeableFldList(prj1 As TProject)
        Dim nav_changeable_field As TNaviChangeableField

        PrjDF = prj1
        GlobalRule = prj1.FindFunctionByName("TNaviView", "GlobalRule")

        nav_changeable_field = New TNaviChangeableField()
        nav_changeable_field.NaviFunction(GlobalRule, Nothing)

        ChangeableFldList = New TList(Of TField)(nav_changeable_field.dicChangeableFld.Values)

    End Sub

    ' 同期用のクラスのソースを作る
    Public Function MakeSyncClassSrc(prj1 As TProject, changeable_fld As TField, sync_fld_list As TList(Of TField)) As String
        Dim sw As New StringWriter

        sw.WriteLine("Public Class {0}", SyncClassName)
        sw.WriteLine("Inherits TSync")
        sw.WriteLine("Public SelfSync As Object")

        ' 値が変更するフィールドのシグナル
        For Each sync_fld In sync_fld_list

            sw.WriteLine("Public {0} As New ManualResetEvent(False)", WaitSyncFldName(sync_fld))
        Next
        sw.WriteLine("Public Sub New(self1 As Object, parent1 As Object, prev1 As Object)")
        sw.WriteLine("SelfSync = self1")
        sw.WriteLine("SelfSync._ParentControl = parent1")
        sw.WriteLine("SelfSync._Prev = prev1")
        sw.WriteLine("self1.Sync = Me")
        sw.WriteLine("End Sub")
        sw.WriteLine("End Class")

        Return sw.ToString()
    End Function

    ' 同期のイベントのフィールドの名前を返す
    Function WaitSyncFldName(sync_fld As TField) As String
        Return "Wait_" + sync_fld.FullFldName()
    End Function

    ' 集計の代入文の子のフィールドを返す
    Function AsnAggregateChildrenFld(asn1 As TAssignment) As TField
        Dim aggr1 As TAggregate = asn1.RelAsn.ArgApp(1)
        Dim dot1 As TDot = CType(aggr1.SeqQry, TDot)

        Debug.Assert(dot1.TrmDot Is Nothing)

        Return CType(dot1.VarRef, TField)
    End Function

    ' 並行処理のソースを作る
    Public Sub MakeParallelForEach(prj1 As TProject, rule_cp As TFunction, changeable_fld As TField, sync_fld_list As TList(Of TField), call1 As TCall, children_fld As TField)
        Dim sw As New StringWriter

        sw.WriteLine("Dim _prev As Object = Nothing")
        sw.WriteLine("For Each x In .{0}", children_fld.NameVar)
        sw.WriteLine("x.Sync = New {0}(x, _current, _prev)", SyncClassName)
        sw.WriteLine("_prev = x")
        sw.WriteLine("Next")

        sw.WriteLine("For Each x In (From y In .{0} Select y.Sync)", children_fld.NameVar)
        sw.WriteLine("Dim t = New Thread(AddressOf {0})", rule_cp.NameVar)
        sw.WriteLine("t.Start(x)")
        sw.WriteLine("Next")

        call1.BeforeSrc = sw.ToString()
    End Sub

    Public Shared Function GetRefType(trm1 As TReference) As ERefPathType

        If TypeOf trm1 Is TDot Then
            Dim dot1 As TDot, dot2 As TDot, ref2 As TReference

            dot1 = CType(trm1, TDot)
            If dot1.TrmDot Is Nothing Then
                If dot1.NameRef.StartsWith(ParentPrefix) Then
                    Return ERefPathType.Parent
                Else
                    Return ERefPathType.SelfField
                End If
            Else

                If TypeOf dot1.TrmDot Is TDot Then
                    dot2 = CType(dot1.TrmDot, TDot)

                    If dot2.TrmDot Is Nothing Then

                        If dot2.NameRef.StartsWith(ParentPrefix) Then
                            Return ERefPathType.ParentField
                        End If

                        If dot2.NameRef.StartsWith(PrevPrefix) Then
                            Return ERefPathType.PrevField
                        End If
                    End If

                ElseIf TypeOf dot1.TrmDot Is TReference Then

                    ref2 = CType(dot1.TrmDot, TReference)
                    If ref2.NameRef.StartsWith(ChildPrefix) Then
                        Return ERefPathType.SelfFieldChild
                    End If
                End If
            End If

        ElseIf TypeOf trm1 Is TReference Then
            Dim ref1 As TReference

            ref1 = CType(trm1, TReference)
            If ref1.NameRef.StartsWith(CurrentPrefix) Then
                Return ERefPathType.Self
            Else
                Return ERefPathType.局所変数
            End If
        End If

        Return ERefPathType.その他
    End Function

    ' 条件を追加する
    Public Sub AddCondition(stmt1 As TStatement, up_stmt As TStatement, is_parent As Boolean, and1 As TApply)
        Dim if_blc As TIfBlock, case1 As TCase, if1 As TIf, not1 As TApply, cnd1 As TTerm
        Dim trans As TTransRelative, is_invalid As Boolean

        If TypeOf up_stmt Is TIf Then
            Debug.Assert(TypeOf stmt1 Is TIfBlock)
            if_blc = CType(stmt1, TIfBlock)

        ElseIf TypeOf up_stmt Is TIfBlock Then
            if_blc = CType(up_stmt, TIfBlock)
            if1 = CType(UpStmt(up_stmt.ParentStmt), TIf)
            For Each _child In if1.IfBlc
                If is_parent Then
                    trans = New TTransRelative(E相対位置.親)
                    cnd1 = trans.TransTrm(_child.CndIf, Nothing)
                    is_invalid = trans.無効
                Else
                    cnd1 = Sys.CopyTrm(_child.CndIf, Nothing)
                    is_invalid = False
                End If

                If _child IsNot if_blc Then
                    ' 手前のIfブロックの場合

                    If Not is_invalid Then

                        ' 否定の条件を追加する
                        not1 = TApply.NewOpr(EToken.eNot)

                        not1.AddInArg(cnd1)
                    End If
                Else
                    ' 現在のIfブロックの場合

                    If Not is_invalid Then

                        ' 条件を追加する
                        and1.AddInArg(cnd1)
                    End If

                    Exit For
                End If
            Next

        ElseIf TypeOf up_stmt Is TSelect Then
            Debug.Assert(TypeOf stmt1 Is TCase)
            case1 = CType(stmt1, TCase)

        ElseIf TypeOf up_stmt Is TCase Then
        ElseIf TypeOf up_stmt Is TFor Then
        End If
    End Sub

    ' 余分な条件を取り除いた前提条件を返す
    Public Function GetPreConditionClean(stmt As TStatement) As TApply
        Dim pre_cond As TApply

        ' 文を実行する前提条件を返す
        pre_cond = TApply.NewOpr(EToken.eAnd)
        CalcPreCondition(stmt, False, pre_cond)

        ' 余分な条件を取り除く
        CleanCondition(pre_cond)

        Return pre_cond
    End Function

    ' 文を実行する前提条件を返す
    Public Sub CalcPreCondition(stmt1 As TStatement, is_parent As Boolean, and1 As TApply)
        Dim up_stmt As TStatement, up_obj As Object

        If TypeOf stmt1 Is TAssignment Then
        ElseIf TypeOf stmt1 Is TCall Then
        ElseIf TypeOf stmt1 Is TIf Then
        ElseIf TypeOf stmt1 Is TIfBlock Then
        ElseIf TypeOf stmt1 Is TSelect Then
        ElseIf TypeOf stmt1 Is TCase Then
        ElseIf TypeOf stmt1 Is TFor Then
        Else
            Debug.Assert(False)
        End If

        If stmt1.TypeStmt = EToken.eProtected Then
            Debug.Assert(False)
        End If

        up_obj = UpStmt(stmt1.ParentStmt)
        If TypeOf up_obj Is TStatement Then
            up_stmt = CType(up_obj, TStatement)

            '            Debug.WriteLine("前提条件 {0} > {1}", stmt1.TypeStmt, up_stmt.ToString())

            AddCondition(stmt1, up_stmt, is_parent, and1)

            If TypeOf up_stmt Is TFor Then
                Debug.Assert(Not is_parent)
                CalcPreCondition(up_stmt, True, and1)
            Else
                CalcPreCondition(up_stmt, is_parent, and1)
            End If

        ElseIf TypeOf up_obj Is TFunction Then

        Else
            Debug.Assert(False)
        End If
    End Sub

    ' 2個の論理式から得られる推論の結果を得る
    Public Function BinomialInference(P As TApply, Q As TApply) As EBinomialInference
        Dim A As TClass, B As TClass, A_subset_B As Boolean, B_subset_A As Boolean, A_EQ_B As Boolean

        Select Case P.TypeApp
            Case EToken.eInstanceof
                A = CType(P.ArgApp(1), TReference).VarRef
                B = CType(Q.ArgApp(1), TReference).VarRef

                A_subset_B = A.IsSubsetOf(B)
                B_subset_A = B.IsSubsetOf(A)
                If Not P.Negation Then
                    If Not Q.Negation Then
                        '     a ∊ A ,     a ∊ B		A=Bならa ∊ Bは不要、A ⋂ B = ∅ なら矛盾、A ⊆ B ならa ∊ Bは不要、B ⊆ A ならa ∊ Aは不要
                        If A Is B Then
                            Return EBinomialInference.SecondRedundant
                        ElseIf A_subset_B Then
                            Return EBinomialInference.SecondRedundant
                        ElseIf B_subset_A Then
                            Return EBinomialInference.FirstRedundant
                        Else
                            Return EBinomialInference.矛盾
                        End If
                    Else
                        '     a ∊ A , Not a ∊ B		A ⊆ B なら矛盾、A ⋂ B = ∅ ならNot a ∊ Bは不要

                        If A Is B OrElse A_subset_B Then
                            Return EBinomialInference.矛盾
                        ElseIf Not B_subset_A Then
                            Return EBinomialInference.SecondRedundant
                        Else
                            Return EBinomialInference.Unknown
                        End If
                    End If
                Else
                    If Not Q.Negation Then
                        ' Not a ∊ A ,     a ∊ B		B ⊆ A なら矛盾、A ⋂ B = ∅ ならNot a ∊ Aは不要

                        If A Is B OrElse B_subset_A Then
                            Return EBinomialInference.矛盾
                        ElseIf Not A_subset_B Then
                            Return EBinomialInference.FirstRedundant
                        Else
                            Return EBinomialInference.Unknown
                        End If
                    Else
                        ' Not a ∊ A , Not a ∊ B		A ⊆ B ならNot a ∊ Aは不要、B ⊆ A ならNot a ∊ Bは不要

                        If A Is B OrElse A_subset_B Then
                            Return EBinomialInference.FirstRedundant
                        ElseIf B_subset_A Then
                            Return EBinomialInference.FirstRedundant
                        Else
                            Return EBinomialInference.Unknown
                        End If
                    End If
                End If

            Case EToken.eIs, EToken.eEq
                Debug.WriteLine("is {0} {1} {2} {3}", P.Negation, P.ArgApp(1), Q.Negation, Q.ArgApp(1))
                A_EQ_B = Sys.IsEqTrm(P.ArgApp(1), Q.ArgApp(1))
                If Not P.Negation Then
                    If Not Q.Negation Then
                        '     a = A ,     a = B		A <> B なら矛盾、A=Bならa = Bは不要

                        If A_EQ_B Then
                            Return EBinomialInference.SecondRedundant
                        Else
                            Return EBinomialInference.Unknown
                        End If
                    Else
                        '     a = A , Not a = B		A = B なら矛盾、A <> BならNot a = Bは不要

                        If A_EQ_B Then
                            Return EBinomialInference.矛盾
                        Else
                            Return EBinomialInference.Unknown
                        End If
                    End If
                Else
                    If Not Q.Negation Then
                        ' Not a = A ,     a = B		A = B なら矛盾、A <> BならNot a = Aは不要

                        If A_EQ_B Then
                            Return EBinomialInference.矛盾
                        Else
                            Return EBinomialInference.Unknown
                        End If
                    Else
                        ' Not a = A , Not a = B

                        Return EBinomialInference.Unknown
                    End If
                End If

            Case EToken.eLT
                Debug.WriteLine("eq {0} {1} {2} {3}", P.Negation, P.ArgApp(1), Q.Negation, Q.ArgApp(1))
                If Not P.Negation Then
                    If Not Q.Negation Then
                        '     a < A ,     a < B		A <= Bならa < Bは不要、B <= Aならa < Bは不要
                    Else
                        '     a < A , Not a < B		A <= B なら矛盾
                    End If
                Else
                    If Not Q.Negation Then
                        ' Not a < A , a < B			B <= A なら矛盾
                    Else
                        ' Not a < A ,     Not a < B	A <= Bなら Not a < Aは不要、B <= AならNot a < Bは不要
                    End If
                End If
        End Select

        Return EBinomialInference.Unknown
    End Function

    ' 余分な条件を取り除く
    Public Sub CleanCondition(and1 As TApply)
        Dim i1 As Integer, i2 As Integer, app1 As TApply, app2 As TApply, inf As EBinomialInference
        Dim is_redundant As Boolean()

        For Each trm In and1.ArgApp
            If TypeOf trm Is TApply Then
                app1 = CType(trm, TApply)
                Select Case app1.TypeApp
                    Case EToken.eIsNot
                        app1.Negation = Not app1.Negation
                        app1.TypeApp = EToken.eIs

                    Case EToken.eNE
                        app1.Negation = Not app1.Negation
                        app1.TypeApp = EToken.eEq
                End Select
            End If
        Next

        ReDim is_redundant(and1.ArgApp.Count - 1)

        For i1 = 0 To and1.ArgApp.Count - 1
            If TypeOf and1.ArgApp(i1) Is TApply Then
                app1 = CType(and1.ArgApp(i1), TApply)
                Select Case app1.TypeApp
                    Case EToken.eInstanceof, EToken.eIs, EToken.eEq

                        For i2 = i1 + 1 To and1.ArgApp.Count - 1
                            If TypeOf and1.ArgApp(i2) Is TApply Then
                                app2 = CType(and1.ArgApp(i2), TApply)

                                If app2.TypeApp = app1.TypeApp AndAlso Sys.IsEqTrm(app1.ArgApp(0), app2.ArgApp(0)) Then
                                    ' 対象が同じ場合

                                    ' 2個の論理式から得られる推論の結果を得る
                                    inf = BinomialInference(app1, app2)
                                    Select Case inf
                                        Case EBinomialInference.矛盾
                                        Case EBinomialInference.FirstRedundant
                                            is_redundant(i1) = True
                                        Case EBinomialInference.SecondRedundant
                                            is_redundant(i2) = True
                                    End Select
                                End If
                            End If
                        Next
                End Select
            End If
        Next

        For i1 = and1.ArgApp.Count - 1 To 0 Step -1
            If is_redundant(i1) Then
                and1.ArgApp.RemoveAt(i1)
            End If
        Next

    End Sub

    ' P ∧ Q が無矛盾なら true を返す。
    Public Function Consistent(P As TApply, Q As TApply) As Boolean
        Dim i1 As Integer, i2 As Integer, app1 As TApply, app2 As TApply, inf As EBinomialInference

        i1 = 0
        Do While i1 < P.ArgApp.Count
            If TypeOf P.ArgApp(i1) Is TApply Then
                app1 = CType(P.ArgApp(i1), TApply)
                Select Case app1.TypeApp
                    Case EToken.eInstanceof, EToken.eIs, EToken.eEq

                        i2 = 0
                        Do While i2 < Q.ArgApp.Count
                            If TypeOf Q.ArgApp(i2) Is TApply Then
                                app2 = CType(Q.ArgApp(i2), TApply)

                                If app2.TypeApp = app1.TypeApp AndAlso Sys.IsEqTrm(app1.ArgApp(0), app2.ArgApp(0)) Then
                                    ' 対象が同じ場合

                                    ' 2個の論理式から得られる推論の結果を得る
                                    inf = BinomialInference(app1, app2)
                                    If inf = EBinomialInference.矛盾 Then
                                        Return False
                                    End If
                                End If
                            End If
                            i2 += 1
                        Loop
                End Select
            End If

            i1 += 1
        Loop

        Return True
    End Function

    Public Shared Function GetFieldBy相対位置(cls1 As TClass, rel_pos As E相対位置) As TField
        Dim fld1 As TField, prefix As String = Nothing

        Select Case rel_pos
            Case E相対位置.親
                prefix = TDataflow.ParentPrefix
            Case E相対位置.直前
                prefix = TDataflow.PrevPrefix

            Case Else
                Return Nothing
        End Select

        For Each fld In cls1.FldCla
            If fld.NameVar.StartsWith(prefix) Then
                Return fld
            End If
        Next

        For Each cla_f In cls1.SuperClassList
            fld1 = GetFieldBy相対位置(cla_f, rel_pos)
            If fld1 IsNot Nothing Then
                Return fld1
            End If
        Next

        Debug.Assert(False)
        Return Nothing
    End Function
End Class

Public Enum E相対位置
    自身
    親
    直前
End Enum

' 参照パス
Public Enum ERefPathType
    Self
    Parent
    Prev
    App

    SelfField
    ParentField
    PrevField
    AppField

    SelfFieldChild
    SelfFieldChildField
    SelfFieldChildFieldChild

    局所変数
    その他
End Enum

Public Enum EBinomialInference
    矛盾
    FirstRedundant
    SecondRedundant
    Unknown
End Enum

' 参照パス
Public Class TRefPath
    Public RefPathType As ERefPathType
    Public FieldPath As New List(Of TField)
End Class

Public Class TChange
    Public AsnChn As TAssignment
    Public VariableChn As TVariable      ' 値が変化した変数
    Public RefChn As TReference           ' 値の代入箇所
    Public ConditionChn As TApply     ' 値が変化したときの条件
    Public AffectedRefList As New TList(Of TAffectedRef)

    Public Sub New(asn1 As TAssignment, var1 As TVariable, ref1 As TReference, cnd1 As TApply)
        AsnChn = asn1
        VariableChn = var1
        RefChn = ref1
        ConditionChn = cnd1
    End Sub
End Class

Public Class TAffectedRef
    Public SrcChange As TChange
    Public RefAfr As TReference
    Public RefTypeAfr As ERefPathType
    Public StmtAfr As TStatement
    Public DstChangeList As New TList(Of TChange)

    Public Sub New(src_change As TChange, ref1 As TReference, ref_type As ERefPathType, stmt1 As TStatement)
        src_change.AffectedRefList.Add(Me)
        SrcChange = src_change

        RefAfr = ref1
        RefTypeAfr = ref_type
        StmtAfr = stmt1
    End Sub
End Class

Public Class TTrans

    '-------------------------------------------------------------------------------- 変換
    Public Overridable Function TransTrm(trm1 As TTerm, arg1 As Object) As TTerm
        Dim trm2 As TTerm = trm1

        If trm1 IsNot Nothing Then
            Try
                If TypeOf trm1 Is TConstant Then
                ElseIf TypeOf trm1 Is TArray Then
                    Debug.Assert(False)
                ElseIf TypeOf trm1 Is TDot Then
                    trm2 = TransDot(CType(trm1, TDot), arg1)
                ElseIf TypeOf trm1 Is TReference Then
                    trm2 = TransRef(CType(trm1, TReference), arg1)
                ElseIf trm1.IsApp() Then
                    trm2 = TransApp(CType(trm1, TApply), arg1)
                ElseIf trm1.IsLog() Then
                    trm2 = TransApp(CType(trm1, TApply), arg1)
                ElseIf TypeOf trm1 Is TParenthesis Then
                    trm2 = TransPar(CType(trm1, TParenthesis), arg1)
                ElseIf TypeOf trm1 Is TFrom Then
                    Debug.Assert(False)
                ElseIf TypeOf trm1 Is TAggregate Then
                    Debug.Assert(False)
                Else
                    Debug.Assert(False)
                End If
            Catch ex As TError
                Debug.Assert(False)
            End Try
        End If


        Return trm2
    End Function

    Public Overridable Function TransDot(dot1 As TDot, arg1 As Object) As TTerm
        Return Sys.CopyDot(dot1, Nothing)
    End Function

    Public Overridable Function TransRef(ref1 As TReference, arg1 As Object) As TTerm
        Return Sys.CopyRef(ref1, Nothing)
    End Function

    Public Overridable Function TransApp(app1 As TApply, arg1 As Object) As TTerm
        Return Sys.CopyApp(app1, Nothing)
    End Function

    Public Overridable Function TransPar(par1 As TParenthesis, arg1 As Object) As TTerm
        Return Sys.CopyPar(par1, Nothing)
    End Function

    Public Overridable Function TransCns(cns1 As TConstant, arg1 As Object) As TTerm
        Return Sys.CopyCns(cns1, Nothing)
    End Function
End Class

' _currentを .Parent や .Prev に変換する
Public Class TTransRelative
    Inherits TTrans
    Dim 相対位置 As E相対位置
    Public 無効 As Boolean

    Public Sub New(a相対位置 As E相対位置)
        相対位置 = a相対位置
        無効 = False
    End Sub

    Public Overrides Function TransDot(dot1 As TDot, arg1 As Object) As TTerm
        Dim 参照のされ方 As ERefPathType
        Dim parent_fld As TField, prev_fld As TField, F As TField

        If Not 無効 Then

            参照のされ方 = TDataflow.GetRefType(dot1)

            If 参照のされ方 = ERefPathType.SelfField Then

                Debug.Assert(dot1.TrmDot Is Nothing)
                F = CType(dot1.VarRef, TField)
                Select Case 相対位置
                    Case E相対位置.親
                        ' .F を .Parent.Fにする。

                        parent_fld = TDataflow.GetFieldBy相対位置(F.ClaFld, 相対位置)
                        Return New TDot(New TDot(Nothing, parent_fld), F)

                    Case E相対位置.直前
                        ' .F を .Prev.Fにする。

                        prev_fld = TDataflow.GetFieldBy相対位置(F.ClaFld, 相対位置)
                        Return New TDot(New TDot(Nothing, prev_fld), F)
                End Select
            End If
        End If

        無効 = True
        Return Nothing
    End Function

    Public Overrides Function TransRef(ref1 As TReference, arg1 As Object) As TTerm
        Dim 参照のされ方 As ERefPathType
        Dim parent_fld As TField, prev_fld As TField

        If Not 無効 Then

            参照のされ方 = TDataflow.GetRefType(ref1)
            If 参照のされ方 = ERefPathType.Self Then
                Select Case 相対位置
                    Case E相対位置.親
                        ' _current を .Parent にする。

                        parent_fld = TDataflow.GetFieldBy相対位置(ref1.VarRef.TypeVar, 相対位置)
                        Return New TDot(Nothing, parent_fld)

                    Case E相対位置.直前
                        ' _current を .Prev にする。

                        prev_fld = TDataflow.GetFieldBy相対位置(ref1.VarRef.TypeVar, 相対位置)
                        Return New TDot(Nothing, prev_fld)
                End Select
            End If
        End If

        無効 = True
        Return Nothing
    End Function
End Class

' 局所変数の参照を列挙する
Public Class TEnumInnerLocalRef
    Inherits TNavi

    Public vVar As New TList(Of TVariable)
    Public vNewVar As New TList(Of TVariable)

    Public Overrides Function StartReference(ref1 As TReference, arg1 As Object) As Object
        If Not ref1.DefRef AndAlso Not ref1.NameRef.StartsWith(TDataflow.CurrentPrefix) AndAlso Not ref1.NameRef.StartsWith(TDataflow.ChildPrefix) Then
            ' 代入ではなく、自身や子でない場合

            If Not vVar.Contains(ref1.VarRef) Then
                ' 未処理の場合

                vVar.Add(ref1.VarRef)
                vNewVar.Add(ref1.VarRef)
            End If
        End If

        Return arg1
    End Function
End Class

' 内部の文を列挙する
Public Class TEnumInnerStmt
    Inherits TNavi
    Public vStmt As New TList(Of TStatement)

    Public Overrides Function StartStatement(stmt1 As TStatement, arg1 As Object) As Object
        If stmt1 IsNot Nothing Then
            vStmt.Add(stmt1)
        End If
        Return arg1
    End Function
End Class

Public Class TNaviChangeableField
    Inherits TNavi

    Public dicFld As New Dictionary(Of String, TField)
    Public dicChangeableFld As New Dictionary(Of String, TField)

    Public Overrides Function StartDot(dot1 As TDot, arg1 As Object) As Object
        Dim fld1 As TField

        If TypeOf dot1.VarRef Is TField AndAlso dot1.NameRef(0) <> "_"c AndAlso Not dicFld.ContainsKey(dot1.VarRef.ToString()) Then
            ' 未処理のフィールドの場合

            fld1 = CType(dot1.VarRef, TField)
            dicFld.Add(dot1.VarRef.ToString(), fld1)

            If fld1.ClaFld.KndCla = EClass.eClassCla OrElse fld1.ClaFld.KndCla = EClass.eStructCla Then

                Dim vref As TList(Of TReference)
                vref = New TList(Of TReference)(From ref1 In dot1.VarRef.RefVar Where ref1.DefRef)
                If vref.Count = 0 Then
                    ' 代入されない場合

                    Debug.WriteLine("可変フィールド {0}", dot1.VarRef.ToString())
                    dicChangeableFld.Add(dot1.VarRef.ToString(), fld1)
                End If
            End If
        End If

        Return arg1
    End Function
End Class

' 同期が必要なフィールドを探す
' 自身のフィールドに代入し、親・子・直前のフィールドで参照されるフィールドを探す。
Public Class TFindSyncField
    Inherits TNavi

    Public ValidStmt As Dictionary(Of TStatement, TStatement)
    Public GetFld As New TList(Of TField)
    Public SetFld As New TList(Of TField)
    Public SyncFldList As New TList(Of TField)

    Public Overrides Function StartDot(dot1 As TDot, arg1 As Object) As Object
        Dim stmt1 As TStatement, ref_type As ERefPathType, fld1 As TField

        stmt1 = TDataflow.UpStmt(dot1)
        If ValidStmt.ContainsKey(stmt1) Then
            ' 有効な文の中の場合

            ref_type = TDataflow.GetRefType(dot1)
            Select Case ref_type
                Case ERefPathType.SelfField
                    fld1 = CType(dot1.VarRef, TField)
                    If dot1.DefRef AndAlso Not SetFld.Contains(fld1) Then
                        ' 自身のフィールドへの代入で、未処理の場合

                        SetFld.Add(fld1)

                        If GetFld.Contains(fld1) Then
                            ' 親・子・直前のフィールドで参照される場合

                            SyncFldList.Add(fld1)
                        End If
                    End If

                Case ERefPathType.ParentField, ERefPathType.SelfFieldChild, ERefPathType.PrevField

                    fld1 = CType(dot1.VarRef, TField)
                    If Not dot1.DefRef AndAlso Not GetFld.Contains(fld1) Then
                        ' 親・子・直前のフィールドで参照され、未処理の場合
                        GetFld.Add(fld1)

                        If SetFld.Contains(fld1) Then
                            ' 自身のフィールドへ代入される場合

                            SyncFldList.Add(fld1)
                        End If
                    End If
            End Select
        End If

        Return arg1
    End Function
End Class

' 並列処理の文を探す
Public Class TNaviParallelForEach
    Inherits TNavi

    Public ParallelForEachList As New TList(Of TCall)

    Public Overrides Sub NaviCall(call1 As TCall, arg1 As Object)
        Dim ref1 As TReference

        If TypeOf call1.AppCall.FncApp Is TReference Then
            ref1 = CType(call1.AppCall.FncApp, TReference)
            If ref1.VarRef.NameVar = "ParallelForEach" Then
                ParallelForEachList.Add(call1)
            End If
        End If
        '        NaviTerm(call1.AppCall, arg1)
    End Sub
End Class

Public Class TCopy
    Public dctVar As New Dictionary(Of TVariable, TVariable)
    Public CurFncCpy As TFunction
End Class

Public Class Sys
    Public Shared Function IsEqTrm(trm1 As TTerm, trm2 As TTerm) As Boolean
        Dim cns1 As TConstant, arr1 As TArray, dot1 As TDot, ref1 As TReference, app1 As TApply, opr1 As TApply, par1 As TParenthesis
        Dim cns2 As TConstant, arr2 As TArray, dot2 As TDot, ref2 As TReference, app2 As TApply, opr2 As TApply, par2 As TParenthesis
        Dim i1 As Integer

        If trm1 Is trm2 Then
            Return True
        End If

        If (trm1 Is Nothing) <> (trm2 Is Nothing) Then
            Return False
        End If

        If Not trm1.GetType() Is trm2.GetType() Then
            Return False
        End If

        If TypeOf trm1 Is TConstant Then
            cns1 = CType(trm1, TConstant)
            cns2 = CType(trm2, TConstant)

            Return cns1.NameRef = cns2.NameRef AndAlso cns1.TypeAtm = cns2.TypeAtm

        ElseIf TypeOf trm1 Is TArray Then
            arr1 = CType(trm1, TArray)
            arr2 = CType(trm2, TArray)
            If arr1.TrmArr.Count <> arr2.TrmArr.Count Then
                Return False
            End If
            For i1 = 0 To arr1.TrmArr.Count - 1
                If Not IsEqTrm(arr1.TrmArr(i1), arr2.TrmArr(i1)) Then
                    Return False
                End If
            Next
            Return True

        ElseIf TypeOf trm1 Is TDot Then
            dot1 = CType(trm1, TDot)
            dot2 = CType(trm2, TDot)
            Debug.Assert(dot1.VarRef IsNot Nothing AndAlso dot2.VarRef IsNot Nothing)
            Return dot1.VarRef Is dot2.VarRef AndAlso IsEqTrm(dot1.TrmDot, dot2.TrmDot)

        ElseIf TypeOf trm1 Is TReference Then
            ref1 = CType(trm1, TReference)
            ref2 = CType(trm2, TReference)

            Debug.Assert(ref1.VarRef IsNot Nothing AndAlso ref2.VarRef IsNot Nothing)
            Return ref1.VarRef Is ref2.VarRef

        ElseIf trm1.IsApp() Then
            app1 = CType(trm1, TApply)
            app2 = CType(trm2, TApply)
            If app1.TypeApp <> app2.TypeApp OrElse app1.KndApp <> app2.KndApp OrElse app1.ArgApp.Count <> app2.ArgApp.Count Then
                Return False
            End If

            If (app1.FncApp Is Nothing) <> (app2.FncApp Is Nothing) OrElse app1.FncApp IsNot Nothing AndAlso Not IsEqTrm(app1.FncApp, app2.FncApp) Then
                Return False
            End If

            For i1 = 0 To app1.ArgApp.Count - 1
                If Not IsEqTrm(app1.ArgApp(i1), app2.ArgApp(i1)) Then
                    Return False
                End If
            Next

            If Not app1.ClassApp Is app2.ClassApp Then
                Return False
            End If

            If Not app1.NewApp Is app2.NewApp Then
                Return False
            End If

            If (app1.IniApp Is Nothing) <> (app2.IniApp Is Nothing) OrElse app1.IniApp IsNot Nothing AndAlso Not IsEqTrm(app1.IniApp, app2.IniApp) Then
                Return False
            End If
            Return True

        ElseIf trm1.IsLog() Then
            opr1 = CType(trm1, TApply)
            opr2 = CType(trm2, TApply)

            If opr1.TypeApp <> opr2.TypeApp OrElse opr1.ArgApp.Count <> opr2.ArgApp.Count Then
                Return False
            End If

            For i1 = 0 To opr1.ArgApp.Count - 1
                If Not IsEqTrm(opr1.ArgApp(i1), opr2.ArgApp(i1)) Then
                    Return False
                End If
            Next
            Return True

        ElseIf TypeOf trm1 Is TParenthesis Then
            par1 = CType(trm1, TParenthesis)
            par2 = CType(trm2, TParenthesis)

            Return IsEqTrm(par1.TrmPar, par2.TrmPar)

        ElseIf TypeOf trm1 Is TFrom Then
            ' Fromの共通部分式は探さない。

            Return False

        Else
            Debug.Assert(False)
            Return False
        End If
    End Function

    Public Shared Function CopyTrm(trm1 As TTerm, cpy As TCopy) As TTerm
        Dim trm2 As TTerm = trm1

        If trm1 IsNot Nothing Then
            Try
                If TypeOf trm1 Is TConstant Then
                ElseIf TypeOf trm1 Is TArray Then
                    Debug.Assert(False)
                ElseIf TypeOf trm1 Is TDot Then
                    trm2 = CopyDot(CType(trm1, TDot), cpy)
                ElseIf TypeOf trm1 Is TReference Then
                    trm2 = CopyRef(CType(trm1, TReference), cpy)
                ElseIf trm1.IsApp() Then
                    trm2 = CopyApp(CType(trm1, TApply), cpy)
                ElseIf trm1.IsLog() Then
                    trm2 = CopyApp(CType(trm1, TApply), cpy)
                ElseIf TypeOf trm1 Is TParenthesis Then
                    trm2 = CopyPar(CType(trm1, TParenthesis), cpy)
                ElseIf TypeOf trm1 Is TFrom Then
                    trm2 = CopyFrom(CType(trm1, TFrom), cpy)
                ElseIf TypeOf trm1 Is TAggregate Then
                    trm2 = CopyAggregate(CType(trm1, TAggregate), cpy)
                Else
                    Debug.Assert(False)
                End If
            Catch ex As TError
                Debug.Assert(False)
            End Try
        End If

        Return trm2
    End Function

    Public Shared Function CopyFrom(from1 As TFrom, cpy As TCopy) As TFrom
        Dim from2 As New TFrom

        from2.SeqQry = CopyTrm(from1.SeqQry, cpy)
        from2.VarQry = CopyVar(from1.VarQry, cpy)
        from2.CndQry = CopyTrm(from1.CndQry, cpy)
        from2.SelFrom = CopyTrm(from1.SelFrom, cpy)
        from2.TakeFrom = CopyTrm(from1.TakeFrom, cpy)
        from2.InnerFrom = CopyTrm(from1.InnerFrom, cpy)

        Return from2
    End Function

    Public Shared Function CopyAggregate(agg1 As TAggregate, cpy As TCopy) As TAggregate
        Dim agg2 As New TAggregate

        agg2.SeqQry = CopyTrm(agg1.SeqQry, cpy)
        agg2.VarQry = CopyVar(agg1.VarQry, cpy)
        agg2.CndQry = CopyTrm(agg1.CndQry, cpy)
        agg2.IntoAggr = CopyTrm(agg1.IntoAggr, cpy)

        agg2.FunctionAggr = agg1.FunctionAggr

        Return agg2
    End Function

    Public Shared Function CopyDot(dot1 As TDot, cpy As TCopy) As TDot
        Dim dot2 As TDot, trm2 As TTerm

        trm2 = CopyTrm(dot1.TrmDot, cpy)
        dot2 = New TDot(trm2, dot1.VarRef)
        dot2.TypeDot = dot1.TypeDot
        dot2.TypeTrm = dot1.TypeTrm
        dot2.DefRef = dot1.DefRef

        If cpy IsNot Nothing Then

            dot2.VarRef.RefVar.Add(dot2)
            dot2.FunctionTrm = cpy.CurFncCpy
        End If

        Return dot2
    End Function

    Public Shared Function CopyRef(ref1 As TReference, cpy As TCopy) As TReference
        Dim ref2 As New TReference

        If ref1 Is Nothing Then
            Return Nothing
        End If

        ref2.NameRef = ref1.NameRef
        If ref1.VarRef IsNot Nothing Then
            If cpy Is Nothing OrElse TypeOf ref1.VarRef Is TClass OrElse TypeOf ref1.VarRef Is TField OrElse TypeOf ref1.VarRef Is TFunction Then
                ref2.VarRef = ref1.VarRef
            Else
                Debug.Assert(cpy.dctVar.ContainsKey(ref1.VarRef))
                ref2.VarRef = cpy.dctVar(ref1.VarRef)
            End If

            If cpy IsNot Nothing Then

                ref2.VarRef.RefVar.Add(ref2)
            End If
        End If
        ref2.TypeTrm = ref1.TypeTrm
        ref2.DefRef = ref1.DefRef

        If cpy IsNot Nothing Then
            ref2.FunctionTrm = cpy.CurFncCpy
        End If

        Return ref2
    End Function

    Public Shared Function CopyApp(app1 As TApply, cpy As TCopy) As TApply
        Dim app2 As TApply, trm2 As TTerm

        app2 = New TApply()

        app2.TypeTrm = app1.TypeTrm
        app2.TypeApp = app1.TypeApp
        app2.Negation = app1.Negation
        For Each trm1 In app1.ArgApp
            trm2 = CopyTrm(trm1, cpy)
            app2.AddInArg(trm2)
        Next

        app2.KndApp = app1.KndApp
        app2.FncApp = CopyTrm(app1.FncApp, cpy)
        app2.ClassApp = app1.ClassApp
        app2.NewApp = app1.NewApp

        Return app2
    End Function

    Public Shared Function CopyPar(par1 As TParenthesis, cpy As TCopy) As TParenthesis
        Return New TParenthesis(CopyTrm(par1.TrmPar, cpy))
    End Function

    Public Shared Function CopyCns(cns1 As TConstant, cpy As TCopy) As TConstant
        Return New TConstant(cns1.TypeAtm, cns1.NameRef)
    End Function


    Public Shared Function CopyAsn(asn1 As TAssignment, cpy As TCopy) As TAssignment
        Return New TAssignment(CopyApp(asn1.RelAsn, cpy))
    End Function

    Public Shared Function CopyCall(call1 As TCall, cpy As TCopy) As TCall
        Return New TCall(CopyApp(call1.AppCall, cpy))
    End Function

    Public Shared Function CopyIf(if1 As TIf, cpy As TCopy) As TIf
        Dim if2 As New TIf

        For Each if_blc In if1.IfBlc
            if2.IfBlc.Add(CopyIfBlc(if_blc, cpy))
        Next

        Return if2
    End Function

    Public Shared Function CopyIfBlc(if_blc As TIfBlock, cpy As TCopy) As TIfBlock
        Dim if_blc2 As New TIfBlock

        if_blc2.BlcIf = CopyBlc(if_blc.BlcIf, cpy)
        if_blc2.CndIf = CopyTrm(if_blc.CndIf, cpy)
        if_blc2.WithIf = CopyTrm(if_blc.WithIf, cpy)

        Return if_blc2
    End Function

    Public Shared Function CopySelect(sel1 As TSelect, cpy As TCopy) As TSelect
        Dim sel2 As New TSelect

        sel2.TrmSel = CopyTrm(sel1.TrmSel, cpy)
        For Each case1 In sel1.CaseSel
            sel2.CaseSel.Add(CopyCase(case1, cpy))
        Next

        Return sel2
    End Function

    Public Shared Function CopyCase(case1 As TCase, cpy As TCopy) As TCase
        Dim case2 As New TCase

        For Each trm1 In case1.TrmCase
            case2.TrmCase.Add(CopyTrm(trm1, cpy))
        Next
        case2.DefaultCase = case1.DefaultCase
        case2.BlcCase = CopyBlc(case1.BlcCase, cpy)

        Return case2
    End Function

    Public Shared Function CopyFor(for1 As TFor, cpy As TCopy) As TFor
        Dim for2 As New TFor

        for2.IdxVarFor = CopyVar(for1.IdxVarFor, cpy)
        for2.InVarFor = CopyVar(for1.InVarFor, cpy)

        for2.IdxFor = CopyRef(for1.IdxFor, cpy)
        for2.FromFor = CopyTrm(for1.FromFor, cpy)
        for2.ToFor = CopyTrm(for1.ToFor, cpy)
        for2.StepFor = CopyTrm(for1.StepFor, cpy)
        for2.InTrmFor = CopyTrm(for1.InTrmFor, cpy)
        for2.IniFor = CopyStmt(for1.IniFor, cpy)
        for2.CndFor = CopyTrm(for1.CndFor, cpy)
        for2.StepStmtFor = CopyStmt(for1.StepStmtFor, cpy)
        for2.BlcFor = CopyBlc(for1.BlcFor, cpy)
        for2.IsDo = for1.IsDo
        for2.LabelFor = for1.LabelFor

        Return for2
    End Function

    Public Shared Function CopyBlc(blc1 As TBlock, cpy As TCopy) As TBlock
        Dim blc2 As New TBlock

        If blc1 Is Nothing Then
            Return Nothing
        End If

        For Each var1 In blc1.VarBlc
            blc2.VarBlc.Add(CopyVar(var1, cpy))
        Next
        For Each stmt1 In blc1.StmtBlc
            blc2.StmtBlc.Add(CopyStmt(stmt1, cpy))
        Next

        Return blc2
    End Function

    Public Shared Function CopyComment(com1 As TComment, cpy As TCopy) As TComment
        Dim com2 As New TComment

        For Each s In com1.LineCom
            com2.LineCom.Add(s)
        Next

        Return com2
    End Function

    Public Shared Function CopyVarDecl(dcl1 As TVariableDeclaration, cpy As TCopy) As TVariableDeclaration
        Dim dcl2 As New TVariableDeclaration

        dcl2.ModDecl = dcl1.ModDecl
        dcl2.TypeDecl = dcl1.TypeDecl
        For Each var1 In dcl1.VarDecl
            dcl2.VarDecl.Add(CopyVar(var1, cpy))
        Next

        Return dcl2
    End Function

    Public Shared Function CopyStmt(stmt1 As TStatement, cpy As TCopy) As TStatement
        If stmt1 Is Nothing Then
            Return Nothing
        End If

        If TypeOf stmt1 Is TAssignment Then
            Return CopyAsn(CType(stmt1, TAssignment), cpy)
        ElseIf TypeOf stmt1 Is TCall Then
            Return CopyCall(CType(stmt1, TCall), cpy)
        ElseIf TypeOf stmt1 Is TIf Then
            Return CopyIf(CType(stmt1, TIf), cpy)
        ElseIf TypeOf stmt1 Is TIfBlock Then
            Return CopyIfBlc(CType(stmt1, TIfBlock), cpy)
        ElseIf TypeOf stmt1 Is TSelect Then
            Return CopySelect(CType(stmt1, TSelect), cpy)
        ElseIf TypeOf stmt1 Is TCase Then
            Return CopyCase(CType(stmt1, TCase), cpy)
        ElseIf TypeOf stmt1 Is TFor Then
            Return CopyFor(CType(stmt1, TFor), cpy)
        ElseIf TypeOf stmt1 Is TComment Then
            Return CopyComment(CType(stmt1, TComment), cpy)
        ElseIf TypeOf stmt1 Is TVariableDeclaration Then
            Return CopyVarDecl(CType(stmt1, TVariableDeclaration), cpy)
        Else
            Debug.Assert(False)
            Return Nothing
        End If

    End Function

    Public Shared Function CopyVar(var1 As TVariable, cpy As TCopy) As TVariable
        Dim var2 As TVariable

        If var1 Is Nothing Then
            Return Nothing
        End If

        Debug.Assert(cpy IsNot Nothing)

        If cpy.dctVar.ContainsKey(var1) Then
            Return cpy.dctVar(var1)
        End If

        var2 = New TVariable(var1.NameVar, var1.TypeVar)
        var2.ParamArrayVar = var1.ParamArrayVar
        var2.InitVar = CopyTrm(var1.InitVar, cpy)
        cpy.dctVar.Add(var1, var2)

        Return var2
    End Function

    Public Shared Function CopyFnc(fnc1 As TFunction) As TFunction
        Dim fnc2 As TFunction, cpy As TCopy
        Dim set_parent_stmt As TNaviSetParentStmt

        If fnc1 Is Nothing Then
            Return Nothing
        End If

        fnc2 = New TFunction(fnc1.NameVar, fnc1.RetType)

        cpy = New TCopy()
        cpy.CurFncCpy = fnc2

        fnc2.ModVar = fnc1.ModVar
        fnc2.TypeFnc = fnc1.TypeFnc
        fnc2.IsNew = fnc1.IsNew
        fnc2.OpFnc = fnc1.OpFnc
        fnc2.ClaFnc = fnc1.ClaFnc
        fnc2.InterfaceFnc = fnc1.InterfaceFnc
        fnc2.ImplFnc = CopyRef(fnc1.ImplFnc, cpy)
        For Each var1 In fnc1.ArgFnc
            fnc2.ArgFnc.Add(CopyVar(var1, cpy))
        Next
        fnc2.ThisFnc = CopyVar(fnc1.ThisFnc, cpy)
        fnc2.BlcFnc = CopyBlc(fnc1.BlcFnc, cpy)
        fnc2.OrgFnc = fnc1.OrgFnc

        'fnc2.__SetParent(fnc2, fnc2.ClaFnc.FncCla)
        set_parent_stmt = New TNaviSetParentStmt()
        set_parent_stmt.NaviFunction(fnc2, Nothing)

        Return fnc2
    End Function
End Class
