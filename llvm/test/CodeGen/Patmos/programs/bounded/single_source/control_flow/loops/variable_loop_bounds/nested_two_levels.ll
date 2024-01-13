; RUN: EXEC_ARGS="0=17 1=17 2=17 3=17 4=17"; \
; RUN: WITH_DEBUG=true; \
; RUN: %test_execution
; END.


@_1 = dso_local global i32 1, align 4

; Function Attrs: norecurse nounwind
define dso_local i32 @func1(i32 %l) local_unnamed_addr #0 {
entry:
  %call = tail call i32 @func3(i32 1, i32 10)
  ret i32 %call
}

; Function Attrs: norecurse nounwind
define dso_local i32 @func3(i32 %p, i32 %x) local_unnamed_addr #0 {
entry:
  %0 = icmp sgt i32 %x, 0
  %smax = select i1 %0, i32 %x, i32 0
  br label %for.cond

for.cond:                                         ; preds = %for.body, %entry
  %p.addr.0 = phi i32 [ %p, %entry ], [ %add, %for.body ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.body ]
  tail call void @llvm.loop.varbound(i32 1, i32 %x) #3
  %exitcond.not = icmp eq i32 %i.0, %smax
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body

for.cond.cleanup:                                 ; preds = %for.cond
  ret i32 %p.addr.0

for.body:                                         ; preds = %for.cond
  %1 = load volatile i32, i32* @_1, align 4, !tbaa !2
  %add = add nsw i32 %1, %p.addr.0
  %inc = add nuw i32 %i.0, 1
  br label %for.cond, !llvm.loop !6
}

; Function Attrs: norecurse nounwind
define dso_local i32 @func2(i32 %l) local_unnamed_addr #0 {
entry:
  %call = tail call i32 @func3(i32 2, i32 4)
  ret i32 %call
}

; Function Attrs: convergent noduplicate noinline nomerge norecurse optnone
declare void @llvm.loop.varbound(i32, i32) #1

; Function Attrs: noinline norecurse nounwind
define dso_local i32 @main(i32 %x) local_unnamed_addr #2 {
entry:
  %call.i = tail call i32 @func3(i32 1, i32 10) #3
  %call.i6 = tail call i32 @func3(i32 2, i32 4) #3
  %add2 = add nsw i32 %call.i6, %call.i
  ret i32 %add2
}

attributes #0 = { norecurse nounwind "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { convergent noduplicate noinline nomerge norecurse optnone }
attributes #2 = { noinline norecurse nounwind "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "sp-root" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 12.0.1"}
!2 = !{!3, !3, i64 0}
!3 = !{!"int", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
