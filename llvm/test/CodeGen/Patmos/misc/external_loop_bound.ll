; RUN: llc %s -mpatmos-singlepath="main" %XFAIL-filecheck %s
; END


; CHECK: A loopbound is defined using one of main() argument.

@_1 = global i32 1
@_4 = global i32 4
@_10 = global i32 10

define i32 @main(i32 %x) {
entry:
  %v = load i32, i32* @_10
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %y.0 = phi i32 [ 0, %entry ], [ %add, %do.cond ]
  %0 = load volatile i32, i32* @_1
  %add = add nsw i32 %y.0, %0
  call void @llvm.loop.varbound(i32 0, i32 %x)
  br label %do.cond

do.cond:                                          ; preds = %do.body
  %cmp = icmp slt i32 %add, %x
  br i1 %cmp, label %do.body, label %do.end

do.end:                                           ; preds = %do.cond
  ret i32 %add
}

declare void @llvm.loop.varbound(i32, i32)
