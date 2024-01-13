; RUN: EXEC_ARGS="0=3 1=4 2=5 3=6 4=7"; \
; RUN: WITH_DEBUG=true; \
; RUN: %test_execution
; END.

; Test a call to a function with a simple loop in it

@_1 = global i32 1

define i32 @add_to(i32 %x, i32 %iterations, i32 %bound)  {
entry:
  %xm = sub i32 %x, 1 ; The base program was incorrect, fix it
  br label %loop
  
loop:
  %x.phi = phi i32 [ %xm, %entry ], [ %x.add, %loop ]
  %i = phi i32 [ %iterations, %entry ], [ %i.dec, %loop ]
  %cmp = icmp eq i32 %i, 0
  %i.dec = sub nsw i32 %i, 1
  %one = load volatile i32, i32* @_1
  %x.add = add i32 %x.phi, %one
  call void @llvm.loop.varbound(i32 0, i32 %bound)
  br i1 %cmp, label %end, label %loop

end:
  ret i32 %x.add
}



define i32 @main(i32 %x)  {
entry:
  %res = call i32 @add_to(i32 %x, i32 3, i32 5)
  ret i32 %res
}

declare void @llvm.loop.varbound(i32, i32)
