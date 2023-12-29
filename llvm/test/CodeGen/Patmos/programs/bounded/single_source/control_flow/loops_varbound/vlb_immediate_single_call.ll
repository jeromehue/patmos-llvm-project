; RUN: EXEC_ARGS="0=4 1=5 2=6 3=7 4=8"; \
; RUN: WITH_DEBUG=true; \
; RUN: %test_execution
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; File vlb_immediate_single_call.ll
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_2 = global i32 2
@_3 = global i32 3
@_4 = global i32 4
@_5 = global i32 5

define i32 @add_to(i32 %x, i32 %iterations)  {
entry:
  br label %loop
  
loop:
  %x.phi = phi i32 [ %x, %entry ], [ %x.add, %loop ]
  %i = phi i32 [ %iterations, %entry ], [ %i.dec, %loop ]
  %cmp = icmp eq i32 %i, 0
  %i.dec = sub nsw i32 %i, 1
  %one = load volatile i32, i32* @_1
  %x.add = add i32 %x.phi, %one
  call void @llvm.loop.varbound(i32 0, i32 %iterations)
  br i1 %cmp, label %end, label %loop

end:
  ret i32 %x.add
}

define i32 @main(i32 %x)  {
entry:
  %res = call i32 @add_to(i32 %x, i32 3)
  ret i32 %res
}

declare void @llvm.loop.varbound(i32, i32)
