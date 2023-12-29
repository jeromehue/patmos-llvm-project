; RUN: EXEC_ARGS="0=0 1=10 2=0 3=12 4=0"; \
; RUN: WITH_DEBUG=true; \
; RUN: %test_execution
; END.

@_0 = global i32 0
@_1 = global i32 1
@_5 = global i32 5

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
  %is_odd = trunc i32 %x to i1
  ;%0 = call i32 @add_to(i32 %x, i32 9, i32 10)
  br i1 %is_odd, label %if.then, label %if.else

if.then:
  %res.then = call i32 @add_to(i32 %x, i32 9, i32 10)
  br label %end

if.else:
  %res.else = load volatile i32, i32* @_0
  br label %end

end:
  %result = phi i32 [%res.then, %if.then], [%res.else, %if.else]
  ret i32 %result
}

declare void @llvm.loop.varbound(i32, i32)
;declare void @llvm.loop.bound(i32, i32)
