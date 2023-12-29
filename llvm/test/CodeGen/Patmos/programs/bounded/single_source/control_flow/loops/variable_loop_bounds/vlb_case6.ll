; RUN: EXEC_ARGS="0=6 1=3 2=6 3=3 4=6 5=3"; \
; RUN: WITH_DEBUG=true; \
; XFAIL:* 
; RUN: %test_execution
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
;	Test for constant loop-bound propagation
; 	Main has a loop depending on result
;	Result is obtained through PHI node
; ;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_2 = global i32 2
@_3 = global i32 3
@_4 = global i32 4
@_5 = global i32 5
@_6 = global i32 6



define i32 @main(i32 %x)  {
entry:
  %is_odd = trunc i32 %x to i1
  br i1 %is_odd, label %if.then, label %if.else
  
if.then:
  %res.then = load volatile i32, i32* @_3
  br label %do.entry

if.else:
  %res.else = load volatile i32,i32* @_6
  br label %do.entry
  
do.entry:
  %result = phi i32 [%res.then, %if.then], [%res.else, %if.else]
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %y.0 = phi i32 [ 0, %do.entry ], [ %add, %do.cond ]
  %0 = load volatile i32, i32* @_1
  %add = add nsw i32 %y.0, %0
  call void @llvm.loop.varbound(i32 0, i32 %result)
  br label %do.cond

do.cond:                                          ; preds = %do.body
  %cmp = icmp slt i32 %add, %result
  br i1 %cmp, label %do.body, label %do.end

do.end:                                           ; preds = %do.cond
  ret i32 %add
}

declare void @llvm.loop.varbound(i32, i32)
