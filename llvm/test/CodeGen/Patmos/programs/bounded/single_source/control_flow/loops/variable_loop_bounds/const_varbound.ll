; RUN: EXEC_ARGS="0=10 1=10 2=10 9=10 10=10"; \
; RUN: WITH_DEBUG=true; \
; RUN: %test_execution
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests a do/while statement.
; The following is the equivalent C code:
;
; volatile int _1 = 1;
; 
; int main(int x){
; 	v = 10;
; 	int y = 0;
; 	#pragma loopbound min 1 max v
; 	do{
; 		y += _1;
; 	}while(y < x);
; 	return y;
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_10 = global i32 10

define i32 @main(i32 %x) {
entry:
  %v = load i32, i32* @_10
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %y.0 = phi i32 [ 0, %entry ], [ %add, %do.cond ]
  %0 = load volatile i32, i32* @_1
  %add = add nsw i32 %y.0, %0
  call void @llvm.loop.varbound(i32 %v, i32 %v)
  br label %do.cond

do.cond:                                          ; preds = %do.body
  %cmp = icmp slt i32 %add, 10
  br i1 %cmp, label %do.body, label %do.end

do.end:                                           ; preds = %do.cond
  ret i32 %add
}

declare void @llvm.loop.varbound(i32, i32)
