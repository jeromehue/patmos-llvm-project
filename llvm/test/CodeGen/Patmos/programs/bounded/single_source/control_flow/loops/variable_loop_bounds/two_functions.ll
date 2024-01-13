; RUN: EXEC_ARGS="0=7 2=7 3=12 4=7"; \
; RUN: WITH_DEBUG=true; \
; RUN: %test_execution
; END.

@_0 = global i32 0
@_1 = global i32 1
@_2 = global i32 2
@_3 = global i32 3
@_4 = global i32 4
@_5 = global i32 5
@_6 = global i32 6
@_7 = global i32 7
@_8 = global i32 8
@_9 = global i32 9
@_10 = global i32 10

define i32 @double(i32 %input) {
entry:
  %0 = alloca i32
  %result = alloca i32
  %x = alloca i32
  %y = alloca i32
  %i = alloca i32
  %i1 = alloca i32
  store i32 %input, i32* %0
  store i32 0, i32* %result
  %1 = load i32, i32* %0
  %2 = sdiv i32 %1, 2
  store i32 %2, i32* %x
  %3 = load i32, i32* %0
  %4 = sdiv i32 %3, 10
  store i32 %4, i32* %y
  store i32 0, i32* %i
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %5 = load i32, i32* %i
  %6 = load i32, i32* %x
  %7 = icmp slt i32 %5, %6
  call void @llvm.loop.varbound(i32 0, i32 %input)
  br i1 %7, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %8 = load volatile i32, i32* @_1
  %9 = load i32, i32* %result
  %10 = add nsw i32 %9, %8
  store i32 %10, i32* %result
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %11 = load i32, i32* %i
  %12 = add nsw i32 %11, 1
  store i32 %12, i32* %i
  br label %for.cond

for.end:                                          ; preds = %for.cond
  store i32 0, i32* %i1
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc4, %for.end
  %13 = load i32, i32* %i1
  %14 = load i32, i32* %y
  %15 = icmp slt i32 %13, %14
  ; This is for testing :
  ; We know that x is input/2, and y = input/10, thus
  ; we always have x > y.
  ; The next thing would be to try to set the real termination condition here
  call void @llvm.loop.varbound(i32 0, i32 %6)
  br i1 %15, label %for.body3, label %for.end5

for.body3:                                        ; preds = %for.cond2
  %16 = load volatile i32, i32* @_2
  %17 = load i32, i32* %result
  %18 = add nsw i32 %17, %16
  store i32 %18, i32* %result
  br label %for.inc4

for.inc4:                                         ; preds = %for.body3
  %19 = load i32, i32* %i1
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %i1
  br label %for.cond2

for.end5:                                         ; preds = %for.cond2
  %21 = load i32, i32* %result
  ret i32 %21
}

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
  %0 = load volatile i32, i32* @_10
  ;%0 = call i32 @add_to(i32 %x, i32 9, i32 10)
  br i1 %is_odd, label %if.then, label %if.else

if.then:
  ; x + 9
  %res.then = call i32 @add_to(i32 %x, i32 9, i32 10)
  br label %end

if.else:
  %res.else = call i32 @double(i32 10)
  br label %end

end:
  %result = phi i32 [%res.then, %if.then], [%res.else, %if.else]
  ret i32 %result
}

declare void @llvm.loop.varbound(i32, i32)
