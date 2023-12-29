// RUN: %clang --target=patmos %s -S -emit-llvm -o - | FileCheck %s
// RUN: %clang --target=patmos %s -S -emit-llvm -o - -O0 | FileCheck %s
// RUN: %clang --target=patmos %s -S -emit-llvm -o - -O1 | FileCheck %s
// RUN: %clang --target=patmos %s -S -emit-llvm -o - -O2 | FileCheck %s
// END.
///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Test that when the loop bound pragma is given, it is propagated into the LLVM-IR.
//
// Also tests that multiple loops with bounds can be declared, resulting in them all
// using the same 'llvm.loop.bound'.
//
// Also tests that no optimizations result in the loop bound call being duplicated somewhere.
// 
///////////////////////////////////////////////////////////////////////////////////////////////////

// CHECK-LABEL: func1
int func1(int x) { 
	int count = x;
	
	_Pragma( "varloopbound min 3 max x" )
	while(x >0){
	// CHECK: call void @llvm.loop.varbound(i32 3, i32 %x)
	// CHECK-NOT: call void @llvm.loop.varbound(i32 3, i32 4)
		x = x/2;
		count++;
	}
	return count;	
}
// CHECK: declare void @llvm.loop.varbound(i32, i32)

// CHECK-LABEL: func2
int func2(int x) { 
	int count = x;
	
	_Pragma( "varloopbound min 24 max x" )
	for(int i = 0; i < x; i++){
	// CHECK: call void @llvm.loop.varbound(i32 24, i32 %x)
	// CHECK-NOT: call void @llvm.loop.varbound(i32 24, i32 %x)
		x = x/2;
		count++;
	}
	return count;	
}

// CHECK-LABEL: func3
int func3(int x) { 
	int count = x;
	
	#pragma varloopbound min 1 max x
	do { 
	// CHECK: call void @llvm.loop.varbound(i32 0, i32 %x)
	// CHECK-NOT: call void @llvm.loop.varbound(i32 0, i32 %x)
		x = x/2;
		count++;
	} while(x >0);
	return count;	
}

// CHECK-LABEL: func4
int func4( int x )
{
  int fvalue, mid, up, low;

  low = 0;
  up = 14;
  fvalue = -1;

  _Pragma( "varloopbound min 1 max up" )
  while ( low <= up ) {
	// CHECK: call void @llvm.loop.varbound(i32 1, i32 %{{[\.a-zA-Z0-9]+}})
	// CHECK-NOT: call void @llvm.loop.varbound(i32 1, i32 %up)
	mid = ( low + up ) >> 1;

    if ( 0 == x ) {
      up = low - 1;
      fvalue = 4;
    } else
      if ( 157 > x )
        up = mid - 1;
      else
        low = mid + 1;
  }

  return fvalue;
}

