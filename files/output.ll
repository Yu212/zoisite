; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

@printf_str = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

define i32 @main() {
entry:
  call void (ptr, ...) @printf(ptr @printf_str, i64 3)
  ret i32 0
}

declare void @printf(ptr, ...)
