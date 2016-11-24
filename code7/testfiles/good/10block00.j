.class public C
.super java/lang/Object

.method public <init>()V
aload_0
invokenonvirtual java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack 1000
ldc 6
dup
istore 0
pop
iload 0
ldc 7
iadd
dup
istore 1
pop
iload 1
invokestatic CSupport/printInt(I)V
ldc 0
pop
ldc 4
dup
istore 2
pop
iload 2
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 2
dup
istore 0
pop
iload 0
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 0
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 1
invokestatic CSupport/printInt(I)V
ldc 0
pop
return
nop
.end method
