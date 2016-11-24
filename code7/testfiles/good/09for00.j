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
ldc 5
istore 0
ldc 4
istore 1
iload 1
istore 2
LBL0:
iload 2
ldc 0
if_icmpgt LBL1
ldc 0
goto LBL2
LBL1:
ldc 1
LBL2:
ifeq LBL3
iload 0
iload 2
iadd
dup
istore 0
pop
iload 0
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 2
ldc 1
isub
dup
istore 2
pop
goto LBL0
LBL3:
iload 1
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 0
invokestatic CSupport/printInt(I)V
ldc 0
pop
return
nop
.end method
