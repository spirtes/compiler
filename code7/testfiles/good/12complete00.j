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
invokestatic CSupport/readInt()I
istore 0
iload 0
ldc 2
idiv
istore 1
LBL0:
iload 1
ldc 1
if_icmpgt LBL1
ldc 0
goto LBL2
LBL1:
ldc 1
LBL2:
ifeq LBL3
iload 1
iload 0
iload 1
idiv
imul
iload 0
if_icmpeq LBL4
ldc 0
goto LBL5
LBL4:
ldc 1
LBL5:
ifeq LBL6
iload 1
invokestatic CSupport/printInt(I)V
ldc 0
pop
goto LBL7
LBL6:
LBL7:
iload 1
dup
ldc 1
isub
istore 1
pop
goto LBL0
LBL3:
return
nop
.end method
