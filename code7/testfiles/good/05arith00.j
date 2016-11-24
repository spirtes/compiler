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
ldc 7
istore 0
ldc 4
istore 1
ldc2_w 3.0
dstore 2
ldc2_w 4.0
dstore 4
iload 0
iload 1
iadd
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 0
iload 1
isub
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 0
iload 1
imul
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 0
iload 1
idiv
invokestatic CSupport/printInt(I)V
ldc 0
pop
iload 0
iload 1
irem
invokestatic CSupport/printInt(I)V
ldc 0
pop
dload 2
dload 4
dadd
invokestatic CSupport/printDouble(D)V
ldc 0
pop
dload 2
dload 4
dsub
invokestatic CSupport/printDouble(D)V
ldc 0
pop
dload 2
dload 4
dmul
invokestatic CSupport/printDouble(D)V
ldc 0
pop
dload 2
dload 4
ddiv
invokestatic CSupport/printDouble(D)V
ldc 0
pop
return
nop
.end method
