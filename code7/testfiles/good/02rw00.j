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
dup
istore 0
pop
iload 0
invokestatic CSupport/printInt(I)V
ldc 0
pop
invokestatic CSupport/readDouble()D
dup2
dstore 1
pop2
dload 1
invokestatic CSupport/printDouble(D)V
ldc 0
pop
invokestatic CSupport/readBool()Z
dup
istore 3
pop
iload 3
invokestatic CSupport/printBool(Z)V
ldc 0
pop
return
nop
.end method
