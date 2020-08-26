	.file	"sort-c-apnom.c"
	.text
	.globl	sort_x86_apnom
	.type	sort_x86_apnom, @function
sort_x86_apnom:
.LFB8:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$156, %esp
	call	__x86.get_pc_thunk.ax
	addl	$_GLOBAL_OFFSET_TABLE_, %eax
	movl	%eax, 12(%esp)
	movl	176(%esp), %esi
	movl	%esi, 24(%esp)
	movl	%gs:20, %eax
	movl	%eax, 140(%esp)
	xorl	%eax, %eax
	movl	180(%esp), %eax
	leal	-4(%esi,%eax,4), %eax #Debe ser el primer while (?)
	movl	%eax, 28(%esp)
	movl	%esi, 4(%esp)
	cmpl	%eax, %esi
	jnb	.L1 #No se como, pero creo que llama a un error
	leal	90(%esp), %eax #Carga direcciones primer for (?)
	movl	%eax, 16(%esp)
	leal	40(%esp), %eax #Carga direcciones segundo for (?)
	movl	%eax, 20(%esp)
	jmp	.L2 #Empieza el while
.L5: #Suma uno, esto es dentro del for
	addl	$1, %edx
.L15: #Esto deberia ser el primer for
	movl	$-1, %ecx
	movl	%ebp, %edi
	movl	$0, %eax
	repnz scasb #repnz = REPeat while Not Zero - scasb = Compare AL with byte at ES:(E)DI
	notl	%ecx
	subl	$1, %ecx
	cmpl	%edx, %ecx
	jbe	.L20 #Continua el while, al segundo for
	movzbl	0(%ebp,%edx), %eax
	cmpb	$32, %al
	cmove	%edx, %ebx
	testl	%ebx, %ebx
	je	.L5 #en L5 se suma 1 en el for
	movb	%al, 40(%esp,%esi)
	leal	1(%esi), %esi
	jmp	.L5 #en L5 se suma 1 en el for
.L20: #Salio del for
	subl	%ebx, %ecx
	movb	$0, 40(%esp,%ecx)
	movl	4(%esp), %eax
	movl	4(%eax), %esi
	movl	$0, %eax
	movl	$0, %edx
	movl	$0, %ebx
	movl	%ebp, 8(%esp)
	movl	%eax, %ebp
	jmp	.L7 #Entro al segundo for
.L9: #Esto le suma 1 al valor
	addl	$1, %edx
.L7: #Tiene pinta de segundo for
	movl	$-1, %ecx
	movl	%esi, %edi
	movl	$0, %eax
	repnz scasb
	notl	%ecx
	subl	$1, %ecx
	cmpl	%edx, %ecx
	jbe	.L21 #Continua el while, a la parte de los if
	movzbl	(%esi,%edx), %eax
	cmpb	$32, %al
	cmove	%edx, %ebx
	testl	%ebx, %ebx
	je	.L9 #en L9 se suma 1 en el for
	movb	%al, 90(%esp,%ebp)
	leal	1(%ebp), %ebp
	jmp	.L9 #en L9 se suma 1 en el for
.L21: #Aqui parten los if (dentro del while)
	movl	8(%esp), %ebp
	subl	%ebx, %ecx
	movb	$0, 90(%esp,%ecx)
	subl	$8, %esp
	pushl	24(%esp)
	pushl	32(%esp)
	movl	28(%esp), %ebx
	call	strcmp@PLT
	addl	$16, %esp
	testl	%eax, %eax
	js	.L22 #Creo que aqui se acaba el while si se da la condicion de salida
	testl	%eax, %eax
	jne	.L13 #Intercambia los valores (ordena)
	subl	$8, %esp
	pushl	%esi
	pushl	%ebp
	movl	28(%esp), %ebx
	call	strcmp@PLT
	addl	$16, %esp
	testl	%eax, %eax
	jle	.L23 #Hace p++ y sigue con el while
.L13: #Intercambia los valores (ordena)
	movl	4(%esp), %eax
	movl	%esi, (%eax)
	movl	%ebp, 4(%eax)
	movl	24(%esp), %eax
	movl	%eax, 4(%esp)
.L2: #Primero pone las variables de cuando parte el while
	movl	4(%esp), %eax
	movl	(%eax), %ebp
	movl	$0, %esi
	movl	$0, %edx
	movl	$0, %ebx
	jmp	.L15
.L22: # Creo que aqui termina el while
	addl	$4, 4(%esp)
.L12: #Empieza el while (denuevo)
	movl	28(%esp), %edi
	cmpl	%edi, 4(%esp)
	jb	.L2
.L1: #Creo que es el punto medio de llevar a un error.
	movl	140(%esp), %eax
	xorl	%gs:20, %eax
	jne	.L24 #error
	addl	$156, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.L23: #Hace p++ y vuelve a aplicar el while
	addl	$4, 4(%esp)
	jmp	.L12 #Aqui se llama al while denuevo
.L24: #error
	call	__stack_chk_fail_local
.LFE8:
	.size	sort_x86_apnom, .-sort_x86_apnom
	.section	.text.__x86.get_pc_thunk.ax,"axG",@progbits,__x86.get_pc_thunk.ax,comdat
	.globl	__x86.get_pc_thunk.ax
	.hidden	__x86.get_pc_thunk.ax
	.type	__x86.get_pc_thunk.ax, @function
__x86.get_pc_thunk.ax:
.LFB9:
	movl	(%esp), %eax
	ret
.LFE9:
	.hidden	__stack_chk_fail_local
	.ident	"GCC: (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0"
	.section	.note.GNU-stack,"",@progbits
