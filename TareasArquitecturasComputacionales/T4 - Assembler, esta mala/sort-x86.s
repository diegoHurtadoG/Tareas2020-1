# sort_x86: Ordena alfabeticamente un arreglo de strings usando un algoritmo ridiculamente
# ineficiente.

# La funcion sort_x86 esta programada en assembler x86. El codigo equivalente en C esta
# comentado, mostrando la ubicacion de las variables en los registros.  La funcion recorre
# el arreglo revisando que los elementos consecutivos esten ordenados.  Si encuentra 2
# elementos consecutivos desordenados, los intercambia y reinicia el recorrido del arreglo
# desde el comienzo.  El arreglo esta ordenado cuando se recorre completamente sin
# encontrar elementos consecutivos desordenados.

	.file "sort-x86.s"
	.text
	.globl sort_x86		# Se necesita para que la etiqueta sea conocida en
				# test-sort-x86.c
	.type sort_x86, @function
sort_x86:
	pushl	%ebp
	movl	%esp, %ebp	# void sort(char **noms /* 8(%ebp) */, int n /* 12(%ebp) */) {
	pushl	%edi		#   // -4(%ebp)  (se resguardan registros inalterables)
	pushl	%esi		#   // -8(%ebp)
	pushl	%ebx		#   // -12(%ebp)
	subl	$20, %esp	#   // -16(%ebp) ... -32(%ebp): este espacio esta para que
				#   // lo use Ud., aunque mi solucion no lo necesito
	movl	8(%ebp), %esi	#   // %esi= noms (no lo altera strcmp)
	movl	%esi, %edi	#   char **p= noms; // %edi (no lo altera strcmp)
	movl	12(%ebp), %ecx	#   // %ecx= n (solo se usa en la siguiente instruccion)
				#   char **ult= &noms[n-1]; // %ebx (no lo altera strcmp)
	leal	-4(%esi, %ecx, 4), %ebx
				#   while (p<ult) {
	jmp	.while_cond	#     // la condicion del while se evalua al final del ciclo
.while_begin:			#     // para mayor eficiencia

	# Hasta aca no puede modificar nada
	# IMPORTANTE: %ebx -> contenido del registro ebx
	#             (%ebx) -> contenido de la direccion contenida en ebx
	#Tengo:
	    # 16(%ebp) ... 32(%ebp) estan disponibles para usar como registro
	    # (%esi) -> noms
	    # (%edi) -> char **p = noms
	    # (%ecx) -> n
	    # (%ebx) -> char**ult = &noms[n-1]

	#################################################
	### Comienza el codigo que Ud. debe modificar ###
	#################################################

	# no puede alterar los registros %edi, %esi y %ebx.  Si lo hace resguarde y restaure
	# en -16(%ebp) ... -32(%ebp).
				#     if (strcmp(p[0], p[1])<=0)
	pushl	4(%edi)		#     // push p[1] (2do. argumento)
	pushl	0(%edi)		#     // push p[0] (1er. argumento)
	call	strcmp		#     // %eax= strcmp(p[0], p[1]), ( 0 si =, !=0 si no)
	addl	$8, %esp	#     // desapila p[1] y p[0]

	#################################################
	### Fin del codigo que Ud. debe modificar     ###
	#################################################

	# Desde aca no puede modificar nada

	# En %eax debe quedar la conclusion de la comparacion:
	# si %eax<=0 p[0] y p[1] estan en orden y no se intercambiaran.
        # Si no, se intercambian p[0] y p[1] y se asigna p= noms para revisar
	# nuevamente que los elementos esten ordenados desde el comienzo del arreglo
.decision:
	cmpl	$0, %eax
	jg	.else		#     // if %eax>0 goto .else
	addl	$4, %edi	#       p++; // en la siguiente interacion 
	jmp	.while_cond	#            // se comparan p[1] y p[2]
.else:				#     else { // intercambar p[0] y p[1], y reiniciar
	movl	(%edi),%eax	#       char *aux= p[0];
	movl	4(%edi), %ecx	#       char *aux2= p[1];
	movl	%ecx, (%edi)	#       p[0]= aux2;
	movl	%eax, 4(%edi)	#       p[1]= aux;
	movl	%esi, %edi	#	p= noms;
				#     }
.while_cond:			#     // se evalua la condicion del while
	cmpl	%ebx, %edi	#     // p ? ult (recuerdar que operandos estan invertidos)
	jb	.while_begin	#     // if p<ult goto .while_begin
				#   }
	addl	$20, %esp	#   // se desapila el espacio pedido
	popl	%ebx		#   // se restauran registros inalterables
	popl	%esi
	popl	%edi
	popl	%ebp
	ret			# }
