.286
.287
data segment
;Argument storage format:
;storage		:byte[STO_SIZE]			- NULL-terminated arguments
;argc			:byte					- number of parsed arguments
;argptrs		:word[ARGNUM]			- offset of every argument relative to segment
;arglens		:byte[ARGNUM]			- table of arguments' lengths, excluding terminator
;
STO_SIZE = 255d
ARGNUM = STO_SIZE / 2d

storage				db STO_SIZE dup(?)
argc				db 0d
argptrs				dw ARGNUM dup(0d)
arglens				db ARGNUM dup(0d)


str_usage			db 'juliaset X_MIN X_MAX Y_MIN Y_MAX C_R C_I',13d,10d,13d,10d
					db 'All arguments should be of form:	-?[0-9]*\.[0-9]*',13d,10d
					db 'In addition, arguments should satisfy:	X_MIN < X_MAX && Y_MIN < Y_MAX',13d,10d,'$'


err_overflow		db 'error: arguments too long',13d,10d,13d,10d,'$'
err_arg_count		db 'error: invalid number of arguments',13d,10d,13d,10d,'$'
err_arg_value		db 'error: invalid arguments',13d,10d,13d,10d,'$'

SCREEN_WIDTH_PX		= 320
SCREEN_HEIGHT_PX	= 200

sw					dw SCREEN_WIDTH_PX - 1
sh					dw SCREEN_HEIGHT_PX - 1

colour_offset		dw ? ;40h ;20h ;51h
julia_colour		db ? ;00h

fbuffer				dw ?

x					dw ?
y					dw ?
ypy					dw ?
yk					dw ?

xmin				real8 ?
ymin				real8 ?

xdp					real8 ?
ydp					real8 ?

cr					real8 ?
ci					real8 ?

four				real8 4.0
ten					real8 10.0

sign				db ?

data ends


;Storage API;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;LD_STO_SEG	(SEG_REG)
;LD_STO (SEG_REG, REG)
;
;IS_STO_FULL (REG)
;
;ARGCX
;
;ARGP (INDEX, REG)
;ARG (INDEX, REG)
;ARGV (INDEX, REG)
;
;ARGLENP (INDEX, REG)
;ARGLEN (INDEX, REG)
;
;COPY_ARG (SEG_SRC, INDEX, SEG_DEST, OFFSET_DEST)
;

LD_STO_SEG macro SEG_REG				;moves storage segment to SEG_REG
	push ax

	mov ax,seg storage
	mov SEG_REG,ax

	pop ax
endm

LD_STO macro SEG_REG, REG				;moves storage segment to SEG_REG and offset to REG
	LD_STO_SEG SEG_REG
	mov REG,offset storage
endm

IS_STO_FULL macro REG					;checks whether pointer REG is after the last byte of storage data; use: jae <=> true / jb <=> false
	cmp REG,offset argc
endm

ARGCX macro								;moves number of arguments in storage to cx
	push ds

	LD_STO_SEG ds

	mov cl,ds:[argc]
	xor ch,ch

	pop ds
endm

ARGP macro INDEX, REG					;returns in REG pointer to near pointer to argument with index INDEX, indices begin from 0
	xor REG,REG

	mov REG,INDEX						;skip pointers (2 bytes) of args before INDEX
	sal REG,1d

	add REG,offset argc + 1d
endm

ARG macro INDEX, REG					;returns in REG near pointer to argument with index INDEX, indices begin from 0
	push di								;REG cannot be di
	push ds

	LD_STO_SEG ds

	ARGP INDEX, REG

	mov di,REG
	mov REG,ds:[di]

	pop ds
	pop di
endm

ARGV macro INDEX, REG					;returns in REG first word of argument with index INDEX, indices begin from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGP INDEX, REG

	mov bx,REG
	mov bx,ds:[bx]						;get pointer to argument

	mov bx,ds:[bx]						;get first word of argument
	mov REG,bx

	pop ds
	pop bx
endm

ARGLENP macro INDEX, REG				;returns in REG pointer to size of argument INDEX, indices start from 0
	xor REG,REG
	mov REG,INDEX
	add REG,offset arglens
endm

ARGLEN macro INDEX, REG					;returns in REG size of argument INDEX, indices start from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGLENP INDEX, REG

	mov bx,REG
	mov REG,ds:[bx]
	and REG,00ffh

	pop ds
	pop bx
endm

;COPY_ARG
;Copies argument with index INDEX from storage to SEG_DEST:OFFSET_DEST
;
COPY_ARG macro INDEX, SEG_DEST, OFFSET_DEST
	push ax
	push si
	push di
	push ds
	push es

	LD_STO_SEG ds
	ARG INDEX, si

	mov ax,SEG_DEST
	mov es,ax
	mov di,OFFSET_DEST

	ARGLEN INDEX, cx

	rep movsb
	
	pop es
	pop ds
	pop di
	pop si
	pop ax
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


code segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LSTR
;Load address of string NAME to ds:dx
;
;params:	NAME		- name of the string to load
;
LSTR macro NAME
	push ax

	mov ax,seg NAME
	mov ds,ax
	mov dx,offset NAME

	pop ax
endm


;error_exit
;If bx isn't 0d, prints $-terminated error string at ds:[dx].
;Exits program with return code set in al.
;
;params:	al			- return code
;			bx			- print error string flag
;			ds:[dx]		- error string
;
ERROR_EXIT macro RET
	mov al,RET
	mov bx,0d
	call $error_exit
endm

ERROR_EXIT_STR macro RET, STR
	mov al,RET
	mov bx,1d
	LSTR STR

	call $error_exit
endm

$error_exit proc
	push ax						;to preserve al return code

	test bx,bx
	jz print_str_usage

	mov ah,09h					;print string at ds:[dx]
	int 21h

print_str_usage:
	LSTR str_usage
	mov ah,09h
	int 21h

	pop ax
	mov ah,4ch
	int 21h
$error_exit endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;eat_whitespace
;Returns position of the first non-whitespace char in the string
;at ds:[si] and sets ah to 1. If enter (13d) found, sets ah to 0.
;
;params:	ds:[si]		- string to search in
;
;returns:	ds:[si]		- first non-whitespace char found in the string
;			ah = 0d		- enter found
;			ah = 1d		- non-whitespace found
;
eat_whitespace proc
	push ax

get_char:
	lodsb

	cmp al,0dh					;0dh = cr = enter
	je enter_handler

	cmp al,21h					;21h = first non-whitespace ascii char
	jb get_char

	cmp al,7fh					;check delete = 7fh
	je get_char

	dec si						;non-whitespace found
	pop ax
	mov ah,1d
	ret

enter_handler:
	pop ax
	mov ah,0d
	ret
eat_whitespace endp


;copy_arg_to_storage
;Copies string at ds:[si] to storage, beginning at es:[di].
;Updates argptr table and argc. Modifies si, di.
;
;params:	ds:[si]		- source
;			es:[di]		- destination inside a storage
;
;returns:	ds:[si]		- character in the string after the first found whitespace
;			ah = 0d		- enter found
;			ah = 1d		- whitespace found
;
copy_arg_to_storage proc
	push bx
	push dx
	push ax

	IS_STO_FULL di				;check free space
	jae error_overflow

	ARGP word ptr es:[argc], bx	;get pointer to pointer to argument with index argc (=counter of args in storage) to bx
	mov es:[bx],di				;save argument ptr

	movsb						;move first char from source to storage
	mov dx,1d					;dx stores length of current argument

get_char:
	lodsb						;load char from source

	cmp al,0dh					;0dh = cr = enter
	je enter_handler

	cmp al,21h					;21h = first non-whitespace ascii char
	jb whitespace_handler

	cmp al,7fh					;check delete = 7fh
	je whitespace_handler

	IS_STO_FULL di				;check free space
	jae error_overflow

	stosb						;move char from al to storage
	inc dx						;increment length

	jmp get_char

enter_handler:
	pop ax						;pop ax now to preserve return value in ah later
	mov ah,0d
	jmp end_arg

whitespace_handler:
	pop ax						;pop ax now to preserve return value in ah later
	mov ah,1d

end_arg:
	IS_STO_FULL di				;check free space
	jae error_overflow

	mov byte ptr es:[di],0d		;terminate argument
	inc di

	ARGLENP word ptr es:[argc], bx	;get pointer to length of argument with index argc
	mov es:[bx],dx				;save argument length

	inc es:[argc]				;increment number of arguments in storage

	pop dx
	pop bx
	ret

error_overflow:
	ERROR_EXIT_STR -1d, err_overflow	;print error and exit if overflow
copy_arg_to_storage endp


;parse_args
;Parses DOS cmdline arguments and saves them in storage.
;
parse_args proc
	push si
	push ds

	mov ah,51h					;get argument segment address from DOS to bx
	int 21h

	mov ds,bx					;DOS segment in bx
	mov si,80h					;80h -> DOS cmdline length

	LD_STO es, di				;init es:di with seg storage:offset storage

	cmp byte ptr ds:[si],0d		;check if there are any arguments
	je warn_no_arguments

	add si,2d					;move si to point at first possible non-whitespace

parse_argument:
	call eat_whitespace

	test ah,ah					;check end of input
	jz return

	call copy_arg_to_storage

	test ah,ah					;check end of input
	jnz parse_argument

return:
	pop ds
	pop si
	ret

warn_no_arguments:				;just print usage info and exit
	ERROR_EXIT 1d
parse_args endp


set_random_colour proc
	push ax
	push cx
	push dx
	push ds

	LD_STO_SEG ds

	mov ah,2ch		;get system time
	int 21h
	mov byte ptr ds:[colour_offset],dl

	mov ah,2ch		;get system time
	int 21h
	add byte ptr ds:[colour_offset],dl

	mov ah,2ch		;get system time
	int 21h
	add byte ptr ds:[julia_colour],dl

	mov ah,2ch		;get system time
	int 21h
	add byte ptr ds:[julia_colour],dl

	pop ds
	pop dx
	pop cx
	pop ax
	ret
set_random_colour endp


;fpusha
;Converts array at ds:[bx] of length cx into floating point value on the FPU stack.
fpusha proc
	fld real8 ptr ds:[ten]
	fldz

	xor ah,ah

	mov al,ds:[bx]

	cmp al,'-'
	je negative

	mov byte ptr ds:[sign],0

next_char:
	cmp al,'.'
	je fractional_part
	
	cmp al,'0'
	jb error_arg_value
	
	cmp al,'9'
	ja error_arg_value

	fmul st(0),st(1)			;x *= 10

	sub al,'0'
	mov ds:[fbuffer],ax
	fild word ptr ds:[fbuffer]
	fadd						;x = x + next_digit

	inc bx
	mov al,ds:[bx]

	loop next_char

	jmp error_arg_value

fractional_part:
	fldz
	add bx,cx					;point bx at the last char of the array
	dec bx

	mov al,ds:[bx]
next_fractional_char:
	cmp al,'.'
	je end_loop

	cmp al,'0'
	jb error_arg_value

	cmp al,'9'
	ja error_arg_value

	sub al,'0'
	mov ds:[fbuffer],ax
	fild word ptr ds:[fbuffer]

	fadd						;x = x + next_digit
	fdiv st(0),st(2)			;x /= 10

	dec bx
	mov al,ds:[bx]

	loop next_fractional_char

end_loop:
	cmp cx,1d
	jne error_arg_value

	fadd						;x = x_integral + x_fractional

	cmp byte ptr ds:[sign],0d
	je cleanup
	fchs

cleanup:
	fstp st(1)
	ret

negative:
	mov byte ptr ds:[sign],1
	inc bx
	mov al,ds:[bx]
	dec cx						;argument is effectively shorter by minus
	jmp next_char

error_arg_value:
	ERROR_EXIT_STR -3d, err_arg_value
fpusha endp


;convert_args
;
;returns:	exits with negative value if cmdline arguments are not valid
;
convert_args proc
	push ax
	push bx
	push cx
	push dx
	push ds

	LD_STO_SEG ds				;load ds with storage segment
	
	mov al,ds:[argc]			;get number of arguments to al
	xor dx,dx					;dx is index of currently verified argument

	cmp al,6d					;check number of arguments
	je process_xmin

	ERROR_EXIT_STR -2d, err_arg_count	;else: invalid number of arguments

process_xmin:
	ARG dx, bx
	ARGLEN dx, cx
	call fpusha
	fst real8 ptr ds:[xmin]

	inc dx
;xmax
	ARG dx, bx
	ARGLEN dx, cx
	call fpusha
;(xmin < xmax)?
	fsubr

	ftst
	fstsw ax
	sahf
	jbe error_arg_value

	fild word ptr ds:[sw]
	fdiv
	fstp real8 ptr ds:[xdp]

	inc dx
;ymin
	ARG dx, bx
	ARGLEN dx, cx
	call fpusha
	fst real8 ptr ds:[ymin]

	inc dx
;ymax
	ARG dx, bx
	ARGLEN dx, cx
	call fpusha
;(ymin < ymax)?
	fsubr

	ftst
	fstsw ax
	sahf
	jbe error_arg_value

	fild word ptr ds:[sh]
	fdiv
	fstp real8 ptr ds:[ydp]

	inc dx
;cr
	ARG dx, bx
	ARGLEN dx, cx
	call fpusha
	fstp real8 ptr ds:[cr]

	inc dx
;ci
	ARG dx, bx
	ARGLEN dx, cx
	call fpusha
	fstp real8 ptr ds:[ci]

	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	ret

error_arg_value:
	ERROR_EXIT_STR -3d, err_arg_value
convert_args endp


draw proc
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push ds
	push es

	LD_STO_SEG ds
	mov ax,0a000h				;graphics segment
	mov es,ax

	mov ax,0013h				;graphics mode
	int 10h

	fld real8 ptr ds:[four]		;push constants
	fld real8 ptr ds:[ci]
	fld real8 ptr ds:[cr]

	mov bx,SCREEN_WIDTH_PX * SCREEN_HEIGHT_PX - 1 ;offset of the end of the graphics buffer

	mov di,word ptr ds:[sh]
y_loop:							;drawing from bottom right to top left
	mov word ptr ds:[y],di		;push current y pixel
	fild word ptr ds:[y]

	fld real8 ptr ds:[ydp]		;calculate zi = y
	fmul
	fld real8 ptr ds:[ymin]
	fadd

	fld st(0)					;calculate 2y and y^2
	fld st(0)
	fadd st(2),st(0)
	fmul

	fstp real8 ptr ds:[yk]
	fstp real8 ptr ds:[ypy]
	mov si,word ptr ds:[sw]
x_loop:							;fstack: cr ci 4.0
	fld real8 ptr ds:[ypy]		;2y cr ci 4.0
	fld real8 ptr ds:[yk]		;y^2 2y cr ci 4.0

	mov word ptr ds:[x],si		;push current x pixel
	fild word ptr ds:[x]

	fld real8 ptr ds:[xdp]		;x y^2 2y cr ci 4.0
	fmul
	fld real8 ptr ds:[xmin]
	fadd

	fmul st(2),st(0)			;x y^2 2xy cr ci 4.0
	fld st(0)					;x x y^2 2xy cr ci 2.0 4.0
	fmul						;x^2 y^2 2xy cr ci 2.0 4.0

	mov cx,1000d
calc_loop:						;fstack: x^2 y^2 2xy cr ci 4.0
	fsubr						;x^2-y^2 2xy cr ci 4.0
	fadd st(0),st(2)			;x^2-y^2+cr=x' 2xy cr ci 4.0
	fld	st(0)					;x' x' 2xy cr ci 4.0
	fld st(0)					;x' x' x' 2xy cr ci 4.0
	fmul st(2),st(0)			;x' x' x'^2 2xy cr ci 4.0
	fadd						;2x' x'^2 2xy cr ci 4.0
	fxch st(2)					;2xy x'^2 2x' cr ci 4.0
	fadd st(0),st(4)			;2xy+ci=y' x'^2 2x' cr ci 4.0
	fmul st(2),st(0)			;y' x'^2 2x'y' cr ci 4.0
	fld st(0)					;y' y' x'^2 2x'y' cr ci 4.0
	fmul						;y'^2 x'^2 2x'y' cr ci 4.0
	fxch						;x'^2 y'^2 2x'y' cr ci 4.0	- initial form for x', y'

	fld st(0)					;if(x^2 + y^2 > 4) break;
	fadd st(0),st(2)
	fcomp st(6)
	fstsw word ptr ds:[fbuffer]
	mov ax,word ptr ds:[fbuffer]
	sahf
	ja break

	loop calc_loop

	mov cl,byte ptr ds:[julia_colour]
	mov byte ptr es:[bx],cl

end_loop:
	fstp st(0)
	fstp st(0)
	fstp st(0)
	dec bx

	dec si
	jns x_loop					;jump if no sign <==> ax >= 0

	dec di
	jns y_loop


	mov ah,00h					;wait for a key
	int 16h

	mov ax,0003h				;back to text mode
	int 10h

	pop es
	pop ds
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret

break:
	add cx,word ptr ds:[colour_offset]
	mov byte ptr es:[bx],cl
	jmp end_loop
draw endp


main:
	mov ax,stack				;init stack segment
	mov ss,ax
	mov sp,offset top

	call set_random_colour

	call parse_args

	finit

	call convert_args

	call draw

	mov ax,4c00h
	int 21h
code ends


stack segment stack

	db 1024 dup(?)
top	db ?

stack ends

end main
