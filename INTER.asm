.286
model tiny
.code 
locals @@
org 100h

HACKER_STYLE equ 3Fh;0ah
FRAME_BUTTON equ 002d ;bscan code button '1'
WIDTH_LENGTH equ 80d
HIGHT_WIDTH_FRAME equ 1010h

;saves generals registers to curRegVal
;-----------------------------------------------------------------
;Assumes: cs
;Destroy: di
;-----------------------------------------------------------------
SAVE_GEN_REG macro     
            
            push di
            pusha

            mov di, 16d
            @@next:
                sub di, 2d
                pop word ptr cs:[curRegVal + di]
            cmp di, 0d
            jne @@next 

            pop di

            endm

Start:

    xor di, di
    mov es, di      ;start interrupt table
    mov di, 9d * 4d ;9th interrupt address
   
    cli     ;stop all interrupts
    mov dx, es:[di]         
    mov Old09Ofs, dx             ;save old interrupt 09h offset

    mov es:[di], offset NewInt09 ;new offset  interrupt program

    mov dx, es:[di + 2]         
    mov Old09Seg, dx             ;save old interrupt 09h offset

    mov dx, cs                   ;new segment interrupt program
    mov es:[di + 2], dx          ;save new segment to interrupt table
    sti     ;start all interrupts

    ;resident memmory
    mov dx, offset ProgramEnd
    shr dx, 4d
    inc dx 

    mov ax, 3100h
    int 21h
    ;resident memmory

    ;-----------------------------------------------------------------
	;Interrupt function for drawing a frame on button click
	;-----------------------------------------------------------------
    ;Entry: none
    ;Exit: none
    ;Destroy: ax, cx, dx, si, di, es, ds
	;-----------------------------------------------------------------
    NewInt09 proc
        pushf           ;save flags
        push ax cx dx si di es ;save registers


        SAVE_GEN_REG


        in al, 60h    ;read from port 60h
        cmp al, FRAME_BUTTON
        jne @@noFrame        
            call RegInfo
        @@noFrame:

        in al, 61h
        or al, 80h
        out 61h, al
        and al, not 80h
        out 61h, al     ;ppi command that character was read

        mov al, 20h ;finished the current interrupt
        out 20h, al ;we can move on to the next interrupt


        pop es di si dx cx ax    ;restore registers
        popf            ;restore flags

        db 0eah     ;far jump
    Old09Ofs dw 0   ;old offset of 09h interrupt
    Old09Seg dw 0   ;old segment og 09h interrupt
        
    NewInt09 endp

    ;-----------------------------------------------------------------
	;Draw frame with registers value
	;-----------------------------------------------------------------
    ;Assumes: curRegVal - registers value
    ;Entry: none
	;Exit: none
	;Destroy: ax, bx, cx, dx, si, di, df
	;-----------------------------------------------------------------
	RegInfo	proc
        mov di, 0b800h
        mov es, di
        xor di, di   ;vidmem addres

        push ds
        mov ax, cs  
        mov ds, ax  ;preservsave to ds cs toing the case of cs in ds in order for string functions to work correctly

        push HIGHT_WIDTH_FRAME ;hight:width

        push offset frameSample1    ;sample
        push HACKER_STYLE ;style
        call DrawFrame

        pop ds

        mov word ptr cs:[regAdrPrt], WIDTH_LENGTH * 1d * 2d + 1d * 2d ;shift 
        call PrintReg

        ret
	RegInfo	endp    

    ;-----------------------------------------------------------------
	;Draw frame in video memory (pascal)
	;-----------------------------------------------------------------
	;Param: [bp + 4] - frame's color, [bp + 6] - frame's sample
	;		[bp + 8] - frame's width, [bp + 9] - frame's highth
	;Assumes: es = VIDMEM_ADR, ds = sourse segment
	;Entry: di (destination index), si (source index)
	;Exit: none
	;Destroy: ax, ch, cl, di, si
	;-----------------------------------------------------------------
	DrawFrame	proc
		push bp
		mov bp, sp
		
		mov ah, byte ptr [bp + 4] ;define frame's color
		mov si, word ptr [bp + 6] ;define frame's sample

		xor cx, cx 	;free cx

		mov cl, byte ptr [bp + 8] 	;cx = width
		call DrawLine

		mov cl, byte ptr [bp + 8]	;cx = width
		shl cl, 1d                  ;cx * 2

		sub di, cx	;return di to start line
		add di, WIDTH_LENGTH * 2d ;next line

		sub byte ptr [bp + 9], 2d 	;hight -= 2

		@@next:
			mov cl, byte ptr [bp + 8] 	;cx = width
			call DrawLine
		
			sub si, 3d	;return si to cur 3 sybmbols

			mov cl, byte ptr [bp + 8]	;expansion to word
			shl cl, 1d                  ;cx * 2
			
			sub di, cx	;return di to start line
			add di, WIDTH_LENGTH * 2d ;next line
			
		dec byte ptr [bp + 9] 		;hight--
		cmp byte ptr [bp + 9], 0d	;-----------------------
		jne @@next  ;check condition вр != 0
		
		add si, 3d  ;next three symbol

		mov cl, byte ptr [bp + 8] 	;cx = width
		call DrawLine

		pop bp
		ret 2d * 3d
		
	DrawFrame	endp

	;-----------------------------------------------------------------
	;Draw line in video memory from source
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR, ds = sourse sigment
	;Entry: ah(frame's color), cx (counter)
	;Exit: none
	;Destroy: al, cx, di, si, df
	;-----------------------------------------------------------------
	DrawLine	proc

		cld 		;DF = 0

		lodsb		;save symbol to al
		stosw 		;draw left symbol

		sub cx, 2d	;counter -= 2	
		lodsb		;save symbol to al
		rep stosw 	;draw middle symbol

		lodsb		;save symbol to al
		stosw 		;draw right symbol

		ret	
		
	DrawLine	endp

    ;-----------------------------------------------------------------
	;Print cur value from General regisers
	;-----------------------------------------------------------------
    ;Assumes: regAdrPrt = entry address, curRegVal - registers value
    ;Entry: none
	;Exit: none
	;Destroy: ax, bx, cx, dx, si, di, df
	;-----------------------------------------------------------------
	PrintReg	proc
        cld 		;DF = 0
        
        mov di, word ptr cs:[regAdrPrt] ;set in di entry addres

        mov ah, HACKER_STYLE

        xor bx, bx
        @@next:
            mov dx, word ptr cs:[curRegVal + bx]
            add di, 8d    ;shift by 3 cell
            call HexRep

            add di, 14d   ;shift by 7 cell
            
            mov al, byte ptr cs:[regGenName + bx]
            stosw
            inc bx

            mov al, byte ptr cs:[regGenName + bx]
            stosw
            inc bx

            add di, WIDTH_LENGTH * 2d  ;next line
            sub di, 10d                ;table alignment

        
        cmp bx, 16d ;dra all registers
        jne @@next

        ret
	PrintReg	endp

    ;-----------------------------------------------------------------
	;hex representation of a number
	;-----------------------------------------------------------------
	;The number is printed from right to left.
    ;The record address will start from the given address - 8d
    ;Assumes: Segment memory
	;Entry: ah (color), dx (input number), di (address in video mem),
	;Exit: none
	;Destroy: al, cx, dx, di, si, df
	;-----------------------------------------------------------------
	HexRep	proc

        std      ;setting the df flag to 1 
        
        mov cx, 4d
		@@next:
			mov si, 000fh               ;hex mask
			and si, dx			        ;get last digit
			mov al, cs:[hexCode + si]  ;suppose cur symbol is num

			stosw

            shr dx, 4d				;get a new next digit
		loop @@next

		ret
	HexRep	endp
hexCode db "0123456789ABCDEF" ;hex represenation
regGenName dw "ax", "bx", "cx", "dx", "si", "di", "sp", "dp"    ;registers name
regAdrPrt  dw 0 ;address where to write register values
curRegVal  dw 8 dup (0) ;current registers value
frameSample1 db 0dah, 0c4h, 0bfh, 0c3h, 20h, 0b4h, 0c0h, 0c4h, 0d9h  ;patern for frame

ProgramEnd:

end Start