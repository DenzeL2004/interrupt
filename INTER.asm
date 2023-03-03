.286
model tiny
.code 
locals @@
org 100h

HACKER_STYLE equ 0ah
FRAME_BUTTON equ 002d ;bscan code button '1'

WIDTH_VID_MEM equ 80d

HIGHT_FRAME equ 0Eh
WIDTH_FRAME equ 0Bh

BYTE_TRUE equ 0FFh

CNT_REGISTERS equ 12d

BUFFER_SIZE equ 200d

;saves generals registers to curRegVal
;-----------------------------------------------------------------
;Assumes: cs
;Destroy: none
;-----------------------------------------------------------------
SAVE_GEN_REG macro     
            
            push di
            pusha
            push ds es ss cs

            mov di, CNT_REGISTERS * 2d
            @@next:
                sub di, 2d
                pop word ptr cs:[curRegVal + di]
            cmp di, 0d
            jne @@next 

            pop di

            endm

;-----------------------------------------------------------------
;Dealy of programm
;-----------------------------------------------------------------
;Entry:   none
;Exit:    none
;Destroy: none
;-----------------------------------------------------------------
Delay   macro cnt
            
        push cx
        
        xor cx, cx
        dec cx
        @@delay:
            nop
        loop @@delay

        pop cx

        endm

Start:

    call DefNewInt09

    call DefNewInt08

    ;resident memmory
    mov dx, offset ProgramEnd
    shr dx, 4d
    inc dx 

    mov ax, 3100h
    int 21h
    ;resident memmory

    ;-----------------------------------------------------------------
	;set a new interrupt 09h function
	;-----------------------------------------------------------------
    ;Entry: none
    ;Exit: Old09Ofs - offset previous interrupt 09h, Old09Seg - segment previous interrupt 09h
    ;Destroy: di, es
	;-----------------------------------------------------------------
    DefNewInt09 proc
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
        
        ret
    DefNewInt09 endp

    ;-----------------------------------------------------------------
	;Interrupt 09h function for drawing a frame on button click
	;-----------------------------------------------------------------
    ;Entry: none
    ;Exit: none
    ;Destroy: ax, cx, dx, si, di, es, ds
	;-----------------------------------------------------------------
    NewInt09 proc
        pushf        ;save flags
        push ax      ;save registers

        in al, 60h    ;read from port 60h
        cmp al, FRAME_BUTTON
        jne @@noFrame        
            not cs:[flagEntryButton]    ;button was entry
        @@noFrame:

        in al, 61h
        or al, 80h
        out 61h, al
        and al, not 80h
        out 61h, al     ;ppi command that character was read

        mov al, 20h ;finished the current interrupt
        out 20h, al ;we can move on to the next interrupt

        pop ax   ;restore registers
        popf        ;restore flags

        db 0eah     ;far jump
    Old09Ofs dw 0   ;old offset of 09h interrupt
    Old09Seg dw 0   ;old segment og 09h interrupt
        
    NewInt09 endp

    ;-----------------------------------------------------------------
	;set a new interrupt 08h function
	;-----------------------------------------------------------------
    ;Entry: none
    ;Exit: Old08Ofs - offset previous interrupt 08h, Old08Seg - segment previous interrupt 09h
    ;Destroy: di, es
	;-----------------------------------------------------------------
    DefNewInt08 proc
        xor di, di
        mov es, di      ;start interrupt table
        mov di, 8d * 4d ;8th interrupt address
    
        cli     ;stop all interrupts
        mov dx, es:[di]         
        mov Old08Ofs, dx             ;save old interrupt 09h offset

        mov es:[di], offset NewInt08 ;new offset  interrupt program

        mov dx, es:[di + 2]         
        mov Old08Seg, dx             ;save old interrupt 09h offset

        mov dx, cs                   ;new segment interrupt program
        mov es:[di + 2], dx          ;save new segment to interrupt table
        sti     ;start all interrupts
        
        ret
    DefNewInt08 endp

    ;-----------------------------------------------------------------
	;Interrupt 08h function for drawing a frame on button click
	;-----------------------------------------------------------------
    ;Entry: none
    ;Exit: none
    ;Destroy: ax, cx, dx, si, di, es, ds
	;-----------------------------------------------------------------
    NewInt08 proc
        pushf                   ;save flags
        push ax bx cx dx si di es ds  ;save registers

        SAVE_GEN_REG

        cmp byte ptr cs:[flagEntryButton], BYTE_TRUE
        jne @@noFrame   
            call RegInfo
            Delay
        @@noFrame:

        mov al, 20h ;finished the current interrupt
        out 20h, al ;we can move on to the next interrupt

        pop ds es di si dx cx bx ax   ;restore registers
        popf                    ;restore flags

        db 0eah     ;far jump
    Old08Ofs dw 0   ;old offset of 09h interrupt
    Old08Seg dw 0   ;old segment og 09h interrupt
        
    NewInt08 endp

    ;-----------------------------------------------------------------
	;Draw frame with registers value
	;-----------------------------------------------------------------
    ;Entry:none
	;Exit: none
	;Destroy: ax, cx, dx, si, di, df
	;-----------------------------------------------------------------
	RegInfo	proc

        SAVE_GEN_REG
       
        call printToDrawBuffer

        mov di, 0b800h
        mov es, di      ;set destination segment

        xor di, di      ;set destination start addres

        mov si, cs
        mov ds, si      ;set source segment

        mov si, offset drawBuffer      ;set source start addres

        push WIDTH_FRAME * 2d
        push WIDTH_VID_MEM * 2d
        push HIGHT_FRAME

        call coppyBuffer

        ret
	RegInfo	endp    

    ;-----------------------------------------------------------------
	;Draw in drawbuffer
	;-----------------------------------------------------------------
    ;Assumes: curRegVal - registers value
    ;Entry: none
	;Exit: none
	;Destroy: ax, cx, dx, si, di, df
	;-----------------------------------------------------------------
	printToDrawBuffer	proc
        mov  di, cs
        
        push es
        mov es, di  ;change segment to correct work strok functions

        push ds 
        mov ds, di  ;change segment to correct work strok functions

        mov di, offset drawBuffer   ;draw buffer addres

        push WIDTH_FRAME * 2d       ;drawDuffer's shift
        push (HIGHT_FRAME shl 8) or WIDTH_FRAME) ;hight:width

        push offset frameSample1    ;sample
        push HACKER_STYLE           ;style
        call DrawFrame

        mov word ptr cs:[regAdrPrt], (offset drawBuffer + WIDTH_FRAME * 1d * 2d + 1d * 2d) ;shift 

        push WIDTH_FRAME * 2d       ;shift
        call PrintReg

        pop ds es     ;recover ds es

        ret
	printToDrawBuffer	endp  

    ;-----------------------------------------------------------------
	;Coppy chanck of memory 
	;-----------------------------------------------------------------
    ;Param: [bp + 8] - width chanck source, [bp + 6] - width chanck destination,  
    ;       [bp + 4] - count of chancks  
    ;Warning: width chanck source must be less than width chanck destination
    ;Assumes: es - destination buffer, ds - source buffer
    ;Entry: si - index sorurce byffer, di - index destination buffer
	;Exit: none
	;Destroy: si, di, cx, df
	;-----------------------------------------------------------------
	coppyBuffer	proc
        push bp
		mov bp, sp

        cld 		;DF = 0
		
        @@next:

            mov cx, word ptr [bp + 8]   ;counter
            shr cx, 1d                  ;cx /= 2 because width chanc mul 2(for correct recalculation) 

            rep movsw                   ;coppy

            sub di, word ptr [bp + 8]   ;------------------
            add di, word ptr [bp + 6]   ;address correction

        dec word ptr [bp + 4]
        cmp word ptr [bp + 4], 0        ;if count of chancks equel 0
        jne @@next
		
		pop bp
		ret 2d * 3d
	coppyBuffer	endp  

    ;-----------------------------------------------------------------
	;Draw frame in video memory (pascal)
	;-----------------------------------------------------------------
	;Param: [bp + 4] - frame's color, [bp + 6] - frame's sample
	;		[bp + 8] - frame's width, [bp + 9] - frame's highth
    ;       [bp + 10] - shift in buffer
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
		add di, word ptr [bp + 10] ;next line

		sub byte ptr [bp + 9], 2d 	;hight -= 2

		@@next:
			mov cl, byte ptr [bp + 8] 	;cx = width
			call DrawLine
		
			sub si, 3d	;return si to cur 3 sybmbols

			mov cl, byte ptr [bp + 8]	;expansion to word
			shl cl, 1d                  ;cx * 2
			
			sub di, cx	;return di to start line
			add di, word ptr [bp + 10] ;next line
			
		dec byte ptr [bp + 9] 		;hight--
		cmp byte ptr [bp + 9], 0d	;-----------------------
		jne @@next  ;check condition вр != 0
		
		add si, 3d  ;next three symbol

		mov cl, byte ptr [bp + 8] 	;cx = width
		call DrawLine

		pop bp
		ret 2d * 4d
		
	DrawFrame	endp

	;-----------------------------------------------------------------
	;Draw line in video memory from source
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR, ds = sourse segment
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
	;Print cur value from regisers
	;-----------------------------------------------------------------
    ;Param: [bp + 4] - shift in buffer
    ;Assumes: regAdrPrt = entry address, curRegVal - registers value
    ;Entry: none
	;Exit: none
	;Destroy: ax, bx, cx, dx, si, di, df
	;-----------------------------------------------------------------
	PrintReg	proc
        push bp
		mov bp, sp

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

            add di, word ptr [bp + 4]  ;next line
            sub di, 10d                 ;table alignment

        cmp bx, CNT_REGISTERS * 2d ;dra all registers
        jne @@next

        pop bp
		ret 2d * 1d
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

flagEntryButton db 0

regGenName dw "ax", "bx", "cx", "dx", "si", "di", "sp", "dp", "ds", "es", "ss", "cs"    ;registers name
regAdrPrt  dw 0 ;address where to write register values
curRegVal  dw CNT_REGISTERS dup (0) ;current registers value

frameSample1 db 0dah, 0c4h, 0bfh, 0c3h, 20h, 0b4h, 0c0h, 0c4h, 0d9h  ;patern for frame

saveBuffer dw BUFFER_SIZE dup (0)
drawBuffer dw BUFFER_SIZE dup (0)

ProgramEnd:

end Start