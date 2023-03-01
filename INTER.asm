model tiny
.code 
locals @@
org 100h

EXITBUTTON equ 1

Start:

    mov di, 0b800h
    mov es, di

    mov di,  160d * 5d + 80d ;set video addres

    mov ah, 0ah 
    @@next: 
        in al, 60h          ;read scancode
        mov es:[di], ax     ;set scan code on video mem

        cmp al, EXITBUTTON
    jne @@next   

    mov ax, 4c00h
    int 21h                 ;exit

end Start