; AtieDOS 2.x< and AngeliDOS 1.x< Bootloader
; Copyright (c) 2020 AngeliSoftware, AtieSoftware & Midn. All rights reserved.
; Licensed under the 3-clause BSD License. See it here:
; Bootsector by AtieSoftWare & Midn.
; https://github.com/AtieP/AtieDOS/blob/master/LICENSE

bits 16
org 0x7c00

jmp 0x00:main

main:

    xor ax,ax
    mov ds,ax
    mov es,ax
    mov ss,ax

    mov bp,0x7c00
    mov sp,bp
    
    mov bx,0x7e00
    mov dh, (KERNEL_END - KERNEL_START + 511) / 512
    call disk_load
    mov ax,0x7e00
    jmp ax

    disk_load:

    pusha 
    push dx

    mov si, 8 ; number of tries
    mov al, dh

    .try:

    mov ah, 0
    int 13h

    mov ah, 0x02
    mov cx, 0x02
    mov ch, 0x00
    mov dh, 0x00
    int 13h

    jnc .itworked

    dec si
    jnz .try
    jmp disk_error
    .itworked:

    pop dx
    cmp al,dh
    jne sectors_error

    popa
    ret

disk_error:

    mov bx, BOOTLOADER_DISK_ERROR_MSG
    call bootloader_print
    call bootloader_nl
    mov dh,ah
    call bootloader_phex
    jmp disk_loop

sectors_error:                  ; if there was a sectors error, code goes here.

  mov bx, BOOTLOADER_SECTORS_ERROR_MSG    ; we move to bx this
  call bootloader_print                   ; to print it with this.

disk_loop:

  jmp disk_loop     ; this is to hang

bootloader_print:     ; this prints strings

  pusha               ; we push all registers

.r:                   ; while loop that ends when a string ends with ,0

  mov al, [bx]        ; moves to al the string in bx
  cmp al, 0           ; if there's a 0
  je .d               ; printing ends
  mov ah, 0x0e        ; we tell to int 10h that we want to print chars
  int 10h             ; video interrupt
  add bx, 1           ; add to bx 1, this means we're reading next char
  jmp .r              ; jmp to .r label

.d:                   ; printing ends
  popa                ; we "pusha", so now we "popa"
  ret                 ; go back to the moment we've called bootloader_print

bootloader_phex:

  pusha

  mov cx, 0

.r:

  cmp cx, 4
  je .d

  mov ax, dx
  and ax, 0x000f
  add al, 0x30
  cmp al, 0x30
  jle .n
  add al, 7

.n:

  mov bx, BOOTLOADER_HEX_OUT
  sub bx, cx
  mov [bx], al
  ror dx, 4

  add cx, 1
  jmp .r

.d:

  mov bx, BOOTLOADER_HEX_OUT
  call bootloader_print

  popa
  ret

bootloader_nl:

  pusha
  mov ah, 0x0e
  mov al, 0x0a
  int 10h
  mov al, 0x0d
  int 10h
  popa
  ret

; strings

BOOTLOADER_HEX_OUT: db "0x0000", 0                            
BOOTLOADER_DISK_ERROR_MSG: db "Disk read error", 0
BOOTLOADER_SECTORS_ERROR_MSG: db "Sectors read error", 0


; a bootloader's size is exactly 512 bytes, so we convert all this code to 510 bytes of compiled code
times 510 - ($ - $$) db 0
dw 0xaa55   ; why 510 and not 512 bytes? we need 2 bytes to put this "magic number", that tells to the BIOS
            ; we're bootable
KERNEL_START:         ; remember we did "mov ax, 0x7e00" and then "jmp ax"? 0x7e00 is here
   ; AngeliDOS 1.0.0 Keyboard system calls
; Copyright (c) 2020 AtieSoftware.
; See LICENSE in root folder



; os_keystroke_echo
; Waits for keystroke and prints the char.
; IN: Nothing
; OUT: AH = BIOS Scan code, AL = ASCII Key
os_keystroke_echo:

    xor ax, ax
    int 16h

    push ax             ; we push ax to avoid the lose
    mov ah, 0x0e        ; of the out registers
    int 10h
    pop ax

    jmp os_keystroke_echo

; os_keystroke
; Waits for keystroke but it doesn't print the character.
; IN: Nothing
; OUT: AH = BIOS Scan code, AL = ASCII Key
os_keystroke:

    xor ax, ax
    int 16h

    ret
                      
times (512 - (($ - $$ + 0x7c00) & 511) + 1) / 2 dw 0xad21   ; 0xad21: AngeliDOS 1.0.0
; ^ magic padding
KERNEL_END:
