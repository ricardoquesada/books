; Program to demonstrate use of the DAC registers by selecting a
; smoothly contiguous set of 256 colors, then filling the screen
; with concentric diamonds in all 256 colors so that they blend
; into one another to form a continuum of color.
;
        .model small
        .stack  200h
        .data

; Table used to set all 256 DAC entries.
;
; Table format:
;       Byte 0: DAC register 0 red value
;       Byte 1: DAC register 0 green value
;       Byte 2: DAC register 0 blue value
;       Byte 3: DAC register 1 red value
;       Byte 4: DAC register 1 green value
;       Byte 5: DAC register 1 blue value
;                       :
;       Byte 765: DAC register 255 red value
;       Byte 766: DAC register 255 green value
;       Byte 767: DAC register 255 blue value

ColorTable      label   byte

; The first 64 entries are increasingly dim pure green.
X=0
        REPT    64
        db      0,63-X,0
X=X+1
        ENDM

; The next 64 entries are increasingly strong pure blue.
X=0
        REPT    64
        db      0,0,X
X=X+1
        ENDM

; The next 64 entries fade through violet to red.
X=0
        REPT    64
        db      X,0,63-X
X=X+1
        ENDM

; The last 64 entries are increasingly dim pure red.
X=0
        REPT    64
        db      63-X,0,0
X=X+1
        ENDM

        .code
Start:
        mov     ax,0013h        ;AH=0 selects set mode function,
                                ; AL=13h selects 320x200 256-color
        int     10h             ; mode

                                ;load the DAC registers with the
                                ; color settings
        mov     ax,@data        ;point ES to the default
        mov     es,ax           ; data segment
        mov     dx,offset ColorTable
                                ;point ES:DX to the start of the
                                ; block of RGB three-byte values
                                ; to load into the DAC registers
        mov     ax,1012h        ;AH=10h selects set color function,
                                ; AL=12h selects set block of DAC
                                ; registers subfunction
        sub     bx,bx           ;load the block of registers
                                ; starting at DAC register #0
        mov     cx,100h         ;set all 256 registers
        int     10h             ;load the DAC registers

                                ;now fill the screen with
                                ; concentric diamonds in all 256
                                ; color attributes
        mov     ax,0a000h       ;point DS to the display memory
        mov     ds,ax           ; segment
                                ;
                                ;draw diagonal lines in the upper
                                ; left quarter of the screen
        mov     al,2            ;start with color attribute #2
        mov     ah,-1           ;cycle down through the colors
        mov     bx,320          ;draw top to bottom (distance from
                                ; one line to the next)
        mov     dx,160          ;width of rectangle
        mov     si,100          ;height of rectangle
        sub     di,di           ;start at (0,0)
        mov     bp,1            ;draw left to right (distance from
                                ; one column to the next)
        call    FillBlock       ;draw it
                                ;
                                ;draw diagonal lines in the upper
                                ; right quarter of the screen
        mov     al,2            ;start with color attribute #2
        mov     ah,-1           ;cycle down through the colors
        mov     bx,320          ;draw top to bottom (distance from
                                ; one line to the next)
        mov     dx,160          ;width of rectangle
        mov     si,100          ;height of rectangle
        mov     di,319          ;start at (319,0)
        mov     bp,-1           ;draw right to left (distance from
                                ; one column to the next)
        call    FillBlock       ;draw it

                                ;draw diagonal lines in the lower
                                ; left quarter of the screen
        mov     al,2            ;start with color attribute #2
        mov     ah,-1           ;cycle down through the colors
        mov     bx,-320         ;draw bottom to top (distance from
                                ; one line to the next)
        mov     dx,160          ;width of rectangle
        mov     si,100          ;height of rectangle
        mov     di,199*320      ;start at (0,199)
        mov     bp,1            ;draw left to right (distance from
                                ; one column to the next)
        call    FillBlock       ;draw it
                                ;
                                ;draw diagonal lines in the lower
                                ; right quarter of the screen
        mov     al,2            ;start with color attribute #2
        mov     ah,-1           ;cycle down through the colors
        mov     bx,-320         ;draw bottom to top (distance from
                                ; one line to the next)
        mov     dx,160          ;width of rectangle
        mov     si,100          ;height of rectangle
        mov     di,199*320+319  ;start at (319,199)
        mov     bp,-1           ;draw right to left (distance from
                                ; one column to the next)
        call    FillBlock       ;draw it

        mov     ah,1            ;wait for a key
        int     21h             ;

        mov     ax,0003h        ;return to text mode
        int     10h             ;

        mov     ah,4ch          ;done--return to DOS
        int     21h

; Fills the specified rectangular area of the screen with diagonal
; lines.
;
; Input:
;       AL = initial attribute with which to draw
;       AH = amount by which to advance the attribute from
;               one pixel to the next
;       BX = distance to advance from one pixel to the next
;       DX = width of rectangle to fill
;       SI = height of rectangle to fill
;       DS:DN = screen address of first pixel to draw
;       BP = offset from the start of one column to the start of
;               the next

FillBlock:
FillHorzLoop:
        push    di              ;preserve pointer to top of column
        push    ax              ;preserve initial attribute
        mov     cx,si           ;column height
FillVertLoop:
        mov     [di],al         ;set the pixel
        add     di,bx           ;point to the next row in the column
        add     al,ah           ;advance the attribute
        loop    FillVertLoop    ;
        pop     ax              ;restore initial attribute
        add     al,ah           ;advance to the next attribute to
                                ; start the next column
        pop     di              ;retrieve pointer to top of column
        add     di,bp           ;point to next column
        dec     dx              ;have we done all columns?
        jnz     FillHorzLoop    ;no, do the next column
        ret                     ;

        end     Start
