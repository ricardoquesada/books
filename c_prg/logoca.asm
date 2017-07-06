;*********************************************************************;
;*                        L O G O C A . A S M                        *;
;*-------------------------------------------------------------------*;
;*    Task           : Creates a function for redefining existing    *;
;*                     characters on EGA and VGA cards.              *;
;*-------------------------------------------------------------------*;
;*    Author         : Michael Tischer                               *;
;*    Developed on   : 08/07/90                                      *;
;*    Last update    : 02/21/92                                      *;
;*-------------------------------------------------------------------*;
;*    Assembly       : TASM -mx LOGOCA   or     MASM -mx LOGOCA;     *;
;*********************************************************************;

	  DOSSEG                  ;Arrange segment
	  .MODEL SMALL, C         ;Link object code to a C program 
				  ;using SMALL memory model
				  
;== Code ==============================================================

	  .CODE

;-- DEFCHAR: Specifies character pattern for EGA/VGA characters
;-- Declaration in C : void defchar( BYTE ascii, BYTE table, BYTE height
;--                                  BYTE numchar, void far * buf );
;-- Input            : ASCII   = Number of first redefinable character
;--                    TABLE   = Number of font table
;--                    HEIGHT  = Character height in scan lines
;--                    NUMCHAR = Number of characters
;--                    BUF     = FAR pointer to buffer
;-- Output   : None

defchar   proc ascii:byte, table:byte, height:byte, \
	       numchar:byte, buf:dword

	  mov  ax,1100h           ;Call function 11H, sub-function 00H
	  mov  bh,height          ;Load parameters into
	  mov  bl,table           ;appropriate registers
	  mov  dl,ascii
	  xor  dh,dh
	  mov  cl,numchar
	  mov  ch,dh

	  push bp                 ;Push BP onto stack
	  les  bp,buf
	  int  10h                ;Call BIOS video interrupt
	  pop  bp                 ;Pop BP off of stack

	  ret                     ;Return to caller

defchar   endp

;== End ===============================================================

	  end

