;*********************************************************************;
;*                      S O F T S C C A . A S M                      *;
;*-------------------------------------------------------------------*;
;*    Task           : Creates a function used by SOFTSCRC.C for     *;
;*                     implementing a pointer to the EGA/VGA         *;
;*                     8x14 font table.                              *;
;*-------------------------------------------------------------------*;
;*    Author         : Michael Tischer                               *;
;*    Developed on   : 08/23/90                                      *;
;*    Last update    : 02/21/92                                      *;
;*-------------------------------------------------------------------*;
;*    Assembly       : MASM /mx SOFTSCCA;    or   TASM -mx SOFTSCCA  *;
;*                     ... link to SOFTSCRC.C                        *;
;*********************************************************************;

IGROUP group _text                ;Program segment
DGROUP group const,_bss,  _data   ;Data segment
       assume CS:IGROUP, DS:DGROUP, ES:DGROUP, SS:DGROUP

CONST  segment word public 'CONST';This segment handles all
CONST  ends                       ;readable constants

_BSS   segment word public 'BSS'  ;This segment handles all uninitial-
_BSS   ends                       ;ized static variables

_DATA  segment word public 'DATA' ;This segment handles all initialized
                                  ;global and static variables
                                
_DATA  ends

;== Program ===========================================================

_TEXT  segment byte public 'CODE' ;Program segment

public   _getfontptr              ;Function accessible from other
                                  ;programs

;-- GETFONTPTR: Returns FAR pointer to the 8x14 font table ------------
;-- Declaration : void far * getfontptr( void )

_getfontptr proc near

          push  bp                ;Push BP onto stack

          mov   ax,1130h          ;Load register for function call
          mov   bh,2
          int   10h               ;Call BIOS video interrupt

          mov   dx,es             ;Move ES;BP to DX:AX
          mov   ax,bp

          pop   bp                ;Pop BP from stack

          ret                     ;Return to caller

_getfontptr endp                  ;End of procedure

;== End ===============================================================

_text    ends                     ;End program segment
         end                      ;End program
