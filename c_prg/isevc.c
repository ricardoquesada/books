/**********************************************************************
*                             I S E V C . C                           *
**-------------------------------------------------------------------**
*  Task             : Tests for an active EGA or VGA card.            *
**-------------------------------------------------------------------**
*  Author           : Michael Tischer                                 *
*  Developed on     : 08/06/90                                        *
*  Last update      : 02/26/92                                        *
**-------------------------------------------------------------------**
*  (MICROSOFT C)                                                      *
*  Compilation      : CL /AS isevc.c                                  *
**-------------------------------------------------------------------**
*  (BORLAND TURBO C)                                                  *
*  Compilation      : Use the integrated development environment      *
**-------------------------------------------------------------------**
*  Call             : isevc                                           *
**********************************************************************/

#include <dos.h>                                /* Add include files */
#include <stdarg.h>
#include <stdio.h>

/*-- Constants ------------------------------------------------------*/

#define EGA_MONO  0                           /* EGA and MDA monitor */
#define EGA_COLOR 1                           /* EGA and EGA monitor */
#define VGA_MONO  2             /* VGA and analog monochrome monitor */
#define VGA_COLOR 3                           /* VGA and VGA monitor */
#define NEITHERNOR 4                               /* No VGA, no EGA */

/*-- Type declarations ----------------------------------------------*/

typedef unsigned char BYTE;

/**********************************************************************
*  IsEgaVga : Determines whether an EGA or a VGA card is installed.   *
**-------------------------------------------------------------------**
*  Input   : None                                                     *
*  Output  : Taken from constants EGA_MONO, EGA_COLOR, etc.           *
**********************************************************************/

BYTE IsEgaVga( void )
{
 union REGS Regs;          /* Processor registers for interrupt call */

 Regs.x.ax = 0x1a00;             /* Function 1AH applies to VGA only */
 int86( 0x10, &Regs, &Regs );
 if ( Regs.h.al == 0x1a )              /* Is the function available? */
   switch ( Regs.h.bl )                         /* Yes --> Pass code */
    {
     case  4 : return EGA_COLOR;
     case  5 : return EGA_MONO;
     case  7 : return VGA_MONO;
     case  8 : return VGA_COLOR;
     default : return NEITHERNOR;
    }
 else                             /* Not a VGA, but it may be an EGA */
  {
   Regs.h.ah = 0x12;                           /* Call function 12H, */
   Regs.h.bl = 0x10;                           /* sub-function 10H   */
   int86(0x10, &Regs, &Regs );                    /* Call video BIOS */
   if ( Regs.h.bl != 0x10 )                                  /* EGA? */
    return Regs.h.bh == 0 ? EGA_COLOR : EGA_MONO;             /* Yes */
   else                                                        /* No */
    return NEITHERNOR;
  }
}

/**********************************************************************
*                        M A I N   P R O G R A M                      *
**********************************************************************/

void main( void )
{
 printf( "ISEVP  -  (c) 1990, 1992 by Michael Tischer\n\n" );
 switch ( IsEgaVga() )
  {
   case NEITHERNOR :
     printf( "The active video card is neither EGA nor VGA");
     break;

   case EGA_MONO  :
     printf( "This is an EGA card with an MDA monitor" );
     break;

   case EGA_COLOR :
     printf( "This is an EGA card with an EGA or " \
             "multiscan monitor" );
     break;

   case VGA_MONO  :
     printf( "This is a VGA card with an analog monochrome monitor");
     break;

   case VGA_COLOR :
     printf( "This is a VGA card with a VGA or " \
             "multiscan monitor" );
  }
 printf( "\n\n" );
}

