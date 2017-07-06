/************* 9 H D E M O B C . C *****************/

#include <dos.h>                /* Borland version */

void main( void )
{
 union REGS pregs;
 struct SREGS sregs;
 char Message[20] = "PC Intern$";

 pregs.h.ah = 0x09;
 sregs.ds = FP_SEG( Message );   /* Get the var. */
 pregs.x.dx = FP_OFF( Message ); /* addresses    */
 intdosx( &pregs, &pregs, &sregs );
}
