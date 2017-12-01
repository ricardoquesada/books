/************* 9 H D E M O M C . C *****************/

#include <dos.h>              /* Microsoft version */

void main( void )
{
 union REGS pregs;
 struct SREGS sregs;
 char Message[20] = "PC Intern$";
 void far *mesptr = Message;/* FAR pntr. to string */

 pregs.h.ah = 0x09;
 sregs.ds = FP_SEG( mesptr );   /* Pass address to */
 pregs.x.dx = FP_OFF( mesptr ); /* FAR pointer     */
 intdosx( &pregs, &pregs, &sregs );
}
