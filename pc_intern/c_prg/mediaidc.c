/*********************** M E D I A I D C . C *************************/

#include <dos.h>
#include <stdio.h>

#ifndef MK_FP      /* MK_FP macro already defined? */
  #define MK_FP(seg,ofs) ((void far *) ((unsigned long) (seg)<<16|(ofs)))
#endif

void main( void )
{
 union REGS pregs;
 struct SREGS sregs;
 unsigned char far *mp;

 pregs.h.ah = 0x1B;
 intdosx( &pregs, &pregs, &sregs );
 mp = MK_FP(sregs.ds, pregs.x.bx);
 printf("Media ID = %d\n ", *mp);
}
