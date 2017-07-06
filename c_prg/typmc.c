/*********************************************************************/
/*                             T Y P M C                             */
/*-------------------------------------------------------------------*/
/*    Task           : Sets the typematic rate on the MF II keyboard */
/*                     according to user preferences.                */
/*-------------------------------------------------------------------*/
/*    Author         : Michael Tischer                               */
/*    Developed on   : 08/28/88                                      */
/*    Last update    : 01/21/92                                      */
/*-------------------------------------------------------------------*/
/*    Memory model   : SMALL                                         */
/*-------------------------------------------------------------------*/
/*    Modules        : TYPMC.C + TYPMCA.ASM                          */
/*    Info           : Microsoft C requires separate compiling and   */
/*                     linking:                                      */
/*                     CL /AS /c TYPMC.C                             */
/*                     LINK TYPMC TYPMCA;                            */
/*                     If you omit the /c switch, Microsoft C will   */
/*                     give an 'unresolved external' error.          */
/*********************************************************************/
                                                                        
/*== Add include files ==============================================*/
                                                                        
#include <stdlib.h>
#include <stdio.h>
                                                                        
/*== Type definitions ===============================================*/
                                                                        
typedef unsigned char byte;                         /* Create a byte */
typedef byte bool;                          /* Specify TRUE or FALSE */
                                                                        
/*== Constants ======================================================*/
                                                                        
#define TRUE  ( 1 == 1 )            /* Needed when working with BOOL */
#define FALSE ( 0 == 1 )
                                                                        
/*== Link external functions from the assembly language module ======*/
                                                                        
extern bool set_typm( byte trate );        /* Set the typematic rate */
                                                                        
/*********************************************************************/
/**                           MAIN PROGRAM                          **/
/*********************************************************************/
                                                                        
void main(int argc, char *argv[] )
{
 int delay,                            /* Stores the specified delay */
     speed;                      /* Stores the specified repeat rate */
                                                                        
 printf("\nTYPMC  -  (c) 1988, 92 by MICHAEL TISCHER\n");
 if (argc!=3 || ( (delay = atoi(argv[1]))<0 || delay>3 ) ||
                ( (speed = atoi(argv[2]))<0 || speed>31 ))
  {                                /* Illegal parameters were passed */
   printf("Syntax: TYPMC       delay       key_repeat_rate\n");
   printf("                     \x1e              \x1e\n");
   printf("                     ณ              ณ\n");
   printf("     ษอออออออออออออออฯอออป  ษอออออออฯอออออออออออออป\n");
   printf("     บ  0 : 1/4 second   บ  บ  0 : 30.0 reps/sec. บ\n");
   printf("     บ  1 : 1/2 second   บ  บ  1 : 26.7 reps/sec. บ\n");
   printf("     บ  2 : 3/4 second   บ  บ  2 : 24.0 reps/sec. บ\n");
   printf("     บ  3 : 1   second   บ  บ  3 : 21.8 reps/sec. บ\n");
   printf("     วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  บ           .         บ\n");
   printf("     บ All values  +-20%% บ  บ           .         บ\n");
   printf("     ศอออออออออออออออออออผ  บ           .         บ\n");
   printf("                            บ 28 :  2.5 reps/sec. บ\n");
   printf("                            บ 29 :  2.3 reps/sec. บ\n");
   printf("                            บ 30 :  2.1 reps/sec. บ\n");
   printf("                            บ 31 :  2.0 reps/sec. บ\n");
   printf("                            ศอออออออออออออออออออออผ\n");
  }
 else                                     /* The parameters are O.K. */
  {
   if (set_typm( (byte) ((delay << 5) + speed )))   /* Set typematic */
    printf("Typematic rate now set.\n");
   else
    printf("Keyboard controller access error.\n");
  }
}
