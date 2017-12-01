/*********************************************************************/
/*                               P R O C C                           */
/*-------------------------------------------------------------------*/
/*    Task           : Determines the processor type in a PC         */
/*-------------------------------------------------------------------*/
/*    Author         : Michael Tischer                               */
/*    Developed on   : 08/14/88                                      */
/*    Last update    : 03/08/92                                      */
/*-------------------------------------------------------------------*/
/*    (MICROSOFT C)   cl /AS /c procc.c                              */
/*                    link procc procca;                             */
/*    (BORLAND TURBO C)                                              */
/*    Compilation    : Create a project file containing these lines: */
/*                     PROCC                                         */
/*                     PROCCA.ASM                                    */
/*********************************************************************/
                                                                        
#include <stdio.h>

extern int getproz( void );   /* For linking the assembler routine  */
extern int getco( void );
                                                                        
/*********************************************************************/
/**                           MAIN PROGRAM                          **/
/*********************************************************************/
                                                                        
void main()
                                                                        
{
 static char * prozname[] = {  /* Vector with pointers to the names */
                            "Intel 8088",                  /* Code 0 */
                            "Intel 8086",                  /* Code 1 */
                            "NEC V20",                     /* Code 2 */
                            "NEC V30",                     /* Code 3 */
                            "Intel 80188",                 /* Code 4 */
                            "Intel 80186",                 /* Code 5 */
                            "Intel 80286",                 /* Code 6 */
                            "Intel 80386",                 /* Code 7 */
                            "Intel 80486"                  /* Code 8 */
                            };
                                                                        
 static char *coname[] = {      /* Vector with pointers to the names */
                          "No coprocessor",                /* Code 0 */
                          "8087",                          /* Code 1 */
                          "80287",                         /* Code 2 */
                          "80387/80487"                    /* Code 3 */
                          };

 printf("лллллллллллл PROCC (c) 1988, 91 by Michael Tischer ллл\n\n");
 printf("Processor   : %s\n", prozname[ getproz() ] );
 printf("Coprocessor : %s\n\n", coname[ getco() ] );
}
