/*********************************************************************/
/*                            S O U N D C                            */
/*-------------------------------------------------------------------*/
/*    Task           : Plays a scale between octaves 3 and 5 of the  */
/*                     PC musical range, using an assembler function */
/*-------------------------------------------------------------------*/
/*    Author         : Michael Tischer                               */
/*    Developed on   : 08/15/87                                      */
/*    Last update    : 02/04/92                                      */
/*-------------------------------------------------------------------*/
/*    (MICROSOFT C)                                                  */
/*    Creation       : CL /AS /c SOUNDC.C                            */
/*                     LINK SOUNDC SOUNDCA;                          */
/*    Call           : SOUNDC                                        */
/*-------------------------------------------------------------------*/
/*    (BORLAND TURBO C)                                              */
/*    Creation       : Create a project file listing the following:  */
/*                     soundc                                        */
/*                     soundca.asm                                   */
/*    Options        : Before compiling and linking, select the      */
/*                     Options menu and Linker option. Under the     */
/*                     Linker options menu, make sure that the       */
/*                     Case sensitive link option is set to Off      */
/*********************************************************************/

#include <stdio.h>

/*== Function declaration from the assembler module =================*/

extern void Sound(int Note, int Duration);     /* Add the external   */
                                               /* assembler routine  */

/*********************************************************************/
/**                           MAIN  PROGRAM                         **/
/*********************************************************************/

void main( void )

{
 int Note;

 printf("\nSOUND (c) 1987, 92 by Michael Tischer\n\n");
 printf("Your PC should now be playing a musical scale in the 3rd & ");
 printf(" 5th octaves of\nits range. If you aren't hearing the notes");
 printf(" your PC's speaker may be damaged.\n\n");

 for (Note = 0; Note < 35; Sound(Note++, 9))    /* Play a note once  */
  ;                                             /* each 1/2 second   */
 printf("End\n");
}
