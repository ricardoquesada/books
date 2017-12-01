/*********************************************************************/
/*                             R A W C O O K                         */
/*-------------------------------------------------------------------*/
/*    Task        : Provides two functions for switching a character */
/*                  device driver into RAW or COOKED mode.           */
/*-------------------------------------------------------------------*/
/*    Author        : MICHAEL TISCHER                                */
/*    Developed on  : 08/16/87                                       */
/*    Last update   : 02/17/92                                       */
/*-------------------------------------------------------------------*/
/*    Compilation:  Microsoft C                                      */
/*                  cl /AS rawcookc                                  */
/*********************************************************************/

#include <dos.h>                                /* Add include files */
#include <stdio.h>
#include <conio.h>

/*== Constants ======================================================*/

#define STANDARDIN 0        /* Handle 0 is the standard input device */
#define STANDARDOUT 1      /* Handle 1 is the standard output device */

/*********************************************************************/
/* GETMODE: Reads device driver attribute.                           */
/* Input  : Handler referring to the addressed device                */
/* Output : Device attribute                                         */
/*********************************************************************/

int GetMode(int Handle)            /* Refers to the character driver */

{
 union REGS Register;        /* Register variable for interrupt call */

 Register.x.ax = 0x4400;      /* Function number for IOCTL: Get Mode */
 Register.x.bx = Handle;
 intdos(&Register, &Register);             /* Call DOS interrupt 21H */
 return(Register.x.dx);                    /* Pass device attribute  */
}

/*********************************************************************/
/* SETRAW : Changes a character driver into RAW mode.                */
/* Input  : Handler referring to the addressed device                */
/* Output : None                                                     */
/*********************************************************************/

int SetRaw(int Handle)             /* Refers to the character driver */

{
 union REGS Register;        /* Register variable for interrupt call */

 Register.x.ax = 0x4401;      /* Function number for IOCTL: Set Mode */
 Register.x.bx = Handle;
 Register.x.dx = GetMode(Handle) & 255 | 32; /* new device attribute */
 intdos(&Register, &Register);             /* Call DOS interrupt 21H */
}

/*********************************************************************/
/* SETCOOKED: Changes a character driver into the COOKED mode.       */
/* Input    : Handler referring to the addressed device              */
/* Output   : None                                                   */
/*********************************************************************/

int SetCooked(int Handle)         /* Refers to the character driver  */

{
 union REGS Register;        /* Register variable for interrupt call */

 Register.x.ax = 0x4401;      /* Function number for IOCTL: Set Mode */
 Register.x.bx = Handle;
 Register.x.dx = GetMode(Handle) & 223;      /* New device attribute */
 intdos(&Register, &Register);             /* Call DOS interrupt 21H */
}

/*********************************************************************/
/* TESTOUTPUT: Displays a test string 1000 times on the standard     */
/*             output device.                                        */
/* Input     : None                                                  */
/* Output    : None                                                  */
/*********************************************************************/

void TestOutput(void)

{
 int i = 1000;                                      /* Loop variable */
 static char Test[] = "Test.... ";            /* The text for output */

 printf("\n");
 for (; i--;)                                  /* Display 1000 times */
  fputs(Test, stdout); /* Display string on the standard output dev. */
 printf("\n");
}

/*********************************************************************/
/**                           MAIN PROGRAM                          **/
/*********************************************************************/

void main()

{
 printf("\nRAWCOOK (c) 1987 by Michael Tischer\n\n");

 printf("The Console Driver (Keyboard, Display) is now in ");
 printf("RAW Mode.\nDuring the following output control characters,\n");
 printf("such as <CTRL-S> will not be recognized.\n");
 printf("Try it.\n\n");
 printf("Please press a key to start...");
 getch();                                            /* Wait for key */
 SetRaw(STANDARDIN);                 /* Console driver into RAW mode */
 TestOutput();
 while (kbhit())                           /* Remove key codes from  */
  getch();                                 /* keyboard buffer        */
 printf("\nThe console driver is now in COOKED mode. ");
 printf("Control keys such as\n<CTRL-S> are recognized during ");
 printf("output and answered accordingly!\n");
 printf("Please press a key to start ...");
 getch();                                            /* Wait for key */
 SetCooked(STANDARDIN);         /* Console driver in the COOKED mode */
 TestOutput();
}
