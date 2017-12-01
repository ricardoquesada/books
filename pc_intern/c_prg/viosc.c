/*********************************************************************/
/*                               V I O S C                           */
/*-------------------------------------------------------------------*/
/*    Task           : Determines the type of video card and monitor */
/*                     installed in the system.                      */
/*-------------------------------------------------------------------*/
/*    Author         : Michael Tischer                               */
/*    Developed on   : 10/02/88                                      */
/*    Last update    : 02/18/92                                      */
/*-------------------------------------------------------------------*/
/*    (MICROSOFT C)                                                  */
/*    Compilation    : CL /AS /c VIOSC.C                             */
/*                     LINK VIOSC VIOSCA;                            */
/*    Call           : VIOSC                                         */
/*-------------------------------------------------------------------*/
/*    (BORLAND TURBO C)                                              */
/*    Compilation    : Create project file made of the following:    */
/*                     VIOSC                                         */
/*                     VIOSCA.ASM                                    */
/*    Info           : Some cards may return errors or "unknown"     */
/*********************************************************************/

/*== Declarations of external functions =============================*/

extern void get_vios( struct vios * );

/*== Type defs ======================================================*/

typedef unsigned char BYTE;                         /* Create a byte */

/*== Structures =====================================================*/

struct vios {            /* Describe video card and attached monitor */
             BYTE vcard,
                  monitor;
            };

/*== Constants ======================================================*/

/*-- Constants for the video card -----------------------------------*/

#define NO_VIOS    0                                /* No video card */
#define VGA        1                                     /* VGA card */
#define EGA        2                                     /* EGA card */
#define MDA        3                   /* Monochrome Display Adapter */
#define HGC        4                       /* Hercules Graphics Card */
#define CGA        5                       /* Color Graphics Adapter */

/*-- Constants for monitor type -------------------------------------*/

#define NO_MON     0                                   /* No monitor */
#define MONO       1                           /* Monochrome monitor */
#define COLOR      2                                /* Color monitor */
#define EGA_HIRES  3                   /* High-res/multisync monitor */
#define ANLG_MONO  4                    /* Analog monochrome monitor */
#define ANLG_COLOR 5                         /* Analog color monitor */

/*********************************************************************/
/**                           MAIN PROGRAM                          **/
/*********************************************************************/

void main()

{
 static char *vcnames[] = {        /* Pointer to the video card name */
                           "VGA",
                           "EGA",
                           "MDA",
                           "HGC",
                           "CGA"
                          };

 static char *monnames[] = {   /* Pointer to the monitor type's name */
                            "monochrome monitor",
                            "color monitor",
                            "high-res/multisync monitor",
                            "analog monochrome monitor",
                            "analog color monitor"
                           };

 struct vios vsys[2];                         /* Vector for GET_VIOS */

 get_vios( vsys );                         /* Determine video system */
 printf("\nVIOSC (c) 1988, 1992 by Michael Tischer\n\n");
 printf("Primary video system:   %s card/ %s\n",
         vcnames[vsys[0].vcard-1], monnames[vsys[0].monitor-1]);
 if ( vsys[1].vcard != NO_VIOS ) /* Is there secondary video system? */
   printf("Secondary video system: %s card/ %s\n",
           vcnames[vsys[1].vcard-1], monnames[vsys[1].monitor-1]);
}
