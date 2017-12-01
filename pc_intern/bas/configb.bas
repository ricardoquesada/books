'**********************************************************************
'*                             CONFIGB                                *
'*--------------------------------------------------------------------*
'*  Task           : Displays the configuration of the PC.            *
'*                   QuickBASIC and the QB.LIB must be loaded using   *
'*                   QB /L QB                                         *
'*                   before loading and running this file.            *
'*--------------------------------------------------------------------*
'*  Author         : Michael Tischer                                  *
'*  Developed on   : 06/10/1991                                       *
'*  Last update    : 01/07/1992                                       *
'**********************************************************************

DECLARE SUB PrintConfig ()
DECLARE FUNCTION GetWord& (Register AS INTEGER)
										
'$INCLUDE: 'QB.BI'                      'Contains register declarations
										
CONST TRUE = -1                                              '   Define
CONST FALSE = NOT TRUE                                       'constants
										
CALL PrintConfig                                 'Display configuration
END

'**********************************************************************
'* GetWord : Converts an integer number (2 bytes plus leading         *
'*           character) into a long integer, which can be modified by *
'*           bit operations to perform math functions(\, MOD).        *
'* Input   : See below                                                *
'* Output  : See below                                                *
'**********************************************************************
FUNCTION GetWord& (Register AS INTEGER)

IF Register <= 0 THEN                                      'BIT 16 set?
  GetWord = 65536 + Register   'Return pos. equivalent of a neg. number
ELSE                                                   'BIT 16 not set?
  GetWord = Register                        'Integer number is positive
END IF
END FUNCTION

'**********************************************************************
'* PrintConfig : Displays PC configuration                            *
'* Input       : None                                                 *
'* Output      : None                                                 *
'* Info        : Configuration varies with the type of PC             *
'**********************************************************************
SUB PrintConfig
										
DIM AT AS INTEGER                                         'Is PC an AT?
DIM Word AS LONG                                            'Get a word
DIM Register AS RegType         'Processor registers for interrupt call
										
CLS                                                       'Clear screen
DEF SEG = &HF000          'Segment address of model identification byte
IF PEEK(&HFFFE) = &HFC THEN                          'Determine PC type
   AT = TRUE                                               'It is an AT
ELSE                                                   'It is not an AT
   AT = FALSE
END IF
PRINT "CONFIGB  -  (c) 1987, 1991 by Michael Tischer": PRINT
PRINT "Your PC Configuration "
PRINT "------------------------------------------------------------"
PRINT "PC type              : ";
SELECT CASE PEEK(&HFFFE)                      'Read PC type and display
    CASE &HFF                                             '&HFF is a PC
       PRINT "PC"
    CASE &HFE                                            '&HFE is an XT
       PRINT "XT"
    CASE &HFC                                            '&HFC is an AT
       PRINT "AT or higher"
END SELECT
CALL INTERRUPT(&H12, Register, Register)       'RAM from BIOS interrupt
PRINT "Conventional RAM     :"; Register.ax; "K"
IF AT THEN                                          'If the PC is an AT
  Register.ax = &H8800        'Read function number for extended memory
  CALL INTERRUPT(&H15, Register, Register)'Call BIOS cassette interrupt
  PRINT "Additional RAM       :"; Register.ax; "K over 1 megabyte"
END IF
CALL INTERRUPT(&H11, Register, Register)       'Call BIOS configuration
PRINT "Default video mode   : ";               'configuration interrupt
SELECT CASE (Register.ax MOD 256) AND 48                'Get video mode
    CASE 0
      PRINT "Undefined"
    CASE 16
      PRINT "40x25 character color card"
    CASE 32
      PRINT "80x25 character color card"
    CASE 48
      PRINT "80x25 character mono card"
END SELECT
Word = GetWord(Register.ax)                    'Convert integer to word
PRINT "Disk drives          :"; (((Word MOD 256) \ 64) AND 3) + 1
PRINT "Serial   interfaces  :"; ((Word \ 256) \ 2) AND 3
PRINT "Parallel interfaces  :"; (Word \ 256) \ 64
END SUB

