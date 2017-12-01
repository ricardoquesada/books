'**********************************************************************
'*                         M E D I A I D B . B A S                    *
'**********************************************************************
'*  Task:                   Demonstrates PEEK in use with             *
'*                          DOS interrupt 1BH.                        *
'**********************************************************************

'$INCLUDE: 'QB.BI'                'Include file for interrupt call

DIM RegsX AS RegTypeX
DIM MediaID AS INTEGER

CLS                               'Clear screen
RegsX.AX = &H1B00                 'Function number 1BH
CALL INTERRUPTX(&H21, RegsX, RegsX)
DEF SEG = RegsX.DS                'Define segment
MediaID = PEEK(RegsX.BX)          'Read media ID
PRINT "Media ID = "; MediaID

