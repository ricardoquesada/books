'**********************************************************************
'*                           9 H D E M O . B A S                      *
'**********************************************************************
'*  Task:                   Demonstrates VARSEG and VARPTR used with  *
'*                          DOS interrupt 09H.                        *
'**********************************************************************

'$INCLUDE: 'QB.BI'                'Include file for interrupt call

DIM S AS STRING * 20              'Allocate 20 bytes for string
DIM RegsX AS RegTypeX

CLS                               'Clear screen
S = "PC Intern" + "$"             'String
RegsX.ax = &H900                  'Function number 09H
RegsX.ds = VARSEG(S)              'Segment address
RegsX.dx = VARPTR(S)              'Offset address
CALL INTERRUPTX(&H21, RegsX, RegsX)

