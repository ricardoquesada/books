'**********************************************************************
'*                            R T C B                                 *
'*--------------------------------------------------------------------*
'*  Task           : Makes different routines available for reading   *
'*                   and writing realtime clock data.                 *
'*  Author         : Michael Tischer                                  *
'*  Developed on   : 07/24/87                                         *
'*  Last update    : 03/03/92                                         *
'**********************************************************************
'
DECLARE FUNCTION RTCRead% (Address%)
DECLARE FUNCTION RTCDT% (Address%)

CLS                                                       'Clear screen
PRINT "RTCB (c) 1987, 92 by Michael Tischer": PRINT
PRINT "Information from the battery operated realtime clock"
PRINT "===================================================="
PRINT

IF (RTCRead(&HE) AND 128) = 128 THEN        'Bit 7 = 0 --> Battery O.K.
  PRINT "       WARNING! Clock battery is low"
ELSE
  PRINT "- The clock is running in";
  PRINT (RTCRead(&HB) AND 2) * 6 + 12; "hour mode"
 
  PRINT "- the time: ";
  PRINT USING "##:"; RTCDT(&H4);
  PRINT USING "##:"; RTCDT(&H2);
  PRINT USING "##"; RTCDT(&H0)
 
  PRINT "- the date: ";
  PRINT USING "##"; RTCDT(&H8);
  PRINT "-";
  PRINT USING "##"; RTCDT(&H7);
  PRINT "-";
  PRINT USING "####"; RTCDT(&H9) + 1900
  PRINT

END IF

'**********************************************************************
'* RTCDT: Reads the contents of a date or time memory location, and   *
'*        converts the contents to decimal.                           *
'*--------------------------------------------------------------------*
'* Input  : ADDRESS% = The memory address (0-63)                      *
'* Output : The contents of this address in decimal notation          *
'**********************************************************************
'
FUNCTION RTCDT% (Address%)
 
  Ret% = RTCRead(Address%)         'Read contents of the memory address
  IF (RTCRead(&HB) AND 2) <> 0 THEN                  'Test for BCD mode
    RTCDT% = (Ret% AND 15) + INT(Ret% / 16) * 10    'Convert BCD to DEC
  ELSE
    RTCDT% = Ret%
  END IF

END FUNCTION

'**********************************************************************
'* RTCRead: Reads the contents of a memory location on the RTC.       *
'*--------------------------------------------------------------------*
'* Input  : ADDRESS% = The memory address (0-63)                      *
'* Output : The contents of this address, or -1 if this address       *
'*          contains an invalid number                                *
'**********************************************************************
'
FUNCTION RTCRead% (Address%)

  IF (Address% < 0) OR (Address% > 63) THEN
    RTCRead% = -1
  ELSE
    OUT &H70, Address%     'Memory location in the RTC address register
    RTCRead% = INP(&H71)        'Read contents of the RTC data register
  END IF

END FUNCTION

'**********************************************************************
'* RTCWrite: Writes a memory location to the RTC.                     *
'*--------------------------------------------------------------------*
'* Input  : ADDRESS%  = The memory address (0-63)                     *
'*          CONTENTS% = New contents of this memory location          *
'**********************************************************************
'
SUB RTCWrite (Address%, Contents%)
 
  OUT &H70, Address%       'Memory location in the RTC address register
  OUT &H71, Contents%      'Write new contents to the RTC data register

END SUB

