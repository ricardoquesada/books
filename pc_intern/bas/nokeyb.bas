'**********************************************************************
'*                           N O K E Y B                              *
'----------------------------------------------------------------------
'*    Task            : Demonstrates clearing the keyboard buffer.    *
'*                      This is useful for protecting the user from   *
'*                      accidental keystrokes during an important     *
'*                      command (e.g., deleting files).               *
'----------------------------------------------------------------------
'*    Author          : Michael Tischer                               *
'*    Developed on    : 01/01/92                                      *
'*    Last update     : 01/28/92                                      *
'**********************************************************************
'
'-- Main program ------------------------------------------------------

DIM i AS INTEGER                                          'Loop counter

CLS
PRINT ("NOKEYB  -  (c) 1992 by Michael Tischer")
PRINT
PRINT ("Keyboard buffer purged when counter reaches 0.")
PRINT

FOR i = 10 TO 0 STEP -1                              'Give user time to
  PRINT i; "     "                                   '  press some keys
  SLEEP 1
NEXT

'ClearKbBuffer                                        'Clear the buffer

'--- Display characters still in keyboard buffer ----------------------

ccount = 0                                          'No more characters
PRINT
PRINT
PRINT ("Characters in keyboard buffer :")

DO                             'Any more characters in keyboard buffer?
  a$ = INKEY$
  IF a$ <> "" THEN
    FOR i = 1 TO LEN(a$)
      PRINT "   "; ASC(MID$(a$, i, 1)),        'Display code only first
      IF ASC(MID$(a$, i, 1)) > 32 THEN                      'Code > 32?
        PRINT "("; MID$(a$, i, 1); ")"; 'Yes --> Display character also
      END IF
      PRINT
      ccount = ccount + 1                'More than one character found
    NEXT
  END IF
LOOP WHILE a$ <> ""

IF ccount = 0 THEN                                  'Out of characters?
  PRINT ("(None)")                                                'Done
END IF
PRINT

END

'**********************************************************************
'* ClearKbBuffer : Clears the contents of the keyboard buffer.        *
'* Input   : None                                                     *
'* Output  : None                                                     *
'**********************************************************************
'
SUB ClearKbBuffer

DO            'Get character from keyboard buffer until buffer is empty
LOOP WHILE INKEY$ <> ""

END SUB

