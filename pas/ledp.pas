{*********************************************************************}
{*                            L E D P                                *}
{*-------------------------------------------------------------------*}
{*    Task           : Sets the various bits in the BIOS keyboard    *}
{*                     status byte,causing the LEDs on the MF II     *}
{*                     keyboard flash.                               *}
{*-------------------------------------------------------------------*}
{*    Author         : Michael Tischer                               *}
{*    Developed on   : 08/16/88                                      *}
{*    Last update    : 01/23/92                                      *}
{*********************************************************************}

program LEDP;

uses CRT,                                 { Add the CRT and DOS units }
     DOS;

const SCRL =  16;                                   { Scroll Lock bit }
      NUML =  32;                                      { Num Lock bit }
      CAPL =  64;                                     { Caps Lock bit }
      INS  = 128;                                        { Insert bit }

{*********************************************************************}
{* SETFLAG: Sets one the flags in the BIOS keyboard status byte.     *}
{* Input  : The flag to be set (see constants)                       *}
{* Output : None                                                     *}
{*********************************************************************}

procedure SetFlag(Flag : byte);

var BiosTSByte : byte absolute $0040:$0017;   { BIOS kbd. status byte }
    Regs       : Registers;  { Processor registers for interrupt call }

begin
  BiosTSByte := BiosTSByte or Flag;  { Mask out the corresponding bit }
  Regs.AH := 1;                    {   Function no.: Character ready? }
  intr($16, Regs);                     { Call BIOS keyboard interrupt }
end;


{*********************************************************************}
{* CLRFLAG: clears one of the flags in the BIOS keyboard status byte.*}
{* Input  : the flag to be cleared (see constants)                   *}
{* Output : none                                                     *}
{*********************************************************************}

procedure ClrFlag(Flag : byte);

var BiosTSByte : byte absolute $0040:$0017;   { BIOS kbd. status byte }
    Regs       : Registers;  { Processor registers for interrupt call }

begin
  BiosTSByte := BiosTSByte and ( not Flag );           { mask out bit }
  Regs.AH := 1;                      { Function no.: character ready? }
  intr($16, Regs);                    { Call BIOS keyboard interrupt  }
end;

{*********************************************************************}
{**                           MAIN PROGRAM                          **}
{*********************************************************************}

var counter : integer;

begin
  writeln('LEDP  -  (c) 1988 by Michael Tischer');
  writeln(#13,#10, 'Watch the LEDs on your keyboard');

  for counter:=1 to 10 do             { Run through the loop 10 times }
    begin
      SetFlag( CAPL);                                   { Enable CAPS }
      Delay( 100 );                           { Wait 100 milliseconds }
      ClrFlag( CAPL );                                 { Disable CAPS }
      SetFlag( NUML);                                    { Enable NUM }
      Delay( 100 );                           { Wait 100 milliseconds }
      ClrFlag( NUML );                                  { Disable NUM }
      SetFlag( SCRL);                            { Enable SCROLL LOCK }
      Delay( 100 );                           { Wait 100 milliseconds }
      ClrFlag( SCRL );                          { Disable SCROLL LOCK }
    end;

  for counter:=1 to 10 do                 { Run through loop 10 times }
    begin
      SetFlag(CAPL or SCRL or NUML);             { All three flags on }
      Delay( 500 );                           { Wait 500 milliseconds }
      ClrFlag(CAPL or SCRL or NUML);            { All flags off again }
      Delay( 500 );                           { Wait 500 milliseconds }
    end;
end.
