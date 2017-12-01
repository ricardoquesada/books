{*********************************************************************}
{*                         M E D I A I D P . P A S                   *}
{---------------------------------------------------------------------}
{*  Task:                  Demonstrates pointer reading in use with  *}
{*                         DOS interrupt 1BH.                        *}
{*********************************************************************}

program MediaIdP;

uses Dos;                                              { Add Dos unit }

type MediaPtr = ^byte;                        { Create a byte pointer }

var  Regs : Registers;       { Processor registers for interrupt call }
     MP   : MediaPtr;                    { Variable for media pointer }

begin
  Regs.AH := $1B;                           { Pass 1BH to AH register }
  MsDos(Regs);                               { Call DOS interrupt 1BH }
  MP := ptr(Regs.DS, Regs.BX);                         { Read pointer }
  writeln('Media ID = ', MP^);                     { Display media ID }
end.
