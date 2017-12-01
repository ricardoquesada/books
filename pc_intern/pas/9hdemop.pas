{*********************************************************************}
{*                        9 H D E M O P . P A S                      *}
{---------------------------------------------------------------------}
{*  Task:                 Demonstrates Seg() and Ofs() in use with   *}
{*                        DOS interrupt 9H.                          *}
{*********************************************************************}

program H9DemoP;

uses Dos;

var Regs    : Registers;
    Message : string[20];

begin
  Message := 'PC Intern' + '$';

  Regs.AH := $09;
  Regs.DS := seg(Message[1]);
  Regs.DX := ofs(Message[1]);
  MsDos(regs);
end.
