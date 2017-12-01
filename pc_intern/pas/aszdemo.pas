{*********************************************************************}
{*                       A S Z D E M O . P A S                       *}
{*-------------------------------------------------------------------*}
{*      Task:  Converts Pascal strings to ASCIIZ strings.            *}
{*********************************************************************}

program ASZDemo;                       

var ASCIIZ : string[100];
         i : integer;

begin
  write('String:');
  readln(ASCIIZ);
  ASCIIZ := ASCIIZ + chr(0);
  for i := 0 to ord(ASCIIZ[0]) do
    begin
      write(i:2, '   ', ord(ASCIIZ[i]):3);
      if (ASCIIZ[i] > ' ') then
        write('  ', ASCIIZ[i]);
      writeln;
    end;
end.
