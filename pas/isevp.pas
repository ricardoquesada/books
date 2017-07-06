{**********************************************************************
*                          I S E V P . P A S                          *
**-------------------------------------------------------------------**
*  Task           : Tests for an active EGA or VGA card.              *
**-------------------------------------------------------------------**
*  Author         : Michael Tischer                                   *
*  Developed on   : 08/05/90                                          *
*  Last update    : 02/26/92                                          *
**********************************************************************}

Program IsEgaVgaP;

uses DOS;                                              { Add Dos unit }

{-- Type declarations ------------------------------------------------}

type VCARD = ( EGA_MONO, EGA_COLOR, VGA_MONO, VGA_COLOR, NEITHERNOR );

{**********************************************************************
*  IsEgaVga : Determines whether an EGA or a VGA card is installed.   *
**-------------------------------------------------------------------**
*  Input   : None                                                     *
*  Output  : Video card type of type VCARD                            *
**********************************************************************}

function IsEgaVga : VCARD;

var Regs : Registers;        { Processor registers for interrupt call }

begin
  Regs.AX := $1a00;                { Function 1AH applies to VGA only }
  Intr( $10, Regs );
  if ( Regs.AL = $1a ) then              { Is the function available? }
    case Regs.BL of                               { Yes --> Pass code }
         4 : IsEgaVga := EGA_COLOR;
         5 : IsEgaVga := EGA_MONO;
         7 : IsEgaVga := VGA_MONO;
         8 : IsEgaVga := VGA_COLOR;
      else IsEgaVga := NEITHERNOR;
    end
  else                              { Not a VGA, but it may be an EGA }
    begin
      Regs.ah := $12;                            { Call function 12H, }
      Regs.bl := $10;                            { sub-function 10H   }
      intr($10, Regs);                              { Call video BIOS }
      if ( Regs.bl <> $10 ) then                          { EGA card? }
        begin                           { Yes --> Then which monitor? }
          if Regs.BH = 0 then IsEgaVga := EGA_COLOR
                         else IsEgaVga := EGA_MONO;
        end
      else IsEgaVga := NEITHERNOR;
    end;
end;

{*********************************************************************}
{**                    M A I N   P R O G R A M                      **}
{*********************************************************************}

begin
  writeln( 'ISEVP  -  (c) 1990, 1992 by Michael Tischer'#13#10 );
  case IsEgaVga of
    NEITHERNOR : writeln( 'The active video card is ' +
                          'neither EGA nor VGA');

    EGA_MONO  : writeln( 'This is an EGA card with an MDA monitor');

    EGA_COLOR : writeln( 'This is an EGA card with an EGA or '+
                         'multiscan monitor' );

    VGA_MONO  : writeln( 'This is a VGA card with an analog ' +
                         'monochrome monitor');

    VGA_COLOR : writeln( 'This is a VGA card with a VGA or '+
                         'multiscan monitor' );
  end;
end.
