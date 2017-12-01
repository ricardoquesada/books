{*********************************************************************}
{*                              V I O S P                            *}
{*-------------------------------------------------------------------*}
{*    Task           : Returns the type of video card installed.     *}
{*-------------------------------------------------------------------*}
{*    Author         : Michael Tischer                               *}
{*    Developed on   :  10/02/88                                     *}
{*    Last update    :  02/18/92                                     *}
{*********************************************************************}

program VIOSP;


{$L viospa}                                   { Link assembler module }

const NO_VIOS    = 0;                                 { No video card }
      VGA        = 1;                                      { VGA card }
      EGA        = 2;                                      { EGA card }
      MDA        = 3;                    { Monochrome Display Adapter }
      HGC        = 4;                        { Hercules Graphics Card }
      CGA        = 5;                        { Color Graphics Adapter }

      NO_MON     = 0;                                    { No monitor }
      MONO       = 1;                            { Monochrome monitor }
      COLOR      = 2;                                 { Color monitor }
      EGA_HIRES  = 3;                       { High-resolution monitor }
      ANLG_MONO  = 4;                     { Monochrome analog monitor }
      ANLG_COLOR = 5;                          { Color analog monitor }

type Vios = record        { Describes video card and attached monitor }
             VCard,
             Monitor : byte;
            end;
     ViosPtr = ^Vios;                   { Pointer to a VIOS structure }

procedure GetVios( vp : ViosPtr ) ; external ;

var VidSys : array[1..2] of Vios; { Array containing video structures }

{*********************************************************************}
{* PrintSys: Gives information about a video system.                 *}
{* Input   : - VCARD: Code number of the video card                  *}
{*           - MON  : Code number of the attached monitor            *}
{* Output  : None                                                    *}
{*********************************************************************}

procedure PrintSys( VCard, Mon : byte );

begin
  write(' ');
  case VCard of
    NO_VIOS : write('Unknown');                    { For "other" code }
    VGA : write('VGA');
    EGA : write('EGA');
    MDA : write('MDA');
    CGA : write('CGA');
    HGC : write('HGC');
  end;
  write(' card/ ');
  case Mon of
    MONO       : writeln('monochrome monitor');
    COLOR      : writeln('color monitor');
    EGA_HIRES  : writeln('high-resolution monitor');
    ANLG_MONO  : writeln('monochrome analog monitor');
    ANLG_COLOR : writeln('color analog monitor');
  end;
end;

{*********************************************************************}
{**                           MAIN  PROGRAM                         **}
{*********************************************************************}

begin
  GetVios( @VidSys );                    { Check installed video card }
  writeln('VIOSP  -  (c) 1988, 1992 by Michael Tischer');
  write('Primary video system: ');
  PrintSys( VidSys[1].VCard, VidSys[1].Monitor );
  if VidSys[2].VCard <> NO_VIOS then { Second video system installed? }
    begin                                                       { Yes }
      write('Secondary video system:');
      PrintSys( VidSys[2].VCard, VidSys[2].Monitor );
    end;
end.
