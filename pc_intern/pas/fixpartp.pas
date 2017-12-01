{*********************************************************************}
{*                      F I X P A R T P . P A S                      *}
{*-------------------------------------------------------------------*}
{*    Task           : Displays hard disk partitioning.              *}
{*-------------------------------------------------------------------*}
{*    Author         : Michael Tischer                               *}
{*    Developed on   : 04/26/89                                      *}
{*    Last update    : 02/12/92                                      *}
{*-------------------------------------------------------------------*}
{*    Call           : FIXPARTP [ Drive_number ]                     *}
{*                     Default is drive 0 (C:)                       *}
{*********************************************************************}

uses Dos;                                              { Add DOS unit }

{== Type declarations ================================================}

type  SecPos    = record                { Describes a sector position }
                    Head : byte;                    { Read/write head }
                    SecCyl : word;       { Sector and cylinder number }
                  end;

      PartEntry = record                      { Partition table entry }
                    Status   : byte;               { Partition status }
                    StartSec : SecPos;                 { First sector }
                    PartTyp  : byte;                 { Partition type }
                    EndSec   : SecPos;                  { Last sector }
                    SecOfs   : longint;       { Offset of boot sector }
                    SecNum   : longint;           { Number of sectors }
                  end;

      PartSec   = record                 { Describes partition sector }
                    BootCode  : array [0..$1BD] of byte;
                    PartTable : array [1..4] of PartEntry;
                    IdCode    : word;                         { $AA55 }
                  end;

{*********************************************************************}
{*  ReadPartSec : Reads a partition sector from the hard drive.      *}
{**-----------------------------------------------------------------**}
{*  Input  : - DS       : BIOS code for drive (80H, 81H, etc.)       *}
{*           - Head     : Number of read/write heads                 *}
{*           - SctCyl   : Sector/cylinder numbers in BIOS format     *}
{*           - Buf      : Buffer to which sector is passed           *}
{*********************************************************************}

function ReadPartSec( DS, Head : byte;
                      SctCyl         : word;
                      var Buf        : PartSec ) : boolean;

var Regs : Registers;        { Processor registers for interrupt call }

begin
  Regs.AX := $0201;               { Funct. no.: READ for first sector }
  Regs.DL := DS;                              { Pass other parameters }
  Regs.DH := Head;                            { to their respective   }
  Regs.CX := SctCyl;                          { registers             }
  Regs.ES := seg( Buf );
  Regs.BX := ofs( Buf );
  Intr( $13, Regs);                       { Call hard drive interrupt }
  ReadPartSec := ( Regs.Flags and 1 ) = 0;       { Carry flag = error }
end;

{*********************************************************************}
{*  GetSecCyl: Gets the combined sector/cylinder coding of the BIOS  *}
{*             sector and cylinder number.                           *}
{**-----------------------------------------------------------------**}
{*  Input  : SctCyl   : Value to be decoded                          *}
{*           Sector   : Sector variable reference                    *}
{*           Cylinder : Cylinder variable reference                  *}
{*********************************************************************}

procedure GetSecCyl( SctCyl : word; var Sector, Cylinder : integer );

begin
  Sector   := SctCyl and 63;                      { Mask bits 6 and 7 }
  Cylinder := hi( SctCyl ) + ( lo( SctCyl) and 192 ) shl 2;
end;

{*********************************************************************}
{*  ShowPartition: Displays hard drive partitioning on the screen.   *}
{**-----------------------------------------------------------------**}
{*  Input  : DS : Number of the corresponding hard drive             *}
{*********************************************************************}

procedure ShowPartition( ds : byte );

var Head     : byte;                      { Head of current partition }
    SecCyl   : byte;       { Sector and cylinder of current partition }
    ParSec   : PartSec;                    { Current partition sector }
    Entry    : byte;                                   { Loop counter }
    Sector,                                         { Get sector and  }
    Cylinder : integer;                             { cylinder number }
    Regs     : Registers;    { Processor registers for interrupt call }

begin
  writeln;
  ds := ds + $80;                     { Prepare drive number for BIOS }
  if ReadPartSec( ds, 0, 1, ParSec ) then     { Read partition sector }
    begin                                      { Sector could be read }
      Regs.AH := 8;                       { Funct. no.: Read drive ID }
      Regs.DL := ds;
      Intr( $13, Regs);                   { Call hard drive interrupt }
      GetSecCyl( Regs.CX, Sector, Cylinder );
      writeln('ษอออออออออออออออออออออออออออออออออออออออออออออ'+
              'ออออออออออออออออออออออออออออออป');
      writeln('บ Drive ', ds-$80, ':    ', Regs.DH+1:2,
              ' heads, ', Cylinder:5, ' cylinders, ',
              Sector:3, ' sectors                        บ');
      writeln('บ Partition Table in Partition Sector       '+
              '                                บ');
      writeln('ฬออัออออัอออออออออออออออออออัออออออออออออออั'+
              'ออออออออออออออัอออออออออัอออออออน');
      writeln('บ  ณ    ณ                   ณ     Start    ณ'+
              '     End      ณDis.from ณ       บ');
      writeln('บNoณBootณType               ณHead Cyl. Sec.ณ'+
              'Head Cyl. Sec.ณBoot Sec.ณ Total บ');
      writeln('ฬออุออออุอออออออออออออออออออุออออออออออออออุ'+
              'ออออออออออออออุอออออออออุอออออออน');
      for Entry:=1 to 4 do                             { Show entries }
        with ParSec.PartTable[ Entry ] do
          begin
          write('บ ', Entry, 'ณ');
          if Status = $80 then write (' Y  ')
                               else write (' N  ');
          write('ณ');
          case PartTyp of                    { Compute partition type }
            $00        : write('Not allocated      ');
            $01        : write('DOS, 12-bit FAT    ');
            $02 or $03 : write('XENIX              ');
            $04        : write('DOS, 16-bit FAT    ');
            $05        : write('DOS, ext. partition');
            $06        : write('DOS 4.0 > 32 MB    ');
            $DB        : write('Concurrent DOS     ');
            else         write('Unknown   (',PartTyp:3,')    ');
          end;
          GetSecCyl( StartSec.SecCyl, Sector, Cylinder );
          write('ณ', StartSec.Head:2,' ',Cylinder:5,'  ',Sector:3 );
          GetSecCyl( EndSec.SecCyl, Sector, Cylinder );
          write(' ณ', EndSec.Head:2,' ',Cylinder:5,'  ',Sector:3 );
          writeln(' ณ  ', SecOfs:7,'ณ', SecNum:7,'บ');
        end;
      writeln('ศออฯออออฯอออออออออออออออออออฯออออออออออออออฯ'+
              'ออออออออออออออฯอออออออออฯอออออออผ'#13#10);
    end
  else
    writeln('Error during boot sector access');
end;

{**********************************************************************
*                          M A I N   P R O G R A M                    *
**********************************************************************}

var DS,                                         { Variables for hard  }
    ErrArg    : integer;                        { drive arguments     }

begin
  writeln( #13#10'ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ FIXPARTP - (c)',
           ' 1989, 92 by MICHAEL TISCHER ÛÛÛÛÛ' );
  DS := 0;                              { Default is first hard drive }
  if ParamCount = 1 then           { User entered different argument? }
    begin                                                       { Yes }
      val( ParamStr(1), DS, ErrArg );      { Convert ASCII to decimal }
      if ErrArg <> 0 then                  { Error during conversion? }
        begin                                                   { Yes }
          writeln(#13#10'Invalid drive number');
          exit;                                         { End program }
        end;
    end;
  ShowPartition( DS );                     { Display partition sector }
end.
