{*********************************************************************}
{*                            P R O C P                              *}
{*-------------------------------------------------------------------*}
{*    Task           : Examines the processor type in the PC and     *}
{*                     tells the user the processor type             *}
{*-------------------------------------------------------------------*}
{*    Author         : MICHAEL TISCHER                               *}
{*    Developed on   : 08/16/1988                                    *}
{*    Last update    : 01/14/1992                                    *}
{*********************************************************************}

program PROCP;

{-- Declaration of assembler routines --------------------------------}

{$L procpa}                                   { Link assembler module }

function GetProz : integer; external;
function GetCo   : integer; external;

{-- Types and global variables ---------------------------------------}

type  ProName = string[20];                { Array of processor names }

const ProcName : array [0..8] of ProName =
                            ( 'INTEL 8088',                  { Code 0 }
                              'INTEL 8086',                  { Code 1 }
                              'NEC V20',                     { Code 2 }
                              'NEC V30',                     { Code 3 }
                              'INTEL 80188',                 { Code 4 }
                              'INTEL 80186',                 { Code 5 }
                              'INTEL 80286',                 { Code 6 }
                              'INTEL 80386',                 { Code 7 } 
                              'INTEL 80486');                { Code 8 }
                 CoName : array[0..3] of ProName =
                               ('No coprocessor',            { Code 0 }
                                '8087',                      { Code 1 }
                                '80287',                     { Code 2 }
                                '80387/80487');              { Code 3 }

{**********************************************************************}
{**                           MAIN PROGRAM                           **}
{**********************************************************************}

begin
  writeln('PROCP  -  (c) 1988 by MICHAEL TISCHER');
  writeln;
  writeln('Processor      : ', ProcName[GetProz]);
  writeln('Coprocessor    : ', CoName [ GetCo ] );
writeln;
end.
