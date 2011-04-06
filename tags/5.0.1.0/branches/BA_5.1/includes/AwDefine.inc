(***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Async Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1991-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{*                   AWDEFINE.INC 5.0.0                  *}
{*********************************************************}
{* Compiler defines affecting all APRO units             *}
{*********************************************************}

{Because AWDEFINE.INC is included in all APRO units, you can specify global
compiler options here. AWDEFINE is included *before* each units' required
compiler options, so options specified here might be overridden by hardcode
options in the source file.}

{.$DEFINE TapiDebug}                      { creates detailed TAPI debug log }
{.$DEFINE AdModemDebug}                   { adds debug info to dispatcher log }



{-------- !! DON'T CHANGE DEFINES BELOW THIS LINE !! -----------------}


 {$ifdef conditionalexpressions}
   {$if Declared( RTLVersion) and (RTLVersion >= +22)}
     {$define DELPHI_Future}
   {$ifend}
 {$endif}

 {$ifdef VER210}
  // Commodore. Delphi 2010? RTLVersion = 21?
  {$define DELPHI_2010}
 {$endif}

 {$ifdef VER200}
  // Tiburón. Delphi 2009. RTLVersion = 20
  {$define DELPHI_2009}
 {$endif}

 {$ifdef VER190}
  // Spacely. Delphi 2007 for .NET. RTLVersion = 19
  {$define DELPHI_2006or7}
  {$define DELPHI_2007}
 {$endif}

 {$ifdef VER180}
  {$define DELPHI_2006or7}
  {$ifdef VER185}
    // Spacely. Delphi 2007 for win32. RTLVersion = 18.5
    {$define DELPHI_2007}
   {$else}
    // DeXter. Delphi 2006. RTLVersion = 18
    {$define DELPHI_2006}
  {$endif}
 {$endif}

 {$ifdef VER170}
  {$define DELPHI_2005}
 {$endif}

 {$ifdef VER160}
  {$define DELPHI_8}
 {$endif}

 {$ifdef VER150}
  {$define DELPHI_7}
 {$endif}

 {$ifdef VER140}
    {$define DELPHI_6}
 {$endif}

 {$ifdef VER130}
    {$define DELPHI_5}
 {$endif}

 {$ifdef VER120}
  {$define DELPHI_4}
 {$endif}

 {$ifdef VER100}
  {$define DELPHI_3}
 {$endif}

 {$ifdef VER90}
  {$define DELPHI_2}
 {$endif}

 {$ifdef VER80}
  {$define DELPHI_1}
 {$endif}

{$ifdef DELPHI_Future}
  {$define DELPHI_2010_UP}
  {$define DELPHI_2009_UP}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2010}
  {$define DELPHI_2010_UP}
  {$define DELPHI_2009_UP}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2009}
  {$define DELPHI_2009_UP}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2007}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2006}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2005}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_8}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_7}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_6}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_5}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_4}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_3}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2}
  {$define DELPHI_2_UP}
{$endif}






{-------- !! Compiler compatibility Check !! -----------------}
{$ifndef DELPHI_7_UP}
  ERROR ! TurboPower AsyncPro is not defined for this compiler.
{$endif}

