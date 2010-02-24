TurboPower Async Professional


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1 Release history from 4.06 to 4.07 RC5
4.2 Release 5.0.0 RC1


==============================================


1. Introduction

Async Professional is a comprehensive communications toolkit for
Embarcadero/CodeGear Delphi for win32 targets. It provides direct
access to serial ports, TAPI, and the Microsoft Speech API. It
supports faxing, terminal emulation, VOIP, & more.

This is a source-only release of TurboPower Async Professional (AsyncPro).
It includes designtime and runtime packages for Delphi 7 through to Delphi 2010.

For help files and a PDF manual, please see the tpapro_docs package on
SourceForge (http://sourceforge.net/projects/tpapro).

==============================================

2. Package names


TurboPower AsyncPro source package names (*.dpk) have the following form:

  packages\<Compiler>\AsyncProGp.groupproj        -- Project Group source package.
  packages\<Compiler>\AsyncPro.dproj/.dpk         -- Run-time source package.
  packages\<Compiler>\dclAsyncPro.dproj/.dpk      -- Design-time source package.

where <Compiler> is one of:
  <D7>        -- for Delphi 7.
  <D2005>     -- for Delphi 2005.  (win 32 target).
  <D2007>     -- for Delphi 2006 and Delphi 2007.
  <D2010>     -- for Delphi 2010.

TurboPower AsyncPro compiled package names (*.bpl) have the following form
  AsyncPro_<Compiler>.bpl         -- Run-time package.
  dclAsyncPro_<Compiler>.bpl      -- Design-time package.

==============================================

3. Installation


To install TurboPower AsyncPro into your IDE, take the following steps:

  1. Unzip the release files into a directory (e.g., v:\Projects\TurboPower\AsyncPro\5).

  2. Start Delphi.
     This step needs to be repeated for each compiler that you want to install
     AsyncPro into.

  3. If you wish to debug-step into AsycPro source, then you can add the
     source subdirectory (e.g., v:\Projects\TurboPower\AsyncPro\5\run) to the IDE's
     library path. This step is optional.
     This step needs to be repeated for each compiler that you want to install
     AsyncPro into.

  4. For both packages, run-time and design-time, open the project options and set
     the DCU, DCP and BPL output directories, as indicated in the notes below,
     according to your local developmental environment.
     This step needs to be repeated for each compiler that you want to install
     AsyncPro into.

  5. Build the runtime package.
     This step needs to be repeated for each compiler that you want to install
     AsyncPro into.

  6. Compile & install the designtime package.
     This step needs to be repeated for each compiler that you want to install
     AsyncPro into.

  7. Set up the default run-time packages list to include AsyncPro. This step is only
      needed if you want to compile your applications with "Build-with-packages" checked.
     Close all packages, projects and files. Open the project default options as
     indicated in the notes below.
     This step needs to be repeated for each compiler that you want to install
     AsyncPro into. If Build-with-packages is unchecked, you may need to temporarily
     check it. Once checked, add AsyncPro to the list, if it is not already there.


Setting the work-product output directories.
--------------------------------------------
Compiler          Active-project-condition  Options-to-set                                                                        Default-value
Delphi 7          <Package active>          Project | Options | Directories/Conditions | Directories | Unit output directory      $(DCU)
Delphi 7          <Package active>          Project | Options | Directories/Conditions | Directories | DCP output directory       $(DCP)
Delphi 7          <Package active>          Project | Options | Directories/Conditions | Directories | Output directory           $(BPL)
Delphi 7          <No projects open>        Project | Options | Packages | Runtime packages                                       AsyncPro
Delphi 2007/2010  <Package active>          Project | Options | Directories/Conditions | (all configs) | Unit output directory    $(DCU)
Delphi 2007/2010  <Package active>          Project | Options | Directories/Conditions | (all configs) | DCP output directory     $(DCP)
Delphi 2007/2010  <Package active>          Project | Options | Directories/Conditions | (all configs) | Output directory         $(BPL)
Delphi 2007/2010  <No projects open>        Project | Default Options | Delphi for win32 | Packages | Runtime packages            AsyncPro

Similar for other compilers.


==============================================

4. Version history


4.1 Release history from 4.06 to 4.07 RC5

Please refer to earlier versions of this file, via the subversion repository.

4.2 Release 5.0.0 RC1
To be developed.
