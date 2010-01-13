//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("A406_D51.res");
USERES("..\source\aproreg.dcr");
USEUNIT("..\source\Aproreg.pas");
USEUNIT("..\source\Adpedit0.pas");
USEUNIT("..\source\Adpeditt.pas");
USEUNIT("..\source\AdPropEd.pas");
USEUNIT("..\source\AdStatEd.pas");
USEUNIT("..\source\AdStatE0.pas");
USEUNIT("..\source\Adabout.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("A406_R51.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
