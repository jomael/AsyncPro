//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("A406_D41.res");
USEUNIT("..\source\Adpedit0.pas");
USEUNIT("..\source\Adpeditt.pas");
USEUNIT("..\source\AdPropEd.pas");
USEUNIT("..\source\Aproreg.pas");
USEUNIT("..\source\Adabout.pas");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vclx40.bpi");
USEFORMNS("..\source\AdStatEd.pas", Adstated, frmStateEdit);
USEFORMNS("..\source\AdStatE0.pas", Adstate0, frmConditionEdit);
USEPACKAGE("A406_R41.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
