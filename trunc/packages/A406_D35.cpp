//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("A406_D35.res");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vclx35.bpi");
USEUNIT("..\source\AproReg.pas");
USEUNIT("..\source\Adpedit0.pas");
USEUNIT("..\source\Adpeditt.pas");
USEUNIT("..\source\AdPropEd.pas");
USEUNIT("..\source\Adabout.pas");
USEFORMNS("..\source\AdStatEd.pas", Adstated, frmStateEdit);
USEFORMNS("..\source\AdStatE0.pas", Adstate0, frmConditionEdit);
USEPACKAGE("A406_R35.bpi");
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
