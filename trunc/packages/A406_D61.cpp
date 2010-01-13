//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\source\AdAbout.pas", Adabout, ApdAboutForm);
USEFORMNS("..\source\AdPEdit0.pas", Adpedit0, AdPEdit);
USEFORMNS("..\source\AdStatE0.pas", Adstate0, frmConditionEdit);
USEFORMNS("..\source\AdStatEd.pas", Adstated, frmStateEdit);
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
