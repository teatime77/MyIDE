// MySys.h

#pragma once

using namespace System;
using namespace System::Reflection;

namespace MySys {

	public ref class TSysC{
	public:
		static void SetCursor(int i){
			static LPWSTR v[] = {
				IDC_ARROW,
				IDC_IBEAM,
				IDC_WAIT,
				IDC_CROSS,
				IDC_UPARROW,
				IDC_SIZE,
				IDC_ICON,
				IDC_SIZENWSE,
				IDC_SIZENESW,
				IDC_SIZEWE,
				IDC_SIZENS,
				IDC_SIZEALL,
				IDC_NO,
				IDC_HAND,
				IDC_APPSTARTING,
				IDC_HELP,
			};
			static HCURSOR vh[sizeof(v)/sizeof(v[0])];
			if(0 <= i && i < sizeof(v)/sizeof(v[0])){
				if(vh[i] == NULL){
					vh[i]	= ::LoadCursor(NULL, v[i]);
				}
				::SetCursor(vh[i]);
			}
		}
	};
}
