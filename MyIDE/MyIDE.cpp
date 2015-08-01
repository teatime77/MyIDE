// MyIDE.cpp : アプリケーションのエントリ ポイントを定義します。
//

#include "stdafx.h"
#include <windowsx.h>
#include <imm.h>
#include "MyIDE.h"
#include <vcclr.h>

using namespace System;
using namespace MyAlgo;

#define MAX_LOADSTRING 100

// グローバル変数:
HINSTANCE hInst;								// 現在のインターフェイス
TCHAR szTitle[MAX_LOADSTRING];					// タイトル バーのテキスト
TCHAR szWindowClass[MAX_LOADSTRING];			// メイン ウィンドウ クラス名
gcroot<TApplication^> gIDE; 
UINT_PTR	gTimerID;
UINT		gTimerInterval;

// このコード モジュールに含まれる関数の宣言を転送します:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

int APIENTRY _tWinMain(_In_ HINSTANCE hInstance,
                     _In_opt_ HINSTANCE hPrevInstance,
                     _In_ LPTSTR    lpCmdLine,
                     _In_ int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

 	// TODO: ここにコードを挿入してください。
	MSG msg;
	HACCEL hAccelTable;

	// グローバル文字列を初期化しています。
	LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadString(hInstance, IDC_MYIDE, szWindowClass, MAX_LOADSTRING);
	MyRegisterClass(hInstance);

	// アプリケーションの初期化を実行します:
	if (!InitInstance (hInstance, nCmdShow))
	{
		return FALSE;
	}

	hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_MYIDE));

	// メイン メッセージ ループ:
	while (GetMessage(&msg, NULL, 0, 0))
	{
		if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return (int) msg.wParam;
}



//
//  関数: MyRegisterClass()
//
//  目的: ウィンドウ クラスを登録します。
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.style			= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon			= LoadIcon(hInstance, MAKEINTRESOURCE(IDI_MYIDE));
	wcex.hCursor		= NULL;	// LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= MAKEINTRESOURCE(IDC_MYIDE);
	wcex.lpszClassName	= szWindowClass;
	wcex.hIconSm		= LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

	return RegisterClassEx(&wcex);
}

//
//   関数: InitInstance(HINSTANCE, int)
//
//   目的: インスタンス ハンドルを保存して、メイン ウィンドウを作成します。
//
//   コメント:
//
//        この関数で、グローバル変数でインスタンス ハンドルを保存し、
//        メイン プログラム ウィンドウを作成および表示します。
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
   HWND hWnd;

   hInst = hInstance; // グローバル変数にインスタンス処理を格納します。

   hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

   if (!hWnd)
   {
      return FALSE;
   }

   ShowWindow(hWnd, nCmdShow);
   UpdateWindow(hWnd);

   return TRUE;
}

//
//  関数: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  目的:  メイン ウィンドウのメッセージを処理します。
//
//  WM_COMMAND	- アプリケーション メニューの処理
//  WM_PAINT	- メイン ウィンドウの描画
//  WM_DESTROY	- 中止メッセージを表示して戻る
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	int wmId, wmEvent;
	PAINTSTRUCT ps;
	HDC hdc;
	HIMC	imc;
	COMPOSITIONFORM	cform;
	RECT	rc1;
	TPnt	^pt1;
	TWnd	^wnd1;
	float	w1, h1;

	switch (message)
	{
	case WM_CREATE:
			TGraphics::Handle	= IntPtr((void*)hWnd);
			TGraphics::InitGraphics();
			GetClientRect(hWnd, &rc1);
			gIDE	= gcnew TIDE(TPnt(float(rc1.right - rc1.left), float(rc1.bottom - rc1.top)));
			gIDE->LoadApp();
			gTimerInterval	= gIDE->MainForm->Interval;
			if(gTimerInterval != 0){
				gTimerID	= SetTimer(hWnd, 1, gTimerInterval, NULL);
			}

		return DefWindowProc(hWnd, message, wParam, lParam);

	case WM_TIMER:
		if(TWnd::Capture != nullptr && TWnd::Capture->MousePressHandler != nullptr){
			TWnd::MouseEvent->TypeEv	= EEvent::eMousePress;
			gIDE->EventHandler(TWnd::MouseEvent);
		}

		gIDE->EventHandler(gcnew TEvent(EEvent::eOnTimer));

		if(gTimerInterval != gIDE->MainForm->Interval){
			gTimerInterval	= gIDE->MainForm->Interval;
			KillTimer(hWnd, gTimerID);
			if(gTimerInterval != 0){
				gTimerID	= SetTimer(hWnd, 1, gTimerInterval, NULL);
			}
		}

		return DefWindowProc(hWnd, message, wParam, lParam);
		
	case WM_SIZE:
		GetClientRect(hWnd, &rc1);
		w1	= float(rc1.right - rc1.left);
		h1	= float(rc1.bottom - rc1.top);
		if(0 < w1 && 0 < h1){
			
			gIDE->EventHandler(gcnew TFormEvent(EEvent::eOnResize, gIDE->MainForm, TPnt(w1, h1)));
		}
		return DefWindowProc(hWnd, message, wParam, lParam);

	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);
		// 選択されたメニューの解析:
		switch (wmId)
		{
		case IDM_ABOUT:
			DialogBox(hInst, MAKEINTRESOURCE(IDD_ABOUTBOX), hWnd, About);
			break;
		case IDM_EXIT:
			DestroyWindow(hWnd);
			break;
		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
		break;
	case WM_PAINT:
		hdc = BeginPaint(hWnd, &ps);
		// TODO: 描画コードをここに追加してください...
		gIDE->EventHandler(gcnew TEvent(EEvent::ePaint));
		EndPaint(hWnd, &ps);
		break;

	case WM_LBUTTONDOWN:
	case WM_MOUSEMOVE:
	case WM_LBUTTONUP:
		TWnd::MouseEvent->Shift		= (HIBYTE(GetKeyState(VK_SHIFT)) != 0);
		TWnd::MouseEvent->Control	= (HIBYTE(GetKeyState(VK_CONTROL)) != 0);
		TWnd::MouseEvent->Alt		= (HIBYTE(GetKeyState(VK_MENU)) != 0);
		TWnd::MouseEvent->PosEv.XPnt = (float)GET_X_LPARAM(lParam); 
		TWnd::MouseEvent->PosEv.YPnt = (float)GET_Y_LPARAM(lParam);
		TWnd::MouseEvent->FormEv	= gIDE->MainForm;
		switch (message){
		case WM_LBUTTONDOWN:
			TWnd::MouseEvent->TypeEv	= EEvent::eMouseDown;
			TWnd::MouseEvent->MouseDownTime	= DateTime::Now;
			SetCapture(hWnd);
			gIDE->EventHandler(TWnd::MouseEvent);
			break;
		case WM_MOUSEMOVE:
			TWnd::MouseEvent->TypeEv	= EEvent::eMouseMove;
			gIDE->EventHandler(TWnd::MouseEvent);
			break;
		case WM_LBUTTONUP:
			TWnd::MouseEvent->TypeEv	= EEvent::eMouseUp;
			gIDE->EventHandler(TWnd::MouseEvent);
			ReleaseCapture();
			break;
		}
		break;
	case WM_KEYDOWN:
		TWnd::KeyEvent->KeyCode	= int(wParam);
		TWnd::KeyEvent->Shift	= (HIBYTE(GetKeyState(VK_SHIFT)) != 0);
		TWnd::KeyEvent->Control	= (HIBYTE(GetKeyState(VK_CONTROL)) != 0);
		TWnd::KeyEvent->Alt		= (HIBYTE(GetKeyState(VK_MENU)) != 0);
		TWnd::KeyEvent->Repeat  = ((HIWORD(lParam) & KF_REPEAT) != 0);
		TWnd::KeyEvent->TypeEv	= EEvent::eOnKeyDown;
		TWnd::KeyEvent->FormEv	= gIDE->MainForm;
		gIDE->EventHandler(TWnd::KeyEvent);
		break;
	case WM_CHAR:
		TWnd::KeyEvent->TypeEv	= EEvent::eOnChar;
		TWnd::KeyEvent->FormEv	= gIDE->MainForm;
		TWnd::KeyEvent->CharEv	= (wchar_t)wParam;
		gIDE->EventHandler(TWnd::KeyEvent);
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	case WM_IME_STARTCOMPOSITION:
		imc	= ImmGetContext(hWnd);
		if(imc != NULL){
			memset(&cform, 0, sizeof(cform));
			cform.dwStyle	= CFS_POINT;
//			cform.dwStyle	= CFS_DEFAULT;

			wnd1	= gIDE->MainForm->GetFocus();
			if(wnd1 != nullptr && TTextBox::typeid->IsInstanceOfType(wnd1)){
				pt1	= ((TTextBox^)wnd1)->GetCursorPos();
				cform.ptCurrentPos.x	= (int)pt1->XPnt;
				cform.ptCurrentPos.y	= (int)pt1->YPnt;
				ImmSetCompositionWindow(imc, &cform);
			}

			ImmReleaseContext(hWnd, imc);
		}
		return DefWindowProc(hWnd, message, wParam, lParam);
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

// バージョン情報ボックスのメッセージ ハンドラーです。
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	UNREFERENCED_PARAMETER(lParam);
	switch (message)
	{
	case WM_INITDIALOG:
		return (INT_PTR)TRUE;

	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
		{
			EndDialog(hDlg, LOWORD(wParam));
			return (INT_PTR)TRUE;
		}
		break;
	}
	return (INT_PTR)FALSE;
}
