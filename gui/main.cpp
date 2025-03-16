// #include <iostream>
// #include <limits>
#include <cstdlib>
#include <cstring>
#include "algorithm.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

// for all others, include the necessary headers (this file is usually all you
// need because it includes almost all "standard" wxWidgets headers)
#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/clipbrd.h"

using namespace std;
 
const char currentVersion[4] = "1.0";

class MyApp : public wxApp {
    public:
    bool OnInit() override;
};
 
wxIMPLEMENT_APP(MyApp);
 
class MyFrame : public wxFrame {
    public:
    MyFrame();

    private:
    void OnGetHash(wxCommandEvent& event);
    void OnExit(wxCommandEvent& event);
    void OnAbout(wxCommandEvent& event);
};
 
enum {
    ID_GetHash = 1
};
 
bool MyApp::OnInit() {
    if ( !wxApp::OnInit() )
        return false;

    MyFrame *frame = new MyFrame();
    frame->Show(true);
    return true;
}
 
MyFrame::MyFrame() : wxFrame(nullptr, wxID_ANY, "pshash-gui") {
    wxMenu *menuFile = new wxMenu;
    menuFile->Append(ID_GetHash, "&Get sample hash\tCtrl-H", "Sample hash computation for testing");
    menuFile->AppendSeparator();
    menuFile->Append(wxID_EXIT);

    wxMenu *menuHelp = new wxMenu;
    menuHelp->Append(wxID_ABOUT);

    wxMenuBar *menuBar = new wxMenuBar;
    menuBar->Append(menuFile, "&File");
    menuBar->Append(menuHelp, "&Help");

    SetMenuBar( menuBar );

    CreateStatusBar();
    SetStatusText("The pshash pseudo-hash algorithm, version 1.0");

    Bind(wxEVT_MENU, &MyFrame::OnGetHash, this, ID_GetHash);
    Bind(wxEVT_MENU, &MyFrame::OnAbout, this, wxID_ABOUT);
    Bind(wxEVT_MENU, &MyFrame::OnExit, this, wxID_EXIT);
}
 
void MyFrame::OnExit(wxCommandEvent& event) {
    Close(true);
}
 
void MyFrame::OnAbout(wxCommandEvent& event) {
    wxMessageBox("This is a wxWidgets Hello World example", "About Hello World", wxOK | wxICON_INFORMATION);
}
 
void MyFrame::OnGetHash(wxCommandEvent& event) {
    char hash[MAXSIZE_BIG];
    mpz_t key1; mpz_init_set_str(key1, "12345", 10);
    mpz_t key2; mpz_init_set_str(key2, "6789", 10);
    get_hash(hash, defaultConfiguration, 4, key1, key2);
    wxLogMessage(hash);
}
