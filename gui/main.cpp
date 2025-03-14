#include <wx/wx.h>

#include <iostream>
using namespace std;

#include "FFI_stub.h"

#include <string>
 
class MyApp : public wxApp {
    public:
    bool OnInit() override;
};
 
wxIMPLEMENT_APP(MyApp);
 
class MyFrame : public wxFrame {
    public:
    MyFrame();
 
    private:
    void OnCompute(wxCommandEvent& event);
    void OnExit(wxCommandEvent& event);
};
 
enum {
    ID_Hello = 1
};
 
bool MyApp::OnInit() {
    MyFrame *frame = new MyFrame();
    frame -> Show(true);
    return true;
}
 
MyFrame::MyFrame() : wxFrame(nullptr, wxID_ANY, "Hello World") {
    wxMenu *menuFile = new wxMenu;
    menuFile->Append(ID_Hello, "&Hello...\tCtrl-H",
                     "Help string shown in status bar for this menu item");
    menuFile->AppendSeparator();
    menuFile->Append(wxID_EXIT);
 
    wxMenu *menuHelp = new wxMenu;
    menuHelp->Append(wxID_ABOUT);
 
    wxMenuBar *menuBar = new wxMenuBar;
    menuBar->Append(menuFile, "&File");
    menuBar->Append(menuHelp, "&Help");
 
    SetMenuBar( menuBar );
 
    CreateStatusBar();
    SetStatusText("Welcome to wxWidgets!");
 
    Bind(wxEVT_MENU, &MyFrame::OnCompute, this, ID_Hello);
    Bind(wxEVT_MENU, &MyFrame::OnExit, this, wxID_EXIT);
}
 
void MyFrame::OnExit(wxCommandEvent& event) {
    Close(true);
}
 
void MyFrame::OnCompute(wxCommandEvent& event) {
    int argc = 2;
    const char *argv[] = { "+RTS", "-A32m", NULL };
    // argv[0] = "+RTS";
    char **pargv = (char**) argv;
    hs_init(&argc, &pargv);
    wxLogMessage(std::to_string(fac(5)).c_str());
    hs_exit();
}
