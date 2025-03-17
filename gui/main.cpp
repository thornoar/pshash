// ========================================================
// IMPORTS
// ========================================================

#include <cstdlib>
#include <cstring>
#include "algorithm.h"
#include "wx/string.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>
// for all others, include the necessary headers (this file is usually all you
// need because it includes almost all "standard" wxWidgets headers)
#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/clipbrd.h"
#include <wx/sizer.h>
#include <wx/dcclient.h>
#include "inputs.h"

using namespace std;

// ========================================================
// CONSTANTS
// ========================================================

const int VERTICAL_OFFSET = 100;
const int BORDER_WIDTH = 10;
const int SUPER_BORDER_WIDTH = 30;
const int BOX_HEIGHT = 28;
const int BOX_WIDTH = 115;
const string currentVersion = "1.0";

class MyApp : public wxApp {
    public:
    bool OnInit() override;
};
wxIMPLEMENT_APP(MyApp);

class GetHashPanel : public wxPanel {
public:
    GetHashPanel(wxWindow* parent) : wxPanel(parent) {
        Bind(wxEVT_PAINT, &GetHashPanel::OnPaint, this);
    }
    void SetTextCtrls (
        wxTextCtrl** newInputs,
        wxTextCtrl** newOutputs
    ) {
        for (int i = 0; i < NUM_INPUTS; i++) {
            inputs[i] = newInputs[i];
        }
        for (int i = 0; i < NUM_OUTPUTS; i++) {
            outputs[i] = newOutputs[i];
        }
    }
    void OnTextChange (wxCommandEvent& event) {
        // wxTextCtrl* textCtrl = dynamic_cast<wxTextCtrl*>(event.GetEventObject());
        // if (textCtrl) {
        //     cout << "Text changed: " << textCtrl->GetValue() << endl;
        //     AdjustTextCtrlSize(textCtrl);
        // }
        Refresh();
    }

private:
    wxTextCtrl* inputs[NUM_INPUTS] = {};
    wxTextCtrl* outputs[NUM_OUTPUTS] = {};

    void GetPenColor(wxPaintDC* dc, wxTextCtrl* textCtrl, int textCtrlID) {
        if (validKey(textCtrl, textCtrlID))
            dc->SetPen(wxPen(*wxGREEN, 2));
        else
            dc->SetPen(wxPen(*wxRED, 2));
    }

    void OnPaint(wxPaintEvent& event) {
        wxPaintDC dc(this);

        wxPoint inputPos[NUM_INPUTS];
        for (int i = 0; i < NUM_INPUTS; i++) {
            inputPos[i] = GetTextCtrlPosition(inputs[i], this);
        }
        wxPoint outputPos[NUM_OUTPUTS];
        for (int i = 0; i < NUM_OUTPUTS; i++) {
            outputPos[i] = GetTextCtrlPosition(outputs[i], this);
        }

        wxPoint publicKeyControl = wxPoint(inputPos[PUBLIC_KEY].x, inputPos[PUBLIC_KEY].y + VERTICAL_OFFSET);
        wxPoint choiceKeyControl = wxPoint(inputPos[CHOICE_KEY].x, inputPos[CHOICE_KEY].y + VERTICAL_OFFSET);
        wxPoint shuffleKeyControl = wxPoint(inputPos[SHUFFLE_KEY].x, inputPos[SHUFFLE_KEY].y + VERTICAL_OFFSET);
        wxPoint outputControl = wxPoint(outputPos[HASH].x, outputPos[HASH].y - VERTICAL_OFFSET);

        wxPoint points1[] = { inputPos[PUBLIC_KEY], publicKeyControl, outputControl, outputPos[HASH] };
        wxPoint points2[] = { inputPos[CHOICE_KEY], choiceKeyControl, outputControl, outputPos[HASH] };
        wxPoint points3[] = { inputPos[SHUFFLE_KEY], shuffleKeyControl, outputControl, outputPos[HASH] };

        GetPenColor(&dc, inputs[PUBLIC_KEY], PUBLIC_KEY);
        dc.DrawSpline(4, points1);
        GetPenColor(&dc, inputs[CHOICE_KEY], CHOICE_KEY);
        dc.DrawSpline(4, points2);
        GetPenColor(&dc, inputs[SHUFFLE_KEY], SHUFFLE_KEY);
        dc.DrawSpline(4, points3);
    }

    wxPoint GetTextCtrlPosition(wxTextCtrl* textCtrl, wxWindow* relativeTo) {
        // Get the position of the text control in screen coordinates
        wxPoint screenPos = textCtrl->ClientToScreen(wxPoint(0, 0));
        // Convert the screen position to the coordinate system of the relative window
        wxPoint relativePos = relativeTo->ScreenToClient(screenPos);
        wxSize size = textCtrl->GetSize();
        // Return the center point of the text control
        return wxPoint(relativePos.x + size.x / 2, relativePos.y + size.y / 2);
    }
};
 
class MyFrame : public wxFrame {
    public:
    MyFrame();

    void OnTextChange(wxCommandEvent& event) {
        wxTextCtrl* textCtrl = dynamic_cast<wxTextCtrl*>(event.GetEventObject());
        if (textCtrl) {
            cout << "Text changed: " << textCtrl->GetValue() << endl;
            AdjustTextCtrlSize(textCtrl);
        }
        // Refresh the panel if needed
        textCtrl->GetParent()->Refresh();
    }

    private:
    void OnExit(wxCommandEvent& event) {
        Close(true);
    }
    void OnAbout(wxCommandEvent& event) {
        wxMessageBox("This is a wxWidgets Hello World example", "About Hello World", wxOK | wxICON_INFORMATION);
    }
    void OnVersion(wxCommandEvent& event) {
        wxMessageBox(
            "This is the pshash pseudo-hash algorithm GUI,\n"
            "version 1.0",
            "Version",
            wxOK | wxICON_INFORMATION
        );
    }
};
 
enum {
    ID_Preferences = 1,
    ID_Version,
    ID_Developer
};
 
bool MyApp::OnInit() {
    if ( !wxApp::OnInit() )
        return false;

    MyFrame *frame = new MyFrame();
    frame->Show(true);
    return true;
}
 
MyFrame::MyFrame() : wxFrame(nullptr, wxID_ANY, "pshash-gui") {
    wxMenu *menuSettings = new wxMenu;
    menuSettings->Append(ID_Preferences, "&Preferences\tCtrl-P", "Open the configuration menu");
    menuSettings->AppendSeparator();
    menuSettings->Append(wxID_EXIT, "&Quit\tCtrl-Q", "Exit from Pshash-GUI");

    wxMenu *menuInfo = new wxMenu;
    menuInfo->Append(wxID_ABOUT, "&About\tCtrl-I", "Display information about Pshash-GUI");
    menuInfo->AppendSeparator();
    menuInfo->Append(ID_Version, "&Version\tCtrl-V", "Show current program version");
    menuInfo->Append(ID_Developer, "&Developer");

    wxMenuBar *menuBar = new wxMenuBar;
    menuBar->Append(menuSettings, "&Settings");
    menuBar->Append(menuInfo, "&Info");

    SetMenuBar(menuBar);

    CreateStatusBar();
    SetStatusText("The pshash pseudo-hash algorithm, version 1.0");

    // Create a custom panel for drawing lines
    GetHashPanel* getHashPanel = new GetHashPanel(this);
    wxTextCtrl* inputs[NUM_INPUTS];
    wxTextCtrl* outputs[NUM_OUTPUTS];

    wxPanel* inputPanel = new wxPanel(getHashPanel);
    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
        inputs[i]->SetMinSize(wxSize(BOX_HEIGHT, BOX_HEIGHT));
        // AdjustTextCtrlSize(inputs[i]);
    }
    inputs[PUBLIC_KEY]->SetMinSize(wxSize(BOX_WIDTH, BOX_HEIGHT));
    inputs[PATCH_KEY]->SetMinSize(wxSize(BOX_WIDTH/2, BOX_HEIGHT));
    inputs[CHOICE_KEY]->SetMinSize(wxSize(BOX_WIDTH, BOX_HEIGHT));
    inputs[SHUFFLE_KEY]->SetMinSize(wxSize(BOX_WIDTH, BOX_HEIGHT));

    wxBoxSizer* inputSizer = new wxBoxSizer(wxHORIZONTAL);
    inputSizer->AddStretchSpacer();
    inputSizer->Add(inputs[PUBLIC_KEY], wxSizerFlags().Border(wxLEFT|wxTOP, BORDER_WIDTH));
    inputSizer->Add(inputs[PATCH_KEY], wxSizerFlags().Border(wxLEFT|wxTOP|wxRIGHT, BORDER_WIDTH));
    inputSizer->AddStretchSpacer();
    inputSizer->Add(inputs[CHOICE_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, BORDER_WIDTH));
    inputSizer->AddStretchSpacer();
    inputSizer->Add(inputs[SHUFFLE_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, BORDER_WIDTH));

    // for (int i = 0; i < 4; i++) {
    //     inputSizer->Add(inputs[i], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
    // }

    inputSizer->AddStretchSpacer();
    inputPanel->SetSizer(inputSizer);

    wxPanel* outputPanel = new wxPanel(getHashPanel);

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputs[i] = new wxTextCtrl(outputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);
        // AdjustTextCtrlSize(outputs[i]);
    }

    wxBoxSizer* outputSizer = new wxBoxSizer(wxHORIZONTAL);
    outputSizer->AddStretchSpacer();

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputSizer->Add(outputs[i], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
    }

    outputSizer->AddStretchSpacer();
    outputPanel->SetSizer(outputSizer);

    getHashPanel->SetTextCtrls(inputs, outputs);

    // Arrange panels in the custom panel
    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
    mainSizer->Add(inputPanel, 1, wxEXPAND | wxALL, SUPER_BORDER_WIDTH);
    mainSizer->Add(outputPanel, 0, wxEXPAND | wxALL, SUPER_BORDER_WIDTH);
    getHashPanel->SetSizer(mainSizer);

    // Set the custom panel as the main sizer for the frame
    SetSizerAndFit(new wxBoxSizer(wxVERTICAL));
    GetSizer()->Add(getHashPanel, 1, wxEXPAND);

    // Bind events
    Bind(wxEVT_MENU, &MyFrame::OnAbout, this, wxID_ABOUT);
    Bind(wxEVT_MENU, &MyFrame::OnVersion, this, ID_Version);
    Bind(wxEVT_MENU, &MyFrame::OnExit, this, wxID_EXIT);

    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i]->Bind(wxEVT_TEXT, &GetHashPanel::OnTextChange, getHashPanel);
        // inputs[i]->Bind(wxEVT_TEXT, &MyFrame::OnTextChange, this);
        // inputs[i]->Bind(wxEVT_TEXT, &([](wxCommandEvent& evt) {  }), getHashPanel);
    }
}
