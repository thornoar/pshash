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
        //     AdjustTextCtrlSize(textCtrl);
        // }
        for (int i = 0; i < NUM_INPUTS; i++) {
            AdjustTextCtrlSize(inputs[i]);
            // if (inputs[i] == event.GetEventObject()) {
            //     return;
            // }
        }
        for (int i = 0; i < NUM_OUTPUTS; i++) {
            AdjustTextCtrlSize(outputs[i]);
            // outputs[i]->GetParent()->Layout();
            // if (outputs[i] == event.GetEventObject()) {
            //     return;
            // }
        }
        Refresh();
    }

private:
    wxTextCtrl* inputs[NUM_INPUTS] = {};
    wxTextCtrl* outputs[NUM_OUTPUTS] = {};

    bool validInput (int id) {
        return validKey(inputs[id], id);
    }

    void GetPenColor(wxPaintDC* dc, bool valid) {
        if (valid)
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

        // wxPoint publicKeyControl = wxPoint(inputPos[PUBLIC_KEY].x, inputPos[PUBLIC_KEY].y + VERTICAL_OFFSET);
        // wxPoint choiceKeyControl = wxPoint(inputPos[CHOICE_KEY].x, inputPos[CHOICE_KEY].y + VERTICAL_OFFSET);
        // wxPoint shuffleKeyControl = wxPoint(inputPos[SHUFFLE_KEY].x, inputPos[SHUFFLE_KEY].y + VERTICAL_OFFSET);
        // wxPoint outputControl = wxPoint(outputPos[HASH].x, outputPos[HASH].y - VERTICAL_OFFSET);
        //
        // wxPoint points1[] = { inputPos[PUBLIC_KEY], publicKeyControl, outputControl, outputPos[HASH] };
        // wxPoint points2[] = { inputPos[CHOICE_KEY], choiceKeyControl, outputControl, outputPos[HASH] };
        // wxPoint points3[] = { inputPos[SHUFFLE_KEY], shuffleKeyControl, outputControl, outputPos[HASH] };

        bool validInputs[NUM_INPUTS];
        for (int i = 0; i < NUM_INPUTS; i++) {
            validInputs[i] = validInput(i);
        }

        // wxPoint midpoint = wxPoint((inputPos[CONFIG_KEYWORD_KEY].x + inputPos[CONFIG_NUMBERS_KEY].x + inputPos[CONFIG_RAW_KEY].x)/3, (inputPos[PATCH_KEY].y + inputPos[CONFIG_NUMBERS_KEY].y)/2);

        GetPenColor(&dc, validInputs[PUBLIC_KEY]);
        dc.DrawLine(inputPos[PUBLIC_KEY], inputPos[PATCH_KEY]);
        GetPenColor(&dc, validInputs[PUBLIC_KEY] && validInputs[PATCH_KEY]);
        dc.DrawLine(inputPos[PATCH_KEY], inputPos[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[PATCH_KEY], inputPos[CONFIG_NUMBERS_KEY]);
        dc.DrawLine(inputPos[PATCH_KEY], inputPos[CONFIG_RAW_KEY]);
        // dc.DrawSpline(4, points1);
        GetPenColor(&dc, validInputs[CHOICE_KEY]);
        dc.DrawLine(inputPos[CHOICE_KEY], inputPos[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[CHOICE_KEY], inputPos[CONFIG_NUMBERS_KEY]);
        dc.DrawLine(inputPos[CHOICE_KEY], inputPos[CONFIG_RAW_KEY]);
        // dc.DrawSpline(4, points2);
        GetPenColor(&dc, validInputs[SHUFFLE_KEY]);
        dc.DrawLine(inputPos[SHUFFLE_KEY], inputPos[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[SHUFFLE_KEY], inputPos[CONFIG_NUMBERS_KEY]);
        dc.DrawLine(inputPos[SHUFFLE_KEY], inputPos[CONFIG_RAW_KEY]);
        // dc.DrawSpline(4, points3);
        GetPenColor(&dc, validInputs[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[CONFIG_KEYWORD_KEY], outputPos[HASH]);
        GetPenColor(&dc, validInputs[CONFIG_NUMBERS_KEY]);
        dc.DrawLine(inputPos[CONFIG_NUMBERS_KEY], outputPos[HASH]);
        GetPenColor(&dc, validInputs[CONFIG_RAW_KEY]);
        dc.DrawLine(inputPos[CONFIG_RAW_KEY], outputPos[HASH]);

        if (validInputs[PUBLIC_KEY] && validInputs[PATCH_KEY] && validInputs[CHOICE_KEY] && validInputs[SHUFFLE_KEY]) {  
            wxString hash = getHash(
                &defaultConfiguration,
                inputs[PUBLIC_KEY]->GetValue(),
                inputs[PATCH_KEY]->GetValue(),
                inputs[CHOICE_KEY]->GetValue(),
                inputs[SHUFFLE_KEY]->GetValue()
            );
            outputs[HASH]->SetValue(hash);
        } else {
            outputs[HASH]->SetValue("");
        }
        AdjustTextCtrlSize(outputs[HASH]);
        Refresh();
    }
};
 
class MyFrame : public wxFrame {
    public:
    MyFrame();

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
    // Menu bar

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

    // Status bar

    CreateStatusBar();
    SetStatusText("The pshash pseudo-hash algorithm, version 1.0");

    // Custom panel

    GetHashPanel* getHashPanel = new GetHashPanel(this);

    wxTextCtrl* inputs[NUM_INPUTS];
    wxPanel* inputPanel = new wxPanel(getHashPanel);
    inputs[PUBLIC_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CHOICE_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[SHUFFLE_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);

    wxBoxSizer* primaryInputSizer = new wxBoxSizer(wxHORIZONTAL);
    // primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[PUBLIC_KEY], wxSizerFlags().Border(wxLEFT|wxTOP, BORDER_WIDTH));
    primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[CHOICE_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, BORDER_WIDTH));
    primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[SHUFFLE_KEY], wxSizerFlags().Border(wxRIGHT|wxTOP, BORDER_WIDTH));
    // primaryInputSizer->AddStretchSpacer();
    inputPanel->SetSizer(primaryInputSizer);

    wxPanel* patchPanel = new wxPanel(getHashPanel);
    inputs[PATCH_KEY] = new wxTextCtrl(patchPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    wxBoxSizer* patchSizer = new wxBoxSizer(wxHORIZONTAL);
    patchSizer->Add(inputs[PATCH_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    // patchSizer->AddStretchSpacer();
    patchPanel->SetSizer(patchSizer);

    wxPanel* configPanel = new wxPanel(getHashPanel);
    inputs[CONFIG_KEYWORD_KEY] = new wxTextCtrl(configPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_NUMBERS_KEY] = new wxTextCtrl(configPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_RAW_KEY] = new wxTextCtrl(configPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    wxBoxSizer* configSizer = new wxBoxSizer(wxHORIZONTAL);
    configSizer->AddStretchSpacer();
    configSizer->AddStretchSpacer();
    configSizer->AddStretchSpacer();
    configSizer->Add(inputs[CONFIG_KEYWORD_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    configSizer->Add(inputs[CONFIG_NUMBERS_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    configSizer->Add(inputs[CONFIG_RAW_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    configPanel->SetSizer(configSizer);

    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i]->SetMinSize(wxSize(BOX_HEIGHT, BOX_HEIGHT));
    }

    wxTextCtrl* outputs[NUM_OUTPUTS];
    wxPanel* outputPanel = new wxPanel(getHashPanel);
    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputs[i] = new wxTextCtrl(outputPanel, wxID_ANY, "", outputPositions[i], wxDefaultSize, wxTE_READONLY | wxTE_CENTER);
        // AdjustTextCtrlSize(outputs[i]);
    }

    wxBoxSizer* outputSizer = new wxBoxSizer(wxHORIZONTAL);
    outputSizer->AddStretchSpacer();

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputSizer->Add(outputs[i], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    }

    outputSizer->AddStretchSpacer();
    outputPanel->SetSizer(outputSizer);
    getHashPanel->SetTextCtrls(inputs, outputs);

    // Arrange panels in the custom panel
    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
    mainSizer->Add(inputPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(patchPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(configPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(outputPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
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
    }
}
