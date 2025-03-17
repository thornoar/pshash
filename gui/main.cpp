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

#define VAR 1

using namespace std;

// ========================================================
// CONSTANTS
// ========================================================

const int VERTICAL_OFFSET = 100;
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
#if VAR
        wxTextCtrl** newInputs,
        wxTextCtrl** newOutputs
#else
        wxTextCtrl* newPublicKey,
        wxTextCtrl* newChoiceKey,
        wxTextCtrl* newShuffleKey,
        wxTextCtrl* newPatchKey,
        wxTextCtrl* newConfigKeywordKey,
        wxTextCtrl* newConfigNumbersKey,
        wxTextCtrl* newConfigRawKey,
        wxTextCtrl* newHashOutput
#endif
    ) {
#if VAR
        for (int i = 0; i < NUM_INPUTS; i++) {
            inputs[i] = newInputs[i];
        }
        for (int i = 0; i < NUM_OUTPUTS; i++) {
            outputs[i] = newOutputs[i];
        }
#else
        publicKey = newPublicKey;
        choiceKey = newChoiceKey;
        shuffleKey = newShuffleKey;
        patchKey = newPatchKey;
        configKeywordKey = newConfigKeywordKey;
        configNumbersKey = newConfigNumbersKey;
        configRawKey = newConfigRawKey;
        hashOutput = newHashOutput;
#endif
    }
    void OnTextChange (wxCommandEvent& event) {
        Refresh();
    }

private:
#if VAR
    wxTextCtrl* inputs[NUM_INPUTS] = {};
    wxTextCtrl* outputs[NUM_OUTPUTS] = {};
#else
    wxTextCtrl* publicKey;
    wxTextCtrl* choiceKey;
    wxTextCtrl* shuffleKey;
    wxTextCtrl* patchKey;
    wxTextCtrl* configKeywordKey;
    wxTextCtrl* configNumbersKey;
    wxTextCtrl* configRawKey;
    wxTextCtrl* hashOutput;
#endif

    void GetPenColor(wxPaintDC* dc, wxTextCtrl* textCtrl, int textCtrlID) {
        if (validKey(textCtrl, textCtrlID))
            dc->SetPen(wxPen(*wxGREEN, 2));
        else
            dc->SetPen(wxPen(*wxRED, 2));
    }

    void OnPaint(wxPaintEvent& event) {
        wxPaintDC dc(this);

#if VAR
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
#else
        wxPoint publicKeyPos = GetTextCtrlPosition(publicKey, this);
        wxPoint choiceKeyPos = GetTextCtrlPosition(choiceKey, this);
        wxPoint shuffleKeyPos = GetTextCtrlPosition(shuffleKey, this);
        wxPoint outputBoxPos = GetTextCtrlPosition(hashOutput, this);

        wxPoint publicKeyControl = wxPoint(publicKeyPos.x, publicKeyPos.y + VERTICAL_OFFSET);
        wxPoint choiceKeyControl = wxPoint(choiceKeyPos.x, choiceKeyPos.y + VERTICAL_OFFSET);
        wxPoint shuffleKeyControl = wxPoint(shuffleKeyPos.x, shuffleKeyPos.y + VERTICAL_OFFSET);
        wxPoint outputControl1 = wxPoint(outputBoxPos.x, outputBoxPos.y - VERTICAL_OFFSET);
        wxPoint outputControl2 = wxPoint(outputBoxPos.x, outputBoxPos.y - VERTICAL_OFFSET);
        wxPoint outputControl3 = wxPoint(outputBoxPos.x, outputBoxPos.y - VERTICAL_OFFSET);

        wxPoint points1[] = { publicKeyPos, publicKeyControl, outputControl1, outputBoxPos };
        wxPoint points2[] = { choiceKeyPos, choiceKeyControl, outputControl2, outputBoxPos };
        wxPoint points3[] = { shuffleKeyPos, shuffleKeyControl, outputControl3, outputBoxPos };

        GetPenColor(&dc, publicKey, PUBLIC_KEY);
        dc.DrawSpline(4, points1);
        GetPenColor(&dc, choiceKey, CHOICE_KEY);
        dc.DrawSpline(4, points2);
        GetPenColor(&dc, shuffleKey, SHUFFLE_KEY);
        dc.DrawSpline(4, points3);
#endif
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

    void AdjustTextCtrlSize(wxTextCtrl* textCtrl) {
        // Get the best size for the text control based on its content
        wxSize bestSize = textCtrl->GetBestSize();
        // Set the minimum size to ensure it can grow but not shrink below this size
        textCtrl->SetMinSize(bestSize);
        // Optionally, set the size directly if you want immediate resizing
        textCtrl->SetSize(bestSize);
        // Layout the parent to apply the new size
        textCtrl->GetParent()->Layout();
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
#if VAR
    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i] = new wxTextCtrl(inputPanel, wxID_ANY);
        // AdjustTextCtrlSize(inputs[i]);
    }
#else
    wxTextCtrl* publicKey = new wxTextCtrl(inputPanel, wxID_ANY);
    wxTextCtrl* choiceKey = new wxTextCtrl(inputPanel, wxID_ANY);
    wxTextCtrl* shuffleKey = new wxTextCtrl(inputPanel, wxID_ANY);
    wxTextCtrl* patchKey = new wxTextCtrl(inputPanel, wxID_ANY);
    wxTextCtrl* configKeywordKey = new wxTextCtrl(inputPanel, wxID_ANY);
    wxTextCtrl* configNumbersKey = new wxTextCtrl(inputPanel, wxID_ANY);
    wxTextCtrl* configRawKey = new wxTextCtrl(inputPanel, wxID_ANY);
#endif

    wxBoxSizer* inputSizer = new wxBoxSizer(wxHORIZONTAL);
    inputSizer->AddStretchSpacer();

#if VAR
    for (int i = 0; i < NUM_INPUTS; i++) {
        inputSizer->Add(inputs[i], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
    }
#else
    inputSizer->Add(publicKey, wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
    inputSizer->Add(choiceKey, wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
    inputSizer->Add(shuffleKey, wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
#endif

    inputSizer->AddStretchSpacer();
    inputPanel->SetSizer(inputSizer);

    wxPanel* outputPanel = new wxPanel(getHashPanel);

#if VAR
    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputs[i] = new wxTextCtrl(outputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);
        // AdjustTextCtrlSize(outputs[i]);
    }
#else
    wxTextCtrl* hashOutput = new wxTextCtrl(outputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);
#endif

    wxBoxSizer* outputSizer = new wxBoxSizer(wxHORIZONTAL);
    outputSizer->AddStretchSpacer();

#if VAR
    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputSizer->Add(outputs[i], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, 30));
    }
#else
    outputSizer->Add(hashOutput, wxSizerFlags().Border(wxRIGHT|wxLEFT|wxBOTTOM, 30));
#endif

    outputSizer->AddStretchSpacer();
    outputPanel->SetSizer(outputSizer);

#if VAR
    getHashPanel->SetTextCtrls(inputs, outputs);
#else
    getHashPanel->SetTextCtrls(publicKey, choiceKey, shuffleKey, patchKey, configKeywordKey, configNumbersKey, configRawKey, hashOutput);
#endif

    // Arrange panels in the custom panel
    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
    mainSizer->Add(inputPanel, 1, wxEXPAND | wxALL, 10);
    mainSizer->Add(outputPanel, 0, wxEXPAND | wxALL, 10);
    getHashPanel->SetSizer(mainSizer);

    // Set the custom panel as the main sizer for the frame
    SetSizerAndFit(new wxBoxSizer(wxVERTICAL));
    GetSizer()->Add(getHashPanel, 1, wxEXPAND);

    // Bind events
    Bind(wxEVT_MENU, &MyFrame::OnAbout, this, wxID_ABOUT);
    Bind(wxEVT_MENU, &MyFrame::OnVersion, this, ID_Version);
    Bind(wxEVT_MENU, &MyFrame::OnExit, this, wxID_EXIT);

#if VAR
    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i]->Bind(wxEVT_TEXT, &GetHashPanel::OnTextChange, getHashPanel);
    }
#else
    publicKey->Bind(wxEVT_TEXT, &GetHashPanel::OnTextChange, getHashPanel);
    choiceKey->Bind(wxEVT_TEXT, &GetHashPanel::OnTextChange, getHashPanel);
    shuffleKey->Bind(wxEVT_TEXT, &GetHashPanel::OnTextChange, getHashPanel);
#endif
}
