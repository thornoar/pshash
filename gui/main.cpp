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
        for (int i = 0; i < NUM_INPUTS; i++) {
            AdjustTextCtrlSize(inputs[i], i);
        }
        for (int i = 0; i < NUM_OUTPUTS; i++) {
            AdjustTextCtrlSize(outputs[i]);
        }
        Refresh();
    }

private:
    wxTextCtrl* inputs[NUM_INPUTS];
    bool validInputs[NUM_INPUTS];
    wxTextCtrl* outputs[NUM_OUTPUTS];
    bool validConnections[NUM_CONNECTIONS];

    bool validInput (int id) {
        return validKey(inputs[id], id);
    }

    void GetPenColor(wxPaintDC* dc, bool valid) {
        if (valid)
            dc->SetPen(wxPen(*wxGREEN, 2));
        else
            dc->SetPen(wxPen(*wxRED, 2));
    }

    // void DrawCOnnection (wxPaintDC* dc, wxPoint* inputPos, wxPoint* outputPos, int id1, int id2, int conid) {
    //     GetPenColor(dc, validConnections[conid]);
    //     dc->DrawLine(inputPos[id1], outputPos[id2]);
    // }

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

        // bool validInputs[NUM_INPUTS];
        for (int i = 0; i < NUM_INPUTS; i++) {
            validInputs[i] = validInput(i);
        }
        // bool validConnections[NUM_CONNECTIONS];
        validConnections[PUBLIC_PATCH] = validInputs[PUBLIC_KEY];
        validConnections[PATCH_CONFIG_KEYWORD] = validInputs[PATCH_KEY] && validInputs[PUBLIC_KEY];
        validConnections[PATCH_CONFIG_NUMBERS] = validInputs[PATCH_KEY] && validInputs[PUBLIC_KEY];
        validConnections[CHOICE_CONFIG_KEYWORD] = validInputs[CHOICE_KEY];
        validConnections[CHOICE_CONFIG_NUMBERS] = validInputs[CHOICE_KEY];
        validConnections[SHUFFLE_CONFIG_KEYWORD] = validInputs[SHUFFLE_KEY];
        validConnections[SHUFFLE_CONFIG_NUMBERS] = validInputs[SHUFFLE_KEY];
        validConnections[CONFIG_KEYWORD_HASH] = validInputs[CONFIG_KEYWORD_KEY] && validConnections[PATCH_CONFIG_KEYWORD] && validConnections[CHOICE_CONFIG_KEYWORD] && validConnections[SHUFFLE_CONFIG_KEYWORD];
        validConnections[CONFIG_NUMBERS_HASH] = validInputs[CONFIG_NUMBERS_1_KEY] && validInputs[CONFIG_NUMBERS_2_KEY] && validInputs[CONFIG_NUMBERS_3_KEY] && validInputs[CONFIG_NUMBERS_4_KEY] && validConnections[PATCH_CONFIG_NUMBERS] && validConnections[CHOICE_CONFIG_NUMBERS] && validConnections[SHUFFLE_CONFIG_NUMBERS];


        GetPenColor(&dc, validConnections[PUBLIC_PATCH]);
        dc.DrawLine(inputPos[PUBLIC_KEY], inputPos[PATCH_KEY]);
        GetPenColor(&dc, validConnections[PATCH_CONFIG_KEYWORD]);
        dc.DrawLine(inputPos[PATCH_KEY], inputPos[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[PATCH_KEY], inputPos[CONFIG_NUMBERS_1_KEY]);
        GetPenColor(&dc, validConnections[CHOICE_CONFIG_NUMBERS]);
        dc.DrawLine(inputPos[CHOICE_KEY], inputPos[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[CHOICE_KEY], inputPos[CONFIG_NUMBERS_1_KEY]);
        GetPenColor(&dc, validConnections[SHUFFLE_CONFIG_KEYWORD]);
        dc.DrawLine(inputPos[SHUFFLE_KEY], inputPos[CONFIG_KEYWORD_KEY]);
        dc.DrawLine(inputPos[SHUFFLE_KEY], inputPos[CONFIG_NUMBERS_1_KEY]);
        GetPenColor(&dc, validConnections[CONFIG_KEYWORD_HASH]);
        dc.DrawLine(inputPos[CONFIG_KEYWORD_KEY], outputPos[HASH]);
        GetPenColor(&dc, validConnections[CONFIG_NUMBERS_HASH]);
        dc.DrawLine(inputPos[CONFIG_NUMBERS_4_KEY], outputPos[HASH]);
        // GetPenColor(&dc, validInputs[CONFIG_RAW_KEY]);
        // dc.DrawLine(inputPos[CONFIG_RAW_KEY], outputPos[HASH]);

        if (
            validInputs[PUBLIC_KEY]
			&& validInputs[PATCH_KEY]
			&& validInputs[CHOICE_KEY]
			&& validInputs[SHUFFLE_KEY]
			&& (validInputs[CONFIG_KEYWORD_KEY]
                || (validInputs[CONFIG_NUMBERS_1_KEY]
                    && validInputs[CONFIG_NUMBERS_2_KEY]
                    && validInputs[CONFIG_NUMBERS_3_KEY]
                    && validInputs[CONFIG_NUMBERS_4_KEY]))
        ) {
            // struct source* srcs = (struct source*) malloc(MAXSIZE_SMALL * sizeof(char) + sizeof(unsigned long));
            // // for (int i = 0; i < 4; i++) {
            // //     srcs[i].elts = (char*) malloc(MAXSIZE_SMALL * sizeof(char));
            // // }
            // struct configuration config = { .size = 4, .srcs = srcs };

            // struct configuration config;
            // config.size = 4;
            // struct source* srcs = (struct source*) malloc(4 * (sizeof(struct source) + MAXSIZE_SMALL * sizeof(char)));
            // config.srcs = srcs;

            struct configuration config;
            config.size = 4;
            config.srcs = (struct source*)malloc(config.size * sizeof(struct source));
            for (unsigned long i = 0; i < config.size; ++i) {
                config.srcs[i].elts = (char*)malloc(MAXSIZE_SMALL * sizeof(char));
                config.srcs[i].amount = 0; // Initialize amount or set it as needed
            }
                // if (!config.srcs) {
                //     // Handle allocation failure
                //     return;
                // }
                // if (!config.srcs[i].elts) {
                //     // Handle allocation failure
                //     // Free previously allocated memory
                //     for (unsigned long j = 0; j < i; ++j) {
                //         free(config.srcs[j].elts);
                //     }
                //     free(config.srcs);
                //     return;
                // }

            if (validInputs[CONFIG_KEYWORD_KEY]) {
                string keyword = inputs[CONFIG_KEYWORD_KEY]->GetValue().ToStdString();
                setConfigWithKeyword(&config, keyword.c_str());
            } else if (validInputs[CONFIG_NUMBERS_1_KEY] && validInputs[CONFIG_NUMBERS_2_KEY] && validInputs[CONFIG_NUMBERS_3_KEY] && validInputs[CONFIG_NUMBERS_4_KEY]) {
                unsigned long numbers[4] = {
                    stoul(inputs[CONFIG_NUMBERS_1_KEY]->GetValue().ToStdString()),
                    stoul(inputs[CONFIG_NUMBERS_2_KEY]->GetValue().ToStdString()),
                    stoul(inputs[CONFIG_NUMBERS_3_KEY]->GetValue().ToStdString()),
                    stoul(inputs[CONFIG_NUMBERS_4_KEY]->GetValue().ToStdString())
                };
                setConfigWithNumbers(&config, numbers);
            }
            wxString hash = getHash(
                &config,
                inputs[PUBLIC_KEY]->GetValue(),
                inputs[PATCH_KEY]->GetValue(),
                inputs[CHOICE_KEY]->GetValue(),
                inputs[SHUFFLE_KEY]->GetValue()
            );
            outputs[HASH]->SetValue(hash);
            for (unsigned long i = 0; i < config.size; ++i) {
                free(config.srcs[i].elts);
            }
            free(config.srcs);
        } else {
            outputs[HASH]->SetValue("ERROR");
        }
        for (int i = 0; i < NUM_INPUTS; i++) { AdjustTextCtrlSize(inputs[i], i); }
        for (int i = 0; i < NUM_OUTPUTS; i++) { AdjustTextCtrlSize(outputs[i]); }
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
    inputs[PUBLIC_KEY]->SetValue(wxString("(PUBLIC)"));
    inputs[CHOICE_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CHOICE_KEY]->SetValue(wxString("(CHOICE)"));
    inputs[SHUFFLE_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[SHUFFLE_KEY]->SetValue(wxString("(SHUFFLE)"));

    wxBoxSizer* primaryInputSizer = new wxBoxSizer(wxHORIZONTAL);
    primaryInputSizer->Add(inputs[PUBLIC_KEY],  wxSizerFlags().Border(wxLEFT|wxTOP, BORDER_WIDTH));
    primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[CHOICE_KEY], wxSizerFlags().Border(wxTOP, BORDER_WIDTH/3));
    primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[SHUFFLE_KEY], wxSizerFlags().Border(wxRIGHT|wxTOP, BORDER_WIDTH));
    inputPanel->SetSizer(primaryInputSizer);

    wxPanel* patchPanel = new wxPanel(getHashPanel);
    inputs[PATCH_KEY] = new wxTextCtrl(patchPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[PATCH_KEY]->SetValue(wxString("0"));
    wxBoxSizer* patchSizer = new wxBoxSizer(wxHORIZONTAL);
    patchSizer->Add(inputs[PATCH_KEY], wxSizerFlags().Border(wxLEFT|wxTOP, BORDER_WIDTH));
    patchPanel->SetSizer(patchSizer);

    wxPanel* configPanel = new wxPanel(getHashPanel);
    inputs[CONFIG_KEYWORD_KEY] = new wxTextCtrl(configPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_KEYWORD_KEY]->SetValue(wxString("(KEYWORD)"));
    wxPanel* configNumbersPanel = new wxPanel(configPanel);
    inputs[CONFIG_NUMBERS_1_KEY] = new wxTextCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_NUMBERS_1_KEY]->SetValue(wxString("(LOWERCASE)"));
    inputs[CONFIG_NUMBERS_2_KEY] = new wxTextCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_NUMBERS_2_KEY]->SetValue(wxString("(UPPERCASE)"));
    inputs[CONFIG_NUMBERS_3_KEY] = new wxTextCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_NUMBERS_3_KEY]->SetValue(wxString("(SPECIAL)"));
    inputs[CONFIG_NUMBERS_4_KEY] = new wxTextCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CONFIG_NUMBERS_4_KEY]->SetValue(wxString("(DIGITS)"));
    wxBoxSizer* configNumbersSizer = new wxBoxSizer(wxVERTICAL);
    configNumbersSizer->Add(inputs[CONFIG_NUMBERS_1_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersSizer->Add(inputs[CONFIG_NUMBERS_2_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersSizer->Add(inputs[CONFIG_NUMBERS_3_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersSizer->Add(inputs[CONFIG_NUMBERS_4_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersPanel->SetSizer(configNumbersSizer);
    // inputs[CONFIG_RAW_KEY] = new wxTextCtrl(configPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    wxBoxSizer* configSizer = new wxBoxSizer(wxHORIZONTAL);
    configSizer->AddStretchSpacer();
    configSizer->Add(inputs[CONFIG_KEYWORD_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP|wxBOTTOM, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    // configSizer->Add(inputs[CONFIG_NUMBERS_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configSizer->Add(configNumbersPanel, wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    // configSizer->AddStretchSpacer();
    // configSizer->Add(inputs[CONFIG_RAW_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    configPanel->SetSizer(configSizer);

    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i]->SetMinSize(wxSize(BOX_HEIGHT, BOX_HEIGHT));
        AdjustTextCtrlSize(inputs[i]);
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
    mainSizer->Add(patchPanel, 2, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(configPanel, 2, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
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
