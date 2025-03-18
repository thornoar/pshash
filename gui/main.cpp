// ========================================================
// IMPORTS
// ========================================================

#include <cstdlib>
#include <cstring>
#include "algorithm.h"
// #include "wx/gtk/spinctrl.h"
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
#include <wx/spinctrl.h>
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
        wxSpinCtrl* newPatchKey,
        wxChoice* newConfigKeyword,
        wxSpinCtrl** newConfigNumbers,
        wxTextCtrl** newOutputs
    ) {
        for (int i = 0; i < NUM_INPUTS; i++) {
            inputs[i] = newInputs[i];
        }
        patchKey = newPatchKey;
        configKeyword = newConfigKeyword;
        for (int i = 0; i < NUM_CONFIG_NUMBERS; i++) {
            configNumbers[i] = newConfigNumbers[i];
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
    // bool validInputs[NUM_INPUTS];
    wxSpinCtrl* patchKey;
    wxChoice* configKeyword;
    wxSpinCtrl* configNumbers[NUM_CONFIG_NUMBERS];
    wxTextCtrl* outputs[NUM_OUTPUTS];
    // bool validConnections[NUM_CONNECTIONS];

    bool validInput (int id) {
        return validTextCtrl(inputs[id], id);
    }

    void GetPenColor(wxPaintDC* dc, bool valid, bool setGrey) {
        if (setGrey)
            dc->SetPen(wxPen(*wxLIGHT_GREY, 2));
        else if (valid)
            dc->SetPen(wxPen(*wxGREEN, 2));
        else
            dc->SetPen(wxPen(*wxRED, 2));
    }

    void OnPaint(wxPaintEvent& event) {
        wxPaintDC dc(this);

        wxPoint positions[NUM_BOXES];
        for (int i = 0; i < NUM_INPUTS; i++) {
            positions[i] = GetCtrlPosition(inputs[i], this);
        }
        positions[NUM_INPUTS] = GetCtrlPosition(patchKey, this);
        positions[NUM_INPUTS + 1] = GetCtrlPosition(configKeyword, this);
        for (int i = 0; i < NUM_CONFIG_NUMBERS; i++) {
            positions[NUM_INPUTS + i + 2] = GetCtrlPosition(configNumbers[i], this);
        }
        positions[NUM_INPUTS + NUM_CONFIG_NUMBERS + 2] = GetCtrlPosition(outputs[HASH], this);
        // wxPoint outputPos[NUM_OUTPUTS];
        // for (int i = 0; i < NUM_OUTPUTS; i++) {
        //     outputPos[i] = GetCtrlPosition(outputs[i], this);
        // }

        bool validInputs[NUM_INPUTS];
        for (int i = 0; i < NUM_INPUTS; i++) {
            validInputs[i] = validInput(i);
        }
        // cout << boolalpha << validInputs[0] << endl;
        // bool validPatch = validSpinCtrl(patchKey, 128);
        string keyword = configKeyword->GetStringSelection().ToStdString();
        bool validKeyword = keyword != "(none)";
        bool validConfigNumbers[NUM_CONFIG_NUMBERS];
        // for (int i = 0; i < NUM_CONFIG_NUMBERS; i++) {
        //     validConfigNumbers[i] = validSpinCtrl(configNumbers[i], i == 0 ? 26 : i == 1 ? 26 : i == 2 ? 12 : 10);
        // }

        bool validConnections[NUM_CONNECTIONS];
        validConnections[PUBLIC_PATCH] = validInputs[PUBLIC_KEY];
        validConnections[PATCH_CONFIG_KEYWORD] = validInputs[PUBLIC_KEY];
        validConnections[PATCH_CONFIG_NUMBERS] = validInputs[PUBLIC_KEY];
        validConnections[CHOICE_CONFIG_KEYWORD] = validInputs[CHOICE_KEY];
        validConnections[CHOICE_CONFIG_NUMBERS] = validInputs[CHOICE_KEY];
        validConnections[SHUFFLE_CONFIG_KEYWORD] = validInputs[SHUFFLE_KEY];
        validConnections[SHUFFLE_CONFIG_NUMBERS] = validInputs[SHUFFLE_KEY];
        validConnections[CONFIG_KEYWORD_HASH] = validKeyword && validConnections[PATCH_CONFIG_KEYWORD] && validConnections[CHOICE_CONFIG_KEYWORD] && validConnections[SHUFFLE_CONFIG_KEYWORD];
        validConnections[CONFIG_NUMBERS_HASH] = validConnections[PATCH_CONFIG_NUMBERS] && validConnections[CHOICE_CONFIG_NUMBERS] && validConnections[SHUFFLE_CONFIG_NUMBERS];

        bool greyOutKeyword = !validConnections[CONFIG_KEYWORD_HASH] && validConnections[CONFIG_NUMBERS_HASH];
        bool greyOutNumbers = validConnections[CONFIG_KEYWORD_HASH];

        GetPenColor(&dc, validConnections[PUBLIC_PATCH], false);
        dc.DrawLine(positions[PUBLIC_POS], positions[PATCH_POS]);
        GetPenColor(&dc, validConnections[PATCH_CONFIG_KEYWORD], greyOutKeyword);
        dc.DrawLine(positions[PATCH_POS], positions[CONFIG_KEYWORD_POS]);
        GetPenColor(&dc, validConnections[PATCH_CONFIG_KEYWORD], greyOutNumbers);
        dc.DrawLine(positions[PATCH_POS], positions[CONFIG_NUM_1_POS]);
        GetPenColor(&dc, validConnections[CHOICE_CONFIG_NUMBERS], greyOutKeyword);
        dc.DrawLine(positions[CHOICE_POS], positions[CONFIG_KEYWORD_POS]);
        GetPenColor(&dc, validConnections[CHOICE_CONFIG_NUMBERS], greyOutNumbers);
        dc.DrawLine(positions[CHOICE_POS], positions[CONFIG_NUM_1_POS]);
        GetPenColor(&dc, validConnections[SHUFFLE_CONFIG_KEYWORD], greyOutKeyword);
        dc.DrawLine(positions[SHUFFLE_POS], positions[CONFIG_KEYWORD_POS]);
        GetPenColor(&dc, validConnections[SHUFFLE_CONFIG_KEYWORD], greyOutNumbers);
        dc.DrawLine(positions[SHUFFLE_POS], positions[CONFIG_NUM_1_POS]);
        GetPenColor(&dc, validConnections[CONFIG_KEYWORD_HASH], greyOutKeyword);
        dc.DrawLine(positions[CONFIG_KEYWORD_POS], positions[HASH_POS]);
        GetPenColor(&dc, validConnections[CONFIG_NUMBERS_HASH], greyOutNumbers);
        dc.DrawLine(positions[CONFIG_NUM_4_POS], positions[HASH_POS]);

        if (
            validInputs[PUBLIC_KEY]
			// && validInputs[PATCH_KEY]
			&& validInputs[CHOICE_KEY]
			&& validInputs[SHUFFLE_KEY]
			// && (validInputs[CONFIG_KEYWORD_KEY]
                // || (validInputs[CONFIG_NUMBERS_1_KEY]
                //     && validInputs[CONFIG_NUMBERS_2_KEY]
                //     && validInputs[CONFIG_NUMBERS_3_KEY]
                //     && validInputs[CONFIG_NUMBERS_4_KEY]))
        ) {
            struct configuration config;
            config.size = 4;
            config.srcs = (struct source*)malloc(config.size * sizeof(struct source));
            if (!config.srcs) {
                return;
            }
            for (unsigned long i = 0; i < config.size; ++i) {
                config.srcs[i].elts = (char*)malloc(MAXSIZE_SMALL * sizeof(char));
                if (!config.srcs[i].elts) {
                    for (unsigned long j = 0; j < i; ++j) {
                        free(config.srcs[j].elts);
                    }
                    free(config.srcs);
                    return;
                }
                config.srcs[i].amount = 0; // Initialize amount or set it as needed
            }

            // if (validInputs[CONFIG_KEYWORD_KEY]) {
            //     string keyword = inputs[CONFIG_KEYWORD_KEY]->GetValue().ToStdString();
            //     setConfigWithKeyword(&config, keyword.c_str());
            if (validKeyword) {
                // string keyword = inputs[CONFIG_KEYWORD_KEY]->GetValue().ToStdString();
                setConfigWithKeyword(&config, keyword.c_str());
            } else { //if (validInputs[CONFIG_NUMBERS_1_KEY] && validInputs[CONFIG_NUMBERS_2_KEY] && validInputs[CONFIG_NUMBERS_3_KEY] && validInputs[CONFIG_NUMBERS_4_KEY]) {
                int numbers[4] = {
                    configNumbers[CONFIG_NUMBERS_1_KEY]->GetValue(),
                    configNumbers[CONFIG_NUMBERS_2_KEY]->GetValue(),
                    configNumbers[CONFIG_NUMBERS_3_KEY]->GetValue(),
                    configNumbers[CONFIG_NUMBERS_4_KEY]->GetValue()
                };
                setConfigWithNumbers(&config, numbers);
            }
            wxString hash = getHash(
                &config,
                inputs[PUBLIC_KEY]->GetValue(),
                patchKey->GetValue(),
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
    wxSpinCtrl* patchKey = new wxSpinCtrl(patchPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 128);
    patchKey->SetValue(0);
    wxBoxSizer* patchSizer = new wxBoxSizer(wxHORIZONTAL);
    patchSizer->Add(patchKey, wxSizerFlags().Border(wxLEFT|wxTOP, BORDER_WIDTH));
    patchPanel->SetSizer(patchSizer);

    wxPanel* configPanel = new wxPanel(getHashPanel);
    wxArrayString keywords;
    for (int i = 0; i < NUM_KEYWORDS; i++) {
        keywords.Add(CONFIG_KEYWORDS[i]);
    }
    wxChoice* configKeyword = new wxChoice(configPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, keywords);
    configKeyword->SetSelection(0);
    wxPanel* configNumbersPanel = new wxPanel(configPanel);
    wxSpinCtrl* configNumbers[NUM_CONFIG_NUMBERS];
    configNumbers[CONFIG_NUMBERS_1_KEY] = new wxSpinCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 26);
    configNumbers[CONFIG_NUMBERS_1_KEY]->SetValue(0);
    configNumbers[CONFIG_NUMBERS_2_KEY] = new wxSpinCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 26);
    configNumbers[CONFIG_NUMBERS_2_KEY]->SetValue(0);
    configNumbers[CONFIG_NUMBERS_3_KEY] = new wxSpinCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 12);
    configNumbers[CONFIG_NUMBERS_3_KEY]->SetValue(0);
    configNumbers[CONFIG_NUMBERS_4_KEY] = new wxSpinCtrl(configNumbersPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 10);
    configNumbers[CONFIG_NUMBERS_4_KEY]->SetValue(0);
    wxBoxSizer* configNumbersSizer = new wxBoxSizer(wxVERTICAL);
    configNumbersSizer->Add(configNumbers[CONFIG_NUMBERS_1_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersSizer->Add(configNumbers[CONFIG_NUMBERS_2_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersSizer->Add(configNumbers[CONFIG_NUMBERS_3_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersSizer->Add(configNumbers[CONFIG_NUMBERS_4_KEY], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configNumbersPanel->SetSizer(configNumbersSizer);
    wxBoxSizer* configSizer = new wxBoxSizer(wxHORIZONTAL);
    configSizer->AddStretchSpacer();
    configSizer->Add(configKeyword, wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP|wxBOTTOM, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    configSizer->Add(configNumbersPanel, wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    configSizer->AddStretchSpacer();
    configPanel->SetSizer(configSizer);

    wxTextCtrl* outputs[NUM_OUTPUTS];
    wxPanel* outputPanel = new wxPanel(getHashPanel);
    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputs[i] = new wxTextCtrl(outputPanel, wxID_ANY, "", outputPositions[i], wxDefaultSize, wxTE_READONLY | wxTE_CENTER);
    }

    wxBoxSizer* outputSizer = new wxBoxSizer(wxHORIZONTAL);
    outputSizer->AddStretchSpacer();
    for (int i = 0; i < NUM_OUTPUTS; i++) {
        outputSizer->Add(outputs[i], wxSizerFlags().Border(wxRIGHT|wxLEFT, BORDER_WIDTH));
    }
    outputSizer->AddStretchSpacer();
    outputPanel->SetSizer(outputSizer);
    getHashPanel->SetTextCtrls(inputs, patchKey, configKeyword, configNumbers, outputs);

    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
    mainSizer->Add(inputPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(patchPanel, 2, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(configPanel, 2, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(outputPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    getHashPanel->SetSizer(mainSizer);

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
