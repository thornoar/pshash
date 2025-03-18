// ========================================================
// IMPORTS
// ========================================================

// #include <cstdlib>
// #include <cstring>
#include "algorithm.h"

// #include "wx/wx.h"
// #include <wx/wxprec.h>
// #ifndef WX_PRECOMP
//     #include "wx/wx.h"
// #endif

#include "inputs.h"
#include <wx/clipbrd.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/dcclient.h>
#include <wx/aboutdlg.h>
#include <wx/notebook.h>

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
    void SetFields (
        wxTextCtrl** newInputs,
        wxSpinCtrl* newPatchKey,
        wxChoice* newConfigKeyword,
        wxSpinCtrl** newConfigNumbers,
        wxTextCtrl* newHashOutput
    ) {
        for (int i = 0; i < NUM_INPUTS; i++) {
            inputs[i] = newInputs[i];
        }
        patchKey = newPatchKey;
        configKeyword = newConfigKeyword;
        for (int i = 0; i < NUM_CONFIG_NUMBERS; i++) {
            configNumbers[i] = newConfigNumbers[i];
        }
        hashOutput = newHashOutput;
        wxColour baseColor = wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOW);
        float scale = 1;
        inputColor = wxColor(baseColor.Red()*scale, baseColor.Green()*scale, baseColor.Blue()*scale);
        scale = 0.8;
        configColor = wxColor(baseColor.Red()*scale, baseColor.Green()*scale, baseColor.Blue()*scale);
        scale = 0.6;
        outputColor = wxColor(baseColor.Red()*scale, baseColor.Green()*scale, baseColor.Blue()*scale);
    }
    void OnTextChange (wxCommandEvent& event) {
        for (int i = 0; i < NUM_INPUTS; i++) {
            AdjustTextCtrlSize(inputs[i], i);
        }
        AdjustTextCtrlSize(hashOutput);
        Refresh();
    }

private:
    wxTextCtrl* inputs[NUM_INPUTS];
    wxSpinCtrl* patchKey;
    wxChoice* configKeyword;
    wxSpinCtrl* configNumbers[NUM_CONFIG_NUMBERS];
    wxTextCtrl* hashOutput;
    wxColor inputColor;
    wxColor configColor;
    wxColor outputColor;

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
        positions[NUM_INPUTS + NUM_CONFIG_NUMBERS + 2] = GetCtrlPosition(hashOutput, this);

        wxSize size = GetSize();

        int y1 = (positions[PATCH_POS].y + positions[CONFIG_NUM_1_POS].y) / 2;
        dc.SetBrush(inputColor);
        dc.SetPen(wxPen(inputColor, 0));
        dc.DrawRectangle(0, 0, size.x, y1);
        int y2 = (positions[CONFIG_NUM_4_POS].y + positions[HASH_POS].y) / 2;
        dc.SetBrush(configColor);
        dc.SetPen(wxPen(configColor, 0));
        dc.DrawRectangle(0, y1, size.x, y2 - y1);
        dc.SetBrush(outputColor);
        dc.SetPen(wxPen(outputColor, 0));
        dc.DrawRectangle(0, y2, size.x, size.y - y2);
        

        bool validInputs[NUM_INPUTS];
        for (int i = 0; i < NUM_INPUTS; i++) {
            validInputs[i] = validInput(i);
        }
        string keyword = configKeyword->GetStringSelection().ToStdString();
        bool validKeyword = keyword != "(NONE)";
        bool validConfigNumbers[NUM_CONFIG_NUMBERS];

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
			&& validInputs[CHOICE_KEY]
			&& validInputs[SHUFFLE_KEY]
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
                config.srcs[i].amount = 0;
            }

            if (validKeyword) {
                setConfigWithKeyword(&config, keyword.c_str());
            } else {
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
            hashOutput->SetValue(hash);
            for (unsigned long i = 0; i < config.size; ++i) {
                free(config.srcs[i].elts);
            }
            free(config.srcs);
        } else {
            hashOutput->SetValue("ERROR");
        }
        for (int i = 0; i < NUM_INPUTS; i++) { AdjustTextCtrlSize(inputs[i], i); }
        AdjustTextCtrlSize(hashOutput);
        Refresh();
    }
};
 
class MyFrame : public wxFrame {
    public:
    MyFrame();

    private:
    wxTextCtrl* hashOutput;
    void OnExit(wxCommandEvent& event) {
        Close(true);
    }
    void OnAbout(wxCommandEvent& event);
    void OnVersion(wxCommandEvent& event) {
        wxMessageBox(
            "This is the pshash pseudo-hash algorithm GUI, version 1.0",
            "Version",
            wxOK | wxICON_INFORMATION
        );
    }
    void OnDocumentation(wxCommandEvent& WXUNUSED(event)) {
        wxLaunchDefaultBrowser("https://thornoar.github.io/pshash/web/help/");
    }
    void OnCopyToClipboard(wxCommandEvent& event) {
        if (wxTheClipboard->Open()) {
            wxTheClipboard->SetData(new wxTextDataObject(hashOutput->GetValue()));
            wxTheClipboard->Close();
        }
    }
};
 
enum {
    ID_Preferences = 1,
    ID_Version,
    ID_Documentation,
    ID_CopyToClipboard
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
    menuInfo->Append(ID_Documentation, "&Algorithm &Description", "Open the algorithm documentation");
    menuInfo->AppendSeparator();
    menuInfo->Append(ID_Version, "&Version\tCtrl-V", "Show current program version");
    wxMenuBar *menuBar = new wxMenuBar;
    menuBar->Append(menuSettings, "&Settings");
    menuBar->Append(menuInfo, "&Info");
    SetMenuBar(menuBar);

    // CreateStatusBar();
    // SetStatusText("The pshash pseudo-hash algorithm, version 1.0");

    // wxNotebook* notebook = new wxNotebook(this, wxID_ANY);
    // GetHashPanel* getHashPanel = new GetHashPanel(notebook);

    // Custom panel
    GetHashPanel* getHashPanel = new GetHashPanel(this);

    wxTextCtrl* inputs[NUM_INPUTS];
    wxPanel* inputPanel = new wxPanel(getHashPanel);
    inputs[PUBLIC_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[PUBLIC_KEY]->SetValue(wxString("(PUBLIC KEY)"));
    inputs[CHOICE_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[CHOICE_KEY]->SetValue(wxString("(CHOICE KEY)"));
    inputs[SHUFFLE_KEY] = new wxTextCtrl(inputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_CENTER);
    inputs[SHUFFLE_KEY]->SetValue(wxString("(SHUFFLE KEY)"));

    wxBoxSizer* primaryInputSizer = new wxBoxSizer(wxHORIZONTAL);
    primaryInputSizer->Add(inputs[PUBLIC_KEY],  wxSizerFlags().Border(wxLEFT|wxTOP, BORDER_WIDTH));
    primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[CHOICE_KEY], wxSizerFlags().Border(wxTOP, BORDER_WIDTH+40));
    primaryInputSizer->AddStretchSpacer();
    primaryInputSizer->Add(inputs[SHUFFLE_KEY], wxSizerFlags().Border(wxRIGHT|wxTOP, BORDER_WIDTH));
    inputPanel->SetSizer(primaryInputSizer);

    wxPanel* patchPanel = new wxPanel(getHashPanel);
    wxSpinCtrl* patchKey = new wxSpinCtrl(patchPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 128);
    patchKey->SetValue(0);
    wxBoxSizer* patchSizer = new wxBoxSizer(wxHORIZONTAL);
    patchSizer->Add(patchKey, wxSizerFlags().Border(wxLEFT, BORDER_WIDTH));
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
    configSizer->Add(configNumbersPanel, wxSizerFlags().Border(wxRIGHT|wxLEFT|wxTOP, BORDER_WIDTH/2));
    configSizer->AddStretchSpacer();
    configPanel->SetSizer(configSizer);

    wxPanel* outputPanel = new wxPanel(getHashPanel);
    hashOutput = new wxTextCtrl(outputPanel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxTE_CENTER);

    wxBoxSizer* outputSizer = new wxBoxSizer(wxHORIZONTAL);
    outputSizer->AddStretchSpacer();
    outputSizer->Add(hashOutput, wxSizerFlags().Border(wxTOP, BORDER_WIDTH/2));
    outputSizer->AddStretchSpacer();
    outputPanel->SetSizer(outputSizer);
    getHashPanel->SetFields(inputs, patchKey, configKeyword, configNumbers, hashOutput);

    wxPanel* clipboardPanel = new wxPanel(getHashPanel);
    wxButton* copyButton = new wxButton(clipboardPanel, ID_CopyToClipboard, "Copy to Clipboard", wxDefaultPosition, wxDefaultSize);
    wxBoxSizer* clipboardSizer = new wxBoxSizer(wxHORIZONTAL);
    clipboardSizer->AddStretchSpacer();
    clipboardSizer->Add(copyButton, wxSizerFlags().Border(wxBOTTOM, BORDER_WIDTH/2));
    clipboardSizer->AddStretchSpacer();
    clipboardPanel->SetSizer(clipboardSizer);

    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
    mainSizer->Add(inputPanel, 3, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(patchPanel, 2, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(configPanel, 3, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(outputPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    mainSizer->Add(clipboardPanel, 1, wxEXPAND | wxLEFT | wxRIGHT, SUPER_BORDER_WIDTH);
    getHashPanel->SetSizer(mainSizer);

    // notebook->AddPage(getHashPanel, "HASH GENERATION");

    // wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);
    // sizer->Add(notebook, 1, wxEXPAND);
    // SetSizer(sizer);
    SetSizerAndFit(new wxBoxSizer(wxVERTICAL));
    GetSizer()->Add(getHashPanel, 1, wxEXPAND);

    // Bind events
    Bind(wxEVT_MENU, &MyFrame::OnAbout, this, wxID_ABOUT);
    Bind(wxEVT_MENU, &MyFrame::OnVersion, this, ID_Version);
    Bind(wxEVT_MENU, &MyFrame::OnDocumentation, this, ID_Documentation);
    Bind(wxEVT_MENU, &MyFrame::OnExit, this, wxID_EXIT);
    Bind(wxEVT_BUTTON, &MyFrame::OnCopyToClipboard, this, ID_CopyToClipboard);

    for (int i = 0; i < NUM_INPUTS; i++) {
        inputs[i]->Bind(wxEVT_TEXT, &GetHashPanel::OnTextChange, getHashPanel);
    }
}

void MyFrame::OnAbout(wxCommandEvent& WXUNUSED(event))
{
    wxAboutDialogInfo info;
    info.SetName("Pshash Pseudo-Hash Algorithm GUI");
    info.SetVersion(currentVersion);
    info.SetDescription(
            "A lightweight GUI password generation tool,\n"
            "using the pshash pseudo-hash algorithm."
    );
    info.SetCopyright("(C) 2024-2025 Roman Maksimovich");
    info.AddDeveloper("Roman Maksimovich");
    info.SetWebSite("http://github.com/thornoar/pshash", "pshash GitHub repository");
    info.SetLicence(wxString::FromAscii(
        "Copyright (c) 2025 Roman Maksimovich\n"
        "\n"
        "Permission is hereby granted, free of charge, to any person obtaining\n"
        "a copy of this software and associated documentation files (the\n"
        "\"Software\"), to deal in the Software without restriction, including\n"
        "without limitation the rights to use, copy, modify, merge, publish,\n"
        "distribute, sublicense, and/or sell copies of the Software, and to\n"
        "permit persons to whom the Software is furnished to do so, subject to\n"
        "the following conditions:\n"
        "\n"
        "The above copyright notice and this permission notice shall be included\n"
        "in all copies or substantial portions of the Software.\n"
        "\n"
        "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\n"
        "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\n"
        "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\n"
        "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY\n"
        "CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,\n"
        "TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE\n"
        "SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n"
    ));

    wxAboutBox(info, this);
}

