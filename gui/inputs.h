#include <cstring>
#include <string>
#include <wx/wx.h>

const int VERTICAL_OFFSET = 60;
const int BORDER_WIDTH = 60;
const int SUPER_BORDER_WIDTH = 10;
const int BOX_HEIGHT = 35;
const int BOX_WIDTH = 115;

const unsigned int NUM_INPUTS = 7;
enum inputId { PUBLIC_KEY = 0, PATCH_KEY, CHOICE_KEY, SHUFFLE_KEY, CONFIG_KEYWORD_KEY, CONFIG_NUMBERS_KEY, CONFIG_RAW_KEY };
const wxPoint inputPositions[NUM_INPUTS] = { wxPoint(30, 10), wxPoint(30, 30), wxPoint(70, 10), wxPoint(150, 10) };

const unsigned int NUM_OUTPUTS = 1;
enum outputId { HASH = 0, PUBLIC_OUTPUT, CHOICE_OUTPUT, SHUFFLE_OUTPUT };
const wxPoint outputPositions[NUM_OUTPUTS] = { wxPoint(50, 450) };

const unsigned int NUM_KEYWORDS = 8;
const std::string CONFIG_KEYWORDS[NUM_KEYWORDS] = { "long", "medium", "short", "anlong", "anshort", "pin", "mediumpin", "longpin" };

void MoveTextCtrl(wxTextCtrl* textCtrl, wxFrame* frame, const wxPoint* position);
void AdjustTextCtrlSize(wxTextCtrl* textCtrl);
int GetTextWidthInPixels(wxTextCtrl* textCtrl);
wxPoint GetTextCtrlPosition(wxControl* textCtrl, wxWindow* relativeTo);
const char* GetTextCtrlValue(wxTextCtrl* textCtrl);
bool validPrivateKey (const char* key, int count, int dashCount);
bool validKey (wxTextCtrl* key, int id);
wxString getHash(const struct configuration* config, wxString publicStr, wxString patchStr, wxString choiceStr, wxString shuffleStr);
