#include <cstring>
#include <string>
#include <wx/wx.h>

const unsigned int NUM_INPUTS = 4;
enum inputId { PUBLIC_KEY = 0, PATCH_KEY, CHOICE_KEY, SHUFFLE_KEY, CONFIG_KEYWORD_KEY, CONFIG_NUMBERS_KEY, CONFIG_RAW_KEY };
const unsigned int NUM_OUTPUTS = 1;
enum outputId { HASH = 0, PUBLIC_OUTPUT, CHOICE_OUTPUT, SHUFFLE_OUTPUT };

const unsigned int NUM_KEYWORDS = 8;
const std::string CONFIG_KEYWORDS[NUM_KEYWORDS] = { "long", "medium", "short", "anlong", "anshort", "pin", "mediumpin", "longpin" };

void AdjustTextCtrlSize(wxTextCtrl* textCtrl);
int GetTextWidthInPixels(wxTextCtrl* textCtrl);
bool validPrivateKey (const char* key, int count, int dashCount);
bool validKey (wxTextCtrl* key, int id);
