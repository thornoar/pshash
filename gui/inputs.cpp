#include "inputs.h"
#include <string>
#include "algorithm.h"
#include "mini-gmp.h"
using namespace std;

wxPoint toScreenCoordinates(wxPoint coords, wxFrame* frame) {
    wxPoint screenCoordinates = frame->ClientToScreen(coords);
    return screenCoordinates;
}

void MoveTextCtrl(wxTextCtrl* textCtrl, wxFrame* frame, const wxPoint* position) {
    wxSize windowSize = frame->GetSize();
    wxSize textCtrlSize = textCtrl->GetSize();
    wxPoint realPosition = wxPoint(windowSize.x * position->x / 100, windowSize.y * position->y / 100);
    textCtrl->Move(realPosition.x - textCtrlSize.x / 2, realPosition.y - textCtrlSize.y / 2);
}

void copySource (struct source* dest, const char* src, unsigned long amount) {
    strcpy(dest->elts, src);
    dest->amount = amount;
}

void setConfigWithKeyword (struct configuration* config, string keyword) {
    if (keyword == "long") {
        config->size = 4;
        copySource(&(config->srcs[0]), sourceLower, 8);
        copySource(&(config->srcs[1]), sourceUpper, 8);
        copySource(&(config->srcs[2]), sourceSpecial, 5);
        copySource(&(config->srcs[3]), sourceNumbers, 4);
    } else if (keyword == "medium") {
        config->size = 4;
        copySource(&config->srcs[0], sourceLower, 5);
        copySource(&config->srcs[1], sourceUpper, 5);
        copySource(&config->srcs[2], sourceSpecial, 5);
        copySource(&config->srcs[3], sourceNumbers, 5);
    } else if (keyword == "short") {
        config->size = 4;
        copySource(&config->srcs[0], sourceLower, 4);
        copySource(&config->srcs[1], sourceUpper, 4);
        copySource(&config->srcs[2], sourceSpecial, 4);
        copySource(&config->srcs[3], sourceNumbers, 4);
    } else if (keyword == "anlong") {
        config->size = 3;
        copySource(&config->srcs[0], sourceLower, 7);
        copySource(&config->srcs[1], sourceUpper, 7);
        copySource(&config->srcs[2], sourceNumbers, 7);
    } else if (keyword == "anshort") {
        config->size = 3;
        copySource(&config->srcs[0], sourceLower, 4);
        copySource(&config->srcs[1], sourceUpper, 4);
        copySource(&config->srcs[2], sourceNumbers, 4);
    } else if (keyword == "pin") {
        config->size = 1;
        copySource(&config->srcs[0], sourceNumbers, 4);
    } else if (keyword == "mediumpin") {
        config->size = 1;
        copySource(&config->srcs[0], sourceNumbers, 6);
    } else if (keyword == "longpin") {
        config->size = 1;
        copySource(&config->srcs[0], sourceNumbers, 8);
    }
}

void setConfigWithNumbers (struct configuration* config, int* numbers) {
    config->size = 4;
    copySource(&config->srcs[0], sourceLower, numbers[0]);
    copySource(&config->srcs[1], sourceUpper, numbers[1]);
    copySource(&config->srcs[2], sourceSpecial, numbers[2]);
    copySource(&config->srcs[3], sourceNumbers, numbers[3]);
}

void AdjustTextCtrlSize(wxTextCtrl* textCtrl, int id) {
    wxWindow* parent = textCtrl->GetParent();

    int width, height;
    wxClientDC dc(parent);
    dc.SetFont(textCtrl->GetFont()); // Set the font to match the window's font
    dc.GetTextExtent(textCtrl->GetValue(), &width, &height); // Get the width and height of the text

    // Get the best size for the text control based on its content
    wxSize bestSize = wxSize(width+BOX_HEIGHT, BOX_HEIGHT);

    // wxSize bestSize = textCtrl->GetBestSize();
    wxSize oldSize = textCtrl->GetSize();

    textCtrl->SetSize(bestSize);
    wxPoint position = textCtrl->GetPosition();
    if (id != PUBLIC_KEY) {
        if (id == SHUFFLE_KEY)
            textCtrl->Move(position.x - (bestSize.x - oldSize.x), position.y);
        else
            textCtrl->Move(position.x - (bestSize.x - oldSize.x)/2, position.y);
    }
    // Layout the parent to apply the new size
    // parent->Layout();
}

void SetTextContent(wxTextCtrl* textCtrl, const char* content) {
    textCtrl->SetSize(wxSize(BOX_HEIGHT, BOX_HEIGHT));
    textCtrl->SetValue(wxString(content));
    AdjustTextCtrlSize(textCtrl, -1);
    // textCtrl->GetParent()->Refresh();
}

int GetTextWidthInPixels(wxTextCtrl* textCtrl) {
    // Create a device context for the text control
    wxClientDC dc(textCtrl);
    // Set the font of the device context to match the text control's font
    dc.SetFont(textCtrl->GetFont());

    // Measure the width and height of the text
    wxSize textSize = dc.GetTextExtent(textCtrl->GetValue());

    // Return the width in pixels
    return textSize.GetWidth();
}

const char* GetTextCtrlValue(wxTextCtrl* textCtrl) {
    return textCtrl->GetValue().ToStdString().c_str();
}

wxPoint GetCtrlPosition(wxControl* ctrl, wxWindow* relativeTo) {
    // Get the position of the text control in screen coordinates
    wxPoint screenPos = ctrl->ClientToScreen(wxPoint(0, 0));
    // Convert the screen position to the coordinate system of the relative window
    wxPoint relativePos = relativeTo->ScreenToClient(screenPos);
    wxSize size = ctrl->GetSize();
    // Return the center point of the text control
    return wxPoint(relativePos.x + size.x / 2, relativePos.y + size.y / 2);
}

// bool isNumber (const char* str) {
//     if (*str == '\0')
//         return false;
//     if (*str < '0' || *str > '9')
//         return false;
//     if (*(str+1) == '\0')
//         return true;
//     return isNumber(str+1);
// }

bool validPrivateKey (const char* key, int count, int dashCount) {
    if (*key == '\0')
        return count > 0 && dashCount <= 1;
    if (*key == '-') {
        if (dashCount > 0 || count == 0 || *(key+1) == '\0') {
            return false;
        }
        dashCount++;
    } else if (*key < '0' || *key > '9') {
        return false;
    }
    return validPrivateKey(key+1, count+1, dashCount);
}

bool validNumber (const char* str, int range) {
    // if (!isNumber(str))
    //     return false;
    try {
        unsigned long num = stoul(str);
        if (range == -1) return true;
        return num <= range;
    } catch (...) {
        return false;
    }
}

// bool validPatch (const char* str) {
//     return validNumber(str, -1);
// }

bool validKeyStr (wxString* keyStrStd, int id) {
    // string stdStr = keyCtrl->GetValue().ToStdString();
    const char* keyStr = keyStrStd->c_str();
    if (*keyStr == '(') // )
        return false;
    switch (id) {
        case PUBLIC_KEY: {
            return keyStr[0] != '\0';
        }
        // case PATCH_KEY: {
        //     return validNumber(keyStr, 128);
        // }
        case CHOICE_KEY: {
            return validPrivateKey(keyStr, 0, 0);
        }
        case SHUFFLE_KEY: {
            return validPrivateKey(keyStr, 0, 0);
        }
        // case CONFIG_KEYWORD_KEY:
        //     for (int i = 0; i < NUM_KEYWORDS; i++) {
        //         if (CONFIG_KEYWORDS[i] == keyStr)
        //             return true;
        //     }
        //     return false;
        // case CONFIG_NUMBERS_1_KEY: {
        //     return validNumber(keyStr, 26);
        // }
        // case CONFIG_NUMBERS_2_KEY: {
        //     return validNumber(keyStr, 26);
        // }
        // case CONFIG_NUMBERS_3_KEY: {
        //     return validNumber(keyStr, 12);
        // }
        // case CONFIG_NUMBERS_4_KEY: {
        //     return validNumber(keyStr, 10);
        // }
    }
    return false;
}

bool validTextCtrl (wxTextCtrl* key, int id) {
    wxString keyStr = key->GetValue();
    // cout << keyStr << endl;
    return validKeyStr(&keyStr, id);
}

// bool validSpinCtrl (wxSpinCtrl* key, int range) {
//     int value = key->GetValue();
//     // cout << keyStr << endl;
//     return value >= 0 && value <= range;
// }

wxString getHash(const struct configuration* config, wxString publicStr, int patch, wxString choiceStr, wxString shuffleStr) {
    // int patch = patchStr.IsEmpty() ? 0 : stoi(patchStr.ToStdString());
    char publicStrChar[MAXSIZE_SMALL];
    string publicStrStd = publicStr.ToStdString();
    strcpy(publicStrChar, publicStrStd.c_str());
    int i = 0;
    while (publicStrChar[i] != '\0') {
        publicStrChar[i] = (publicStrChar[i] + patch) % 128;
        i++;
    }
    mpz_t key1; mpz_init(key1); parse_key(key1, choiceStr.ToStdString().c_str());
    mpz_t public_key; mpz_init(public_key); get_public_key(public_key, publicStrChar);
    mpz_add(key1, key1, public_key);
    mpz_t key2; mpz_init(key2); parse_key(key2, shuffleStr.ToStdString().c_str());
    char hash[MAXSIZE_BIG];
    get_hash(hash, config, key1, key2);
    return wxString(hash);
}

// int main () {
//     struct configuration config;
//     config.size = 4;
//     config.srcs = (struct source*)malloc(config.size * sizeof(struct source));
//     for (unsigned long i = 0; i < config.size; ++i) {
//         config.srcs[i].elts = (char*)malloc(MAXSIZE_SMALL * sizeof(char));
//         config.srcs[i].amount = 0; // Initialize amount or set it as needed
//     }
//     unsigned long nums[4] = { 0, 0, 0, 2 };
//     setConfigWithNumbers(&config, nums);
//     // setConfigWithKeyword(&config, "long");
//     cout << config.size << endl;
//     for (int i = 0; i < 4; i++) {
//         cout << config.srcs[i].elts << "   ";
//         cout << config.srcs[i].amount;
//         cout << endl;
//     }
//     mpz_t key1; mpz_init_set_str(key1, "43", 10);
//     mpz_t key2; mpz_init_set_str(key2, "42", 10);
//     char hash[MAXSIZE_BIG];
//     get_hash(hash, &config, key1, key2);
//     cout << hash << endl;
//     // cout << getHash(&config, wxString("aaaaaaa"), wxString("0"), wxString("42"), wxString("42")) << endl;
// }
