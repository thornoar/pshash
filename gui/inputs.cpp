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
    if (keyword == "LONG") {
        config->size = 4;
        copySource(&(config->srcs[0]), sourceLower, 8);
        copySource(&(config->srcs[1]), sourceUpper, 8);
        copySource(&(config->srcs[2]), sourceSpecial, 5);
        copySource(&(config->srcs[3]), sourceNumbers, 4);
    } else if (keyword == "MEDIUM") {
        config->size = 4;
        copySource(&config->srcs[0], sourceLower, 5);
        copySource(&config->srcs[1], sourceUpper, 5);
        copySource(&config->srcs[2], sourceSpecial, 5);
        copySource(&config->srcs[3], sourceNumbers, 5);
    } else if (keyword == "SHORT") {
        config->size = 4;
        copySource(&config->srcs[0], sourceLower, 4);
        copySource(&config->srcs[1], sourceUpper, 4);
        copySource(&config->srcs[2], sourceSpecial, 4);
        copySource(&config->srcs[3], sourceNumbers, 4);
    } else if (keyword == "ANLONG") {
        config->size = 3;
        copySource(&config->srcs[0], sourceLower, 7);
        copySource(&config->srcs[1], sourceUpper, 7);
        copySource(&config->srcs[2], sourceNumbers, 7);
    } else if (keyword == "ANSHORT") {
        config->size = 3;
        copySource(&config->srcs[0], sourceLower, 4);
        copySource(&config->srcs[1], sourceUpper, 4);
        copySource(&config->srcs[2], sourceNumbers, 4);
    } else if (keyword == "PIN") {
        config->size = 1;
        copySource(&config->srcs[0], sourceNumbers, 4);
    } else if (keyword == "MEDIUMPIN") {
        config->size = 1;
        copySource(&config->srcs[0], sourceNumbers, 6);
    } else if (keyword == "LONGPIN") {
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
    dc.SetFont(textCtrl->GetFont());
    dc.GetTextExtent(textCtrl->GetValue(), &width, &height);
    wxSize bestSize = wxSize(width+BOX_HEIGHT, BOX_HEIGHT);
    wxSize oldSize = textCtrl->GetSize();
    textCtrl->SetSize(bestSize);
    wxPoint position = textCtrl->GetPosition();
    if (id != PUBLIC_KEY) {
        if (id == SHUFFLE_KEY)
            textCtrl->Move(position.x - (bestSize.x - oldSize.x), position.y);
        else
            textCtrl->Move(position.x - (bestSize.x - oldSize.x)/2, position.y);
    }
}

void SetTextContent(wxTextCtrl* textCtrl, const char* content) {
    textCtrl->SetSize(wxSize(BOX_HEIGHT, BOX_HEIGHT));
    textCtrl->SetValue(wxString(content));
    AdjustTextCtrlSize(textCtrl, -1);
}

int GetTextWidthInPixels(wxTextCtrl* textCtrl) {
    wxClientDC dc(textCtrl);
    dc.SetFont(textCtrl->GetFont());
    wxSize textSize = dc.GetTextExtent(textCtrl->GetValue());
    return textSize.GetWidth();
}

const char* GetTextCtrlValue(wxTextCtrl* textCtrl) {
    return textCtrl->GetValue().ToStdString().c_str();
}

wxPoint GetCtrlPosition(wxControl* ctrl, wxWindow* relativeTo) {
    wxPoint screenPos = ctrl->ClientToScreen(wxPoint(0, 0));
    wxPoint relativePos = relativeTo->ScreenToClient(screenPos);
    wxSize size = ctrl->GetSize();
    return wxPoint(relativePos.x + size.x / 2, relativePos.y + size.y / 2);
}

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
    try {
        unsigned long num = stoul(str);
        if (range == -1) return true;
        return num <= range;
    } catch (...) {
        return false;
    }
}

bool validKeyStr (wxString* keyStrStd, int id) {
    const char* keyStr = keyStrStd->c_str();
    if (*keyStr == '(') // )
        return false;
    switch (id) {
        case PUBLIC_KEY: {
            return keyStr[0] != '\0';
        }
        case CHOICE_KEY: {
            return validPrivateKey(keyStr, 0, 0);
        }
        case SHUFFLE_KEY: {
            return validPrivateKey(keyStr, 0, 0);
        }
    }
    return false;
}

bool validTextCtrl (wxTextCtrl* key, int id) {
    wxString keyStr = key->GetValue();
    return validKeyStr(&keyStr, id);
}

wxString getHash(const struct configuration* config, wxString publicStr, int patch, wxString choiceStr, wxString shuffleStr) {
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
