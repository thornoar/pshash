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
    // wxWindow* parent = textCtrl->GetParent();
    wxSize windowSize = frame->GetSize();
    cout << "Window size: " << windowSize.x << ", " << windowSize.y << endl;
    wxSize textCtrlSize = textCtrl->GetSize();
    wxPoint realPosition = wxPoint(windowSize.x * position->x / 100, windowSize.y * position->y / 100);
    // textCtrl->Move(wxPoint(position.x - textCtrl->GetSize().x / 2, position.y - textCtrl->GetSize().y / 2));
    textCtrl->Move(realPosition.x - textCtrlSize.x / 2, realPosition.y - textCtrlSize.y / 2);
}

void AdjustTextCtrlSize(wxTextCtrl* textCtrl) {
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
    textCtrl->Move(position.x - (bestSize.x - oldSize.x)/2, position.y);
    // Layout the parent to apply the new size
    // parent->Layout();
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

wxPoint GetTextCtrlPosition(wxControl* textCtrl, wxWindow* relativeTo) {
    // Get the position of the text control in screen coordinates
    wxPoint screenPos = textCtrl->ClientToScreen(wxPoint(0, 0));
    // Convert the screen position to the coordinate system of the relative window
    wxPoint relativePos = relativeTo->ScreenToClient(screenPos);
    wxSize size = textCtrl->GetSize();
    // Return the center point of the text control
    return wxPoint(relativePos.x + size.x / 2, relativePos.y + size.y / 2);
}

bool isNumber (const char* str) {
    if (*str == '\0')
        return true;
    if (*str < '0' || *str > '9')
        return false;
    return isNumber(str+1);
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

bool validKey (wxTextCtrl* keyCtrl, int id) {
    wxString key = keyCtrl->GetValue();
    switch (id) {
        case PUBLIC_KEY:
            return !key.IsEmpty();
        case PATCH_KEY:
            return isNumber(key.ToStdString().c_str());
        case CHOICE_KEY:
            return validPrivateKey(key.ToStdString().c_str(), 0, 0);
        case SHUFFLE_KEY:
            return validPrivateKey(key.ToStdString().c_str(), 0, 0);
        case CONFIG_RAW_KEY:
            for (int i = 0; i < NUM_KEYWORDS; i++) {
                if (CONFIG_KEYWORDS[i] == key.ToStdString().c_str())
                    return true;
            }
            return false;
    }
    return false;
}

wxString getHash(const struct configuration* config, wxString publicStr, wxString patchStr, wxString choiceStr, wxString shuffleStr) {
    int patch = patchStr.IsEmpty() ? 0 : stoi(patchStr.ToStdString());
    char publicStrChar[MAXSIZE_SMALL];
    strcpy(publicStrChar, publicStr.ToStdString().c_str());
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
