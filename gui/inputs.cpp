#include "inputs.h"
#include <iostream>
using namespace std;
#include <ostream>
// #include "wx/string.h"

void AdjustTextCtrlSize(wxTextCtrl* textCtrl) {
    // Get the best size for the text control based on its content
    wxSize newSize = wxSize(GetTextWidthInPixels(textCtrl), textCtrl->GetSize().y);
    // wxSize bestSize = textCtrl->GetBestSize();
    // wxSize currentSize = textCtrl->GetSize();
    // cout << "Best size: " << bestSize.x << ", " << bestSize.y << endl;
    // cout << "Current size: " << currentSize.x << ", " << currentSize.y << endl;
    // Set the minimum size to ensure it can grow but not shrink below this size
    // textCtrl->SetMinSize(bestSi);
    // Optionally, set the size directly if you want immediate resizing
    textCtrl->SetSize(newSize);
    // Layout the parent to apply the new size
    textCtrl->GetParent()->Layout();
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
