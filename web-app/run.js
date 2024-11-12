function copyToClipboard() {
    var keyword = document.getElementById("configKeyword").value;
    var publicStr = document.getElementById("publicStr").value;
    var choiceStr = document.getElementById("choiceStr").value;
    var shuffleStr = document.getElementById("shuffleStr").value;
    var hash = getFinalHash(determineConfiguration(keyword), publicStr, choiceStr, shuffleStr);
    navigator.clipboard.writeText(hash);
    document.getElementById("copymessage").innerHTML = "(copied)";
    // document.getElementById("output").innerHTML = hash;
}

function showHash() {
    var keyword = document.getElementById("configKeyword").value;
    var publicStr = document.getElementById("publicStr").value;
    var choiceStr = document.getElementById("choiceStr").value;
    var shuffleStr = document.getElementById("shuffleStr").value;
    var hash = getFinalHash(determineConfiguration(keyword), publicStr, choiceStr, shuffleStr);
    // navigator.clipboard.writeText(hash);
    // document.getElementById("copymessage").innerHTML = "Copied to clipboard!";
    document.getElementById("output").innerHTML = hash;
}
// function updateNameDisplay() {
//     document.getElementById('nameDisplay').innerHTML = this.value || "??";
// }
