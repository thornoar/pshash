const currentVersion = "0.1.3.0-web"

// General-purpose functions

function arr (str) {
    return str.split("")
}

function factorial (n) {
    if (n == 0) { return 1n }
    else { return BigInt(n) * factorial(n-1) }
}
function factorialP (n, m) {
    if (m == 0) { return 1n }
    else { return BigInt(n - (m-1)) * factorialP(n, m-1) }
}

function cnk (n, k) {
    return factorialP(n, k) / factorial(k)
}

function len (arr) {
    return arr.length
}

function shift (a) {
    return BigInt(a.charCodeAt(0))
}

function shiftlst (as) {
    var sum = 0n
    for (var i = 0; i < as.length; i++) { sum += shift(as[i]) }
    return sum
}

function shiftlstlst (ass) {
    var sum = 0n
    for (var i = 0; i < ass.length; i++) { sum += shiftlst(ass[i]) }
    return sum
}

function mapHashing (f, spr, lst, key) {
    if (lst.length == 0) { return [] }
    else {
        var a = lst[0]
        var rest = lst.slice(1)
        var s = spr(a)
        var keyDiv = key / s
        var keyMod = key % s
        var b = f(a, keyMod)
        var nextKey = keyDiv + shiftlst(b)
        return [b].concat(mapHashing(f, spr, rest, nextKey))
    }
}

function composeHashing (f, g, a, key1, key2) {
    return g(f(a, key1), key2)
}

function composeHashingP (f, spr, g, a, key) {
    var s = spr(a)
    var keyDiv = key / s
    var keyMod = key % s
    var b = f(a, keyMod)
    var nextKey = keyDiv + shiftlstlst(b)
    return g(b, nextKey)
}

const sourceLower = Object.freeze(arr("ckapzfitqdxnwehrolmbyvsujg"))
const sourceUpper = Object.freeze(arr("RQLIANBKJYVWPTEMCZSFDOGUHX"))
const sourceSpecial = Object.freeze(arr("=!*@?$%#&-+^"))
const sourceNumbers = Object.freeze(arr("1952074386"))

const defaultConfiguration = Object.freeze([[sourceLower, 8], [sourceUpper, 8], [sourceSpecial, 5], [sourceNumbers, 4]])
const mediumConfiguration = Object.freeze([[sourceLower, 5], [sourceUpper, 5], [sourceSpecial, 5], [sourceNumbers, 5]])
const shortConfiguration = Object.freeze([[sourceLower, 4], [sourceUpper, 4], [sourceSpecial, 4], [sourceNumbers, 4]])
const anlongConfiguration = Object.freeze([[sourceLower, 7], [sourceUpper, 7], [sourceNumbers, 7]])
const anshortConfiguration = Object.freeze([[sourceLower, 4], [sourceUpper, 4], [sourceNumbers, 4]])
const pinCodeConfiguration = Object.freeze([[sourceNumbers, 4]])
const mediumPinCodeConfiguration = Object.freeze([[sourceNumbers, 6]])
const longPinCodeConfiguration = Object.freeze([[sourceNumbers, 8]])

// Hash-generating functions

function chooseOrdered (source, key) {
    if (source[1] == 0 || source[0].length == 0) { return []; }
    else {
        var s = BigInt(source[0].length)
        var keyDiv = key / s
        var keyMod = key % s
        var curElt = source[0][keyMod]
        var nextSource = [source[0].toSpliced(Number(keyMod), 1), source[1]-1]
        var nextKey = keyDiv + shift(curElt)
        return [curElt].concat(chooseOrdered(nextSource, nextKey))
    }
}

function chooseSpread (amts) {
    return factorialP(amts[0], amts[1]);
}

function mergeTwoLists (lst1, lst2, key) {
    if (lst1.length == 0) { return lst2; }
    if (lst2.length == 0) { return lst1; }
    
    function mergeTwoBoundary (m1, m2) {
        return factorial(m1 + m2) / (factorial(m1) * factorial(m2));
    }

    var spr1 = mergeTwoBoundary(lst1.length - 1, lst2.length)
    var spr2 = mergeTwoBoundary(lst1.length, lst2.length - 1)

    var curKey = key % (spr1 + spr2)

    if (curKey < spr1) {
        var elt = lst1[0]
        return [elt].concat(mergeTwoLists(lst1.slice(1), lst2, (curKey + shift(elt))))
    } else {
        var elt = lst2[0]
        return [elt].concat(mergeTwoLists(lst1, lst2.slice(1), (curKey - spr1 + shift(elt))))
    }
}

function sum (arr) {
    function add (acc, a) {
        return acc + a
    }
    return arr.reduce(add, 0)
}

function prod (arr) {
    function mult (acc, a) {
        return acc * a
    }
    return arr.reduce(mult, 1n)
}

function mergeListsSpread (amts) {
    return factorial(sum(amts)) / prod(amts.map(factorial))
}

function mergeLists (lsts, key) {
    if (lsts.length == 0) { return [] }
    if (lsts.length == 1) { return lsts[0] }
    if (lsts.length == 2) {
        return mergeTwoLists(lsts[0], lsts[1], key)
    }

    var l = lsts[0]
    var rest = lsts.slice(1)
    var s = mergeListsSpread(rest.map((lst) => lst.length))
    var keyDiv = key / s
    var keyMod = key % s
    var nextKey = keyDiv + shiftlst(l)
    return mergeTwoLists(l, mergeLists(rest, keyMod), nextKey)
}

function dropElementInfo (source) {
    return [source[0].length, source[1]]
}

function getChoiceAndMerge (config, key) {
    function chooseSpreadP (source) {
        return chooseSpread(dropElementInfo(source))
    }
    function f (conf, key) {
        return mapHashing(chooseOrdered, chooseSpreadP, conf, key)
    }
    function spr (conf) {
        return prod(conf.map((source) => chooseSpreadP(source)))
    }

    return composeHashingP(f, spr, mergeLists, config, key)
}

function getHash (config, key1, key2) {
    function shuffleList (lst, key) {
        return chooseOrdered([lst, lst.length], key)
    }
    return composeHashing(getChoiceAndMerge, shuffleList, config, key1, key2)
}

// Reading keys

function getPublicKey (arr) {
    if (arr.length == 0) { return 0n }
    var c = arr[0]
    var rest = arr.slice(1)
    return (BigInt(c.charCodeAt(0)) * (128n ** BigInt(rest.length))) + BigInt(getPublicKey(rest))
}

function breakArray (arr, elt) {
    for (var i = 0; i < arr.length; i++) {
        if (arr[i] == elt) {
            return [arr.slice(0,i), arr.slice(i+1, arr.length)]
        }
    }
    return [arr]
}

function getPrivateKey (arr) {
    var broken = breakArray(arr, '-')
    if (broken.length == 1) { return BigInt(parseInt(broken[0].join(""))) }
    else {
        var base = BigInt(parseInt(broken[0].join("")))
        var pow = BigInt(parseInt(broken[1].join("")))
        return base ** pow
    }
}

function determineConfiguration (str) {
    if (str == "") return defaultConfiguration
    if (str == "long") return defaultConfiguration
    if (str == "medium") return mediumConfiguration
    if (str == "short") return shortConfiguration
    if (str == "anlong") return anlongConfiguration
    if (str == "anshort") return anshortConfiguration
    if (str == "pin") return pinCodeConfiguration
    if (str == "mediumpin") return mediumPinCodeConfiguration
    if (str == "longpin") return longPinCodeConfiguration
    return defaultConfiguration
}

// Computing the final hash

function numberOfPrivateChoiceKeys (amts) {
    return prod(amts.map(chooseSpread)) * mergeListsSpread(amts.map((lst) => lst[1]))
}

function getFinalHash (config, publicStr, choiceStr, shuffleStr) {
    var publicArr = arr(publicStr)
    var choiceArr = arr(choiceStr)
    var shuffleArr = arr(shuffleStr)

    var publicKey = getPublicKey(publicArr)
    var div = numberOfPrivateChoiceKeys(config.map(dropElementInfo))
    var choiceKey = (publicKey + getPrivateKey(choiceArr)) % div
    var shuffleKey = getPrivateKey(shuffleArr)

    return getHash(config, choiceKey, shuffleKey).join("")
}
