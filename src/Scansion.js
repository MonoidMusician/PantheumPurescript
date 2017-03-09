exports._findall = function (re) {
    return function (str) {
        var out = [];
        var string = str;
        var lastIndex = re.lastIndex;
        var prevLastIndex = -1;
        var result;

        while (result = re.exec(string)) {
            if (prevLastIndex === re.lastIndex) break;

            var i = result.length - 1;
            out.push(result[0]);

            prevLastIndex = re.lastIndex;
        }

        re.lastIndex = lastIndex;
        return out;
    }
}
