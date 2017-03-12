exports._findall = function (re) {
    return function (string) {
        var out = [];
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
