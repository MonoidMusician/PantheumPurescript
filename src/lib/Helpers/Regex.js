// module Helpers.Regex

exports.escape = function (str) {
    return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
};
