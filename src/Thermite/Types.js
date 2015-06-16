/* global exports */
"use strict";

// module Thermite.Types

exports.emptyAttr = {};

exports.appendAttr = function(attr1) {
    return function(attr2) {
        var o = {};
        for (var k in attr1) {
            if (attr1.hasOwnProperty(k)) {
                o[k] = attr1[k];
            }
        }
        for (var k in attr2) {
            if (attr2.hasOwnProperty(k)) {
                o[k] = attr2[k];
            }
        }
        return o;
    };
};
