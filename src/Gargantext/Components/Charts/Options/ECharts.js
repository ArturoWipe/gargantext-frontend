"use strict";

var ReactEcharts = require("echarts-for-react");

exports.eChartsClass = ReactEcharts.default;

/**
 * @XXX "echarts-for-react" unsuitable to proper PureScript implementation
 * @name listenerFn1
 * @param {function} fn
 * @returns
 */
exports.listenerFn1 = function(fn) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    fn(args[0])()
  }
};
