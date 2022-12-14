'use strict';

function addRootElement(rootElem) {
  document.body.insertBefore(
    rootElem,
    document.body.lastElementChild.nextElementSibling
  );
}

function getSelection(_u) {
  return window.getSelection();
}

function stringify(j, indent) {
  return JSON.stringify(j, null, indent);
}

function postMessage(obj, msg, src) {
    obj.contentWindow.postMessage(msg, src);
}

function setCookie(c) {
  document.cookie = c;
}

function domRectFromRect(obj) {
  return DOMRectReadOnly.fromRect(obj)
}

exports._addRootElement = addRootElement;
exports._getSelection = getSelection;
exports._stringify = stringify;
exports._postMessage = postMessage;
exports._setCookie = setCookie;
exports._domRectFromRect = domRectFromRect;

exports._keyCode = function(e) {
  // https://www.w3schools.com/jsref/event_key_keycode.asp
  return e.which || e.keyCode;
}
