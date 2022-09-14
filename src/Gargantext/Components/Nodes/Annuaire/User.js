"use strict";

exports.handleRef = handleRef;

function handleRef(ref) {
    return ref.current.files[0]
};