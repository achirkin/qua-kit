/**
 * Created by Artem Chirkin
 */
#include <ghcjs/rts.h>

/*global HS_NIL, IS_CONS, MK_CONS, CONS_HEAD, CONS_TAIL, JSVAL_VAL, MK_JSVAL*/

/** @define {boolean} */
var DEBUG = true;

var LikeHS = (function () {
    'use strict';

    var Either = (function () {
        // return object Either, default - Right, if isLeft === true then Left
        Either = function (value, isLeft) {
            if (isLeft) {
                this.left = value;
            } else {
                this.right = value;
            }
        };

        Either.prototype.isRight = function () {
            return this.hasOwnProperty('right');
        };

        return Either;
    }());

    return {
        Either: Either,
        /* Convert Haskell List to JS Array, with unwrapping each value from a JSVal constructor */
        listToArray: function (xs) {
            var arr = [];
            while (IS_CONS(xs)) {
                arr.push(JSVAL_VAL(CONS_HEAD(xs)));
                xs = CONS_TAIL(xs);
            }
            return arr;
        },
        /* Convert Haskell List to JS Array, without unwrapping each value from a JSVal constructor (for really primitive types, like bool, Int32, Double, etc.) */
        listToArrayNoUnwrap: function (xs) {
            var arr = [];
            while (IS_CONS(xs)) {
                arr.push(CONS_HEAD(xs));
                xs = CONS_TAIL(xs);
            }
            return arr;
        },
        /* Convert JS Array to Haskell List, with wrapping each value into JSVal constructor */
        listFromArray: function (a) {
            var i, r = HS_NIL;
            /*jslint plusplus: true */
            for (i = a.length - 1; i >= 0; i--) { r = MK_CONS(MK_JSVAL(a[i]), r); }
            return r;
        },
        /* Convert JS Array to Haskell List, without wrapping each value into JSVal constructor */
        listFromArrayNoWrap: function (a) {
            var i, r = HS_NIL;
            /*jslint plusplus: true */
            for (i = a.length - 1; i >= 0; i--) { r = MK_CONS(a[i], r); }
            return r;
        }
    };
}());