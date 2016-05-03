/**
 * Created by Artem Chirkin
 */

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
        Either: Either
    };
}());