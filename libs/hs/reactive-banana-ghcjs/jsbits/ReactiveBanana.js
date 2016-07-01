/**
 * Created by Artem Chirkin
 */


var ReactiveBanana = (function () {
    'use strict';

    // a pointer represents various kinds of pointer events
    var Pointer = function Pointer() {

    };
    Pointer.prototype.checkSomething = function () {
        return this.hasOwnProperty('something');
    };

    return {
      Pointer: Pointer
    };
}());
