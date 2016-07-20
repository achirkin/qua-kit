/**
 * Created by Artem Chirkin
 */


var ReactiveBanana = (function () {
    'use strict';

    // helper function to enable MouseEvent.buttons
    function toButtons(btn){
        switch(btn) {
            case	0: return 1;
            case	1: return 4;
            case	2: return 2;
            case	3: return 8;
            case	4: return 16;
            case	5: return 32;
            default: return 0;
        }
    }

    // A pointer represents various kinds of pointer events.
    // eventType:
    //   0 - pointerUp;
    //   1 - pointerDown;
    //   2 - pointerMove;
    //   3 - pointerCancel;
    var PointerEvent = function(ev, t) {
        this.target = ev.target;
        this.pointerEventType = t;
        this.type = ev.type;
        this.button = ev.button ? ev.button : 0;
        this.buttons = ev.buttons != undefined ? ev.buttons : toButtons(this.button);
        this.altKey = ev.altKey ? ev.altKey : false;
        this.ctrlKey = ev.ctrlKey ? ev.ctrlKey : false;
        this.metaKey = ev.metaKey ? ev.metaKey : false;
        this.shiftKey = ev.shiftKey ? ev.shiftKey : false;
        var pk = this.target.pointerKeeper;
        if (ev['touches']) {
            this.pointers = Array.prototype.slice.call(ev['touches']).map(function(t) {return {x: (t.clientX - pk.clientX) * pk.clientScaleX, y: (t.clientY - pk.clientY) * pk.clientScaleY};});
        } else {
            this.pointers = ev.clientX ? [{x: (ev.clientX - pk.clientX) * pk.clientScaleX, y: (ev.clientY - pk.clientY) * pk.clientScaleY}] : [];
        }
    };


    var PointerKeeper = function PointerKeeper(el, pointerUp, pointerDown, pointerMove, pointerCancel) {
        this.target = el;
        el.pointerKeeper = this;
        this.updateLocation();
        var pk = this;
        var observer = new MutationObserver(function() {pk.updateLocation(); });
        observer.observe(el, { attributes : true });
        window.addEventListener('scroll',function() { pk.updateLocation.apply(pk, []) });
        window.addEventListener('resize',function() { pk.updateLocation.apply(pk, []) });

        el.addEventListener('contextmenu',function(ev){var e = window.event||ev; e.preventDefault();e.stopPropagation();return false;});
        el['style']['touch-action'] = "none";

        var pUp = this.convertEvent(pointerUp, 0);
        el.addEventListener('mouseup', pUp);
        el.addEventListener('touchend', pUp);
        var pDown = this.convertEvent(pointerDown, 1);
        el.addEventListener('mousedown', pDown);
        el.addEventListener('touchstart', pDown);
        var pMove = this.convertEvent(pointerMove, 2);
        el.addEventListener('mousemove', pMove);
        el.addEventListener('touchmove', pMove);
        var pCancel = this.convertEvent(pointerCancel, 3);
        el.addEventListener('mouseleave', pCancel);
        el.addEventListener('touchcancel', pCancel);

    };

    // Update position of an element so that it serves as a veiwport for pointer position;
    // computes available space within margin, border, and padding (so that it works with an html canvas size).
    PointerKeeper.prototype.updateLocation = function () {
        var elstyle = window.getComputedStyle(this.target, null),
            pleft = parseFloat(elstyle.getPropertyValue('padding-left')) + parseFloat(elstyle.getPropertyValue('border-left-width')),
            ptop = parseFloat(elstyle.getPropertyValue('padding-top')) + parseFloat(elstyle.getPropertyValue('border-top-width')),
            pbottom = parseFloat(elstyle.getPropertyValue('padding-bottom')) + parseFloat(elstyle.getPropertyValue('border-bottom-width')),
            pright = parseFloat(elstyle.getPropertyValue('padding-right')) + parseFloat(elstyle.getPropertyValue('border-right-width')),
            bbox = this.target.getBoundingClientRect(),
            iheight = bbox.height - ptop - pbottom,
            iwidth = bbox.width - pleft - pright;
        this.clientX = bbox.left + pleft;
        this.clientY = bbox.top + ptop;
        this.clientScaleX = this.target.width ? this.target.width / iwidth : 1;
        this.clientScaleY = this.target.height ? this.target.height / iheight : 1;
        //console.log(elstyle, pleft, ptop, pright, pbottom, bbox, iwidth, iheight, this.target.width, this.target.height, this.clientX, this.clientY, this.clientScaleX, this.clientScaleY);
    };


    PointerKeeper.prototype.convertEvent = function (f, pType) {
        return function (ev) {
            var e = window.event || ev;
            e.preventDefault();
            e.stopPropagation();
            f(new PointerEvent(e, pType));
            return false;
        };
    };


    return {
        PointerEvent: PointerEvent,
        PointerKeeper: PointerKeeper,
        listenToWheel: function(el, f) {
            el.addEventListener('wheel', function(ev){
                var e = window.event || ev;
                e.preventDefault();
                e.stopPropagation();
                f(e['wheelDelta'] > 0 || e['detail'] < 0 || e['deltaY'] < 0 ? (1.0) : (-1.0));
                return false;
            });
        },
        getPointers: function (event){
            return event.target.pointerKeeper
                ? event.target.pointerKeeper
                : (event.target.pointerKeeper = new PointerKeeper(event.target));
        }
    };
}());