// window.addResizeListener to do something with canvas when it is resized
(function(){
  var attachEvent = document.attachEvent;
  var isIE = navigator.userAgent.match(/Trident/);
  var requestFrame = (function(){
    var raf = window.requestAnimationFrame || window.mozRequestAnimationFrame || window.webkitRequestAnimationFrame ||
        function(fn){ return window.setTimeout(fn, 20); };
    return function(fn){ return raf(fn); };
  })();
  
  var cancelFrame = (function(){
    var cancel = window.cancelAnimationFrame || window.mozCancelAnimationFrame || window.webkitCancelAnimationFrame ||
           window.clearTimeout;
    return function(id){ return cancel(id); };
  })();
  
  function resizeListener(e){
    var win = e.target || e.srcElement;
    if (win.__resizeRAF__) cancelFrame(win.__resizeRAF__);
    win.__resizeRAF__ = requestFrame(function(){
      var trigger = win.__resizeTrigger__;
      trigger.__resizeListeners__.forEach(function(fn){
        fn.call(trigger, e);
      });
    });
  }
  
  function objectLoad(e){
    this.contentDocument.defaultView.__resizeTrigger__ = this.__resizeElement__;
    this.contentDocument.defaultView.addEventListener('resize', resizeListener);
  }
  
  window['addResizeListener'] = function(element, fn){
    if (!element.__resizeListeners__) {
      element.__resizeListeners__ = [];
      if (attachEvent) {
        element.__resizeTrigger__ = element;
        element.attachEvent('onresize', resizeListener);
      }
      else {
        if (getComputedStyle(element).position == 'static') element.style.position = 'relative';
        var obj = element.__resizeTrigger__ = document.createElement('object'); 
        obj.setAttribute('style', 'display: block; position: absolute; top: 0; left: 0; height: 100%; width: 100%; overflow: hidden; pointer-events: none; z-index: -1;');
        obj.__resizeElement__ = element;
        obj.onload = objectLoad;
        obj.type = 'text/html';
        if (isIE) element.appendChild(obj);
        obj.data = 'about:blank';
        if (!isIE) element.appendChild(obj);
      }
    }
    element.__resizeListeners__.push(fn);
  };
  
  window['removeResizeListener'] = function(element, fn){
    element.__resizeListeners__.splice(element.__resizeListeners__.indexOf(fn), 1);
    if (!element.__resizeListeners__.length) {
      if (attachEvent) element.detachEvent('onresize', resizeListener);
      else {
        element.__resizeTrigger__.contentDocument.defaultView.removeEventListener('resize', resizeListener);
        element.__resizeTrigger__ = !element.removeChild(element.__resizeTrigger__);
      }
    }
  }
}());

// make sure that RequestAnimationFrame and CancelAnimationFrame work on all browsers
(function() {
    var lastTime = 0;
    var vendors = ['ms', 'moz', 'webkit', 'o'];
    for(var x = 0; x < vendors.length && !window['requestAnimationFrame']; ++x) {
        window['requestAnimationFrame'] = window[vendors[x]+'RequestAnimationFrame'];
        window['cancelAnimationFrame'] = window[vendors[x]+'CancelAnimationFrame']
                                   || window[vendors[x]+'CancelRequestAnimationFrame'];
    }
 
    if (!window['requestAnimationFrame'])
        window['requestAnimationFrame'] = function(callback, element) {
            var currTime = new Date().getTime();
            var timeToCall = Math.max(0, 16 - (currTime - lastTime));
            var id = window.setTimeout(function() { callback(currTime + timeToCall); },
              timeToCall);
            lastTime = currTime + timeToCall;
            return id;
        };
 
    if (!window['cancelAnimationFrame'])
        window['cancelAnimationFrame'] = function(id) {
            clearTimeout(id);
        };
}());


// Toggle fullscreen and change the fullscreen button shape
function toggleFullScreen() {
    if (!document['fullscreenElement'] && !document['mozFullScreenElement']
     && !document['webkitFullscreenElement'] && !document['msFullscreenElement'] && !document['fullScreen']) {
      if (document.documentElement['requestFullscreen']) {
        document.documentElement.requestFullscreen();
      } else if (document.documentElement['msRequestFullscreen']) {
        document.documentElement.msRequestFullscreen();
      } else if (document.documentElement['mozRequestFullScreen']) {
        document.documentElement.mozRequestFullScreen();
      } else if (document.documentElement['webkitRequestFullscreen']) {
        document.documentElement.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT);
      }
    } else {
      if (document['exitFullscreen']) {
        document.exitFullscreen();
      } else if (document['msExitFullscreen']) {
        document.msExitFullscreen();
      } else if (document['mozCancelFullScreen']) {
        document.mozCancelFullScreen();
      } else if (document['webkitExitFullscreen']) {
        document.webkitExitFullscreen();
      } else {
        document.cancelFullScreen();
        document.exitFullscreen();
      }
    }
}

// change fullscreen button
function checkfullscreen() {
    if (document['fullscreenElement'] || document['webkitFullscreenElement'] || document['mozFullScreenElement']) {
        document.getElementById('fullscreenbshape').setAttribute('d','M14,14H19V16H16V19H14V14M5,14H10V19H8V16H5V14M8,5H10V10H5V8H8V5M19,8V10H14V5H16V8H19Z');
    } else {
        document.getElementById('fullscreenbshape').setAttribute('d','M5,5H10V7H7V10H5V5M14,5H19V10H17V7H14V5M17,14H19V19H14V17H17V14M10,17V19H5V14H7V17H10Z');
    }
};
document.addEventListener('webkitfullscreenchange', function(e) {checkfullscreen();}, false);
document.addEventListener('mozfullscreenchange', function(e) {checkfullscreen();}, false);
document.addEventListener('msfullscreenchange', function(e) {checkfullscreen();}, false);
document.addEventListener('fullscreenchange', function(e) {checkfullscreen();}, false);

// Toggle GUI panel on the right side of window
function toggleGUIPanel(){
    var panel = document.getElementById('guipanel');
    if (panel.className == 'idleguipanel') {
        panel.className = 'activeguipanel';
        document.getElementById('guiplaceholder').className = 'activeplaceholder';
    } else {
        panel.className = 'idleguipanel';
        document.getElementById('guiplaceholder').className = 'idleplaceholder';
    }
}

