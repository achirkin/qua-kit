#include <ghcjs/rts.h>

/*
   The same as h$runSyncAction, but does not throw errors and bypasses the scheduler
 */
function h$runSyncActionUnsafe(t, a, cont) {
  "use strict";
  h$runInitStatic();
  var c = h$return;
  t.stack[2] = h$ghcjszmprimZCGHCJSziPrimziInternalzisetCurrentThreadResultException;
  t.stack[4] = h$ap_1_0;
  t.stack[5] = a;
  t.stack[6] = h$return;
  t.sp = 6;
  t.status = THREAD_RUNNING;
#ifdef GHCJS_PROF
  // fixme this looks wrong
  // t.ccs = h$currentThread.ccs; // TODO: not sure about this
#endif
  t.isSynchronous = true;
  t.continueAsync = cont;
  var ct = h$currentThread;
  var csp = h$sp;
  var cr1 = h$r1; // do we need to save more than this?
  var caught = false, excep = null;
  h$currentThread = t;
  h$stack = t.stack;
  h$sp = t.sp;
#ifdef GHCJS_PROF
  h$reportCurrentCcs();
#endif
  // 
  while(c !== h$reschedule){c = c();}
  if(ct !== null) {
    h$currentThread = ct;
    h$stack = ct.stack;
    h$sp = csp;
    h$r1 = cr1;
  } else {
    h$currentThread = null;
    h$stack = null;
  }
#ifdef GHCJS_PROF
  // fixme?
  h$reportCurrentCcs();
#endif
  if(t.status !== THREAD_FINISHED && !cont) {
    h$removeThreadBlock(t);
    h$finishThread(t);
  }
}

/*
   The same as h$runSyncReturn, but is not interruptable at all.
   h$runSyncActionUnsafe inside does not throw errors and bypasses the scheduler
   returns: the result of the IO action
 */
function h$runSyncReturnUnsafe(a, cont) {
  "use strict";
  var t = new h$Thread();
  TRACE_SCHEDULER("h$runSyncReturnUnsafe created thread: " + h$threadString(t));
  var aa = MK_AP1(h$ghcjszmprimZCGHCJSziPrimziInternalzisetCurrentThreadResultValue, a);
  h$runSyncActionUnsafe(t, aa, cont);
  if(t.status === THREAD_FINISHED) {
    if(t.resultIsException) {
      throw t.result;
    } else {
      return t.result;
    }
  } else if(t.status === THREAD_BLOCKED) {
    throw new h$WouldBlock();
  } else {
    throw new Error("h$runSyncReturnUnsafe: Unexpected thread status: " + t.status);
  }
}

// Return functions if they are defined, otherwise return "undefined"

function h$isDefined(a) {
    return a !== undefined && a !== null;
}
function h$retIfDef(f) {
    return function(a,i){return (a !== undefined && a !== null) ? f(a,i) : undefined;};
}
function h$doIfDef(f) {
    return function(a,i){if(a !== undefined && a !== null){f(a,i);}};
}
function h$retIfDef2(f) {
    return function(a,b,i){return (a !== undefined && a !== null && b !== undefined && b !== null) ? f(a,b,i) : undefined;};
}
function h$doIfDef2(f) {
    return function(a,b,i){if(a !== undefined && a !== null && b !== undefined && b !== null){f(a,b,i);}};
}
function h$retIfDef2o(f) {
    return function(a,b,i){return (b !== undefined && b !== null) ? f(a,b,i) : undefined;};
}
function h$retIfDef2oa(f) {
    return function(a,b,i){return (b !== undefined && b !== null) ? f(a,b,i) : a;};
}
function h$doIfDef2o(f) {
    return function(a,b,i){if(b !== undefined && b !== null){f(a,b,i);}};
}

