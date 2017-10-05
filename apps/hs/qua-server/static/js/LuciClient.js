/**
 * Created by achirkin on 08/06/16.
 */

// WebSocket Luci client operates the data exactly like the TCP version of LuciProtocol, but via websockets binary data.
// Message size is limited to uint32 (~4GB), which from my point of view is far more than enough for JavaScript data transfer
// and for Luci data transfers in general too.
var Luci = (function () {
    'use strict';

    // Marshals a string to Uint8Array.
    function encodeUTF8(s) {
        var i = 0;
        var bytes = new Uint8Array(s.length * 4);
        for (var ci = 0; ci != s.length; ci++) {
            var c = s.charCodeAt(ci);
            if (c < 128) {
                bytes[i++] = c;
                continue;
            }
            if (c < 2048) {
                bytes[i++] = c >> 6 | 192;
            } else {
                if (c > 0xd7ff && c < 0xdc00) {
                    if (++ci == s.length) throw 'UTF-8 encode: incomplete surrogate pair';
                    var c2 = s.charCodeAt(ci);
                    if (c2 < 0xdc00 || c2 > 0xdfff) throw 'UTF-8 encode: second char code 0x' + c2.toString(16) + ' at index ' + ci + ' in surrogate pair out of range';
                    c = 0x10000 + ((c & 0x03ff) << 10) + (c2 & 0x03ff);
                    bytes[i++] = c >> 18 | 240;
                    bytes[i++] = c>> 12 & 63 | 128;
                } else { // c <= 0xffff
                    bytes[i++] = c >> 12 | 224;
                }
                bytes[i++] = c >> 6 & 63 | 128;
            }
            bytes[i++] = c & 63 | 128;
        }
        return bytes.slice(0, i);
    }

    // Unmarshals an Uint8Array to string.
    function decodeUTF8(bytes) {
        var s = '';
        var i = 0;
        while (i < bytes.length) {
            var c = bytes[i++];
            if (c > 127) {
                if (c > 191 && c < 224) {
                    if (i >= bytes.length) throw 'UTF-8 decode: incomplete 2-byte sequence';
                    c = (c & 31) << 6 | bytes[i] & 63;
                } else if (c > 223 && c < 240) {
                    if (i + 1 >= bytes.length) throw 'UTF-8 decode: incomplete 3-byte sequence';
                    c = (c & 15) << 12 | (bytes[i] & 63) << 6 | bytes[++i] & 63;
                } else if (c > 239 && c < 248) {
                    if (i+2 >= bytes.length) throw 'UTF-8 decode: incomplete 4-byte sequence';
                    c = (c & 7) << 18 | (bytes[i] & 63) << 12 | (bytes[++i] & 63) << 6 | bytes[++i] & 63;
                } else throw 'UTF-8 decode: unknown multibyte start 0x' + c.toString(16) + ' at index ' + (i - 1);
                ++i;
            }
            if (c <= 0xffff) s += String.fromCharCode(c);
            else if (c <= 0x10ffff) {
                c -= 0x10000;
                s += String.fromCharCode(c >> 10 | 0xd800);
                s += String.fromCharCode(c & 0x3FF | 0xdc00);
            } else throw 'UTF-8 decode: code point 0x' + c.toString(16) + ' exceeds UTF-16 reach';
        }
        return s;
    }

    var LuciClient = (function () {
        var socket, onmessage, callIdSeq = 777;

        LuciClient = function LuciClient(connString, onMsg, onOpen, onClose, onError) {
            this.connectionString = connString;
            socket = new WebSocket(this.connectionString);
            socket.binaryType = "arraybuffer";
            socket.onopen = onOpen;
            socket.onclose = onClose;
            socket.onerror = function(e){onError(e['message'] ? e['message'] : 'WebSocket onerror event occured.')};
            onmessage = onMsg;


            var inBuffer = new ArrayBuffer(32), curRead = 0, headSize = 0, attSize = 0, startedReading = false;
            var processMessage = function(msg) {
                if (!startedReading) {
                    var dv = new DataView(inBuffer);
                    if (curRead >= 16) {
                        headSize = dv.getUint32(4,false);
                        attSize = dv.getUint32(12,false);
                        if (inBuffer.byteLength < headSize + attSize + 16) {
                            var t = new ArrayBuffer(headSize + attSize + 16);
                            new Uint8Array(t).set(new Uint8Array(inBuffer, 0, curRead));
                            inBuffer = t;
                        }
                        startedReading = true;
                    }
                } // at this moment either [startedReading == false] or [headSize, attSize, and inBuffer are set]

                var toread = Math.min(inBuffer.byteLength, headSize + attSize + 16)  - curRead,
                    canread = Math.min(toread, msg.data.byteLength),
                    remains = msg.data.byteLength - canread;
                // copy whole data in input buffer if possible
                if (canread > 0) {
                    (new Uint8Array(inBuffer, curRead)).set(new Uint8Array(msg.data, 0, canread));
                    curRead += canread;
                }

                if (startedReading && curRead === headSize + attSize + 16) { // the whole buffer is available, can return msg
                    var msghead = decodeUTF8(new Uint8Array(inBuffer, 16, headSize)), n = 0, i, atts, cpos, csize;
                    if (attSize >= 8) {
                        n = (new DataView(inBuffer, 16 + headSize)).getUint32(4,false);
                    }
                    atts = new Array(n);
                    cpos = 24 + headSize;
                    for (i = 0; i < n; i++) {
                        csize = (new DataView(inBuffer, cpos)).getUint32(4,false);
                        cpos += 8;
                        atts[i] = (new Uint8Array(inBuffer, cpos, csize)).slice().buffer;
                        cpos += csize;
                    }
                    // reset state and return result (pass it in continuation "onmessage")
                    startedReading = false;
                    headSize = 0;
                    attSize = 0;
                    curRead = 0;
                    onmessage(msghead, atts);
                }

                // proceed with remaining data
                if (remains > 0) {
                    processMessage( {'data': (new Uint8Array(msg.data, canread, remains)).slice().buffer });
                }
            };
            socket.onmessage = processMessage;

        };
        LuciClient.prototype.objectName = 'LuciClient';

        /*
         Generate a new callId to create a run message
         */
        LuciClient.prototype.genCallId = function () {
          return callIdSeq++;
        }

        /*
         Gets header string and array of ArrayBuffers as attachments.
         Sends data via websocket in Luci protocol format.
         */
        LuciClient.prototype.sendMessage =  function (header) {
            var attachments = arguments.length > 1 ? arguments[1] : [],
                bhead = encodeUTF8(header),
                bx = new ArrayBuffer(8),
                bxv = new DataView(bx),
                lengths = attachments.map(function(a){return a.byteLength;});
            // just in case: initialize first four bytes of binary buffer with zeroes.
            bxv.setUint32(0, 0, false);
            // send the header size
            bxv.setUint32(4, bhead.length, false);
            socket.send(bx);
            // send attachments size
            bxv.setUint32(4, lengths.reduce(function(a,b){return a+b;}, (attachments.length + 1) * 8 ), false);
            socket.send(bx);
            // send UTF-8 encoded JSON header
            socket.send(bhead);
            // send one 8-byte value indicating number of attachments
            bxv.setUint32(4, attachments.length, false);
            socket.send(bx);
            // send all attachments
            attachments.forEach(function(att,i){
                bxv.setUint32(4, lengths[i], false);
                socket.send(bx);
                socket.send(att);
            });
        };

        return LuciClient;
    }());

    // these are the functions we expose in the namespace Luci
    return {
        Client: LuciClient,
        decodeUTF8: decodeUTF8
    };
}());
