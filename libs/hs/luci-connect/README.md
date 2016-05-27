## TCP / Websockets
Luci is primarily intended to connect machines in a LAN. {...}

##JSON
Luci's communication protocol is JSON based.
It's designed for asynchronous execution of remote procedures (services).
Unlike in RPC protocol, client requests don't include an ID.
IDs (callIDs) are globally created by Luci and sent back as the first answer to any request as {'newCallID':1}.
A client can ask for execution of a service by sending a request like {'run': 'ServiceA'}.
The first answer to every request is {'newCallID':x}.
The second answer is one or many progress notifications that refer to the callID.
It might indicate the progress with a number (integer) between 0 and 100 and contain intermediate results.
The last answer is the final result also referring to callID.
Error messages can be returned any time after the newCallID answer.
Error messages are not followed by any subsequent answers.

Requests:

* **run**: {'run': 'ServiceA'}
* **cancel**: {'cancel': callID (integer)}

Answers:

* **Result**: {'result':{},'callID':integer,'serviceName':string,'taskID':integer (0 if the service has been run not as a task but has been called "directly")}
* **Progress**: {'progress':{<intermediate result>},'callID':integer, 'serviceName':string,'taskID':integer,'percentage':integer}
* **Error**: {'error':string}

Refer to luci.core.ServiceWriterComplete#doNotifications to get a precise idea of Luci's answers.

Luci sends a progress answer whenever a service is being started with percentage being 0.
This is how a client is being notified of service execution start events.

###JSON input/output parameter description
While services expect inputs to be json formatted (with binary attachments described below) its specification is described using an additional rules set on top of json as follows:

* JSON keys can have one of the **modifiers** *XOR*, *OPT*, *ANY*.
* * Keys preceeded by **XOR** allow only one of the marked keys to be given as an input.
* * Keys preceeded by **OPT** are optional keys.
* * Keys preceeded by **ANY** allow to use any name as a key. **ANY** can be used only once in a json object.

* Type specification: Input/Output specifications are allowed to consist of arbitrary levels of hierarchy (both nested objects as well as nested arrays). The leafs nevertheless must denote one of the types *json*, *list*, *string*, *number*, *boolean*, *attachment*, *jsongeometry*, *any* or a list with a selection of these types like [*attachment*, *jsongeometry*].

##Attachments (TCP)
Luci messages can contain binary data.
It is attached to messages as byte arrays.
Every message must contain a JSON "header", attachments are optional.
The UTF8 encoded JSON header is human-readable;
a complete luci message consists of a json string and a thin binary wrapper around the json header:
16 big endian bytes before the json header at the very beginning of every Luci message.

![luci_message.png](https://bitbucket.org/repo/M9EBx5/images/430173227-luci_message.png)

The first 8 bytes encode the byte length of the header, the second 8 bytes encode the length of the rest, the attachment part.
The attachment part itself starts with 8 bytes encoding the amount of attachments and every attachment byte array is preceded by additional 8 bytes encoding the length of the following attachment.
Attachments must be referenced in the json header by a json object that is structured as follows:

```
#!javascript
{   'format':        string,
    'attachment':{
        'length':    number,
        'checksum':  string,
        'position':  integer (starting at 1; 0 = undefined position)
    }
    'OPT name':      string,
    'ANY key':       string
}
```

Attachments can be referenced multiple times in a JSON header.
Attachments - if they need to be forwarded to only one service - are forwarded directly to remote services,
i.e. LUCI will not wait until the whole attachment is being transferred to LUCI before sending it to a remote service.

##JSONGeometry
Similar to attachments a JSON header can also contain JSON encoded geometry like GeoJSON. Since there are several JSON based geometry formats (e.g. TopoJSON) a JSONGeometry object must follow this structure:

```
#!javascript
{   'format'   :     string,
    'geometry' :     {<format-specific json object>},
    'OPT name' :     string,
    'OPT crs'  :     string,
    'OPT attributeMap': json,
}
```

##Remote Services
Luci allows a client to be registered as a service that is promoted by Luci as a regular service, that by clients is indistinguishable from services loaded by Luci locally.
In order to support this the Luci network protocol reserves two additional keywords in JSON headers:

* **run**: {'run':'ServiceBRemote'}; sent by clients and eventually forwarded by Luci to the remote service.
* **cancel**: {'cancel':callID}; sent by clients and eventually forwarded by Luci to the remote service.
