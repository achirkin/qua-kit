# luci-connect

Luci-connect is a Haskell library for services and clients of [Luci middleware](https://bitbucket.org/treyerl/luci2).
Please, use `haddock` to generate and explore documentation.

To create a simple service look at `runReallySimpleLuciClient` or `runSimpleLuciClient` in `Luci.Connect` module.
These functions provide an interface to write Luci service performing
  with formatted logging, arbitrary `IO` actions, and its own state throughout execution.

## Executables

### luci-test

Luci-test is a command-line tool serving as a client or a server for sending Luci binary messages.
Run `stack install --flag luci-connect:luci-test` and then `luci-test` to see command-line options.
Use it to test another implementation of Luci protocol.

### adding-numbers-service

Adding-numbers-service is a simple example service that adds two numbers and returns result.
Run `stack install --flag luci-connect:examples` and then `adding-numbers-service` to register it in Luci.
This service is useful for checking if Luci is functional and also can be used as a service template.

## Luci reference

### Luci as a TCP server

Luci is primarily intended to connect machines in a LAN. {...}
Luci works as a TCP server, and all remote services and clients are TCP clients.
In the beginning, a service registers itself in Luci,
and then Luci sends run requests to the service from time to time.

### Luci JSON status

Luci's communication protocol is JSON based.
It's designed for asynchronous execution of remote procedures (services).
Unlike in RPC protocol, client requests don't include an ID.
IDs (callIDs) are globally created by Luci and sent back as the first answer to any request as `{'newCallID':1}`.
A client can ask for execution of a service by sending a request like `{'run': 'ServiceA'}`.
The first Luci answer to every request is `{'newCallID':x}`.
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

Refer to [`luci.core.ServiceWriterComplete#doNotification`]
(https://bitbucket.org/treyerl/luci2/src/master/core/src/main/java/luci/core/ServiceWriterComplete.java) to get a precise idea of Luci's answers.

Luci sends a progress answer whenever a service is being started with percentage being 0.
This is how a client is being notified of service execution start events.

#### JSON input/output parameter description
While services expect inputs to be json formatted (with binary attachments described below) its specification is described using an additional rules set on top of json as follows:

* JSON keys can have one of the **modifiers** *XOR*, *OPT*, *ANY*.
* * Keys preceeded by **XOR** allow only one of the marked keys to be given as an input.
* * Keys preceeded by **OPT** are optional keys.
* * Keys preceeded by **ANY** allow to use any name as a key. **ANY** can be used only once in a json object.

* Type specification: Input/Output specifications are allowed to consist of arbitrary levels of hierarchy (both nested objects as well as nested arrays). The leafs nevertheless must denote one of the types *json*, *list*, *string*, *number*, *boolean*, *attachment*, *jsongeometry*, *any* or a list with a selection of these types like [*attachment*, *jsongeometry*].

#### Attachments
Luci messages can contain binary data.
It is attached to messages as byte arrays.
Every message must contain a JSON "header", attachments are optional.
The UTF8 encoded JSON header is human-readable;
a complete luci message consists of a json string and a thin binary wrapper around the json header plus a binary attachements part.
See haddock for `Luci.Connect.Base` for detailed protocol description.

Usually binary attachments should be referenced in a header using a special attachment description.
Here is a convention for JSON format of such description:
```
#!javascript
{   'format':        string, // e.g. "binary" or "4-byte float array"
    'attachment': {
        'length':    number, // length of attachments in bytes
        'checksum':  string, // MD5 checksum
        'position':  integer (starting at 1; 0 = undefined position)
    }
    'OPT name':      string,
    'ANY key':       string
}
```

Attachments can be referenced multiple times in a JSON header.
Attachments - if they need to be forwarded to only one service - are forwarded directly to remote services,
i.e. Luci will not wait until the whole attachment is being transferred to Luci before sending it to a remote service.

#### JSONGeometry
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

#### Remote Services
Luci allows a client to be registered as a service that is promoted by Luci as a regular service, that by clients is indistinguishable from services loaded by Luci locally.
In order to support this the Luci network protocol reserves two additional keywords in JSON headers:

* **run**: {'run':'ServiceBRemote'}; sent by clients and eventually forwarded by Luci to the remote service.
* **cancel**: {'cancel':callID}; sent by clients and eventually forwarded by Luci to the remote service.

#### Common key/type names:

* `serviceName: string` - name of a Luci service.
* `callID: number (java long)` - id of a service execution given by Luci. 
* `run: serviceName` - run a service, its value is a string, service name.
* `cancel: callID` - cancel running service by its id.
* `result: object` - generic json object - successfull result of a service execution.
* `error: string` - erroneous result of a service execution **need to check whether it is always string**.
* `taskID: ?string ?number` - **not sure; how it relates to callID and to services?**
* `duration: number (java long)` - execution time, computed by Luci **what are the units?**.
