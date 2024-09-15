# Test Peer-to-Peer

> **Disclaimer**
>
> In the spirit of full disclosure and due to my utter laziness in coming up with the bulk of the boilerplate code, I turned to `ChatGPT`.\
> You can see the series of prompts and answers I've got in the [research](research) folder.

This is an attempt at a very basic peer class that can participate on a Peer-to-Peer( `P2P` ) network.

It will not have peer discovery via `UDP` broadcast, nor will it have any `DHT` features.

Just a simple message exchange protocol in a `P2P` context.

## Todo

- Hook up event for data received from connection thread to the internal peer method
- Add a property to allow hooking to data received from outside of the peer
- Hook up event for connection removal from connection thread to peer
- Understand if a connection needs to be disconnected when being removed