import { surveyEvent,
  topicEvent,
  DISCONNECT,
  CONNECT,
  connected,
  disconnected } from '../actions/websocketActions';

const url = 'ws://pulse.ideo.com/pulse_events';

// const url = 'ws://localhost:8000/pulse_events'
const socketMiddleware = (() => {
  let socket = null;

  const onOpen = (ws, store) => (evt) => {
    // Send a handshake, or authenticate with remote end
    console.log(evt);

    // Tell the store we're connected
    store.dispatch(connected());
  };

  const onClose = (ws, store) => (evt) => {
    // Tell the store we've disconnected
    store.dispatch(disconnected());
  };

  const onMessage = (ws, store) => evt => {
    //Parse the JSON message received on the websocket
    var msg = JSON.parse(evt.data);
    console.log('received message ');
    console.log(msg);
    switch (msg.type) {
      case 'POLL':

        //Dispatch an action that adds the received message to our state
        store.dispatch(surveyEvent(msg));
        break;
      case 'TOPIC':

        //Dispatch an action that adds the received message to our state
        store.dispatch(topicEvent(msg));
        break;
      default:
        console.log('Received unknown message type: \'' + msg.type + '\'');
        break;
    }
  };

  return store => next => action => {
    switch (action.type) {

      //The user wants us to connect
      case CONNECT:

        //Start a new connection to the server
        if (socket != null) {
          socket.close();
        }

        //Send an action that shows a "connecting..." status for now
        //store.dispatch(actions.connecting());

        //Attempt to connect (we could send a 'failed' action on error)
        socket = new WebSocket(url);
        socket.onmessage = onMessage(socket, store);
        socket.onclose = onClose(socket, store);
        socket.onopen = onOpen(socket, store, action.token);

        break;

      //The user wants us to disconnect
      case DISCONNECT:
        if (socket !== null) {
          socket.close();
        }

        socket = null;

        //Set our state to disconnected
        store.dispatch(actions.disconnected());
        break;

      //This action is irrelevant to us, pass it on to the next middleware
      default:
        return next(action);
    }
  };

})();

export default socketMiddleware;