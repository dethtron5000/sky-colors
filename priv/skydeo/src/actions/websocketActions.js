
export const CONNECT = 'CONNECT';
export const DISCONNECT = 'DISCONNECT';

export const CONNECTED = 'CONNECTED';
export const DISCONNECTED = 'DISCONNECTED';

export const MSG = 'MESSAGE';

export const NEWIMG = 'newimage';

export function connected() {
  return {
    type: CONNECTED,
  };
}

export function disconnected() {
  return {
    type: DISCONNECTED,
  };
}

export function connect() {
  return {
    type: CONNECT,
  };
}

export function disconnect() {
  return {
    type: DISCONNECT,
  };
}

export function msg() {
  return {
    type: MSG,
    message: 'yep',
  };
}

export function imgEvt(payload) {
  console.log(payload);
  return {
    type: NEWIMG,
    payload: payload.message,
  };
}
