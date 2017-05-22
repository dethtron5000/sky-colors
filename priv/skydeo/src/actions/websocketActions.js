import { route } from '../actions/routeActions';
import { BASE_URL, SCENES } from '../actions/constants';

import _ from 'lodash';

export const CONNECT = 'CONNECT';
export const DISCONNECT = 'DISCONNECT';

export const CONNECTED = 'CONNECTED';
export const DISCONNECTED = 'DISCONNECTED';

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

