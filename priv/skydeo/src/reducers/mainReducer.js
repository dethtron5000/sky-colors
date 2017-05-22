import {
    REQUEST_LOGIN_TOKEN,
    RECEIVED_LOGIN_TOKEN
} from '../actions/authActions';

const initialState = {
    loading: false,
    token: '',
  };

function auth(state = initialState, action) {
  switch (action.type) {
      case REQUEST_LOGIN_TOKEN:
        return Object.assign({}, state, { loading: true });
      case RECEIVED_LOGIN_TOKEN:
        return Object.assign({}, state, { token: action.token });
      default:
        return state;
    }
}

export auth;
