import {
    MSG,
    NEWIMG,
} from '../actions/websocketActions';

const initialState = {
  loading: false,
  token: '',
  connected: false,
  img: [],
};

export default function mr(state = initialState, action) {
  switch (action.type) {
    case MSG:
      return Object.assign({}, state, { loading: action.message });
    case NEWIMG:
      return Object.assign({}, state, { img: action.message.message });
    default:
      return state;
  }
}
