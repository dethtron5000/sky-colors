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
  const v = state.img.slice();
  switch (action.type) {
    case MSG:
      return Object.assign({}, state, { loading: action.message });
    case NEWIMG:
      v.unshift(action.message.message);
      return Object.assign({}, state, { img: v });
    default:
      return state;
  }
}
