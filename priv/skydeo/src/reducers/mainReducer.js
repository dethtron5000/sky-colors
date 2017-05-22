import {
    MSG,
} from '../actions/websocketActions';

const initialState = {
  loading: false,
  token: '',
};

export default function mr(state = initialState, action) {
  switch (action.type) {
    case MSG:
      return Object.assign({}, state, { loading: action.message });

    default:
      return state;
  }
}
