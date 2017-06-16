import {
    MSG,
    NEWIMG,
} from '../actions/websocketActions';

const initialState = {
  loading: false,
  token: '',
  connected: false,
  img: {
    cambridge: [],
    newyork: [],
    chicago: [],
    sanfrancisco: [],
    paloalto: [],
    tokyo: [],
    shanghai: [],
    munich: [],
    london: [],
  },
};

export default function mr(state = initialState, action) {
  const newimg = state.img;
  switch (action.type) {
    case MSG:
      return Object.assign({}, state, { loading: action.message });
    case NEWIMG:
      if (Object.prototype.hasOwnProperty.call(newimg, action.message.location)) {
        newimg[action.message.location] = action.message;
        return Object.assign({}, state, { img: newimg });
      }

      return state;
    default:
      return state;
  }
}
