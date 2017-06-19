import {
    MSG,
    NEWIMG,
} from '../actions/websocketActions';

const filler = {
  img: [],
  location: '',
};

const initialState = {
  loading: false,
  token: '',
  connected: false,
  locations: {
    cambridge: filler,
    newyork: filler,
    chicago: filler,
    sanfrancisco: filler,
    paloalto: filler,
    tokyo: filler,
    shanghai: filler,
    munich: filler,
    london: filler,
  },
};

export default function mr(state = initialState, action) {
  const newimg = state.locations;
  console.log(action);

  switch (action.type) {
    case MSG:
      return Object.assign({}, state, { loading: action.message });
    case NEWIMG:
      console.log(action.payload.location);
      console.log(Object.prototype.hasOwnProperty.call(newimg, action.payload.location));
      console.log(Object.prototype.hasOwnProperty.call(newimg, 'shanghai'));

      if (Object.prototype.hasOwnProperty.call(newimg, action.payload.location)) {
        newimg[action.payload.location] = action.payload;
        console.log(action.payload);
        return Object.assign({}, state, { locations: newimg });
      }

      return state;
    default:
      return state;
  }
}
