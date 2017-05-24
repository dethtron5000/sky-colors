import React from 'react';
import ReactDOM from 'react-dom';
import { createStore, applyMiddleware } from 'redux';
import { Provider } from 'react-redux';
import { Route, HashRouter } from 'react-router-dom';

import { connect } from './actions/websocketActions';
import mr from './reducers/mainReducer';
import App from './App';
import socketMiddleware from './middleware/socketMiddleware';

import './index.css';

const store = createStore(mr, applyMiddleware(socketMiddleware));
store.dispatch(connect());

ReactDOM.render((
  <Provider store={store}>
    <HashRouter>
      <Route path="/" component={App} />
    </HashRouter>
  </Provider>
), document.getElementById('root'));
