import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import { createStore, applyMiddleware } from 'redux';
import { Provider } from 'react-redux';
import { Router, Route, hashHistory } from 'react-router';

import mr from './reducers/mainReducer';
import App from './App';
import socketMiddleware from './middleware/socketMiddleware';

import './index.css';

const store = createStore(mr, applyMiddleware(socketMiddleware));

ReactDOM.render((
  <Provider store={store}>
    <Router history={hashHistory}>
      <Route path="/" component={App} />
    </Router>
  </Provider>
), document.getElementById('root'));
