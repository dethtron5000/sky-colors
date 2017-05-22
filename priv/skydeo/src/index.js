import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import { createStore, applyMiddleware } from 'redux';
import { Provider } from 'react-redux';

import './index.css';
import { Router, Route, hashHistory } from 'react-router';

const store = createStore(reducer, applyMiddleware(socketMiddleware));

ReactDOM.render(
    <Provider store={store}>
       <Router history={hashHistory}>
        <Route path="/" component={App}/>
      </Router>
    </Provider>,
  document.getElementById('root')
);
