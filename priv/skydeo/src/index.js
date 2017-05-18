import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import { createStore, applyMiddleware } from 'redux';
import { Provider } from 'react-redux';

import './index.css';

ReactDOM.render(
  <App />,
  document.getElementById('root')
);
