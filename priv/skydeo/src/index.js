import React from 'react';
import ReactDOM from 'react-dom';
import { createStore, applyMiddleware } from 'redux';
import { Provider } from 'react-redux';
import { Route, HashRouter } from 'react-router-dom';
import thunkMiddleware from 'redux-thunk';

import { connect } from './actions/websocketActions';
import mr from './reducers/mainReducer';
import App from './pages/App';
import GridPage from './pages/GridPage';
import TreePage from './pages/TreePage';
import DotPage from './pages/DotPage';
import LinearPage from './pages/LinearPage';
import GradientPage from './pages/GradientPage';

import socketMiddleware from './middleware/socketMiddleware';

import './index.css';

const store = createStore(mr, applyMiddleware(thunkMiddleware, socketMiddleware));
store.dispatch(connect());

ReactDOM.render((
  <Provider store={store}>
    <HashRouter>
      <div>
        <Route exact path="/" component={App} />
        <Route path="/grid" component={GridPage} />
        <Route path="/tree" component={TreePage} />
        <Route path="/dots" component={DotPage} />
        <Route path="/linear" component={LinearPage} />
        <Route path="/gradient" component={GradientPage} />


      </div>
    </HashRouter>
  </Provider>
), document.getElementById('root'));
