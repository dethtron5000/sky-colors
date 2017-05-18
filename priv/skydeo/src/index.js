import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import { createStore, applyMiddleware } from 'redux';
import { Provider } from 'react-redux';

import './index.css';
import { Router, Route, hashHistory } from 'react-router';

export default class App extends Component {
    componentDidMount() {
      AppState.addEventListener('change', (currentAppState) => {
        if (currentAppState === 'active') {
          // clear answer ids on open
          store.dispatch(fetchSurveyState());
          store.dispatch(connect());
        }
      });
    }

    render() {
      return (
          <Provider store={store}>
                              <Router>
                                      <Scene key='stateManager' initial={true} component={StateManagerContainer} hideNavBar/>
                                      <Scene key='main' component={MainContainer} title='Pulse' hideNavBar/>
                                      <Scene key='login' component={LoginContainer} title='Pulse' hideNavBar/>
                                      <Scene key='survey' component={SurveyContainer} title='Pulse' hideNavBar/>
                                      <Scene key='comments' component={CommentContainer} title='Pulse' hideNavBar/>
                                      <Scene key='commentList' component={CommentListContainer} title='Pulse' hideNavBar/>
                                      <Scene key='waiting' component={WaitingContainer} title='Pulse' hideNavBar/>
                                      <Scene key='hotTopics' component={HotTopicContainer} title='Pulse' hideNavBar/>
                              </Router>
                      </Provider>
      );
    }
}
