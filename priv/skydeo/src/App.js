import React, { Component } from 'react';
import { connect } from 'react-redux';
import logo from './logo.svg';
import './App.css';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const AppInner = (props, context) => {
  console.log(context, props);
  return (
    <div className="App">
      <div className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <h2>Welcome to React</h2>
      </div>
      <p className="App-intro">
        This State: {props.appState.loading}
        To get started, edit <code>src/App.js</code> and save to reload.
      </p>
    </div>
  );
};

const App = connect(
    mapStateToProps,
    mapDispatchToProps,
)(AppInner);

export default App;
