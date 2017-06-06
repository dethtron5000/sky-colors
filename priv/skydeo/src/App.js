import React from 'react';
import logo from './logo.svg';
import './App.css';
import CloudGraph from './components/CloudGraph';

const App = () => (<div>
  <div className="App">
  </div>
  <CloudGraph width={window.innerWidth} height={0.75 * window.innerHeight} />
</div>);

export default App;
