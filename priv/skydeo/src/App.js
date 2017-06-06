import React from 'react';
import './App.css';
import CloudGraph from './components/CloudGraph';

const App = () => (<div>
  <div className="App" />
  <CloudGraph width={0.9 * window.innerWidth} height={0.9 * window.innerHeight} />
</div>);

export default App;
