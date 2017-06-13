import React from 'react';
import './App.css';
import CloudGraph from '../components/CloudGraph';

const App = () => (<div>
  <div className="App" />
  <CloudGraph width={window.innerWidth} height={window.innerHeight} />
</div>);

export default App;
