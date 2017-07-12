import React from 'react';
import './App.css';
import DotGraph from '../components/DotGraph';

const TreePage = () => (<div>
  <div className="App" >dots</div>
  <DotGraph width={window.innerWidth} height={window.innerHeight} />
</div>);

export default TreePage;
