import React from 'react';
import './App.css';
import TreeGraph from '../components/TreeGraph';

const TreePage = () => (<div>
  <div className="App" >grid</div>
  <TreeGraph width={window.innerWidth} height={window.innerHeight} />
</div>);

export default TreePage;
