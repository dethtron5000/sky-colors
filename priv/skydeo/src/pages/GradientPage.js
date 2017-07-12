import React from 'react';
import './App.css';
import GradientGraph from '../components/GradientGraph';

const GridPage = () => (<div>
  <div className="App" >grid</div>
  <GradientGraph width={window.innerWidth} height={window.innerHeight} />
</div>);

export default GridPage;
