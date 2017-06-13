import React from 'react';
import './App.css';
import GridGraph from '../components/GridGraph';

const GridPage = () => (<div>
  <div className="App" >grid</div>
  <GridGraph width={window.innerWidth} height={window.innerHeight} />
</div>);

export default GridPage;
