import React from 'react';
import './App.css';
import LinearGraph from '../components/LinearGraph';

const LinearPage = () => (<div>
  <div className="App" >grid</div>
  <LinearGraph width={window.innerWidth} height={window.innerHeight} />
</div>);

export default LinearPage;
