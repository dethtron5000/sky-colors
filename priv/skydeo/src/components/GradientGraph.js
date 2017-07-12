import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import * as d3 from 'd3';

import StateModel from './StateModel';
import './fonts.css';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const makeScale = (size, domain) => d3.scaleLinear()
          .range([0, size])
          .domain([0, domain]);

class GradientGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
    this.padding = 0;
    this.w = ((this.props.width));
    this.h = ((this.props.height - (9 * this.padding)) / 9);
  }

  componentDidMount() {
    this.svg = d3.select(this.target).append('svg');
    this.defs = this.svg.append('defs');
    this.gradientGraph = this.svg
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');




    const transformFunc = (d, i) => `translate(0,${i * (this.h + this.padding)})`;

    const objectData = Object.values(this.props.appState.locations);
    const objectKeys = Object.keys(this.props.appState.locations);

    const defsholder = this.defs.selectAll('linearGradient')
      .data(objectData)
      .enter();

    defsholder
      .append('linearGradient')
      .attr('id', (d, i) => objectKeys[i]);


    const graphholder = this.gradientGraph.selectAll('.stack')
      .data(objectData)
      .enter();

    const stack = graphholder
      .append('g')
        .attr('class', 'stack')
        .attr('transform', transformFunc)
        .attr('id', (d, i) => objectKeys[i]);

    stack.append('rect')
      .attr('class', 'linearholder')
      .attr('x', '14')
      .attr('width', this.w)
      .attr('height', this.h)
      .attr('fill', (d, i) => `url(#${objectKeys[i]})`);

    stack.append('g')
      .attr('class', 'textHolder');

    this.gradientGraph.selectAll('.textHolder')
      .data(objectData)
      .append('text')
        .attr('y', -5)
        .attr('x', 5)
        .attr('transform', 'rotate(90)')
        .attr('fill', '#990000')
        .text((d, i) => {
          const v = objectKeys;
          return StateModel.displayNames[v[i]];
        })
        .attr('class', 'location');

    // .attr('transform', `rotate(90)`);

    graphholder.exit().remove();

    graphholder.transition().duration(1000);
  }

  componentDidUpdate() {
    const extract = this.props.appState.locations[this.props.appState.lastLocation];
    const stopholder = this
      .defs
      .selectAll(`#${this.props.appState.lastLocation}`);
      // console.log(stopholder);

    // const d = arr.img;
    const sum = d3.sum(extract.img, j => j.count);
    const wscale = makeScale(100, sum);
    /* const xscale = d3.scaleLinear()
          .range([0, this.w])
          .domain([0, extract.img.length]); */

    let runningcount = 0;
    const scalefunc = (j, k) => {
      if (k === 0) {
        return '0%';
      }

      const out = wscale(extract.img[k - 1].count + runningcount);
      runningcount += extract.img[k - 1].count;
      return `${out}%`;
    };

    const st = stopholder
      .selectAll('stop')
        .data(extract.img);
    console.log(st);

    st
      .transition()
      .duration(45000)
      .attr('offset', scalefunc)
      .attr('stop-color', j => `#${j.hex}`);

    // .style('opacity', 1);

    st
      .exit()
      .transition()
      .duration(45000)
      .attr('stop-opacity', 0)
      .remove();

    st
      .enter()
      .append('stop')
      .attr('offset', scalefunc)
      .attr('stop-color', j => `#${j.hex}`)
      .attr('stop-opacity', 0)
      .transition()
      .duration(5000)
      .attr('stop-opacity', 1);
  }

  render() {
    return (<div id="graph" />);
  }

}

GradientGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: StateModel.appState.isRequired,

};

const GradientGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(GradientGraphInner);

export default GradientGraph;
