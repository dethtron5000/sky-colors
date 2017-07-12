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

class GridGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
    this.padding = 0;
    this.w = ((this.props.width - (3 * this.padding)) / 3);
    this.h = ((this.props.height - (3 * this.padding)) / 3);
  }

  componentDidMount() {
    this.bargraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');

    const transformFunc = (d, i) => `translate(${(i % 3) * (this.w + this.padding)},${Math.floor(i / 3) * (this.h + this.padding)})`;

    const objectData = Object.values(this.props.appState.locations);
    const objectKeys = Object.keys(this.props.appState.locations);

    const graphholder = this.bargraph.selectAll('.stack')
      .data(objectData)
      .enter();

    const stack = graphholder
      .append('g')
        .attr('class', 'stack')
        .attr('transform', transformFunc)
        .attr('id', (d, i) => objectKeys[i]);

    stack.append('g')
      .attr('class', 'barholder');

    stack.append('g')
      .attr('class', 'textHolder');

    this.bargraph.selectAll('.textHolder')
      .data(objectData)
      .append('text')
        .attr('y', (this.h - 5))
        .attr('x', 5)
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
    const graphholder = this
      .bargraph
      .selectAll(`#${this.props.appState.lastLocation} .barholder`);

    // const d = arr.img;
    const sum = d3.sum(extract.img, j => j.count);
    const yscale = makeScale(this.h, sum);
    let runningcount = 0;
    const scalefunc = (j, k) => {
      if (k === 0) {
        return 0;
      }

      const out = yscale(extract.img[k - 1].count + runningcount);
      runningcount += extract.img[k - 1].count;
      return out;
    };

    const v = graphholder
      .selectAll('rect')
        .data(extract.img);

    v
      .transition()
      .duration(10000)
      .attr('fill', j => `#${j.hex}`)
      .attr('height', j => yscale(j.count))
      .attr('y', scalefunc)
      .attr('width', this.w);

    v
      .exit()
      .transition()
      .duration(5000)
      .style('opacity', 0)
      .attr('width', 3 * this.w)
      // .attr('transform', `translate(${this.w})`)
      .remove();
    v
      .enter()
        .append('rect')
        .attr('fill', j => `#${j.hex}`)
        .attr('height', j => yscale(j.count))
        .attr('width', 0)
        .attr('y', scalefunc)
        .style('opacity', 0)
        .transition()
        .duration(10000)
        .attr('width', this.w)
        .style('opacity', 1);
  }

  render() {
    return (<div id="graph" />);
  }
}

GridGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: StateModel.appState.isRequired,

};

const GridGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(GridGraphInner);

export default GridGraph;

