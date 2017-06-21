import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import * as d3 from 'd3';

import StateModel from './StateModel';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const makeScale = (size, domain) => d3.scaleLinear()
          .range([0, size])
          .domain([0, domain]);

const group = function group(height, w) {
  return function z(arr) {
    const d = arr.img;
    const len = 16; // d.length;
    const max = d3.max(d, j => j.count);
    const xscale = makeScale(w, max);

    // let runningcount = 0;
    /* const scalefunc = (j, k) => {
      if (k === 0) {
        return 0;
      }

      const out = xscale(d[k - 1].count + runningcount);
      runningcount += d[k - 1].count;
      return out;
    };*/

    const v = d3.select(this)
      .selectAll('rect')
        .data(d);

    v
      .transition()
      .duration(5000)
      .attr('fill', j => `#${j.hex}`)
      .attr('width', j => xscale(j.count))
      .attr('height', height / len)
      .attr('y', (j, k) => k * (height / len))
      .attr('x', j => 0.5 * (w - xscale(j.count)));

    v
      .exit()
      .transition()
      .duration(5000)
      .style('opacity', 0)
      .remove();
    v
      .enter()
        .append('rect')
        .attr('fill', j => `#${j.hex}`)
        .attr('width', j => xscale(j.count))
        .attr('height', height / len)
        .style('opacity', 0)
        .attr('x', j => 0.5 * (w - xscale(j.count)))
        .transition()
        .duration(5000)
        .style('opacity', 1)
        .attr('y', (j, k) => k * (height / len))
;
  };
};

class TreeGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
    this.padding = 3;
    this.w = ((this.props.width - (3 * this.padding)) / 3);
    this.h = ((this.props.height - (3 * this.padding)) / 3);
  }

  componentDidMount() {
    this.bargraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');

    const graphholder = this.bargraph.selectAll('.stack')
      .data(Object.values(this.props.appState.locations))
      .enter().append('g')
        .attr('class', 'stack')
        .attr('transform', (d, i) => `translate(${(i % 3) * (this.w + this.padding)},${Math.floor(i / 3) * (this.h + this.padding)})`);

    graphholder.exit().remove();

    graphholder.transition().duration(1000);
  }

  componentDidUpdate() {
    const graphholder = this.bargraph.selectAll('.stack')
      .data(Object.values(this.props.appState.locations));

    graphholder
      .enter()
        .append('g')
        .each(group(this.h, this.w))
        .attr('class', 'stack');

    graphholder.exit().remove();

    graphholder
      .each(group(this.h, this.w));
  }

  render() {
    return (<div id="graph" />);
  }
}

TreeGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: StateModel.appState.isRequired,
};

const TreeGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(TreeGraphInner);

export default TreeGraph;

