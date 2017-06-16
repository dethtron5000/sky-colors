import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import StateModel from './StateModel';

import * as d3 from 'd3';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const makeScale = (size, domain) => d3.scaleLinear()
          .range([0, size])
          .domain([0, domain]);

const group = function group(height, w) {
  return function z(d) {
    const sum = d3.sum(d, j => j.count);
    const xscale = makeScale(w, sum);
    let runningcount = 0;
    const scalefunc = (j, k) => {
      if (k === 0) {
        return 0;
      }

      const out = xscale(d[k - 1].count + runningcount);
      runningcount += d[k - 1].count;
      return out;
    };

    const v = d3.select(this)
      .selectAll('rect')
        .data(d);

    v
      .transition()
      .duration(5000)
      .attr('fill', j => `#${j.hex}`)
      .attr('width', j => xscale(j.count))
      .attr('x', scalefunc);

    v
      .exit()
      .remove();
    v
      .enter()
        .append('rect')
        .attr('fill', j => `#${j.hex}`)
        .attr('width', j => xscale(j.count))
        .attr('height', height)
        .attr('x', scalefunc)
        .style('opacity', 0)
        .transition()
        .duration(5000)
        .style('opacity', 1);
  };
};

class CloudGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
    this.padding = 3;
  }

  componentDidMount() {
    this.bargraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');

    const graphholder = this.bargraph.selectAll('.stack')
      .data(this.props.appState.img)
      .enter().append('g')
        .attr('class', 'stack');

    graphholder.exit().remove();

    graphholder.transition().duration(1000);
  }

  componentDidUpdate() {
    const h = ((this.props.height - (9 * this.padding)) / 9);

    const graphholder = this.bargraph.selectAll('.stack')
      .data(this.props.appState.img);

    graphholder.exit().remove();

    graphholder
      .enter()
        .append('g')
        .each(group(h, this.props.width))
        .attr('class', 'stack')
        .attr('transform', (d, i) => `translate(0,${i * (h + this.padding)})`);

    graphholder
      .each(group(h, this.props.width));
  }

  render() {
    return (<div id="graph" />);
  }
}

CloudGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: PropTypes.shape({
    img: PropTypes.arrayOf(
        PropTypes.arrayOf(
          PropTypes.shape({
            r: PropTypes.number.isRequired,
            g: PropTypes.number.isRequired,
            b: PropTypes.number.isRequired,
            hex: PropTypes.string.isRequired,
            count: PropTypes.number.isRequired,
          }))),
  }).isRequired,
};

const CloudGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(CloudGraphInner);

export default CloudGraph;

