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
    const yscale = makeScale(height, sum);
    let runningcount = 0;
    const scalefunc = (j, k) => {
      if (k === 0) {
        return 0;
      }

      const out = yscale(d[k - 1].count + runningcount);
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
      .attr('height', j => yscale(j.count))
      .attr('y', scalefunc);

    v
      .exit()
      .remove();
    v
      .enter()
        .append('rect')
        .attr('fill', j => `#${j.hex}`)
        .attr('height', j => yscale(j.count))
        .attr('width', w)
        .attr('y', scalefunc)
        .style('opacity', 0)
        .transition()
        .duration(5000)
        .style('opacity', 1);
  };
};

class GridGraphInner extends Component {

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
    const w = ((this.props.width - (3 * this.padding)) / 3);
    const h = ((this.props.height - (3 * this.padding)) / 3);

    const graphholder = this.bargraph.selectAll('.stack')
      .data(this.props.appState.img);

    graphholder
      .enter()
        .append('g')
        .each(group(h, w))
        .attr('class', 'stack')
        .attr('transform', (d, i) => `translate(${(i % 3) * (w + this.padding)},${Math.floor(i / 3) * (h + this.padding)})`);

    graphholder.exit().remove();

    graphholder
      .each(group(h, w));
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

