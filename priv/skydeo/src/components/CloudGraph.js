import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import * as d3 from 'd3';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const makeScale = (size, domain) => d3.scaleLinear()
          .range([0, size])
          .domain([0, domain]).nice();

const group = function group(height, w) {
  return function z(d) {
    const sum = d3.sum(d, j => j.count);
    const yscale = makeScale(height, sum);
    let runningcount = 0;
    d3.select(this)
      .selectAll('rect')
        .data(d)
        .enter()
        .append('rect')
        .attr('fill', j => `#${j.hex}`)
        .attr('width', w)
        .attr('height', j => yscale(j.count))
        .attr('y', (j, k) => {
          if (k === 0) {
            return 0;
          }

          const out = yscale(d[k - 1].count + runningcount);
          runningcount += d[k - 1].count;
          return out;
        });
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
    const w = ((this.props.width - (9 * this.padding)) / 9);

    const graphholder = this.bargraph.selectAll('.stack')
      .data(this.props.appState.img)
      .enter()
        .append('g')
        .each(group(this.props.height, w))
        .attr('class', 'stack')
        .attr('transform', (d, i) => `translate(${i * (w + this.padding)})`);
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

