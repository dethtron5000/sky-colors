import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import * as d3 from 'd3';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const makeY = (size, domain) => d3.scaleLinear()
          .rangeRound([0, size])
          .domain([0, domain]).nice();

class CloudGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
  }

  componentDidMount() {
    const yscale = makeY(this.props.height);
    this.bargraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');

    const keys = Object.keys(this.props.appState.img);

    const graphholder = this.bargraph.selectAll('.stack')
        .data(d3.stack().keys(keys)(vals))
        .enter().append('g')
          .attr('fill', color)
          .attr('class', 'stack')
        .selectAll('rect')
        .data((d) => { return d; })
        .enter().append('rect')
          .attr('x', function (d) {return x(d[0]);})
          .attr('y', 0)
          .attr('width', function (d) { return x(d[1]) - x(d[0]); })
          .attr('height', 37);

    graphholder.exit().remove();

    graphholder.transition().duration(1000);
  }

  componentDidUpdate(prevProps, prevState) {

  }

  render() {
    return (<div id="graph" />);
  }
}

CloudGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: PropTypes.shape({
    img: PropTypes.shape({
      r: PropTypes.number.isRequired,
      g: PropTypes.number.isRequired,
      b: PropTypes.number.isRequired,
      hex: PropTypes.string.isRequired,
      count: PropTypes.number.isRequired,
    }),
  }).isRequired,
};

const CloudGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(CloudGraphInner);

export default CloudGraph;

