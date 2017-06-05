import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import * as d3 from 'd3';

const mapStateToProps = state => ({ appState: state });

const mapDispatchToProps = () => ({});

const makeScale = (size, domain) => d3.scaleLinear()
          .rangeRound([0, size])
          .domain([0, domain]).nice();

// const color = d => d.hex;

class CloudGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
  }

  componentDidMount() {
    const yscale = makeScale(this.props.height, 13);

    const xscale = makeScale(this.props.width, this.props.appState.img.length);

    this.bargraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');

    // const stack = d3.stack().keys(d3.range(16));

    /* const accessor = d3.stack()
      .value((d, i) => d[i].count)
      .keys(d3.range(13))(this.props.appState.img);
*/
    const graphholder = this.bargraph.selectAll('.stack')
      .data(this.props.appState.img)
      .enter().append('g')
        .attr('class', 'stack')
        .attr('x', (d, i) => i * 37)
      .selectAll('rect')
      .data(d => d)
      .enter()
        .append('rect')
        .attr('y', (d, i, data) => {
          console.log(d, i, data);
          if (i === 0) {
            return 0;
          }
          return d.r;
        })
        .attr('x', 0)
        .attr('height', d => yscale(d[1]) - yscale(d[0]))
        .attr('width', 37)
        .attr('fill', function(d) { console.log(d); });

    graphholder.exit().remove();

    graphholder.transition().duration(1000);
  }

  componentDidUpdate() {
    const yscale = makeScale(this.props.height, 13);

    const xscale = makeScale(this.props.width, this.props.appState.img.length);

    const accessor = d3.stack()
      .value((d, i) => d[i].count)
      .keys(d3.range(13))(this.props.appState.img);


    const graphholder = this.bargraph.selectAll('.stack')
      .data(this.props.appState.img)
      .enter().append('g')
        .attr('class', 'stack')
        .attr('x', (d, i) => i * 37)
      .selectAll('rect')
      .data(d => d)
      .enter()
        .append('rect')
        .attr('y', (d, i, data) => {
          if (i === 0) {
            return 0;
          }
          console.log(this);
          return yscale(data[i-1].y + data[i-1].count);
        })
        .attr('x', 0)
        .attr('height', d => yscale(d.count))
        .attr('width', 37)
        .attr('fill', d => d.hex);
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

