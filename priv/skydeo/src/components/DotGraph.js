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

class DotGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
    this.padding = 0;
    this.w = ((this.props.width));
    this.h = ((this.props.height - (9 * this.padding)) / 9);
  }

  componentDidMount() {
    this.dotgraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');
    const transformFunc = (d, i) => `translate(0,${i * (this.h + this.padding)})`;

    const objectData = Object.values(this.props.appState.locations);
    const objectKeys = Object.keys(this.props.appState.locations);

    const graphholder = this.dotgraph.selectAll('.stack')
      .data(objectData)
      .enter();

    const stack = graphholder
      .append('g')
        .attr('class', 'stack')
        .attr('transform', transformFunc)
        .attr('id', (d, i) => objectKeys[i]);

    stack.append('g')
      .attr('class', 'dotholder');

    stack.append('g')
      .attr('class', 'textHolder');

    this.dotgraph.selectAll('.textHolder')
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
      .dotgraph
      .selectAll(`#${this.props.appState.lastLocation} .dotholder`);

    // const d = arr.img;
    const max = d3.max(extract.img, j => j.count);
    const rscale = makeScale(0.75 * this.h, max);
    const xscale = d3.scaleLinear()
          .range([this.h, this.w - (this.h)])
          .domain([0, extract.img.length - 1]);

    const v = graphholder
      .selectAll('circle')
        .data(extract.img);

    v

      // .style('opacity', 0.1)
      .transition()
      .duration(60000)
      .attr('fill', j => `#${j.hex}`)
      .attr('r', j => rscale(j.count))
      .attr('cx', (j, i) => xscale(i));

    // .style('opacity', 1)

    v
      .exit()
      .transition()
      .duration(60000)

      // .style('opacity', 0)
      .attr('cx', this.w + 500)
      .remove();

    v
      .enter()
        .append('circle')
        .attr('fill', j => `#${j.hex}`)

        // .attr('r', 0)
        .attr('r', j => rscale(j.count))

        .attr('cy', 0.5 * this.h)
        .attr('cx', 0)
        .attr('cx', (j, i) => xscale(i))
        .style('opacity', 0)
        .transition()
        .duration(10000)
        .style('opacity', 0.8)
        /* .transition()
        .duration(15000) */;
  }

  render() {
    return (<div id="graph" />);
  }

}

DotGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: StateModel.appState.isRequired,

};

const DotGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(DotGraphInner);

export default DotGraph;
