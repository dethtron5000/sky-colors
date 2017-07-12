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

class LinearGraphInner extends Component {

  constructor(props) {
    super(props);
    this.target = '#graph';
    this.padding = 0;
    this.w = ((this.props.width));
    this.h = ((this.props.height - (9 * this.padding)) / 9);
  }

  componentDidMount() {
    this.lineargraph = d3.select(this.target).append('svg')
      .attr('height', this.props.height)
      .attr('width', this.props.width)
      .append('g');
    const transformFunc = (d, i) => `translate(0,${i * (this.h + this.padding)})`;

    const objectData = Object.values(this.props.appState.locations);
    const objectKeys = Object.keys(this.props.appState.locations);

    const graphholder = this.lineargraph.selectAll('.stack')
      .data(objectData)
      .enter();

    const stack = graphholder
      .append('g')
        .attr('class', 'stack')
        .attr('transform', transformFunc)
        .attr('id', (d, i) => objectKeys[i]);

    stack.append('g')
      .attr('class', 'linearholder')
      .attr('transform', 'translate(14)');

    stack.append('g')
      .attr('class', 'textHolder');

    this.lineargraph.selectAll('.textHolder')
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
    const graphholder = this
      .lineargraph
      .selectAll(`#${this.props.appState.lastLocation} .linearholder`);

    // const d = arr.img;
    const max = d3.max(extract.img, j => j.count);
    const wscale = makeScale(this.w / extract.img.length, max);
    const xscale = d3.scaleLinear()
          .range([0, this.w])
          .domain([0, extract.img.length]);

    const v = graphholder
      .selectAll('rect')
        .data(extract.img);

    v
      .transition()
      .duration(50000)
      .attr('width', j => wscale(j.count))
      .attr('x', (j, i) => xscale(i))
      .attr('fill', j => `#${j.hex}`);
     // .style('opacity', 1);


    v
      .exit()
      .transition()
      .duration(10000)
      .attr('width', 0)
      .remove();
    v
      .enter()
        .append('rect')
        .attr('fill', j => `#${j.hex}`)

        // .attr('r', 0)
        .attr('width', 0)

        .attr('height', this.h - 10)
        .attr('x', (j, i) => xscale(i))

        // .style('opacity', 0)
        .transition()
        .duration(20000)
        .attr('width', j => wscale(j.count))

        .style('opacity', 1);
  }

  render() {
    return (<div id="graph" />);
  }

}

LinearGraphInner.propTypes = {
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  appState: StateModel.appState.isRequired,

};

const LinearGraph = connect(
    mapStateToProps,
    mapDispatchToProps,
)(LinearGraphInner);

export default LinearGraph;
