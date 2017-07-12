import PropTypes from 'prop-types';

const colorArray = PropTypes.arrayOf(
PropTypes.arrayOf(
        PropTypes.shape({
          r: PropTypes.number.isRequired,
          g: PropTypes.number.isRequired,
          b: PropTypes.number.isRequired,
          hex: PropTypes.string.isRequired,
          count: PropTypes.number.isRequired,
        })));

const locationShape = PropTypes.shape({
  location: PropTypes.string.isRequired,
  img: PropTypes.arrayOf(
      colorArray,
  ),
});

const appState = PropTypes.shape({
  cambridge: locationShape,
  newyork: locationShape,
  chicago: locationShape,
  sanfrancisco: locationShape,
  paloalto: locationShape,
  tokyo: locationShape,
  shanghai: locationShape,
  munich: locationShape,
  london: locationShape,
});

const displayNames = {
  cambridge: 'Cambridge',
  newyork: 'New York',
  chicago: 'Chicago',
  sanfrancisco: 'San Francisco',
  paloalto: 'Palo Alto',
  tokyo: 'Tokyo',
  shanghai: 'Shanghai',
  munich: 'Munich',
  london: 'London',
};

export default { appState, displayNames };
