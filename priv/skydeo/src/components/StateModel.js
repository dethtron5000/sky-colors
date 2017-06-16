import PropTypes from 'prop-types';

const colorArray = PropTypes.arrayof(
PropTypes.arrayOf(
        PropTypes.shape({
          r: PropTypes.number.isRequired,
          g: PropTypes.number.isRequired,
          b: PropTypes.number.isRequired,
          hex: PropTypes.string.isRequired,
          count: PropTypes.number.isRequired,
        })));

const appState = PropTypes.shape({
  img: PropTypes.arrayOf(
      colorArray,
), });

export default { appState };
