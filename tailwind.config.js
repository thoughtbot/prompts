const defaultTheme = require('tailwindcss/defaultTheme');
const colors = require('tailwindcss/colors');

module.exports = {
  darkMode: 'media',
  purge: ['./src/**/*.elm', './public/**/*.html'],
  theme: {
    extend: {},
    colors:
      Object.assign(colors, {
        gray: colors.warmGray,
      }),
    fontFamily: {
      sans: ['Lato', 'Helvetica Neue', 'Arial Black', 'sans-serif'],
      serif: ['Lusitana', 'Georgia', 'serif'],
      mono: ['Droid Sans Mono', 'monospace'],
    },
  },
  variants: {
    extend: {
      textOpacity: ['dark']
    }
  },
  plugins: [],
};
