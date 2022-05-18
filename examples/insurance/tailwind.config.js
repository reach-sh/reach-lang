const colors = require('tailwindcss/colors');

module.exports = {
  purge: ["./src/**/*.{js,jsx,ts,tsx}", "./public/index.html"],
  mode: "jit",
  theme: {
    fontFamily: {
      display: ["Open Sans", "sans-serif"],
      body: ["Open Sans", "sans-serif"],
    },

    colors: {
      transparent: 'transparent',
      current: 'currentColor',
      white: {
        DEFAULT: '#ffffff',
      },
      greenn: {
        light: '#6fcf97',
        DEFAULT: '#27AE60',
        dark: '#219653',
        darker: '#1e874b',
      },
      redd: {
        light: '#FFEAEA',
        DEFAULT: '#EB5757',
        dark: '#C20D0D',
      },
      orangee: {
        light: '#FFEBDA',
        DEFAULT: '#F66A0A',
        dark: '#A04100',
      },
      primary: {
        DEFAULT: '#24292E',
      },
      warning: {
        DEFAULT: '#D1711C',
      },
      stone: colors.stone,
      red: colors.red,
      orange: colors.orange,
      green: colors.green,
      black: colors.black,
      blue: colors.blue,
      gray: colors.gray,
      emerald: colors.emerald,
      indigo: colors.indigo,
      yellow: colors.yellow,
      pink: colors.pink,
      purple: colors.purple,
      indigo: colors.indigo,
      teal: colors.teal,
    },

    extend: {
      screens: {
        mf: "990px",
      },
      keyframes: {
        "slide-in": {
          "0%": {
            "-webkit-transform": "translateX(120%)",
            transform: "translateX(120%)",
          },
          "100%": {
            "-webkit-transform": "translateX(0%)",
            transform: "translateX(0%)",
          },
        },
      },
      animation: {
        "slide-in": "slide-in 0.5s ease-out",
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [require("@tailwindcss/forms")],
};
