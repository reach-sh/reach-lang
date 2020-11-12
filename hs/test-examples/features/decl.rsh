'reach 0.1';

export const main =
  Reach.App(
    {},
    [],
    () => {
      { const x = 1;
        assert(x == 1); }

      { const [ x, y ] = [ 1, 2 ];
        assert(x + y == 3); }

      { const { x, y } = { x: 1, y: 2 };
        assert(x + y == 3); }

      { const [ x, [ y ] ] = [ 1, [ 2 ] ];
        assert(x + y == 3); }

      { const [ x, { y } ] = [ 1, { y: 2 } ];
        assert(x + y == 3); }

      { const { x: [ a, b ] } = { x: [ 1, 2 ] };
        assert(a == 1 && b == 2); }

      ((x, y) => { assert(x + y == 3); })(1, 2);
      ((x, y) => { assert(x + y == 3); })(...[1, 2]);
      (([x, y]) => { assert(x + y == 3); })([1, 2]);
      (({x, y}) => { assert(x + y == 3); })({x: 1, y: 2});
      (([x, [y]]) => { assert(x + y == 3); })([1,[2]]);
      (([x, {y}]) => { assert(x + y == 3); })([1,{ y: 2 }]);

    });
