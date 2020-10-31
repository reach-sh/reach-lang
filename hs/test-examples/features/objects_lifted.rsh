'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {getX: Fun([], UInt),
          getObj: Fun([], Object({'x': UInt}))}]],
  (A) => {
    const key = 'x';
    A.only(() => {
      const obj0 = declassify(interact.getObj());
      assume(obj0.x == 1);

      const x = declassify(interact.getX());
      assume(x == 1);
    });
    A.publish(obj0, x);
    require(obj0.x == 1);

    const obj1 = {[key]: x};
    require(obj1.x == 1);

    // TODO: object field ref []
    // "Invalid array index"
    // const obj1_x1 = obj1["x"];
    // assert(obj1_x1 == 1);

    // object splice, keys can be added
    const obj2 = {...obj0, y: 'y_val'};
    require(obj2.x == 1);
    // TODO require(obj2.y == 'y_val');

    // object field shorthand
    const obj3 = {x};
    require(obj3.x == 1);

    // object splice + keys can be overridden
    const obj5 = {...obj3, x: 2};
    assert(obj5.x == 2);

    // object splice binding
    const {y, ...obj6} = obj2;
    // TODO assert(y == 'y_val');
    require(obj6.x == 1);

    // TODO: structural object equality comparison
    // "Err_Type_Mismatch" (int vs obj)
    // assert(obj0 == obj1);
  }
);
