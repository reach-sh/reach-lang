'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    // empty object literal
    const obj0 = {};

    // Normal object literal
    const obj1 = {x: 1};

    // object field reference .
    const obj1_x0 = obj1.x;
    assert(obj1_x0 == 1);

    // TODO: object field ref []
    // "Invalid array index"
    // const obj1_x1 = obj1["x"];
    // assert(obj1_x1 == 1);

    // object str field
    const obj2 = {'x': 1};
    assert(obj2.x == 1);

    // object computed field
    const field = 'x';
    const obj3 = {[field]: 1};
    assert(obj3.x == 1);

    // object splice, keys can be added
    const obj4 = {...obj3, y: 'yval'};
    assert(obj4.x == 1);
    assert(obj4.y === 'yval');

    // object destructuring
    const {x, ...obj4a} = obj4;
    assert(x == 1);
    assert(obj4a.y == 'yval');

    const obj4b = object_set(obj4, field, 2);
    assert(obj4b.x == 2);

    // object field shorthand
    const obj5 = {x};
    assert(obj5.x == 1);

    // object splice + keys can be overridden
    const obj6 = {...obj3, x: 2};
    assert(obj6.x == 2);

    // TODO: structural object equality comparison
    // "Err_Type_Mismatch" (int vs obj)
    // assert(obj2 == obj3);
  }
);
