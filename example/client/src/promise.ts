let bind = <T, U>(mx: Promise<T>, f: ((x: T) => Promise<U>)): Promise<U> => {
  return new Promise((resolve, reject) => {
    mx.then(x => f(x)
      .then(resolve)
      .catch(reject))
    .catch(reject);
  });
};

let fmap = <T, U>(f:((x: T) => U)) => (mx: Promise<T>): Promise<U> => {
    return new Promise((resolve, reject) => {
        mx.then((x: T) => resolve(f(x))).catch(reject);
    });
};

let isPure = <T>(x: T | Promise<T>): x is T => {
  return !(x instanceof Promise);
};

let dispatch = <T, U>(f: ((x: T) => Promise<U>)) => (mx: T | Promise<T>): Promise<U> => {
  return isPure(mx) ? f(mx) : bind(mx, f);
};

let sequence = <T>(xs: Promise<T>[]): Promise<T[]> => {

  return new Promise((resolve, reject) => {
    let result:T[] = new Array(xs.length);
    let count = 0;
    let rejected = false;

    let onComplete = () => {
      ++count;
      if(result.length == count) {
        (rejected ? reject : resolve)(result);
      }
    };

    xs.forEach((x, i) => x
      .then(y => {
        result[i] = y;
        onComplete();
      })
      .catch(err => {
        rejected = true;
        onComplete();
      }));
  });
};

export {bind, fmap, isPure, dispatch, sequence};

