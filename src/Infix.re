
let optMap = (fn, items) => List.fold_left((result, item) => switch (fn(item)) { | None => result | Some(res) => [res, ...result]}, [], items);

let (|!) = (o, d) => switch o { | None => failwith(d) | Some(v) => v };
let (|?) = (o, d) => switch o { | None => d | Some(v) => v };
let (|??) = (o, d) => switch o { | None => d | Some(v) => Some(v) };
/** Lazy optional default operator */
let (|?#) = (o, d) => switch o { | None => Lazy.force(d) | Some(v) => Some(v) };
/** Lazy optional default wrapped operator */
let (|??#) = (o, d) => switch o { | None => Lazy.force(d) | Some(v) => Some(v) };
let (|?>) = (o, fn) => switch o { | None => None | Some(v) => fn(v) };
let (|?>>) = (o, fn) => switch o { | None => None | Some(v) => Some(fn(v)) };
let fold = (o, d, f) => switch o { | None => d | Some(v) => f(v) };
let (|.!) = (fn, message, arg) => fn(arg) |! message;

let (|?<) = (o, fn) => switch o { | None => () | Some(v) => fn(v) };

let fileConcat = (a, b) => {
  if (b != "" && b.[0] == '.' && String.length(b) >= 2 && b.[1] == '/') {
    Filename.concat(a, String.sub(b, 2, String.length(b) - 2))
  } else {
    Filename.concat(a, b);
  }
};

let (/+) = fileConcat;