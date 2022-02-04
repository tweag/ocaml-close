module A = struct
  let x = ()
  let y = ()
  let z = ()
  module S = struct
    let s = ()
  end
  type 'a t = T1 | T2 of 'a
end

module B = struct
  let y = ()
  let z = ()
  module S = struct
    let s = ()
  end
  type t = {f1: unit}
  let f = {f1 = ()}
end

module C = struct
  let z = ()
  let a = ()
  module S = struct
    let s = ()
  end
end
