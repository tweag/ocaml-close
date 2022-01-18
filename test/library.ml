module A = struct
  let x = ()
  let y = ()
  module S = struct
    let s = ()
  end
end

module B = struct
  let y = ()
  let z = ()
  module S = struct
    let s = ()
  end
end

module C = struct
  let z = ()
  let a = ()
  module S = struct
    let s = ()
  end
end
