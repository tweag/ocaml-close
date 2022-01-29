open Library.A

module M = struct
  open Library.A

  let _ = x
end

let use_a _ _ = x; y

let link () = ()
