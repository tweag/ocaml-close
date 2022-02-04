open Library.A

module M = struct
  open Library.A

  let _ = x
  let no_use = Library.A.z
end

let use_a _ _ = x; y

let link () = ()
