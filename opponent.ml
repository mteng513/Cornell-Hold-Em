(* Ml file for opponent *)
open Player

module Opponent = struct 

  include Player

  type difficulty = One | Two | Three 

  let decide () = failwith "Unimplemented"



end 