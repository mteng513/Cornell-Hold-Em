(* Ml file for the player *)
open Types
open Gengine

module Player = struct

	include Game_Engine

	type cash = int 

	let bet amt = 
		let state = get_state () in 
		match !state.c_player with 
			| 0 -> ()
			| i -> ()

	let call () = 
		failwith "Unimplemented"

	let fold () =
		failwith "Unimplemented"

	let check () = 
		failwith "Unimplemented"

	let state_getter () = 
		failwith "Unimplemented"
		
end 