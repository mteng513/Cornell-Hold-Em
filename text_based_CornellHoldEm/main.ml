open Gengine
open Player
open Opponent
(* open GUpoker *)
open Test

module Engine = Game_Engine
module User = Player 
module AI = Opponent 
(* module GUI = GUpoker *)

let main () = 
	Engine.init ()

let _ = main ()