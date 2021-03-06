(* Main play interface, provides the betting interface that
 * will be sufficient for players, but will need to be extended
 * upon for the AI players. *)
open Types
open Gengine

module Player : sig 

	open Game_Engine

	(* Amount of money remaining to be bet. Player loses if they
	 * hit 0. *)
	type cash

	(* Bet amt passed in. Returns amount bet if possible or
	 * otherwise. Bet must equal or exceed all other bets. *)
	val bet : int -> unit

	(* Call the amount that has currently been bet. Returns true
	 * if bet has succesfully been called. Player will go all in
	 * to call if need be. *)
	val call : unit -> unit

	(* Fold function that indicates that this player has folded. *)
	val fold : unit -> unit 

	(* Check function that indicates the player does not wish to bet. *)
	val check : unit -> unit

	(* Getter for global state. *)
	val state_getter : unit -> global_state ref 

end