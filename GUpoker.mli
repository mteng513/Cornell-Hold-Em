open Types
(* GUI interface for the game. It will consist of a few basic
 * components. Again, this interface is kind of an "extra" thing
 * that we're going to do our best to implement. We'll be using
 * Labltk. The type definitions are subject to change. *)
module GU_Poker : sig

	val bet_amt : int ref

	val get_num : unit -> int

	(* Draws main window for the game *)
	val draw_start : unit -> unit

	(* Draw difficulty box *)
	val draw_difficulty : unit -> unit

	(* Draw number of players box that will allow the
	 * user to enter the number of players *)
	val draw_n_players : unit -> unit

	(* Close Graphics Window *)
	val closeg : unit -> unit

	(* Functions for the user to play the game *)
	val player_home : card -> card -> int -> int -> int -> card list -> unit

	(* Draws players hand *)
	val draw_cards : rank * suit -> rank * suit -> int -> unit

	(* Indicates the player has folded *)
	val draw_fold : unit -> unit

	(* Draws the Flop *)
	val draw_flop : unit -> unit

	(* Draws the call *)
	val draw_call : unit -> unit

	(* Draws the win box if you win *)
	val draw_w : unit -> unit

	(* Draws lose box if you lose *)
	val draw_l : unit -> unit

	(* Draws win if you win the whole game *)
	val draw_win : unit -> unit

	(* Draws lose if you lose the whole game *)
	val draw_lose : unit -> unit

end