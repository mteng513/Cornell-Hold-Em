open Types
(* GUI interface for the game. It will consist of a few basic
 * components. Again, this interface is kind of an "extra" thing
 * that we're going to do our best to implement. We'll be using
 * Labltk. The type definitions are subject to change. *)
module GU_Poker : sig

	(* Draws main window for the game *)
	val draw_start : unit -> unit

	(* Draw difficulty box *)
	val draw_difficulty : unit -> unit

	(* Draw number of players box that will allow the
	 * user to enter the number of players *)
	val draw_n_players : unit -> unit

	(* Sends the number of players to the GUI, indicating
	 * that the user has started the game *)
	val init_game : int -> unit

	(* Draws players hand *)
	val draw_cards : rank * suit -> rank * suit -> unit

	(* Indicates the player has folded *)
	val draw_fold : unit -> unit

	(* Draws the Flop *)
	val draw_flop : unit -> unit

	(* Draws the turn *)
	val draw_turn : unit -> unit

	(* Draws the river *)
	val draw_river : unit -> unit

	(* Draws the win box if you win *)
	val draw_w : unit -> unit

	(* Draws lose box if you lose *)
	val draw_l : unit -> unit

	(* Draws win if you win the whole game *)
	val draw_win : unit -> unit

	(* Draws lose if you lose the whole game *)
	val draw_lose : unit -> unit

end