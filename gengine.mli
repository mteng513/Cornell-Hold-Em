(* Main interface file for the game engine. This is just a rough
 * sketch of what we think the game engine is going to look like.
 * It is entirely possible that this (and others) interface will
 * be modified heavily *)

module type Game_Engine = sig

	(* Card type for the engine *)
	type card

	(* Deck type for the engine *)
	type deck

	(* Play type for the engine (ie. the cards that are in play) *)
	type play_cards

	(* An enumerator type for the game state as it applies to a 
	 * particular round of poker. The states are as follows:
		1. START
		2. DEAL
		3. BET_ONE
		4. BET_TWO
		5. BET_THREE
		6. SCORE

	  * The STATE type will maintain additional information, but will
	  * perform decisions based off of the current game state.
	  *)
	type current_state

	(* Global identifier for the entire game state. It will need to
	 * contain several pieces of information, such as: 
		1. The deck
		2. The hands of every player
		3. Amount of money every player has
		4. Current game state
		5. Number of players
	*) 
	type global_state 


	(* Switch function - performs the transition from player to player
	 * in the betting rounds by sending signals to the players that it
	 * is their turn to bet. *)
	val switch : int -> int

	(* Deal function for the game engine. Deals two cards to every player,
	 * recording the results for the player in the engine. Transitions into 
	 * the deal stage, and then into the first round of betting *)
	val deal : unit -> unit 

	(* Flop function for the engine. Will draw three cards and display them
	 * to the user and the opponents. Transitions into the second round of
	 * betting. *)
	val flop : unit -> unit 

	(* Turn function for the engine. Draws the 4th card and displays it to the
	 * players. Transitions into the third round of betting. *)
	val turn : unit -> unit 

	(* River function for the engine. Lays the final card, and transitions to the
	 * final betting round *)
	val river : unit -> unit

	(* Score function for the engine. Transitions to the score stage, where the 
	 * score of all hands still in is computed and the winner is declared. If
	 * someone won, the game ends. If someone does not win, we go back to the
	 * deal stage. *)
	val score : unit -> unit

	(* Main initialization function for the engine. Will perform all 
	 * bootstrapping necessary to start the engine. The game will start
	 * in the START state, then progress through the states and loop 
	 * until the game is over. This function should not terminate until
	 * the game is over. Takes in an int to specify the number of opponents.
	 *)
	val init : int -> unit 






end