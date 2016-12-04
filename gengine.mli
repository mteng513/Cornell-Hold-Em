(* Main interface file for the game engine. This is just a rough
 * sketch of what we think the game engine is going to look like.
 * It is entirely possible that this (and others) interface will
 * be modified heavily *)

module Game_Engine : sig

	type suit = Clarkson | Gries | Dijkstra | George

	type rank = Two | Three | Four | Five | Six | Seven
		| Eight | Nine | Ten | Jack | Queen | King | Ace

	type card = rank * suit

	type deck = card list  

	type hand = card list

	type current_state = START | DEAL | BET_ZERO | BET_ONE | BET_TWO | BET_THREE | SCORE
		| END

	type global_state = {
		mutable current_st : current_state;
		mutable cards_in_play : card list;
		mutable pot : int;
		mutable current_bet : int;
		mutable n_players : int;
		mutable c_player : int;
		mutable hands : hand list; 
		mutable bets : int array;
		mutable players_in : bool array;
		mutable scores: int array;
		mutable chips: int array
	}

	(* Switch function - performs the transition from player to player
	 * in the betting rounds by sending signals to the players that it
	 * is their turn to bet. *)
	val switch : global_state -> unit

	(* Deal function for the game engine. Deals two cards to every player,
	 * recording the results for the player in the engine. Transitions into 
	 * the deal stage, and then into the first round of betting *)
	val deal : global_state -> card list ref -> unit 

	(* Flop function for the engine. Will draw three cards and display them
	 * to the user and the opponents. Transitions into the second round of
	 * betting. *)
	val flop : global_state -> unit 

	(* Turn function for the engine. Draws the 4th card and displays it to the
	 * players. Transitions into the third round of betting. *)
	val turn : global_state -> unit 

	(* River function for the engine. Lays the final card, and transitions to the
	 * final betting round *)
	val river : global_state -> unit

	val flush : card list -> int 

	val straight : card list -> int

	val pair : card list -> int

	val four_kind : card list -> int

	val three_kind : card list -> int

	val two_pair : card list -> int

    val high_card : hand -> int

    val full_house : hand -> int

    val score_calculation : hand -> int

	(* Score function for the engine. Transitions to the score stage, where the 
	 * score of all hands still in is computed and the winner is declared. If
	 * someone won, the game ends. If someone does not win, we go back to the
	 * deal stage. *)
	val score : global_state -> unit

	(* Main initialization function for the engine. Will perform all 
	 * bootstrapping necessary to start the engine. The game will start
	 * in the START state, then progress through the states and loop 
	 * until the game is over. This function should not terminate until
	 * the game is over. Takes in an int to specify the number of opponents.
	 *)
	val init : unit -> unit 

	(* Getter function for global state. *)
	val get_state : unit -> global_state ref 






end