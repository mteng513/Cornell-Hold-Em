(* Types file that is included in all other files. *)

type suit = Clarkson | Gries | Dijkstra | George

type rank = Two | Three | Four | Five | Six | Seven
		| Eight | Nine | Ten | Jack | Queen | King | Ace

type card = rank * suit

type deck = card list

type hand = card list

type current_state = START | DEAL | BET_ZERO | BET_ONE | BET_TWO | BET_THREE 
	| SCORE| END

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