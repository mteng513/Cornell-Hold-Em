(* ml file for the Game Engine *)


module Game_Engine = struct

	type suit = Clarkson | Gries | Dijkstra | George

	type rank = One | Two | Three | Four | Five | Six | Seven
		| Eight | Nine | Jack | Queen | King | Ace

	type card = rank * suit

	type deck = card list 

	type play_cards = card list 

	type hand = card list

	type current_state = START | DEAL | BET_ONE | BET_TWO | BET_THREE | SCORE
		| END

	type global_state = {
		mutable current_st : current_state;
		mutable cards_in_play : play_cards;
		mutable pot : int;
		mutable current_bet : int;
		mutable n_players : int;
		mutable c_player : int;
		hands : (int * hand) list;
	}

	let score g_state = 
		failwith "Unimplemented"

	(* let transition_state g_state = 
		ignore (g_state.c_player <- 0);
		match g_state.current_st with
			| START -> g_state.current_st <- DEAL 
			| DEAL -> g_state.current_st <- BET_ONE
			| BET_ONE -> g_state.current_st <- BET_TWO
			| BET_TWO -> g_state.current_st <- BET_THREE
			| BET_THREE -> g_state.current_st <- SCORE; score g_state
			| SCORE -> if g_state.n_players = 1 then 
				g_state.current_st <- END else g_state.current_st <- DEAL
			| END -> failwith "Bad"

	let mutable_incr i = 
		i := i + 1; *)

	let switch g_state = 
		failwith "Unimplemented"

	let deal g_state = 
		failwith "Unimplemented"

	let flop g_state = 
		failwith "Unimplemented"

	let turn g_state = 
		failwith "Unimplemented"

	let river g_state = 
		failwith "Unimplemented"

	let init g_state = 
		failwith "Unimplemented"

end 