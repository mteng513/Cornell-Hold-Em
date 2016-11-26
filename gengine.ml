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
		mutable hands : (int * hand) list;
	}

	(* End of type declarations. We will now declare some references that
	 * will need to be updated as we go. *)

	let deck = ref [] 
	let state = ref {current_st = START; cards_in_play = []; pot = 0;
		current_bet = 0; n_players = 0; c_player = 0; hands = []} 	

	let reset_deck () = deck := 
		[(One, Clarkson); (Two, Clarkson); (Three, Clarkson); 
		(Four, Clarkson); (Five, Clarkson); (Six, Clarkson);
		(Seven, Clarkson); (Eight, Clarkson); (Nine, Clarkson);
		(Jack, Clarkson); (Queen, Clarkson); (King, Clarkson);
		(Ace, Clarkson); 
		(One, Gries); (Two, Gries); (Three, Gries); 
		(Four, Gries); (Five, Gries); (Six, Gries);
		(Seven, Gries); (Eight, Gries); (Nine, Gries);
		(Jack, Gries); (Queen, Gries); (King, Gries);
		(Ace, Gries);
		(One, Dijkstra); (Two, Dijkstra); (Three, Dijkstra); 
		(Four, Dijkstra); (Five, Dijkstra); (Six, Dijkstra);
		(Seven, Dijkstra); (Eight, Dijkstra); (Nine, Dijkstra);
		(Jack, Dijkstra); (Queen, Dijkstra); (King, Dijkstra);
		(Ace, Dijkstra);
		(One, George); (Two, George); (Three, George); 
		(Four, George); (Five, George); (Six, George);
		(Seven, George); (Eight, George); (Nine, George);
		(Jack, George); (Queen, George); (King, George);
		(Ace, George);
		]

	(* Pushes an element onto a ref list *)
	let push lst ele = 
		lst := ele::!lst

	(* Pops an element off of a ref list *)
	let pop lst = 
		match !lst with
			| [] -> failwith "no more card in the deck"
			| h::t -> lst := t; h 

	let rec shuffle_helper deck lst1 lst2 = 
		match !deck with
			| [] -> ()
			| h::t -> (if (Random.int 5200 mod 2 = 0) then 
				push lst1 h else push lst2 h); let tt = ref t in
				shuffle_helper tt lst1 lst2


	(* Started shuffle. I think it's clear what I'm trying to do here, but
	 * lmk if unclear *)
	let shuffle () = 
		let a_list = ref [] in 
		let b_list = ref [] in 
    shuffle_helper deck a_list b_list; 
    deck := (!b_list) @ (!a_list)





	let score g_state = 
		failwith "Unimplemented"

	let transition_state g_state = 
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
		i := !i + 1

	let switch g_state = 
		failwith "Unimplemented"

	let deal g_state deck = 
		(* step 1: make sure deck has enough cards *)
		(if List.length !deck < (g_state.n_players+5) 
		then (reset_deck (); shuffle ())
		else ());
		(* step 2: deal cards to all players using pop *)
		(for i = 1 to g_state.n_players do
			g_state.hands <- 
				(g_state.n_players - i, [pop deck; pop deck])::(g_state.hands)
		done)(* ;
		step 3: MAYBE?!?!?! change state to BET_ONE 
		transition_state g_state *)

	let flop g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		(* change to formatted print function? *)
		g_state.cards_in_play

	let turn g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		(* change to formatted print function? *)
		g_state.cards_in_play

	let river g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		(* change to formatted print function? *)
		g_state.cards_in_play

	let init g_state = 
		failwith "Unimplemented"

end 