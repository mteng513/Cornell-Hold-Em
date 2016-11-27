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

	exception GameEnded

	(* End of type declarations. We will now declare some references that
	 * will need to be updated as we go. *)

	let deck = ref [] 
	let state = ref {current_st = START; cards_in_play = []; pot = 0;
		current_bet = 0; n_players = 0; c_player = 0; hands = []}

	let suit_print suit =
		match suit with 
			| Clarkson -> "Clarkson "
			| Gries -> "Gries "
			| Dijkstra -> "Dijkstra "
			| George -> "George "

	let print_helper ele = 
		match ele with 
			| (One, suit) -> "One of " ^ suit_print suit 
			| (Two, suit) -> "Two of " ^ suit_print suit
			| (Three, suit) -> "Three of " ^ suit_print suit
			| (Four, suit) -> "Four of " ^ suit_print suit
			| (Five, suit) -> "Five of " ^ suit_print suit
			| (Six, suit) -> "Six of " ^ suit_print suit
			| (Seven, suit) -> "Seven of " ^ suit_print suit
			| (Eight, suit) -> "Eight of " ^ suit_print suit
			| (Nine, suit) -> "Nine of " ^ suit_print suit
			(* | (Ten, suit) -> "Ten of " ^ suit_print suit  We have no ten? *)
			| (Jack, suit) -> "Jack of " ^ suit_print suit
			| (Queen, suit) -> "Queen of " ^ suit_print suit
			| (King, suit) -> "King of " ^ suit_print suit
			| (Ace, suit) -> "Ace of " ^ suit_print suit

	let rec formatted_print lst = 
		match lst with 
			| [] -> ""
			| h::t -> (print_helper h) ^ formatted_print t

	let printc lst =
		print_endline (formatted_print lst)

	(* Deck reset function. Sets the deck back to normal state. *)
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

	(* Helper for shuffle. Takes in a deck, and pushes the elements
	 * of the deck onto two lists psuedo randomly. *)
	let rec shuffle_helper deck lst1 lst2 = 
		match !deck with
			| [] -> ()
			| h::t -> (if (Random.int 5200 mod 2 = 0) then 
				push lst1 h else push lst2 h); let tt = ref t in
				shuffle_helper tt lst1 lst2

	(* Shuffling mechanism. Creates two ref lists to be randomly
	 * pushed onto, punts to the helper, then sets the deck equal to
	 * the newly shuffeled deck. *)
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


	(* DO WE NEED A LIST THAT KEEPS TRACK OF WHAT EACH PLAYER HAS BET?
		HOW ARE WE KEEPING TRACK OF WHO HAS ALREADY BET? IF someone raises
		a bet, then how do we account for that in switch? Why don't we just input g_state entirely
		instead of c_player? Does the signal_bet function even depend on which
		player is being prompted? *)

	(* Signals bets to the players. *)
	let rec signal_bet g_state (* current_player *) = 
		print_endline ("Place your bet. 
			The current bet is " ^ (string_of_int g_state.current_bet));
		let bet = read_int () in
		match bet with
		(* must make every other player match the bet, raise, or fold *)
		| bet when bet > g_state.current_bet -> g_state.current_bet <- bet
		| bet when bet = g_state.current_bet -> (* continue to to other players *) ()
		| bet when bet = 0 -> () (* this means the player folds *)
		| _ -> print_endline "invalid input received. try again"; 
							signal_bet g_state

	(* Called in after deal and each bet stage to signal bets to
	 * all the players. *)
	let switch g_state = 
		match g_state.current_st with 
			| BET_ONE | BET_TWO | BET_THREE ->
				(for i = 0 to g_state.n_players do 
					signal_bet g_state; 
					g_state.c_player <- g_state.c_player + 1; done);
				g_state.c_player <- 0;
				transition_state g_state;
			| _ -> failwith "Bad state"

	(* deal takes in the current_st g_state and the current deck ref (deck) and
	   updates the inputted state with hands *)
	let deal g_state deck = () 
		let deal g_state deck = 
		(* step 1: make sure deck has enough cards *)
		(if List.length !deck < (g_state.n_players+5) 
		then (reset_deck (); shuffle ())
		else ());
		(* step 2: deal cards to all players using pop *)
		(for i = 1 to g_state.n_players do
			g_state.hands <- 
				(g_state.n_players - i, [pop deck; pop deck])::(g_state.hands);
		done)(* ;
		(*step 3: MAYBE?!?!?! change state to BET_ONE*) 
		transition_state g_state *)

	let flop g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let turn g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let river g_state = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let init g_state = 
		try 
			deal g_state deck; 


		with 
			| GameEnded -> ()
			| _ -> failwith "This is bad"



end 