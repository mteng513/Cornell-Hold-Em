open Types
open Opponent
(* ml file for the Game Engine *)

module Game_Engine = struct
	open Opponent
	exception GameEnded

	(* End of type declarations. We will now declare some references that
	 * will need to be updated as we go. *)

	let deck = ref [] 
	let state = ref {current_st = START; cards_in_play = []; pot = 0;
		current_bet = 0; n_players = 0; c_player = 0; hands = []; 
		bets = [||]; players_in = [||]; scores = [||]; chips = [||]}

	(* [suit_to_string suit] takes in a suit [suit] and returns its
	 * string representation formatted for printing *)
	let suit_to_string (suit: suit) : string =
		match suit with 
			| Clarkson -> "Clarkson "
			| Gries -> "Gries "
			| Dijkstra -> "Dijkstra "
			| George -> "George "

	(* [card_to_string ele] takes in a card [ele] and returns its
	 * string representation formatted for printing *)
	let card_to_string (ele: card) : string = 
		match ele with  
			| (Two, suit) -> "Two of " ^ suit_to_string suit
			| (Three, suit) -> "Three of " ^ suit_to_string suit
			| (Four, suit) -> "Four of " ^ suit_to_string suit
			| (Five, suit) -> "Five of " ^ suit_to_string suit
			| (Six, suit) -> "Six of " ^ suit_to_string suit
			| (Seven, suit) -> "Seven of " ^ suit_to_string suit
			| (Eight, suit) -> "Eight of " ^ suit_to_string suit
			| (Nine, suit) -> "Nine of " ^ suit_to_string suit
			| (Ten, suit) -> "Ten of " ^ suit_to_string suit
			| (Jack, suit) -> "Jack of " ^ suit_to_string suit
			| (Queen, suit) -> "Queen of " ^ suit_to_string suit
			| (King, suit) -> "King of " ^ suit_to_string suit
			| (Ace, suit) -> "Ace of " ^ suit_to_string suit

	(* [formatted_print lst] takes in a card list [lst] and returns a
	 * formatted string containing info on all of the cards in the list *)
	let rec formatted_print (lst: card list) : string = 
		match lst with 
			| [] -> ""
			| h::t -> (card_to_string h) ^ formatted_print t

	(* [printc lst] take in a card list [lst] and converts it into a formatted
	 * string using formatted_print and then prints the string. Returns unit *)
	let printc lst =
		print_endline (formatted_print lst)

	(* [reset_deck] resets the deck back to normal, full, ordered 52-card 
	 * state, just like a freshly opened deck of cards. Returns a unit *)
	let reset_deck () : unit = deck := 
		[(Two, Gries); (Three, Dijkstra); (Four, Clarkson); 
		(Five, Gries); (Six, George); (Seven, Dijkstra); 
		(Eight, George); (Nine, Clarkson); (Ten, Dijkstra); (Jack, George);
		(Queen, Clarkson); (King, Clarkson); (Ace, Clarkson); 
		(Two, Clarkson); (Three, George); (Four, George); (Five, George);
		(Six, Dijkstra); (Seven, Gries); (Eight, Gries); (Nine, Gries);
		(Ten, Gries); (Jack, Gries); (Queen, George); (King, Dijkstra);
		(Ace, Gries);
		(Two, George); (Three, Clarkson); (Four, Gries); (Five, Dijkstra);
		(Six, Clarkson); (Seven, Clarkson); (Eight, Dijkstra); 
		(Nine, George); (Ten, Clarkson); (Jack, Dijkstra); (Queen, Dijkstra);
		(King, Gries); (Ace, Dijkstra);
		(Two, Dijkstra); (Three, Gries); (Four, Dijkstra); (Five, Clarkson); 
		(Six, Gries); (Seven, George); (Eight, Clarkson); (Nine, Dijkstra);
		(Ten, George); (Jack, Clarkson); (Queen, Gries); (King, George); 
		(Ace, George);
		]

	(* [push lst ele] prepends an element [ele] onto a list ref [lst]. 
	 * Returns unit*)
	let push (lst: 'a list ref) (ele: 'a): unit = 
		lst := ele::!lst

	(* [pop lst] Pops an element off of a 'a list ref [lst] and returns that
	 * popped element of type 'a *)
	let pop (lst: 'a list ref) : 'a = 
		match !lst with
			| [] -> failwith "no more card in the deck"
			| h::t -> lst := t; h 

	(* Helper for shuffle. Takes in a deck, and pushes the elements
	 * of the deck onto two lists psuedo randomly. *)
	let rec shuffle_helper deck lst1 lst2 lst3 lst4 lst5 lst6 lst7 lst8 = 
		match !deck with
			| [] -> ()
			| h::t -> let roll = Random.int 5200 in 
				(if (roll mod 8 = 0) then push lst2 h 
				else if ((roll mod 8) = 1) then push lst3 h
				else if ((roll mod 8) = 2) then push lst4 h
				else if ((roll mod 8) = 3) then push lst7 h
				else if ((roll mod 8) = 4) then push lst5 h
				else if ((roll mod 8) = 5) then push lst6 h
				else if ((roll mod 8) = 6) then push lst8 h
				else push lst1 h); let tt = ref t in
				shuffle_helper tt lst2 lst3 lst4 lst1 lst8 lst5 lst6 lst7

	let cut () = 
		Random.self_init();
		let a_list = ref [] in
		let b_list = ref [] in 
		(for i = 0 to 51 do 
			(if ((Random.int 3000) mod 2 = 0) then push a_list (List.nth !deck i)
			else push b_list (List.nth !deck i))
		done); 
		deck := (!b_list) @ (!a_list)

	(* Shuffling mechanism. Creates two ref lists to be randomly
	 * pushed onto, punts to the helper, then sets the deck equal to
	 * the newly shuffeled deck. *)
	let shuffle () = 
		Random.self_init();
		let a_list = ref [] in 
		let b_list = ref [] in 
		let c_list = ref [] in 
		let d_list = ref [] in 
		let e_list = ref [] in 
		let f_list = ref [] in 
		let g_list = ref [] in 
		let h_list = ref [] in 
    shuffle_helper deck a_list b_list c_list d_list e_list 
    	f_list g_list h_list; 
    deck := (!b_list) @ (!d_list) @ (!c_list) @ (!a_list) @ (!h_list) @
    	(!f_list) @ (!e_list) @ (!g_list);
    cut ()

	(* [sort_card hand] takes in a hand [hand] and returns a hand, sorted using
	 * List.sort Pervasives.compare on the hand *)
	let sort_cards (h:hand) : hand = 
		List.sort Pervasives.compare h

	(* [filter_suit hand i] takes in a hand [hand] and an int [i] and filters
	 * out all cards from [hand] of number [i]. Returns the filtered hand *)
	let filter_suit (hand: hand) (i: int) : hand = 
	List.(hand |> filter (fun x -> snd x = snd(List.nth hand i)))

	(* BEGIN STRAIGHT CALC *)
	(* [make_enum_rank rank] takes in [rank] of type rank and returns the int
	 * that corresponds to that rank *)
	let make_enum_rank (rank: rank) : int = 
		match rank with 
		| Two -> 2
		| Three -> 3
		| Four -> 4
		| Five -> 5
		| Six -> 6
		| Seven -> 7
		| Eight -> 8
		| Nine -> 9
		| Ten -> 10
		| Jack -> 11
		| Queen -> 12
		| King -> 13
		| Ace -> 14

	(* [make_enum_hand hand acc] Returns sorted an int list in ascending order.
	 * Takes in hand [h] and the int list accumulator acc, generally designed
	 * to take in an empty list that accumulates ints. Returns that int list *)
	let rec make_enum_hand (h: hand) (acc: int list) : int list = 
		match h with 
		|[] -> List.sort Pervasives.compare acc
		|(rank, suit)::t -> make_enum_hand t (make_enum_rank rank::acc)

	let remove_elt e l =
    	let rec go l acc = match l with
	    	| [] -> List.rev acc
	    	| x::xs when e = x -> go xs acc
	    	| x::xs -> go xs (x::acc)
    	in go l []

	let remove_duplicates l =
	    let rec go l acc = match l with
		    | [] -> List.rev acc
		    | x :: xs -> go (remove_elt x xs) (x::acc)
	    in go l []

	(* Computes the max of 4 numbers. *)
  	let four_max a b c d = 
	  	max (max a b) (max c d)

 	(* Returns true if there is a flush or there is a high possibility 
 	 * of a flush. *)
	let flush_bool lst = 
	  	let c1 = ref 0 in let c2 = ref 0 in let c3 = ref 0 in let c4 = ref 0 in 
	  	(for i = 0 to (List.length lst - 1) do 
	  		match (snd (List.nth lst i)) with 
	  		| Clarkson -> c1 := !c1 + 1
	  		| Gries -> c2 := !c2 + 1
	  		| Dijkstra -> c3 := !c3 + 1
	  		| George -> c4 := !c4 + 1
	  	done);
	  	let best_suit = four_max !c1 !c2 !c3 !c4 in 
	  	best_suit = 5
	  	
	(* [flush hand] takes in a hand [hand],
	 * Returns the sorted card list, in ascending order of rank,
	 * containing the flush if there is one (may exceed 5 cards 
	 * if 6 or all cards have same suit). Otherwise, returns the empty list.
	 *)
	let flush_hand (hand: card list) : card list = 
		let h = sort_cards hand in 
		if ((filter_suit h 0) |> List.length) >= 5 then 
			filter_suit h 0
		else if ((filter_suit h 1) |> List.length) >= 5 then 
			filter_suit h 1
		else if ((filter_suit h 2) |> List.length) >= 5 then 
			filter_suit h 2
		else 
			[]

	let flush (hand: card list) : int =
		let card_values =remove_duplicates (make_enum_hand (flush_hand hand) []) in
		match card_values with 
		| _::_::a::b::c::d::e::[] -> 16000000 + e*10000 + d*1000 + c*100 + b*10 + a
		| _::a::b::c::d::e::[] -> 16000000 + e*10000 + d*1000 + c*100 + b*10 + a
		| a::b::c::d::e::[] -> 16000000 + e*10000 + d*1000 + c*100 + b*10 + a
		| _ -> 0
	
	(* [are_consec a b c d e] returns a bool and takes in 5 ints, [a], [b],  
	 * [c], [d], and [e] and checks if they are consecutive, meaning 
	 * consecutively 1 more than the last. If so, then returns true, 
	 * else returns false *)
	let are_consec (a:int) (b:int) (c:int) (d:int) (e:int) : bool =
		if (a+1=b) && (b+1=c) && (c+1=d) && (d+1=e) then true else false  
	
	let remove_dup_rank e l =
  		let rec removing l acc = match l with
    		| [] -> List.rev acc
	    	| h::t when fst e = fst h -> removing t acc
	    	| h::t -> removing t (h::acc)
	  	in removing l []
	
	let remove_dups_ranks l =
	    let rec removing l acc = match l with
	        | [] -> List.rev acc
	        | h::t -> removing (remove_dup_rank h t) (h::acc)
	    in removing l []
	

	let rec remove_nonconsec (l: int list) (e: int) (acc: int list): int list =
		match l with
		| [] -> acc
		| h::[] -> acc @ [h]
		| h1::h2::t -> if h1 = h2 - 1 || e = h1 -1 then 
			remove_nonconsec (h2::t) h1 (acc @ [h1])
			else remove_nonconsec (h2::t) h1 acc

	(* [straight hand] takes in a [hand] of type hand and returns the 
	 * hand of the 5 cards making up the straight, sorted lowest to 
	 * highest *)
	let straight (hand: card list) : int = 
		let len = List.length hand - 1 in 
		let contains_ace = make_enum_rank (fst (List.nth hand len)) = 14 in
		let card_values = remove_duplicates (make_enum_hand hand []) in
		let consec_values = remove_nonconsec card_values 0 [] in
		match consec_values with
		| _::_::10::11::12::13::14::[] 
					when flush hand > 0 -> 1000000000
		| _::10::11::12::13::14::[] 
					when flush hand > 0 -> 1000000000
		| 10::11::12::13::14::[] when flush hand > 0 -> 1000000000
		| _::_::a::b::c::d::e::[] when are_consec a b c d e -> 
								if flush hand > 0 then 20000000 + e else 15000000 + e
		| _::a::b::c::d::e::[] when are_consec a b c d e -> 
								if flush hand > 0 then 20000000 + e else 15000000 + e
		| a::b::c::d::e::[] when are_consec a b c d e -> 
								if flush hand > 0 then 20000000 + e else 15000000 + e
		| a::b::c::d::e::[] when are_consec a b c d e -> 
								if flush hand > 0 then 20000000 + e else 15000000 + e
		| a::b::c::d::e::_::[] when are_consec a b c d e -> 
								if flush hand > 0 then 20000000 + e else 15000000 + e						
		| a::b::c::d::e::_::_::[] when are_consec a b c d e -> 
								if flush hand > 0 then 20000000 + e else 15000000 + e 
		| 2::3::4::5::_::_::_::[] when contains_ace -> if flush hand > 0 
								then 20000005 else 15000005  
		| 2::3::4::5::_::_::[] when contains_ace -> if flush hand > 0 
								then 20000005 else 15000005 
		| 2::3::4::5::[] when contains_ace -> if flush hand > 0 
								then 20000005 else 15000005 
		| _ -> 0

	(* BEGIN 4Kind 3Kind PAIR CALC *)
	let filter_rank (hand: card list) (i: int): card list = 
		List.(hand |> filter (fun x -> fst x = fst(List.nth hand i)))

	let rev_filter_card_rank (hand: card list) (i: int): card list =
		List.(hand |> filter (fun x -> not (make_enum_rank (fst x) = i)))

	(* [four_kind hand] *)
	let rec four_kind_hand (hand: card list): card list = 
		let h = List.rev (sort_cards hand) in
		if (List.length h < 4) then 
			[]
		else 
			match h with 
			|[]-> []
			|h'::t-> 
				if (filter_rank h 0 |> List.length) = 4 
				then filter_rank h 0
				else four_kind_hand t

	let largest_unincluded_element (hand: card list) (i: int): card list =
		let unincluded_list = sort_cards (rev_filter_card_rank hand i) in 
		[List.nth unincluded_list (List.length unincluded_list - 1)] 


	let four_kind (hand: card list) : int =
		let four_cards = (four_kind_hand hand) in
		let other_card_int = 
			match four_cards with
			| f1::f2::f3::f4::[] -> make_enum_rank (fst f1)
			| _ -> 0 
		in
		let card_values = 
		make_enum_hand (sort_cards 
			(four_cards @ largest_unincluded_element hand other_card_int)) [] in
		match card_values with 
		| h::f1::f2::f3::f4::[] when f2 = f1 && f3 = f2 && f4 = f3 -> 
			19000000 + f1 * 1000 + h
		| f1::f2::f3::f4::h::[] when f2 = f1 && f3 = f2 && f4 = f3 -> 
			19000000 + f1 * 1000 + h
		| _ -> 0


		(* [three_kind hand] *)
	let rec three_kind_hand (hand: card list): card list = 
		let h = List.rev (sort_cards hand) in
		if (List.length h < 3) then 
			[]
		else 
			match h with 
			|[]-> []
			|_::t-> 
				if (filter_rank h 0 |> List.length) = 3 then filter_rank h 0 
				else three_kind_hand t

	let complete_three_hand (hand: card list) (i: int): card list =
		let unincluded_list = sort_cards (rev_filter_card_rank hand i) in 
		let len = List.length unincluded_list - 1 in
		[List.nth unincluded_list (len-1); List.nth unincluded_list len] 

	let three_kind (hand: card list) : int =
		let three_cards = three_kind_hand hand in
		let three_int = 
			match three_cards with
			| t1::t2::t3::[] -> make_enum_rank (fst t1)
			| _ -> 0 
		in
		let card_values = make_enum_hand (sort_cards 
			(three_cards @ complete_three_hand hand three_int)) [] in
		match card_values with 
		| h1::h2::t1::t2::t3::[] when t2 = t1 && t3 = t2 -> 
			1000000 * t1 + 100 * h2 + h1
		| h1::t1::t2::t3::h2::[] when t2 = t1 && t3 = t2 -> 
			1000000 * t1 + 100 * h2 + h1
		| t1::t2::t3::h1::h2::[] when t2 = t1 && t3 = t2 -> 
			1000000 * t1 + 100 * h2 + h1
		| _ -> 0

	(* [pair hand] *)
	let rec pair_hand (hand: card list) : card list = 
		let h = List.rev (sort_cards hand) in
		if (List.length h < 2) then 
			[]
		else 
			match h with 
			| []-> []
			| h'::t-> 
				if (filter_rank h 0 |> List.length) = 2 then (filter_rank h 0)
				else pair_hand t

	let complete_pair_hand (hand: card list) (i: int): card list =
		let unincluded_list = sort_cards (rev_filter_card_rank hand i) in 
		let len = List.length unincluded_list - 1 in
		[List.nth unincluded_list (len-2); 
		List.nth unincluded_list (len-1); 
		List.nth unincluded_list len]

	let pair (hand: card list) : int =
		let pair_cards = pair_hand hand in
		let pair_int = 
			match pair_cards with
			| p1::p2::[] -> make_enum_rank (fst p1)
			| _ -> 0 
		in
		let card_values = 
		make_enum_hand (sort_cards 
			(pair_cards @ complete_pair_hand hand pair_int)) [] in
		match card_values with
		| h1::h2::h3::p1::p2::[] when p1 = p2 -> 1500 * p1 + 100 * h3 + 10 * h2 +h1
		| h1::h2::p1::p2::h3::[] when p1 = p2 -> 1500 * p1 + 100 * h3 + 10 * h2 +h1
		| h1::p1::p2::h2::h3::[] when p1 = p2 -> 1500 * p1 + 100 * h3 + 10 * h2 +h1
		| p1::p2::h1::h2::h3::[] when p1 = p2 -> 1500 * p1 + 100 * h3 + 10 * h2 +h1
		| _ -> 0


	(* run pair then pair again  *)
	let two_pair_hand (hand: card list) : card list = 
		let h = List.rev (sort_cards hand) in
		if not (pair_hand h = []) then 
			pair_hand h @ pair_hand 
				(List.(h |> filter (fun x -> fst x <> fst(List.hd (pair_hand h)))))
		else 
			[]

	let complete_two_pair_hand (hand: card list) (ints: int*int): card list =
		let pre_unincluded_list = (rev_filter_card_rank hand (fst ints)) in
		let unincluded_list = 
			sort_cards (rev_filter_card_rank pre_unincluded_list (snd ints)) in
		let len = List.length unincluded_list - 1 in
		[List.nth unincluded_list len]

	let two_pair (hand: card list) : int = 
		(* let card_values = List.rev (make_enum_hand (two_pair_hand hand) []) in *)
		let two_pair_cards = two_pair_hand hand in
		let pair_ints = 
			match two_pair_cards with
			| fp1::fp2::sp1::sp2::[] -> (make_enum_rank (fst fp1), make_enum_rank (fst sp1))
			| _ -> (0, 0)
		in
		let card_values = 
		make_enum_hand (sort_cards 
			(two_pair_hand hand @ complete_two_pair_hand hand pair_ints)) [] in
		match card_values with
		| h::fp1::fp2::sp1::sp2::[] when fp1 = fp2 && sp1 = sp2 -> 
			100000 * sp1 + 100 * fp1 + h
		| fp1::fp2::h::sp1::sp2::[] when fp1 = fp2 && sp1 = sp2 -> 
			100000 * sp1 + 100 * fp1 + h
		| fp1::fp2::sp1::sp2::h::[] when fp1 = fp2 && sp1 = sp2 -> 
			100000 * sp1 + 100 * fp1 + h
		| _ -> 0

	(* full_house [hand] *)
	let full_house_hand (hand: card list) : card list = 
		let h = List.rev (sort_cards hand) in 
		if not (three_kind_hand h = []) then  
			(three_kind_hand h) @ 
			(pair_hand (List.(h |> filter 
				(fun x -> fst x <> fst(List.hd (three_kind_hand h))))))
		else 
			[]

	let full_house (hand: card list) : int =
		let card_values = List.rev (make_enum_hand (full_house_hand hand) []) in
		match card_values with
		| t1::t2::t3::p1::p2::[] when t1 = t2 && t2 = t3 && p1 = p2 -> 
			18000000 + 1000 * t1 + p1
		| p1::p2::t1::t2::t3::[] when t1 = t2 && t2 = t3 && p1 = p2 -> 
			18000000 + 1000 * t1 + p1
		| _ -> 0

	let high_card (hand: card list) : int =
		let card_values = make_enum_hand (sort_cards hand) [] in
		match card_values with
		| _::_::a::b::c::d::e::[] -> 100*e + 50*d + 20*c + 10*b + a
		| _::a::b::c::d::e::[] -> 100*e + 50*d + 20*c + 10*b + a
		| a::b::c::d::e::[] -> 100*e + 50*d + 20*c + 10*b + a
		| a::b::[] -> 10*b + a
		| _ -> 0

	(* [max_of_array bets] takes in an array [bets] and returns the greatest
	 * array element value *)
	let rec max_of_array (bets: 'a array) : 'a =
		let len = Array.length bets in
		match bets with
		| [||] -> 0
		| bets when len = 1 -> Array.get bets 0
		| bets when len > 0 -> 
			let x = Array.get bets 0 in
			let y = max_of_array (Array.sub bets 1 (len - 1)) in
			if x > y then x else y
		| _ -> failwith "wtf is going on"

	(* [index_in_array a e index_acc] takes in array [a], element [e], and the
	 * index accumulator int [index_acc]. returns the index of element e in 
	 * array a. The index_acc should start at 0/the desired starting point *)
	let rec index_in_array (a: 'a array) (e: 'a) (index_acc: int) : int =
		if a.(index_acc) = e 
		then index_acc 
		else index_in_array a e (index_acc + 1)

	(* [index_of_max bets] gets the index of the max element in an int array. 
	 * Takes in the int array [bets] and returns int index of the greatest 
	 * element in the array *)
	let rec index_of_max (bets: int array) : int =
		let max_element = max_of_array bets in
		index_in_array bets max_element 0 

	let score_calculation (hand: card list): int =
		match hand with
		| [] -> 0
		| hand when straight hand = 1000000000 -> straight hand
		| hand when straight hand >= 20000000 -> straight hand
		| hand when four_kind hand >= 19002000 -> four_kind hand
		| hand when full_house hand >= 18002000 -> full_house hand
		| hand when flush hand >= 16000000 -> flush hand
		| hand when straight hand >= 15000000 -> straight hand
		| hand when three_kind hand >= 1000000 -> three_kind hand
		| hand when two_pair hand >= 100000 -> two_pair hand
		| hand when pair hand >= 1500 -> pair hand
		| hand when high_card hand > 0 -> high_card hand
		| _ -> 0 

    (* [score g_state] takes in the global_state [g_state] (POTENTIALLY NEEDS MORE INPUTS)
     * and updates the winning players scores with the points they won from the
 	 * pot. Returns a unit *)
	let score (g_state : global_state) : unit = 
		let max_score = max_of_array g_state.bets in
		let max_index = index_in_array g_state.bets max_score 0 in
		let score_array = Array.make 8 0 in 
		let p_in_list = Array.to_list (g_state.players_in) in
		(for i = 0 to (List.length p_in_list) - 1 do
			(if (List.nth p_in_list i) 
			then Array.set score_array i (score_calculation 
				((List.nth g_state.hands i)@(g_state.cards_in_play))) 
			else ());
		done);
		g_state.scores <- score_array
		(* match p_in_list with
		| a::b::c::d::e::f::g::h::[] -> () 
		| _ -> () *)

		

		(* case 2:  *)

	(* [transition_state g_state] takes in the global_state [g_state] and 
	 * transitions its current_st property to the next current_st of type
	 * current_state. Returns unit*)
	let transition_state (g_state : global_state) : unit = 
		ignore (g_state.c_player <- 0);
		match g_state.current_st with
			| START -> g_state.current_st <- DEAL 
			| DEAL -> g_state.current_st <- BET_ZERO
			| BET_ZERO -> g_state.current_st <- BET_ONE
			| BET_ONE -> g_state.current_st <- BET_TWO
			| BET_TWO -> g_state.current_st <- BET_THREE
			| BET_THREE -> g_state.current_st <- SCORE
			| SCORE -> if g_state.n_players = 1 then 
				g_state.current_st <- END else g_state.current_st <- DEAL
			| END -> failwith "Bad"

	(* [mutable_incr i] increments an int ref [i], returns unit*)
	let mutable_incr (i: int ref) : unit = 
		i := !i + 1

	(* [get_state ()] returns the global_state reference, state *)
	let get_state () : global_state ref = state 


	(* [signal_bet g_state] signals bets to the players. 
	 * Takes in the current_st [g_state], makes necessary updates to g_state,
	 * and then returns unit *)
	let rec signal_bet (g_state : global_state) (* current_player *) = 
		print_endline (""); 
		print_endline ("PLAYER " ^ (string_of_int g_state.c_player));
		let current_bet = g_state.current_bet in
		print_endline ("The pot is " ^ (string_of_int g_state.pot));
		print_endline ("The current bet is " ^(string_of_int current_bet));
		let past_bet_total = (Array.get g_state.bets (g_state.c_player)) in
		print_endline ("You have bet: " ^ (string_of_int past_bet_total));
		let chips_left = Array.get g_state.chips g_state.c_player in
		print_endline ("Player " ^ (string_of_int g_state.c_player) ^ " has " ^ 
			(string_of_int chips_left) ^ " left");
		(if g_state.c_player = 0 then (print_string ("Your hand is: "); 
								printc (List.nth g_state.hands 0);
								print_endline ("Make your bet")) 
			else (print_endline ("Player " ^ (string_of_int g_state.c_player) ^ 
					" is betting:")));
		print_string ("> ");
		let decide_score = if (List.length g_state.cards_in_play) >= 3 
			then (score_calculation ((List.nth g_state.hands g_state.c_player)
									@(g_state.cards_in_play)))     else 0 in
		let bet = if g_state.c_player = 0 then read_int () 
		else decide g_state decide_score in
		(* let bet = read_int () in *)
		print_endline ("THE BET IS: " ^ (string_of_int bet));
		(* must make every other player match the bet, raise, or fold *)
		match bet with
		| bet when (bet = 0) && ((current_bet - past_bet_total) > 0 ) -> 
		(* when the player bets 0 and the bet has been raised, fold *)
					print_endline ("Player " ^ (string_of_int g_state.c_player)
									^ " has folded");
					(* set its players_in value to false *)
					Array.set g_state.players_in g_state.c_player false;
					()
		| bet when (bet > chips_left) && 
							((chips_left + past_bet_total) <= current_bet) ->
		(* when the player bets more than they have and it is not a raise *)
					(* set player's chips value to 0 *)
					Array.set g_state.chips g_state.c_player 0;
					(* increase bets by what the player put in, chips left *)
					Array.set g_state.bets g_state.c_player (past_bet_total
															+ chips_left);
					(* increase pot by what the player put in, chips left *)
					g_state.pot <- g_state.pot + chips_left;
					print_endline ("The pot is " ^ (string_of_int g_state.pot))
					; ()
		| bet when (bet>chips_left) && 
								((chips_left+past_bet_total)>current_bet) ->
		(* when the player bets more than they have and it's a raise *)
					(* set player's chips value to 0 *)
					Array.set g_state.chips g_state.c_player 0;
					(* increase bets by what the player put in, chips left *)
					Array.set g_state.bets g_state.c_player (past_bet_total
															+chips_left);
					(* increase pot by what the player put in, chips left *)
					g_state.pot <- g_state.pot + chips_left;
					(* increases current_bet *)
					g_state.current_bet <- (chips_left + past_bet_total);
					print_endline ("The pot is " ^ (string_of_int g_state.pot))
					; ()
		| bet when (bet<=chips_left) && ((bet+past_bet_total)>current_bet) ->
		(* when bet is a valid raise*)
					(* decrease player's chips value by bet *)
					Array.set g_state.chips g_state.c_player (chips_left-bet);
					(* increase bets by the amount the player bet *)
					Array.set g_state.bets g_state.c_player (past_bet_total
															+ bet);
					(* increases pot by bet *)
					g_state.pot <- g_state.pot + bet;
					(* increases current_bet *)
					g_state.current_bet <- (bet + past_bet_total);
					print_endline ("The pot is " ^ (string_of_int g_state.pot))
					; ()
		| bet when (bet + past_bet_total) = current_bet ->
		(* when bet is a valid match/call *)
					(* decrease a player's chips by value of bet *)
					Array.set g_state.chips g_state.c_player (chips_left-bet);
					(* incease bets by the amount the player bet *)
					Array.set g_state.bets g_state.c_player (past_bet_total
															+ bet);
					(* increases pot by bet *)
					g_state.pot <- g_state.pot + bet;
					print_endline ("The pot is " ^ (string_of_int g_state.pot))
					; ()
		(* this means the player folds *)
		(* if negative # or # less than bet, retry *)
		| _ -> print_endline "Invalid input received. Please try again"; 
							signal_bet g_state

	
	(* [switch g_state] is called in after deal and each bet stage to signal bets
	 * to all the players. Takes in global_state [g_state], returns unit *)
	let switch (g_state : global_state) : unit = 
		match g_state.current_st with 
			| BET_ZERO | BET_ONE | BET_TWO | BET_THREE ->
					signal_bet g_state; 
					g_state.c_player <- (g_state.c_player + 1) 
													mod g_state.n_players;
				(while not (g_state.c_player = index_of_max g_state.bets) do 
					signal_bet g_state;  
					g_state.c_player <- (g_state.c_player + 1) 
													mod g_state.n_players;
				done);
				g_state.c_player <- 0;
				transition_state g_state;
			| _ -> failwith "Bad state"

	(* [deal g_state deck] takes in the current_st [g_state] and the current 
	 * deck ref [deck] and updates the state with the hands. returns unit *)
	let deal (g_state : global_state) (deck: deck ref) : unit =
		(for i = 0 to (g_state.n_players - 1) do
			(* if Array.get g_state.players_in i then *)
				g_state.hands <- 
					([(pop deck); (pop deck)]::(g_state.hands))
			(* else g_state.hands <- []::g_state.hands *)
		done);
		g_state.hands <- List.rev g_state.hands;
		transition_state g_state

	(* [flop g_state] takes in the global_state [g_state] and updates it with
	 * the flop stage of the game. it draws and plays the 3 cards revealed in 
	 * the flop stage. Returns unit *)
	let flop (g_state : global_state) : unit = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	(* [turn g_state] takes in the global_state [g_state] and updates it with
	 * the turn stage of the game. it draws and plays the card revealed in 
	 * the turn stage. Returns unit *)
	let turn (g_state : global_state) : unit = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	(* [river g_state] takes in the global_state [g_state] and updates it with
	 * the river stage of the game. it draws and plays the card revealed in 
	 * the river stage. Returns unit *)
	let river (g_state : global_state) : unit = 
		g_state.cards_in_play <- pop deck::g_state.cards_in_play;
		printc (g_state.cards_in_play)

	let find_winners (g_state: global_state) : int array = 
		let winner = max_of_array (g_state.scores) in 
		let winner_index = index_of_max (g_state.scores) in 
		let winner_list = Array.make (Array.length g_state.scores) (-1) in 
		(for i = winner_index to (Array.length g_state.scores - 1) 
			do 
			if (Array.get g_state.scores i) = winner 
				then winner_list.(i) <- i
			else ()
		 done); winner_list

	let award_chips (g_state: global_state) = 
		let winners_list = Array.to_list (find_winners (g_state)) in 
		let num_winners = 
			List.length (List.filter (fun x -> x <> (-1)) winners_list) in
		let rec award lst g_state= 
			match lst with 
			|[]-> ()
			|h::t -> 
				if h <= g_state.n_players - 1 then
					if h <> (-1) then 
						((g_state.chips).(h) <- 
						(g_state.pot/num_winners)+ (g_state.chips).(h); 
						award t g_state)
					else (award t g_state)
				else ()
				in 
		award winners_list g_state

	let reinitialize_game (g_state: global_state): unit =
    	g_state.bets <- Array.make g_state.n_players 0;
    	g_state.scores <- Array.make g_state.n_players 0;
    	g_state.players_in <- Array.make g_state.n_players true;
    	g_state.cards_in_play <- [];
		g_state.hands <- [];
		g_state.pot <- 0;
		g_state.current_bet <- 0;
		if (g_state.n_players = 1) then raise GameEnded else ()

	let remove_players (g_state: global_state) = 
		(* update chips *)
		if (g_state.chips.(0) <> 0) then
			let chip_lst = Array.to_list g_state.chips in 
			let removed_chips = List.filter (fun x -> x <> 0 || x < 0) chip_lst in 
			g_state.chips <- Array.of_list removed_chips;
			(* update n_players *)
			let num_in_game = List.length removed_chips in 
			g_state.n_players <- num_in_game
		else 
			raise GameEnded

	let end_message (g_state: global_state) = 
		print_endline("-----------------------------End of the Game");
		if (Array.get g_state.chips 0 = 0) then 
			print_endline "You lost! Type 'make play' to play again"
		else 
			print_endline "Congratulations on winning!"

	(* [game_loop g_state] takes in the global_state [g_state] and updates it
	 * as the game goes along. Terminates when a player has won the game
	 *  *)
	let rec game_loop (g_state : global_state) : unit = 
		(* Transition to the deal stage. Deal. *)
		transition_state g_state;
		deal g_state deck; 

		(* Bet after dealing. BET_ZERO state, transition to flop *)
		print_endline("-----------------------------Start of Initial Bet");
		switch g_state;
		print_endline("-----------------------------Finishing Initial Bet");
		print_endline("");
		(* FOR NOW: Deal flop, then bet (taken care of in flop). Move on to turn *)
		print_endline("-----------------------------Start of Flop");
		flop g_state;
		switch g_state;
		print_endline("-----------------------------Finishing Flop Betting");
		print_endline("");
		(* Drop the turn, bet. *)
		print_endline("-----------------------------Start of Turn");
		turn g_state;
		switch g_state;
		print_endline("-----------------------------Finishing Turn Betting");
		print_endline("");

		(* Drop the river, bet. *)
		print_endline("-----------------------------Start of River");
		river g_state;
		switch g_state;
		print_endline("-----------------------------Finishing River Betting");
		print_endline("");

		(* Now, we are in the score stage. *)
		print_endline("-----------------------------Final Results");
		print_endline("");
		print_endline("The cards in play:");
		printc (g_state.cards_in_play);
		(for i = 0 to g_state.n_players - 1 do
			if Array.get g_state.players_in i = true 
			then (print_endline ("");
				print_endline ("Player " ^ (string_of_int i) ^"'s hand:");
				printc (List.nth g_state.hands i))
			else ()
		done);
		score g_state;

		(* At this point, we want to do a few thing while not touching the 
		 * state since the outter loop will need it to determine whether or 
		 * not the game has ended. Score needs to allocate chips to whoever won
		 * them. *)
		award_chips g_state;
		remove_players g_state;
		reinitialize_game g_state;
		reset_deck (); shuffle (); shuffle (); shuffle (); shuffle (); shuffle (); 
		print_endline("-----------------------------End of the Round");
		print_endline ("")

	let init () = 
		try 
			(* Initial state is set to Start. There are no players, no cards,
       * no bets. Read in number of players. *)
      print_endline ("Welcome to Cornell Hold'Em! Enter the number of people 
      				you'd like in the game. No more than 8, no less than 2!");
      let input_int = read_int () in
      let players = if input_int > 8 || input_int < 2 
      		then (print_endline ("Idiot."); 8) 
  			else input_int in
      !state.n_players <- players;
      !state.bets <- Array.make players 0;
      !state.chips <- Array.make players 5000;
      !state.scores <- Array.make players 0;
      !state.players_in <- Array.make players true;


      (* Shuffle deck. *)
      reset_deck (); shuffle (); shuffle (); shuffle (); shuffle ();

      (while (!state.current_st <> END) do game_loop !state done); 
      raise GameEnded

		with 
			| GameEnded -> end_message !state
			| _ -> failwith "This is bad"

end 