(* Ml file for opponent *)
open Player
open Gengine

module Opponent = struct 

	open Game_Engine
	open Player

  type difficulty = One | Two | Three 

  let suit_helper (c1 : rank) (c2 : rank) = 
  	match (c1, c2) with 
  		| (l, r) when (l = r) ->
  			(match l with 
  				| Ace -> (true, (Random.int 800))
  				| King -> (true, (Random.int 775))
  				| Queen -> (true, (Random.int 760))
  				| Jack -> (true, (Random.int 725))
  				| Ten -> (true, (Random.int 675))
  				| Nine -> (true, (Random.int 550))
  				| Eight -> (true, (Random.int 475))
  				| Seven -> (true, (Random.int 400))
  				| Six -> (true, (Random.int 350))
  				| Five -> (true, (Random.int 300))
  				| Four -> (true, (Random.int 200))
  				| Three -> (true, (Random.int 125))
  				| Two -> (true, (Random.int 100)))

  		| l, r when (l <> r) -> 
  			(match (l, r) with 
  				| (King, Ace) -> (true, (Random.int 600))
  				| (Queen, Ace) -> (true, (Random.int 400))
  				| (Jack, Ace) -> (true, (Random.int 200))
  				| (Ten, Ace) -> (true, (Random.int 160))
  				| (Nine, Ace) -> (true, (Random.int 100))
  				| (Eight, Ace) -> (true, (Random.int 150))
  				| (Queen, King) -> (true, (Random.int 400))
  				| (Jack, King) -> (true, (Random.int 300))
  				| (Ten, King) -> (true, (Random.int 275))
  				| (Nine, King) -> (true, (Random.int 180))
  				| (Jack, Queen) -> (true, (Random.int 300))
  				| (Ten, Queen) -> (true, (Random.int 300))
  				| (Ten, Jack) -> (true, (Random.int 350))
  				| (Nine, Ten) -> (true, (Random.int 300))
  				| (Eight, Ten) -> (true, (Random.int 275))
  				| (Seven, Ten) -> (true, (Random.int 250))
  				| (Six, Ten) -> (true, (Random.int 200))
  				| (Eight, Nine) -> (true, (Random.int 260))
  				| (Seven, Nine) -> (true, (Random.int 180))
  				| (Six, Nine) -> (true, (Random.int 140))
  				| (Seven, Eight) -> (true, (Random.int 120))
  				| (Six, Eight) -> (true, (Random.int 100))
  				| (Six, Seven) -> (true, (Random.int 80))
  				| (Five, Seven) -> (true, (Random.int 60))
  				| (Five, Six) -> (true, (Random.int 50))
  				| (Four, Five) -> (true, Random.int 30)
  				| _ -> (true, 20))

  		| _ -> failwith "Bad"

  let unsuit_helper (c1 : rank) (c2 : rank) = 
  	match (c1, c2) with 
  		| (l, r) when (l = r) -> 
  			(match l with 
  				| Ace -> (true, (Random.int 400))
  				| King -> (true, (Random.int 375))
  				| Queen -> (true, (Random.int 360))
  				| Jack -> (true, (Random.int 325))
  				| Ten -> (true, (Random.int 375))
  				| Nine -> (true, (Random.int 350))
  				| Eight -> (true, (Random.int 375))
  				| Seven -> (true, (Random.int 300))
  				| Six -> (true, (Random.int 250))
  				| Five -> (true, (Random.int 200))
  				| Four -> (true, (Random.int 100))
  				| Three -> (false, 0)
  				| Two -> (false, 0))

  		| (l, r) when (l <> r) ->
  			(match (l, r) with 
  				| (Ace, King) -> (true, (Random.int 300))
  				| (Ace, Queen) -> (true, (Random.int 200))
  				| (Ace, Jack) -> (true, (Random.int 100))
  				| (Ace, Ten) -> (true, (Random.int 80))
  				| (Ace, Nine) -> (true, (Random.int 50))
  				| (Ace, Eight) -> (true, (Random.int 30))
  				| (King, Queen) -> (true, (Random.int 200))
  				| (King, Jack) -> (true, (Random.int 175))
  				| (King, Ten) -> (true, (Random.int 150))
  				| (King, Nine) -> (true, (Random.int 90))
  				| (Queen, Jack) -> (true, (Random.int 150))
  				| (Queen, Ten) -> (true, (Random.int 150))
  				| (Jack, Ten) -> (true, (Random.int 250))
  				| (Ten, Nine) -> (true, (Random.int 150))
  				| (Ten, Eight) -> (true, (Random.int 130))
  				| (Ten, Seven) -> (true, (Random.int 110))
  				| (Ten, Six) -> (true, (Random.int 100))
  				| (Nine, Eight) -> (true, (Random.int 130))
  				| (Nine, Seven) -> (true, (Random.int 115))
  				| _ -> (false, 0))

  		| _ -> failwith "Bad"
  				
  let b0_helper2 card1 card2 = 
  	match card1, card2 with
  		| (c1, s1), (c2, s2) when (s1 = s2) -> 
  			(match (suit_helper c1 c2) with 
  				| (true, amt) -> (if Random.int 2700 mod 5 = 0 then check () 
  					else ignore (bet amt))
  				| _ -> check ())

  		| (c1, s1), (c2, s2) when (s1 <> s2) -> 
  			(match (unsuit_helper c1 c2) with 
  				| (true, amt) -> (if Random.int 2700 mod 3 = 0 then check () 
  					else ignore (bet amt))
  				| _ -> (if Random.int 25 mod 2 = 0 then fold () else check ())) 

  		| _ -> ignore (bet (Random.int 100))

  let bet_zero_helper hand = 
  	match hand with 
  		| (c1, c2) -> b0_helper2 c1 c2

  let four_max a b c d = 
  	let max = ref a in 
    if (b > !max) then 
        max := b else 
    if (c > !max) then 
        max := c else 
    if (d > !max) then 
        max := d
    else (failwith "Bad");
 		!max

  let flush lst = 
  	let c1 = ref 0 in let c2 = ref 0 in let c3 = ref 0 in let c4 = ref 0 in 
  	(for i = 0 to (List.length lst) do 
  		match (snd (List.nth lst i)) with 
  			| Clarkson -> c1 := !c1 + 1
  			| Gries -> c2 := !c2 + 1
  			| Dijkstra -> c3 := !c3 + 1
  			| George -> c4 := !c4 + 1
  		done);
  	let best_suit = four_max !c1 !c2 !c3 !c4 in 
  	match best_suit with
  		| (5) -> true
  		| (4) -> true
  		| _ -> false

  let flop_better cardlst = 
  	if (flush cardlst) then ignore (bet (Random.int 750))
  	else check ()


  let bet_one_helper hand play_cards = 
  	flop_better (hand @ play_cards)

  let decide () = 
  	let st = get_state () in 
  	match !st.current_st with
  		| BET_ZERO -> (false, 0)
  		| _ -> (false, 0)



end 