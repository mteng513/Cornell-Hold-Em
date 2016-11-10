(* Main play interface, provides the betting interface that
 * will be sufficient for players, but will need to be extended
 * upon for the AI players. *)

module type Player = sig 
	
	(* Hand type for the better. Consists of two cards. *)
	type hand

	(* Amount of money remaining to be bet. Player loses if they
	 * hit 0. *)
	type cash

	(* Bet amt passed in. Returns true if bet succeeded, false
	 * otherwise. Bet must equal or exceed all other bets. *)
	val bet : int -> bool 

	(* Call the amount that has currently been bet. Returns true
	 * if bet has succesfully been called. Player will go all in
	 * to call if need be. *)
	val call : unit -> bool

	(* Fold function that indicates that this player has folded. *)
	val fold : unit -> unit 

	(* Check function that indicates the player does not wish to bet. *)
	val check : unit -> unit

end