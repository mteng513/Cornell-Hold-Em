(* AI opponent module. The AI will make probabilistic
 * decisions on whether or not to bet, in an effort 
 * to deceive the player. It will still use the Player
 * interface. *)

module Opponent : sig 

	(* Identifier for the user set difficulty. *)
	type difficulty

	(* Because we include the player interface, we
	 * have access to the hand. We will also be able
	 * to see all the card currently in play. At each
	 * betting round, the opponents will probabilistically
	 * decide whether or not to bet in a psuedo-random
	 * (unpredictable) way. The only difference between
	 * the player and the AI is that the AI needs to make
	 * the betting decisions. *)
	val decide : unit -> (bool * int) 




end