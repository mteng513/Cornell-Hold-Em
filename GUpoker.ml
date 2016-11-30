open GMain
open GdkKeysyms

(* Ml file for the GUpoker *)
module GU_Poker = struct

	let clarksonAce = GMisc.image
		~file:"../CHoldEmImgs/Classic/c01.png" ()
	let clarkson2 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c02.png" ()
	let clarkson3 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c03.png" ()
	let clarkson4 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c04.png" ()
	let clarkson5 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c05.png" ()
	let clarkson6 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c06.png" ()
	let clarkson7 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c07.png" ()
	let clarkson8 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c08.png" ()
	let clarkson9 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c09.png" ()
	let clarkson10 = GMisc.image
		~file:"../CHoldEmImgs/Classic/c10.png" ()
	let clarksonJack = GMisc.image
		~file:"../CHoldEmImgs/Classic/c11.png" ()
	let clarksonQueen = GMisc.image
		~file:"../CHoldEmImgs/Classic/c12.png" ()
	let clarksonKing = GMisc.image
		~file:"../CHoldEmImgs/Classic/c13.png" ()

	let griesAce = GMisc.image
		~file:"../CHoldEmImgs/Classic/d01.png" ()
	let gries2 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d02.png" ()
	let gries3 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d03.png" ()
	let gries4 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d04.png" ()
	let gries5 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d05.png" ()
	let gries6 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d06.png" ()
	let gries7 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d07.png" ()
	let gries8 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d08.png" ()
	let gries9 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d09.png" ()
	let gries10 = GMisc.image
		~file:"../CHoldEmImgs/Classic/d10.png" ()
	let griesJack = GMisc.image
		~file:"../CHoldEmImgs/Classic/d11.png" ()
	let griesQueen = GMisc.image
		~file:"../CHoldEmImgs/Classic/d12.png" ()
	let griesKing = GMisc.image
		~file:"../CHoldEmImgs/Classic/d13.png" ()

	let hAce = GMisc.image
		~file:"../CHoldEmImgs/Classic/h01.png" ()
	let h2 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h02.png" ()
	let h3 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h03.png" ()
	let h4 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h04.png" ()
	let h5 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h05.png" ()
	let h6 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h06.png" ()
	let h7 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h07.png" ()
	let h8 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h08.png" ()
	let h9 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h09.png" ()
	let h10 = GMisc.image
		~file:"../CHoldEmImgs/Classic/h10.png" ()
	let hJack = GMisc.image
		~file:"../CHoldEmImgs/Classic/h11.png" ()
	let hQueen = GMisc.image
		~file:"../CHoldEmImgs/Classic/h12.png" ()
	let hKing = GMisc.image
		~file:"../CHoldEmImgs/Classic/h13.png" ()

	let georgeAce = GMisc.image
		~file:"../CHoldEmImgs/Classic/s01.png" ()
	let george2 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s02.png" ()
	let george3 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s03.png" ()
	let george4 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s04.png" ()
	let george5 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s05.png" ()
	let george6 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s06.png" ()
	let george7 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s07.png" ()
	let george8 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s08.png" ()
	let george9 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s09.png" ()
	let george10 = GMisc.image
		~file:"../CHoldEmImgs/Classic/s10.png" ()
	let georgeJack = GMisc.image
		~file:"../CHoldEmImgs/Classic/s11.png" ()
	let georgeQueen = GMisc.image
		~file:"../CHoldEmImgs/Classic/s12.png" ()
	let georgeKing = GMisc.image
		~file:"../CHoldEmImgs/Classic/s13.png" ()

	(* Draw difficulty box *)
	let draw_difficulty () = ()

	(* Draw number of players box that will allow the
	 * user to enter the number of players *)
	let draw_n_players () =
		failwith "Unimplemented"

	(* Sends the number of players to the GUI, indicating
	 * that the user has started the game *)
	let init_game i =
		failwith "Unimplemented"

	(* Draws players hand *)
	let draw_cards () =
		failwith "Unimplemented"

	(* Draws cash box for player. Takes in the amount of
	 * cash the player currently has. *)
	let draw_balance amt =
		failwith "Unimplemented"

	(* Draws pot amount. Takes in the new amount in
	 * the pot. *)
	let draw_pot amt =
		failwith "Unimplemented"

	(* Indicates the player has folded *)
	let draw_fold () =
		failwith "Unimplemented"

	(* Draws the Flop *)
	let draw_flop () =
		failwith "Unimplemented"

	(* Draws the turn *)
	let draw_turn () =
		failwith "Unimplemented"

	(* Draws the river *)
	let draw_river () =
		failwith "Unimplemented"

	(* Draws the win box if you win *)
	let draw_w () =
		failwith "Unimplemented"

	(* Draws lose box if you lose *)
	let draw_l () =
		failwith "Unimplemented"

	(* Draws win if you win the whole game *)
	let draw_win () =
		failwith "Unimplemented"

	(* Draws lose if you lose the whole game *)
	let draw_lose () =
		failwith "Unimplemented"

	(* Draws main window for the game *)
	let draw_start () =

	let locale = GtkMain.Main.init () in ()

	(* Moved this down here, will need functions above. *)
	let main () =
  	let window = GWindow.window ~width:320 ~height:240
    	                          ~title:"Cornell Hold Em" () in
  	let vbox = GPack.vbox ~packing:window#add () in
  	window#connect#destroy ~callback:Main.quit;

  	(* Menu bar *)
  	let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  	let factory = new GMenu.factory menubar in
  	let accel_group = factory#accel_group in
  	let file_menu = factory#add_submenu "File" in

  	(* File menu *)
  	let factory = new GMenu.factory file_menu ~accel_group in
  	factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

			(* Draws the start button *)
			let draw_start_button () =
				let button = GButton.button ~label:"Start"
            ~packing:vbox#add () in
  				button#connect#clicked ~callback: (fun () -> ()) in ()

end