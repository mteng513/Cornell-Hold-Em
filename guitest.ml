open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()
let draw_n_players () =
    let difwindow = GWindow.window ~width:640 ~height:480
                              ~title:"Choose Difficulty" () in
    let difvbox = GPack.vbox ~packing:difwindow#add () in
    difwindow#connect#destroy ~callback:Main.quit;

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:difvbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

      (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

    let opp1 = GButton.button ~label:"vs 1 CPU"
          ~packing:difvbox#add () in
    opp1#connect#clicked ~callback: (fun () -> prerr_endline "init_game 1");
    let opp2 = GButton.button ~label:"vs 2 CPUs"
          ~packing:difvbox#add () in
    opp2#connect#clicked ~callback: (fun () -> prerr_endline "init_game 2");
    let opp3 = GButton.button ~label: "vs 3 CPUs"
          ~packing:difvbox#add () in
    opp3#connect#clicked ~callback: (fun () -> prerr_endline "init_game 3");
    let opp4 = GButton.button ~label:"vs 4 CPUs"
          ~packing:difvbox#add () in
    opp4#connect#clicked ~callback: (fun () -> prerr_endline "init_game 4");
    let opp5 = GButton.button ~label:"vs 5 CPUs"
          ~packing:difvbox#add () in
    opp5#connect#clicked ~callback: (fun () -> prerr_endline "init_game 5");
    let opp6 = GButton.button ~label: "vs 6 CPUs"
          ~packing:difvbox#add () in
    opp6#connect#clicked ~callback: (fun () -> prerr_endline "init_game 6");
    let opp7 = GButton.button ~label:"vs 7 CPUs"
          ~packing:difvbox#add () in
    opp7#connect#clicked ~callback: (fun () -> prerr_endline "init_game 7");

    difwindow#add_accel_group accel_group;
    difwindow#show ();
    Main.main ()

let draw_difficulty () =
    let difwindow = GWindow.window ~width:640 ~height:480
                              ~title:"Choose Difficulty" () in
    let difvbox = GPack.vbox ~packing:difwindow#add () in
    difwindow#connect#destroy ~callback:Main.quit;

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:difvbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in

      (* File menu *)
    let factory = new GMenu.factory file_menu ~accel_group in
    factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

    let diff1 = GButton.button ~label:"Easy"
      ~packing:difvbox#add () in
    diff1#connect#clicked ~callback: (fun () -> difwindow#destroy (); draw_n_players ();
    prerr_endline "Medium is the only difficult supported at this time");
    let diff2 = GButton.button ~label:"Medium"
      ~packing:difvbox#add () in
    diff2#connect#clicked ~callback: (fun () -> difwindow#destroy (); draw_n_players ());
    let diff3 = GButton.button ~label: "Hard"
      ~packing:difvbox#add () in
    diff3#connect#clicked ~callback: (fun () -> difwindow#destroy (); draw_n_players ();
    prerr_endline "Medium is the only difficult supported at this time");

    difwindow#add_accel_group accel_group;
    difwindow#show ();
    Main.main ()

let main () =
    let window = GWindow.window ~width:640 ~height:480
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

    let startbutton = GButton.button ~label:"Start"
          ~packing:vbox#add () in
    startbutton#connect#clicked ~callback: (fun () -> window#destroy ();
    draw_difficulty ());

    window#add_accel_group accel_group;
    window#show ();
    Main.main ()

let () = main ()