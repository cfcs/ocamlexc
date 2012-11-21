(***********************************************************************)
(*                                                                     *)
(*                           Ocamlexc                                  *)
(*                                                                     *)
(*        Francois Pessaux, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)



open Tk ;;


let select_module_bnd parent_w listb_w =
 Toplevel.configure parent_w ~cursor:(`Xcursor "watch") ; update () ;
 begin
 match Listbox.curselection listb_w with
  | [] -> ()
  | h :: _ ->
      let mod_name = Listbox.get listb_w h in
      let mod_ident = Ident.create_global mod_name in
      let mod_path = Path.Pident mod_ident in
      let module_source = List.assoc mod_name !Global.modules_sources in
      let new_window1 = Toplevel.create parent_w in
      let new_window2 = Toplevel.create parent_w in
      Rootwindow.create new_window1 mod_path
                        module_source Global.Syntaxdisplay ;
      Rootwindow.create new_window2 mod_path module_source Global.Typedisplay
 end ;
 Toplevel.configure parent_w ~cursor:(`Xcursor "") ; update () ;
;;



let create parent_w =
 Wm.title_set parent_w "Compilations units" ;
 (* Containers *)
 let frame2_w = Frame.create parent_w in
 let frame0_w = Frame.create parent_w in
 let frame1_w = Frame.create parent_w in
 (* Menu *)
 let mbut0_w = Menubutton.create frame2_w ~text:"Misc"
					  ~underline:0 in
 let menu0_w = Menu.create mbut0_w in
 Menu.add_command menu0_w ~label:"Preferences"
			  ~command:(fun _ -> Preferences.open_pref_window parent_w)
			  ~underline:0 ;
 Menubutton.configure mbut0_w ~menu:menu0_w ;
 (* Listbox containing all linked files *)
 let listb0_w = Listbox.create frame0_w ~width:30 ~height:10 in
 (* Scrollbar for this listbox *)
 let scroll0_w = Scrollbar.create frame0_w ~orient:`Vertical in
 (* Quit button *)
 let butt0_w = Button.create frame1_w
                             ~text:"Quit" ~command:(fun _ -> exit 0) in
 (* Link scrollbar and listbox together *)
 Listbox.configure listb0_w ~yscrollcommand:(Scrollbar.set scroll0_w) ;
 Scrollbar.configure scroll0_w ~command:(Listbox.yview listb0_w) ;
 (* Initialize listbox content *)
 let mod_names = List.map
   (fun n -> String.capitalize (Filename.basename (Filename.chop_extension n)))
   !Global.link_files in
 Listbox.insert listb0_w `End mod_names ;
 (* Set bindings for the listbox *)
 bind listb0_w ~events:[`Modified ([`Double], `ButtonPressDetail 1)]
      ~action:(fun _ -> select_module_bnd parent_w listb0_w) ;
 bind listb0_w ~events:[`KeyPressDetail "Return"]
      ~action:(fun _ -> select_module_bnd parent_w listb0_w) ;
 (* Set the focus *)
 Focus.set listb0_w ;
 Listbox.activate listb0_w ~index:(`Num 0) ;
 Listbox.selection_set listb0_w ~first:(`Num 0) ~last:(`Num 0) ;
 (* Finally pack the whole stuff *)
 pack [frame2_w] ~side:`Top ~fill:`Both ;
 pack [frame0_w] ~side:`Top ~expand:true ~fill:`Both ;
 pack [frame1_w] ~side:`Top ~fill:`X ;
 pack [mbut0_w] ~side:`Right ;
 pack [listb0_w] ~side:`Left ~expand:true ~fill:`Both ;
 pack [scroll0_w] ~expand:true ~fill:`Y ;
 pack [butt0_w] ~fill:`X
;;
