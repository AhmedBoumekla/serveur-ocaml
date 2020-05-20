 
 #require "lwt.unix";;
 #require "base";;
 #require "stdio";;
 #require "lwt" ;;
 
 open Base
 
 (* open Lwt *)
 
 type player = {input: Lwt_io.input_channel; output: Lwt_io.output_channel}
 
 let (>>=) = Lwt.(>>=)
 let return = Lwt.return
 
 
 let getPlayers n : player list Lwt.t =
   let sockaddr = Lwt_unix.ADDR_INET (UnixLabels.inet_addr_loopback, 3003) in
   let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
   Lwt_unix.set_close_on_exec sock ;
   Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true ;
   Lwt_unix.bind sock sockaddr >>= fun () ->
   Lwt_unix.listen sock 3003 ;
   let rec acceptPlayer n acc : player list Lwt.t =
     if n > 0 then
       let pt =
         Lwt_unix.accept sock >>= fun (cliFD, _sock) ->
         let inputChan = Lwt_io.of_fd ~mode:Lwt_io.input cliFD in
         let outputChan = Lwt_io.of_fd ~mode:Lwt_io.output cliFD in
         {input=inputChan; output= outputChan} |> return
       in
       pt >>= fun p ->
       acceptPlayer (n - 1) (p :: acc)
     else
       acc |> return
   in
   acceptPlayer n []
 
 
 
 let closePlayers listPlayers =
   Lwt_list.map_p
     (fun  player -> Lwt_io.close player.input)
     listPlayers
     ;;
 
 
 (*fonction qui prend un string et qu'il envoi a la liste des joueur  *)
 let sendMsg string (l : 'a list) =  
     Lwt_list.map_p (fun player ->  (Lwt_io.fprintf player.output "%s"string )>>=fun()-> Lwt_io.flush player.output
     >>=fun()->return player ) l
     >>= fun l -> return l 
     
 
 (* la fonction get answer qui prend une liste de player et renvoi une liste de couple (player,string lwt) *)
 let get_answer (l: player list)  = 
   let rec get_answer_rec l acc = 
     match l with 
       |[]->return acc 
       |p::ll -> get_answer_rec ll ((p,(Lwt_io.read_line(p.input))) :: acc)
   in return (get_answer_rec l [] );;
 
   
 (*  la fonction filter_winner prend un string et une liste de couple (player,string lwt ) et renvoi une liste de player 
 qui ont la bonne rep je n'utilise pas la fonction map j'ai fait manuellement avec match *)
 let filter_winner (rep : string) (l ) = 
   let rec filter_winner_rec rep l acc = l >>= fun l ->
     match l with 
     |[]->return acc
     |e::l ->match e with 
             |(p,s)-> s >>= fun res-> 
                     if String.((=)) res rep then filter_winner_rec rep (return l) (p::acc)
                     else filter_winner_rec rep (return l) acc 
   
   in filter_winner_rec rep l [] 
   ;;
 
   
 let filter_fastest (l) = 
     let pre_filter l acc = match l with
       |[]-> return acc
       |e::ll -> return (e::acc)
     in pre_filter l [];;
 
 
 
 let filter_fastest_correct rep l = 
     let rec pre_filter_corr rep l acc = match l with 
       |[]-> return acc 
       |p::ll -> (Lwt_io.read_line(p.input)) >>= fun s ->
                 if String.((=)) s rep then return (p::acc)
                 else pre_filter_corr rep ll acc
     in pre_filter_corr rep l []
   ;;
 
 (* 
 let filter_fastest (l: 'a list) = 
   [Lwt.choose(List.map (l) (fun player -> player))]
   ;;
 *)
 
 (*
 let filter_fastest_correct (s : string) (l : player list) = 
   [Lwt.choose(List.map l (
       fun player -> if String.((=)) s (Lwt_io.read_line(player.input)) then return player 
       ))]
   ;;
 *)
 
         
 let _ =
   Lwt_main.run
 
     (* création des player *)
     (
       (Lwt_io.fprintf Lwt_io.stderr "Attente des joueurs...\n") >>=
       fun () -> let threadListPlayers = getPlayers 2 in
      (* actions *)
       threadListPlayers >>=
       fun listPlayers -> 
 
       sendMsg "Ocaml est assez cool : vrai ou faux ?" listPlayers >>= get_answer >>= filter_winner "vrai"
       >>=sendMsg "Javascript est mieux qu'Ocaml: vrai ou faux ?" >>=
       get_answer  >>=
       filter_winner "faux" >>=
       sendMsg "Question de rapidité, avec quoi programment les vrais programmeurs : 1) nano, 2) emacs, 3) vi, 4) des papillons ?"
        >>=
        filter_fastest_correct "4">>=sendMsg "Bravo !!"
        >>=fun _ -> threadListPlayers 
       >>= closePlayers
      
      
     )
