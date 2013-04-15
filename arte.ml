
(*Pour le REPL*)

(*#require "netclient";;
#require "xml-light";;
#require "extlib";;
*)
type def_debug = { mutable debug : bool};;
let isdebug = { debug = false };;

let execute_commande command = 
  let lines = ref "" in
let chan = Unix.open_process_in command in
        print_endline command;
     try
      while true; do
        lines := !lines^(input_line chan)^"\n"
      done; !lines
    with End_of_file -> 
      ignore(Unix.close_process_in chan);         
      !lines;;

let match_expre_reguliere rex str = 
  let regexp = Netstring_pcre.regexp rex in
  let resultas = Netstring_pcre.full_split regexp str in
      match resultas with
        | [Netstring_pcre.Text s]       -> (false,[])
        | (Netstring_pcre.Delim s)::queue   -> (true,List.map (fun elem -> match elem with 
                                                                      | Netstring_pcre.Group (nbr,st) -> st
                                                                      | _ -> "") queue)
        | _                            -> (false,[]);;

let replace ch ~pattern:regex ~par:remplacement = Netstring_pcre.global_replace (Netstring_pcre.regexp regex ) remplacement ch

 
(*TODO faire en sorte que ça crache ce qu'on cherche, pas l'élément*)
let rec collect_arbre f lst arbre = 
match arbre with 
| Xml.Element ( a , b, lst) as el -> if f el then el::(List.flatten(List.map (collect_arbre f lst) lst)) else List.flatten(List.map (collect_arbre f lst) lst)
| Xml.PCData a -> [];;


let rec find_item_videos el = match el with  
| Xml.Element("item", [], Xml.Element ("title", [], [Xml.PCData titre])::infos) -> true,titre, infos  
| _ -> false, "", [Xml.PCData ""];;


let rec find_item_videos_bool el = match el with  
| Xml.Element("item", [], Xml.Element ("title", [], [Xml.PCData titre])::infos) -> true
| _ -> false;;

let find_videos arb = collect_arbre find_item_videos_bool [] arb;;


type video_arte = {
	titre 		: string;
	description 	: string;
	urlHtml		: string;
	date 		: string;
};;

let dbg s = if isdebug.debug then print_endline s else ();;
let get s = dbg s; execute_commande ("curl -s '"^s^"'");;
(*ETAPE 1 : XML DE BASE*)
let xmlListeVideos() = 
	let brut =  get  "http://videos.arte.tv/fr/do_delegate/videos/index--3188698,view,asCoverflowXml.xml?hash=/tv/coverflow///1/120/" in
        let _ = "dbg get  http://videos.arte.tv/fr/do_delegate/videos/index--3188698,view,asCoverflowXml.xml?hash=/tv/coverflow///1/120/" in
	Xml.parse_string brut;;



(*ETAPE 1 : XML DE BASE, ON CHERCHER LES URL HTML POUR TROUVER LE XML 2*)
let find_element_video  el  =
match el with
| Xml.Element ("video", [] , b ) -> true
| _ 			       -> false;;

(*
 *Revoir le parse avec ça :
         <video>
         <programType> ARTE+7 </programType>
         <stickerUrl>
         /image/web/i18n/view/arte7-over_2-3445012-standardTeaserData-4943539.png
         </stickerUrl>
         <title> Judith Milberg - Design fait maison </title>
         <teaserText>
         La styliste Judith Milberg transforme les vieilleries en objets décalés
         et poétiques. Aujourd'hui: Le fauteuil surprise.
         </teaserText>
         <imageUrl>
         /image/web/i18n/view/14-08-12-milberg-jpg_1-6861756-imageData-5113104.jpg
         </imageUrl>
         <targetUrl>
         /fr/videos/judith-milberg-design-fait-maison--6856278.html
         </targetUrl>
         <startDate> Aujourd'hui, 06h46 </startDate>
         <endDate> 2012-09-12T06:46:26 </endDate>
         <addToPlaylistUrl>
         /fr/do_addToPlaylist/videos/judith-milberg-design-fait-maison--6856512.html
         </addToPlaylistUrl>
         <duration> 26:05 </duration>
         </video>
 *
 * *)

let renvoi_structure_video el =
match el with
| Xml.Element ("video", [] , pgrmtype::stycker::(Xml.Element ("title", [], [Xml.PCData titre]))::(Xml.Element("teaserText", [],[Xml.PCData description]))::
	imageurl::(Xml.Element("targetUrl",[],[Xml.PCData lien]))::(Xml.Element("startDate",[],[Xml.PCData date]))::reste  ) ->
   { 
        titre           = replace titre ~pattern:"&#039;" ~par:"'";
	description 	= replace description ~pattern:"&#039;" ~par:"'";
	urlHtml 	= "http://videos.arte.tv"^lien;
	date    	= date;
 }
| _ ->  { 
	titre 		= "";
	description 	= "";
	urlHtml 	= "";
	date    	= "";
 };;



let construit_liste_videos () = List.map renvoi_structure_video (collect_arbre find_element_video [] (xmlListeVideos()));;


let affiche_liste_video ()  = ExtList.List.iteri (fun nbr -> (fun  elem -> let struc = renvoi_structure_video elem in 
							print_endline ("["^(string_of_int nbr)^"]"); 	
							print_endline ("\t"^struc.titre);
							print_endline ("\t"^struc.description); 
							 ))
						  (collect_arbre find_element_video [] (xmlListeVideos()));;


let entoure_bouton  url = 
"<form name=\"input\" action=\""^url^"\" method=\"get\">\n<input type=\"submit\" value=\"Lancer le téléchargement\"></form>"
let html_liste_videos urlsend = ExtList.List.mapi (fun nbr -> (fun  elem -> let struc = print_endline (string_of_int nbr); renvoi_structure_video elem in 
							 ("\t<b>["^(string_of_int nbr)^"]</b>\n")^
                                                         ("\t<p>"^struc.titre^"<br/>" )^
                                                         (struc.description^"</p><br/>\n")
							 ))
						  (collect_arbre find_element_video [] (xmlListeVideos()));;

let html_page()   = "<html><head></head><body>"^(String.concat "<br/>" (html_liste_videos  ""))^"</body></html>";;

let menu() = affiche_liste_video () ;;

(*Fonctionne*)
(*ON CHERCHE LE XML 2 DANS LE HTML*)
let get_url_rtmp_from url  = 
	let htmlbrut = get url in
	let html = replace htmlbrut ~pattern:"[\\t\\n]" ~par:"" in
	let html2 = replace html ~pattern:".*videorefFileUrl" ~par:"videorefFileUrl" in 
	let xml1brutUrl  = let (a,b) = match_expre_reguliere "videorefFileUrl\\s*=\\s*\"(.+?)\"" html2 in List.hd b in
	let find_element_url_lang_fr el = 
	 match el with
	  |  Xml.Element( "video", [("lang", "fr");("ref", url)], [] ) -> true 
	  | _ -> false in
	let find_element_url_rtmp_video el =
	  match el with
	  | Xml.Element ("url", [("quality", "hd")], (Xml.PCData url)::[] ) -> true
	  | _ -> false in
        (**)
	let find_url_rtmp xml2 =  
		let balise = List.hd (collect_arbre find_element_url_rtmp_video [] xml2) in
		match balise with
		| Xml.Element ("url", [("quality", "hd")], (Xml.PCData url)::[] ) -> url
		| _ -> "" in
	let xml1    =  
		let brut = get xml1brutUrl in
		Xml.parse_string brut in
	let xml2 =
		let xml2_url = 
		   let elem = List.hd (collect_arbre find_element_url_lang_fr [] xml1) in
			match elem with
			|  Xml.Element( "video", [("lang", "fr");("ref", url)], [] ) -> url
			| _ -> "" in
		Xml.parse_string (get xml2_url) in
	find_url_rtmp xml2
	
	
(*ETAPE 2 : ON RECUPERE LE XML 2 À PARTIR DU HTML 1*)        
let get_url_rtmp_par_numero num =
		let liste_video = construit_liste_videos () in
                print_endline (List.nth liste_video num).urlHtml;
		get_url_rtmp_from (List.nth liste_video num).urlHtml
		
			
let dump_par_numero num =
	let liste_video = construit_liste_videos () in
        let titre       = replace (List.nth  liste_video num).titre ~pattern:"['/]" ~par:" " in
	let url_rtmp    =  get_url_rtmp_par_numero num in
        print_endline ("Téléchargement de "^titre); 
	execute_commande ("rtmpdump  -e -r '"^url_rtmp^"' -o '"^titre^".flv'")
	
let dl num =  dump_par_numero num ;;


let i_am_interactive () =
  Unix.isatty Unix.stdin && Unix.isatty Unix.stdout;;


let repl() =
  try
    menu();
    while true do
      if i_am_interactive ()
      then print_string "Choisissez une vidéo - Quit ou ligne vide pour sortir : ";
      let line = read_line () in
      match String.lowercase line with
      | "quit" | "" -> raise End_of_file
      | "debug"  -> isdebug.debug = true;  menu()
      | s -> dl (int_of_string line);
              print_endline "Téléchargement terminé";
              menu();
       
      (* do something with the line *)
    done
  with End_of_file -> ();;


repl();;
