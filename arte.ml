#require "netclient";;
#require "xml-light";;
#require "extlib";;

let execute_commande command = 
  let lines = ref "" in
let chan = Unix.open_process_in command in
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


let xmlListeVideos() = 
	let get      = Http_client.Convenience.http_get in
	let brut =  get "http://videos.arte.tv/fr/do_delegate/videos/index-3188666,view,rss.xml" in
	Xml.parse_string brut;;


let find_element_video  el  =
match el with
| Xml.Element ("item", [] , b ) -> true
| _ 			       -> false;;



let renvoi_structure_video el =
match el with
| Xml.Element ("item", [] , (Xml.Element ("title", [], [Xml.PCData titre]))::(Xml.Element("description", [],[Xml.PCData description]))::
	(Xml.Element("link",[],[Xml.PCData lien]))::(Xml.Element("pubDate",[],[Xml.PCData date]))::reste  ) ->
   { 
	titre 		= titre;
	description 	= replace description ~pattern:"&#039;" ~par:"'";
	urlHtml 	= lien;
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

let menu() = affiche_liste_video () ;;

(*Fonctionne*)
let get_url_rtmp_from url  = 
	let get      = Http_client.Convenience.http_get in
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
	
	
let get_url_rtmp_par_numero num =
		let liste_video = construit_liste_videos () in
		get_url_rtmp_from (List.nth liste_video num).urlHtml
		
			
let dump_par_numero num =
	let liste_video = construit_liste_videos () in
	let titre       = (List.nth  liste_video num).titre in
	let url_rtmp    =  get_url_rtmp_par_numero num in
	execute_commande ("rtmpdump  -e -r '"^url_rtmp^"' -o '"^titre^".flv'")
	
let dl num =  dump_par_numero num ;;
