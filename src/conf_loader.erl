-module(conf_loader).
-export([loadConf/1]).
-vsn( p01 ).

%% loardConf : Permet d'ouvrir un fichier donnée et de charger les
%%  configuration.
loadConf(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
    loadConf(Device, []).
	
loadConf(Device, Conf) ->
	case io:get_line(Device, "") of
		eof  -> 
			file:close(Device), 
			wait(Conf);
		Line -> 
			%% Supprime les " ", \n, et le "="
			loadConf(Device, [string:tokens(
								string:strip(
									string:strip(Line, both, $\n), both, $=), " =")|Conf])
	end.

%% Attend un message.
%% Le message se compose du Pid de l'appelant ainsi que du nom de la configuration a charger
%%  Ex : dans le fichier de conf charger on a une conf : name = EIS. 
%%       Le message a envoyer est de la forme : < Pid, name > et le résultat retourné sera EIS
wait (Conf) ->
	receive
		{Pid, Name} -> 
			getElement (Conf, Name, Pid),
			wait(Conf);
		_ -> error
	end.

%% Autre fonction de test
%%getElement (Conf, Seek) -> 
%%	[Head | Queue] = Conf,
%%	[Name | Val] = Head,
%%	if 
%%		Name == Seek ->
%%			Val;
%%		true ->
%%			getElement(Queue, Seek)
%%	end.
	
%% Recherche la valeur correspondant a la clé Seek dans les conf
%%  et envoi la réponse.
getElement ([[ Name| Val] | Queue], Seek, Pid) ->
	if Name == Seek ->
		Pid ! Val;
	true -> 
		getElement (Queue, Seek, Pid)
	end.