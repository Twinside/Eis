-module(conf_loader).
-export([loadConf/1]).
-vsn( p01 ).

%% loardConf : Permet d'ouvrir un fichier donn�e et de charger les
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
			%% Supprime les " ", \n,le "=" et les "
			loadConf(Device, [string:tokens(
								string:strip(
									string:strip(
										string:strip(Line, both, $\n),
										both, $=),
									both, $"),
								" =")|Conf])
	end.

%% @doc
%% Attend un message.
%% Le message se compose du Pid de l'appelant ainsi que du nom de la configuration a charger
%%  Ex : dans le fichier de conf charger on a une conf : name = EIS. 
%%       Le message a envoyer est de la forme : < Pid, name > et le r�sultat retourn� sera EIS
%% @end
wait (Conf) ->
	receive
		{Pid, Name} -> 
			getElement (Conf, Name, Pid),
			wait(Conf);
		_ -> error
	end.

%% @doc
%% Autre fonction de test
%% Recherche la valeur correspondant a la cl� Seek dans les conf
%%  et envoi la r�ponse.
%% @spec getElement (Conf, Seek) -> Result
%% where
%%	conf = [Head|Queue]
%%	Head = [Name | Val]
%%	if 
%%		Name == Seek ->
%%			Val;
%%		true ->
%%			getElement(Queue, Seek)
%%	end.
%%	
getElement ([[ Name| Val] | Queue], Seek, Pid) ->
	if Name == Seek ->
		Pid ! Val;
	true -> 
		getElement (Queue, Seek, Pid)
	end.
