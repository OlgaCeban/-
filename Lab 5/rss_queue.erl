%% запуск из командной строки документирования edoc:files(["rss_queue.erl"]). 

%% @doc Очередь RSS. 
%% Это процесс, который будет хранить элементы RSS ленты. 
%% Сервер, извлекающий данные из ленты RSS, будет передавать их очереди. 
%% Очередь получая сообщения с элементами ленты RSS, будет обоабатывать их разными способами, 
%% в зависимости от того, новый это элемент или новая версия уже известного элемента.

%% Дополнения
%% + обеспечить запуск процесса очереди так, чтобы он связывался с процессом чтения. 
%% + обеспечить возможность добавлять подписчиков очереди, 
%% так чтобы элементы ленты RSS можно было передать другим очередям.

-module(rss_queue).

-export([start/0, start/1, server/2, add_item/2, add_feed/2, get_all/1]).
% для тестирования
-export([test1/1, test2/2, print_list/1]).
-import(xmerl_xpath, [string/2]).

-include("logging.hrl").

% Таймаут на время ожижания приема сообщений = 1 с.
-define(TIMEOUT,10000).

% @doc Функция Инициализации очереди и создание процесса, 
%      возвращает PID вызванного процесса.
%
init([]) ->
	Q = [],
	spawn(?MODULE,server,[Q, sets:new()]);

init([Url]) ->
	QPid = init([]),
	rss_reader:start(Url, QPid). 

% @doc Функция для запуска нового серверного процесса, обслуживающего очередь,
%      возвращает Pid созданного процесса.
%
start() ->
	init([]).

start(Url)->
	init([Url]).

% @doc Функция, реализующая цикл сервера процесса очереди RSS и  осуществляющая
%      управление очередью и администрирование подписок.
%
server(Q, Subscribers) ->
	receive
		{add_item,RSSItem} ->
	    	NewQ = add_item_to_queue(RSSItem, Q, Subscribers), 
	    	SortQ = sort(NewQ),
	    	server(SortQ, Subscribers);

	    {get_all,ReqPid} ->
	      	ReqPid ! {self(),Q},
	      	?INFO("Sent rss items to ~p~n",[ReqPid]),
	      	server(Q, Subscribers);
		
		{subscribe, QPid} ->
	    	case sets:is_element(QPid, Subscribers) of
	    		true ->
	    			?INFO("This pid ~p already exists in the set~n", [QPid]),
	    			warning;
	    		false ->
	    			erlang:monitor(process,QPid),
	    			?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
	    			[add_item(QPid,Item) || Item <- Q],
	    			server(Q,sets:add_element(QPid, Subscribers))
	    	end;

	    {unsubscribe, QPid} ->
	    	case sets:is_element(QPid, Subscribers) of
	    		true ->
	    			?INFO("Process ~p is unsubscribed~n", [QPid]),
	    			server(Q,sets:del_element(QPid, Subscribers));
	    		false ->
	    			?INFO("This process ~p does not exist in the set~n", QPid),
	    			error
	    	end;

	    {'DOWN', _, _, QPid,Reason} ->
	    	?INFO("This process ~p is down: ~p. ~n", [QPid, Reason]),
	    	server(Q,sets:del_element(QPid, Subscribers));

	    _Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
	end.


% @doc Вспомогательная функция, упрощающая процедуру отправки элемента в очередь;
%      QPid - это PID процесса очереди, а Item - это элемент добавляемый к очереди.
% 
add_item(QPid, Item) when is_pid(QPid) ->
 	QPid ! {add_item, Item}, ok.

% @doc Эта функция должна извлекать все элементы из документа ленты, 
% и отправлять все элементы по порядку в очередь.
% 
add_feed(QPid,RSS2Feed) when is_pid(QPid) ->
	case rss_parse:is_rss2_feed(RSS2Feed) of
		true ->
			Items = rss_parse:get_feed_items(RSS2Feed),
			[add_item(QPid,Item) || Item <- Items],
			io:format("Size of RSS2Feed: ~p ~n", [length(Items)]),
			ok;
		false ->
			?ERROR("Version not 2.0! PID: ~p~n", [QPid]),
			error
	end.

% @doc Эта вспомогательная функция упрощает процедуру получения списка элементов 
%      ленты от процесса.
% 
get_all(QPid) when is_pid(QPid) ->
 	QPid ! {get_all, self()},
 	receive
 		{QPid, Q} -> 
 			io:format("Queue size = ~p items from the feed to ~p ~n", [length(Q),QPid]), 
 			{ok, Q};
 		_Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
 	after 
 		?TIMEOUT -> 
 			?ERROR("Timeout exceeded: module ~p, ~p in line ~p.~n", 
 					[?MODULE_STRING, ?FUNCTION_NAME, ?LINE]),
 			{error,timeout}
 	end.

% @private
% @doc Вспомогательная функция инициирующая процесс добавления 
%      нового элемента ленты в очередь.
% 
add_item_to_queue(NewItem, Q, Subscribers) ->
	add_item_to_queue(NewItem, [], Q, Subscribers).

% @private
% @doc Вспомогательная функция, осуществляющая добавление 
%      нового элемента в пустую очередь.
% 
add_item_to_queue(NewItem, List, [], Subscribers) ->
	?INFO("New item added to Queue. PID: ~p~n", [self()]), 
	List++[NewItem];

% @private
% @doc Вспомогательная функция, осуществляющая добавление 
%      нового элемента в непустую очередь по правилу:
%      если при сравнении получен атом same - новый элемент отбрасывается,
%      если получен updated - старая запись удаляется, а новая записывается соответственно дате,
%      если получен different - новый элемент добавляем в порядке возрастания даты.
% 
add_item_to_queue(NewItem, L1, L = [OldItem | Rest], Subscribers) ->
	case rss_parse:compare_feed_items(OldItem, NewItem) of
		same -> 
			?INFO("Same items. Item ignored. PID: ~p~n",[self()]),
			L1++L;
		updated -> 
			?INFO("Updated item. PID: ~p~n",[self()]),
			[add_item(Pid, NewItem) || Pid <- sets:to_list(Subscribers)],
			L1++Rest++[NewItem];
		different -> 
			add_item_to_queue(NewItem, L1++[OldItem], Rest, Subscribers)
	end.

% @private
% @doc Функция сортировки списка элементов очереди
%      по возрастанию даты публикации.
% @spec sort([rssItem()]) -> [rssItem()]
sort([]) -> [];
sort([H|T]) -> sort([X || X <- T, rss_parse:get_item_time(X) < rss_parse:get_item_time(H)]) 
					++ [H] 
					++ sort([X || X <-T , rss_parse:get_item_time(X) >= rss_parse:get_item_time(H)]).


%% Testing
% @doc Test1: Загрузка в очередь XML файла.
% @spec test1(RSSFile::filename()) -> ok | {unknown_msg | error, string() | timeout}
test1(RSSFile) ->
	PID = rss_queue:start(),
	test_file_load(PID, RSSFile),
	exit(PID, kill).

% @doc Test2: Загрузка двух файлов с различными временами
% @spec test2(RSSFile1::filename(),RSSFile2::filename()) -> ok | {unknown_msg | error, string() | timeout}
test2(RSSFile1, RSSFile2) ->
	PID = rss_queue:start(),
	test_file_load(PID, RSSFile1),
	test_file_load(PID, RSSFile2),
	exit(PID, kill).

% @private
% @doc Вспомогательная функция для считывания элементов RSS ленты 
%      из XML файла, отправки их в RSS очередь и печати измененной очереди.
% @spec test_file_load(PID::pid(), RSSFile::filename()) -> ok | {unknown_msg | error, string() | timeout}
test_file_load(PID, RSSFile) ->
	XML = xmerl_scan:file(RSSFile),
	Ret = add_feed(PID, XML),
	case Ret of
		ok ->
			{Resp, Q} = get_all(PID),
			case Resp of
				ok -> print_list(Q), ok;
				_ -> {Resp, Q}
			end;
		_Else -> {version_err, _Else}
	end.
	
% @doc Функция печати очереди RSS, вызывающая 
%      всопомгательную функцию с итерацией.
% @spec print_list(List::[rssItem()]) -> ok
print_list(List) -> io:format("~nQueue: ~n"), print_list(List, 1).

% @private
% @doc Вспомогательная функция печати очереди RSS, осуществляющая 
%      вывод содержимого полей Guid, Title, Link и PubDate для каждого элемента.
% @spec print_list([rssItem()], Num::integer()) -> ok
print_list([], _) -> ok;
print_list([H | Rest], Num) ->
	[Guid, Title, Link, PubDate] = rss_parse:collect_feed_item_data(H),
	io:format("Item #~p:~n", [Num]),
	io:format("  Guid: ~p,~n  Title: ~p,~n  Link: ~p,~n  PubDate: ~p.~n", [Guid, Title, Link, PubDate]),
	print_list(Rest, Num + 1).