%% запуск из командной строки документирования edoc:files(["rss_queue.erl"]). 

%% @doc Очередь RSS. 
%% Это процесс, который будет хранить элементы RSS ленты. 
%% Сервер, извлекающий данные из ленты RSS, будет передавать их очереди. 
%% Очередь получая сообщения с элементами ленты RSS, будет обоабатывать их разными способами, 
%% в зависимости от того, новый это элемент или новая версия уже известного элемента.
-module(rss_queue).

-export([start/0, server/1, add_item/2, add_feed/2, get_all/1]).
% для тестирования
-export([test1/1, test2/2, print_list/1]).
-import(xmerl_xpath, [string/2]).

% Макроопределения лог-сообщений
-define(INFO(Format, Data),
    error_logger:info_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).
-define(WARN(Format, Data),
    error_logger:warning_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).
-define(ERROR(Format, Data),
	error_logger:error_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).
% Таймаут на время ожижания приема сообщений = 1 с.
-define(TIMEOUT,10000).

% @doc Функция для запуска нового серверного процесса, обслуживающего очередь,
%      возвращает Pid созданного процесса.
% @spec start() -> pid()
start() ->
	Q = [],
	spawn(?MODULE,server,[Q]).


% @doc Функция, реализующая цикл сервера процесса очереди RSS, обрабатывающая
%      сообщения двух типов: {add_item,RSSItem} и {get_all,ReqPid}.
% @spec server([rssItem()]) -> ok 
server(Q) ->
	receive
		{add_item,RSSItem} ->
	    	NewQ = add_item_to_queue(RSSItem,Q), 
	    	SortQ = sort(NewQ),
	    	server(SortQ);

	    {get_all,ReqPid} ->
	      	ReqPid ! {self(),Q},
	      	?INFO("Sent rss items to ~p~n",[ReqPid]),
	      	server(Q);

	    _Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
	end.


% @doc Вспомогательная функция, упрощающая процедуру отправки элемента в очередь;
%      QPid - это PID процесса очереди, а Item - это элемент добавляемый к очереди.
% @spec add_item(QPid::pid(), Item::rssItem()) -> ok
add_item(QPid, Item) when is_pid(QPid) ->
 	QPid ! {add_item, Item}, ok.

% @doc Эта функция должна извлекать все элементы из документа ленты, 
% и отправлять все элементы по порядку в очередь.
% @spec add_feed(QPid::pid(), RSS2Feed::rssDoc()) -> ok | error
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
% @spec get_all(QPid::pid()) -> {ok, [rssItem()]} | {error,unknown_msg, string()} | {error,timeout}
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
% @spec add_item_to_queue(NewItem::rssItem(), Q::[rssItem()]) -> [rssItem()]
add_item_to_queue(NewItem, Q) ->
	add_item_to_queue(NewItem, [], Q).

% @private
% @doc Вспомогательная функция, осуществляющая добавление 
%      нового элемента в пустую очередь.
% @spec add_item_to_queue(NewItem::rssItem(), L1::[rssItem()], []) -> [rssItem()]
add_item_to_queue(NewItem, List, []) ->
	?INFO("New item added to Queue. PID: ~p~n", [self()]), 
	List++[NewItem];

% @private
% @doc Вспомогательная функция, осуществляющая добавление 
%      нового элемента в непустую очередь по правилу:
%      если при сравнении получен атом same - новый элемент отбрасывается,
%      если получен updated - старая запись удаляется, а новая записывается соответственно дате,
%      если получен different - новый элемент добавляем в порядке возрастания даты.
% @spec add_item_to_queue(NewItem::rssItem(), L1::[rssItem()], L::[rssItem()]) -> [rssItem()]
add_item_to_queue(NewItem, L1, L = [OldItem | Rest]) ->
	case rss_parse:compare_feed_items(OldItem, NewItem) of
		same -> 
			?INFO("Same items. Item ignored. PID: ~p~n",[self()]),
			L1++L;
		updated -> 
			?INFO("Updated item. PID: ~p~n",[self()]),
			L1++Rest++[NewItem];
		different -> 
			add_item_to_queue(NewItem, L1++[OldItem], Rest)
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