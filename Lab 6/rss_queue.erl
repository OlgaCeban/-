%% запуск из командной строки документирования edoc:files(["rss_queue.erl"]). 

-module(rss_queue).
-include("logging.hrl").
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([start/1]).

-record(rssQ,{queue,subscribers}).

%% Таймаут на время ожижания приема сообщений = 1 с.
-define(TIMEOUT,10000).

%% @doc Запуск gen_server процесса с именем Name (автономный режим).
%% 
start(Name) -> 
  gen_server:start({local, Name}, ?MODULE, [], []).

%% @doc Запуск gen_server процесса с именем Name c чтением URL.
%%
start(Name,Url)->
  gen_server:start({local, Name}, ?MODULE, [Url], []).

%% @doc init
%% Функция инициализации очереди с настройкой прерывания процессов,
%%     и запуска процесса чтения (опционально).
%%
init([]) ->
  process_flag(trap_exit,true),
  {ok, #rssQ{queue=[],subscribers=sets:new()} };


init([Url]) -> 
  State = #rssQ{queue=[],subscribers=sets:new()},
  process_flag(trap_exit,true),
  rss_reader:start(Url,self()),
  {ok, State }.

%% @doc handle_call
%% @doc Обработчик запросов от методов gen_server:call
%%      или gen_server:multi_call.
%%
handle_call(_Request={subscribe,QPid}, _From, State=#rssQ{queue=Q,subscribers=Subs}) ->
  {Reply,NewState} = case sets:is_element(QPid,Subs) of
    true -> {{error,already_subscribed},State};
    false ->  erlang:monitor(process,QPid),
    ?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
    [add_item(QPid,Item) || Item <- Q],
    {ok,State#rssQ{subscribers=sets:add_element(QPid,Subs)}}
  end,
  {reply,Reply, NewState};

handle_call(_Request={get_all}, _From, State=#rssQ{queue=Q}) -> 
  {reply,Q,State};
handle_call(_Request, _From, State) -> 
  {reply,{error,{unknown_request,_Request}}, State}.

%% @doc handle_cast
%% @doc Эта функция вызывается в тот момент когда gen_server 
%% процесс получает запрос, посланый с использованием 
%% функции gen_server:cast/2 или gen_server:abcast/2,3.
%%
handle_cast(_Msg={add_item,RSSItem=#xmlElement{name=item}}, State=#rssQ{queue=Q,subscribers=Subs}) -> 
  NewQ = add_item_to_queue(RSSItem,Q,Subs),
  {noreply,State#rssQ{queue = sort(NewQ)}};

handle_cast(_Msg={unsubscribe,QPid}, State=#rssQ{subscribers=Subs}) -> 
  {noreply,State#rssQ{subscribers=sets:del_element(QPid,Subs)}};

handle_cast(_Msg, State) -> 
  ?WARN("Unknown msg {~p} to Q{~p}",[_Msg,State]),
  {noreply, State}.

%% @doc handle_info
%% @doc Эта функция вызывается когда происходит таймаут ожидания 
%% сообщений на gen_server процессе или когда приходит любое сообщение 
%% отличное от асинхронного, синхронного запросов.
%%
handle_info(_Info={'DOWN',_,_,QPid,_Reason},State=#rssQ{subscribers=Subs})->
  {noreply, State#rssQ{subscribers=sets:del_element(QPid,Subs)}};

handle_info(_Info={'EXIT',FromPid,_Reason},State)->
  ?ERROR("RSS Reader ~p died for ~p with reason ~n",[FromPid,self(),_Reason]),
  {noreply, State};

handle_info(_Info, State) -> 
  {noreply, State}.

%% @doc terminate
%% @doc Эта функция вызывается перед завершением gen_server процесса 
%% и является противоположностью функции Module:init/1, выполняя необходимую очистку. 
%% Как только происходит возврат контроля в gen_server процесс, он завершается с причиной 
%% Reason. Возвращаемое значение этой функции игнорируется.
%%
terminate(_Reason, _State) -> ok.

%% @doc code_change
%% @doc Эта функция вызывается когда происходит 
%% обновление/откат версии gen_server процесса 
%% и ему необходимо обновить свое внутреннее состояние, 
%% для соответстия реалиям работающего кода
%%
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @doc Эта функция подписывает очередь Queue1 на сообщения очереди Queue2.
%%
subscribe(From,To)->
  gen_server:call(To,{subscribe,From}).

%% @doc Эта функция отписывает очередь Queue.
%%
unsubscribe(Queue)->
  gen_server:cast({unsubscribe, Queue}).

%% @private
%% @doc Вспомогательная функция инициирующая процесс добавления 
%%      нового элемента ленты в очередь.
%%   
add_item_to_queue(NewItem,Q,Subs)->
  add_item_to_queue(NewItem,[],Q,Subs).

add_item_to_queue(NewItem,L1,[],Subs)->
  ?INFO("New item added to Queue. PID: ~p~n", [self()]),
  broadcast(NewItem,Subs),
  L1++[NewItem];

add_item_to_queue(NewItem,L1,L=[OldItem|Rest],Subs)->
  case rss_parse:compare_feed_items(OldItem,NewItem) of
    same -> 
      L1++L ;
    updated -> 
      ?INFO("Updated item ~p ~n",[self()]),
      broadcast(NewItem,Subs),
      L1++Rest++[NewItem] ;
    different -> 
      add_item_to_queue(NewItem,L1++[OldItem],Rest,Subs)
  end.

%% @doc Вспомогательная функция, упрощающая процедуру отправки элемента в очередь;
%%      QPid - это PID процесса очереди, а Item - это элемент добавляемый к очереди.
%%
add_item(QPid,Item)->
  ok = gen_server:cast(QPid , {add_item,Item} ),ok.

%% @doc Эта функция должна извлекать все элементы из документа ленты, 
%% и отправлять все элементы по порядку в очередь
%%
add_feed(QPid,RSS2Feed)->
 Items=rss_parse:get_feed_items(RSS2Feed),
 [add_item(QPid,Item) || Item <- Items],
 ?INFO("Size of RSS2Feed: ~p ~n", [length(Items)]),
 ok. 

%% @doc Эта вспомогательная функция упрощает процедуру получения списка элементов 
%%      ленты от процесса.
%%
get_all(QPid)->
  gen_server:call(QPid,{get_all}).

%% @doc передача объекта нескольким пидам
%%
broadcast(Item,PidSet)->
  [ add_item(Pid,Item) || Pid <- sets:to_list(PidSet) ]. 

%% @private
%% @doc Функция сортировки списка элементов очереди
%%      по возрастанию даты публикации.
%%
sort([]) -> [];
sort([H|T]) -> sort([X || X <- T, rss_parse:get_item_time(X) < rss_parse:get_item_time(H)]) 
					++ [H] 
					++ sort([X || X <-T , rss_parse:get_item_time(X) >= rss_parse:get_item_time(H)]).