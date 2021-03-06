-module rss_parse.

-export([is_rss2_feed/1, get_feed_items/1]).
-export([get_item_time/1, compare_feed_items/2]).
-export([run/0]). %% testing

-include_lib("xmerl/include/xmerl.hrl").

is_rss2_feed(XML) ->
    RSS_version = xmerl_xpath:string("/rss[@version='2.0']/text()", XML), %% search if RSS=2.0
    length(RSS_version) > 0. %% if RSS=2.0 returns true
    
get_feed_items(RSS2Feed) ->
    xmerl_xpath:string("//channel/item", RSS2Feed).

get_item_time(Item) ->
    [T] = xmerl_xpath:string("/item/pubDate/text()", Item),
    ND = httpd_util:convert_request_date(T#xmlText.value),
    calendar:datetime_to_gregorian_seconds(ND).

% @private
% @doc Эта вспомогательная функция просматривает заданный XML элемент
%      и удаляет из него сведения о других XML элементах, например содержащиеся в полях
%      "parents" или "pos".
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[], pos=0,
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
    Other.

%% get items' certain tag contents
collect_feed_item_data(Item) when is_record(Item, xmlElement) ->
    [Guid] = xmerl_xpath:string("guid/text()", Item),
    % io:format("Guid: ~p~n", [Guid]),
    [Title] = xmerl_xpath:string("title/text()", Item),
    % io:format("Title: ~p~n", [Title]),
    [Link] = xmerl_xpath:string("link/text()", Item),
    % io:format("Link: ~p~n", [Link]),
    [PubDate] = xmerl_xpath:string("pubDate/text()", Item),
    [Guid, Title, Link, PubDate].

%% items' classification
compare_feed_normalised_items(Guid, Title, Link, PubDate, Guid, Title, Link, PubDate) ->
    same;
compare_feed_normalised_items(Guid, _, _, _, Guid, _, _, _) ->
    updated;
compare_feed_normalised_items(_, Title, _, _, _, Title, _, _) ->
    updated;
compare_feed_normalised_items(_, _, Link, _, _, _, Link, _) ->
    updated;
compare_feed_normalised_items(_, _, _, _, _, _, _, _) ->
    different.

%% items' comparation
compare_feed_items(OldItem, NewItem) when 
        is_record(OldItem, xmlElement),
        is_record(NewItem, xmlElement)->
    [Guid1, Title1, Link1, PubDate1] = collect_feed_item_data(OldItem),
    [Guid2, Title2, Link2, PubDate2] = collect_feed_item_data(NewItem),
    compare_feed_normalised_items(Guid1, Title1, Link1, PubDate1, Guid2, Title2, Link2, PubDate2).

%% Testing
run() ->
    F = "digg-science-rss1.xml",
    {XML, _} = xmerl_scan:file(F),

    %% check RSS version correct
    % is_rss2_feed(XML). 

    %% get items
    % get_feed_items(XML). 

    %% get items' time
    % map(fun get_item_time/1, get_feed_items(XML)). 

    %% testing help function
    % [X1| _] = get_feed_items(XML),
    % extract_xml(X1).

    %% compare items
    extract_xml(XML), 
    [X1, X2, X3| _] = get_feed_items(XML),
    [compare_feed_items(X1, X2), compare_feed_items(X1, X1), 
    compare_feed_items(X2, X3), compare_feed_items(X3, X3)].
