%%%-------------------------------------------------------------------
%%% @author serg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2018 16:22
%%%-------------------------------------------------------------------
-module(test_lists).
-author("serg").

-type ne_binary() :: <<_:8,_:_*8>>.

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-define(SERVER_URL, <<"http://127.0.0.1:8000/">>).
-define(API_VERSION, <<"v2">>).
-define(ACCOUNT_ID, <<"c5518687e9f42c4645816293a5a29d06">>).
-define(AUTH_TOKEN_NAME,<<"X-Auth-Token">>).
-define(LIST_ID_NAME, <<"List-ID">>).
-define(BASE_URL, <<?SERVER_URL/binary, ?API_VERSION/binary, "/accounts/", ?ACCOUNT_ID/binary, "/lists">>).
-define(MAX_ENTRIES, 500).

%% API
-export([
    start_link/0
    , init/0
    , get_auth/0, set_auth/1
    , get_list_id/0, set_list_id/1
    , get_url/0, get_entries_url/0
    , get_lists/0
    , get_entry_json/1
    , delete_enries/0
    , gen_entries/1
    , add_enry/1
]).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = init(),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    io:format("~p.init()\n",[?MODULE]),
    ets:new(?MODULE, [set, public, named_table, {read_concurrency,true}]),
    'ok'.

-spec set_auth(ne_binary())-> boolean().
set_auth(AuthKey) ->
    AuthPair = {?AUTH_TOKEN_NAME, AuthKey},
    case get_auth() of
        'undefined' -> ets:insert_new(?TAB, AuthPair);
        _ -> ets:insert(?TAB, AuthPair)
    end.

-spec get_auth()-> ne_binary().
get_auth() ->
    case ets:select(?TAB, [{ {?AUTH_TOKEN_NAME,'$1'}, [], ['$1'] }]) of
        [] -> io:format("auth key is not set\n"), 'undefined';
        [Key|_] -> Key
    end.

-spec set_list_id(ne_binary()) -> boolean().
set_list_id(ListId) ->
    ListIdPair = {?LIST_ID_NAME, ListId},
    case get_list_id() of
        'undefined' -> ets:insert_new(?TAB, ListIdPair);
        _ -> ets:insert(?TAB, ListIdPair)
    end.

-spec get_list_id() -> ne_binary().
get_list_id() ->
    case ets:select(?TAB, [{ {?LIST_ID_NAME,'$1'}, [], ['$1'] }]) of
        [] -> io:format("list id is not set\n"), 'undefined';
        [Key|_] -> Key
    end.


-spec get_url() -> any().
get_url() ->
    ?BASE_URL.

-spec get_entries_url() -> ne_binary() | 'undefined'.
get_entries_url() ->
    case get_list_id() of
        'undefined' -> io:format("can't get list url\n"), 'undefined';
        ListId -> <<?BASE_URL/binary,"/",ListId/binary,"/entries">>
    end.


-spec get_lists() -> any().
get_lists() ->
    Method = 'get',
    hackney:start(),
    io:format("get list of lists\n"),
    ReqHeaders = [
        {<<"Content-Type">>, <<"application/json">>},
        {?AUTH_TOKEN_NAME, get_auth()}
    ],
    {ok, _, _, Ref} = hackney:request(Method, ?BASE_URL, ReqHeaders, <<>>),
    {ok, Body} = hackney:body(Ref),
    io:format("body: ~p~n~n", [Body]),

    IsClosed = hackney_manager:get_state(Ref) =:= req_not_found,
    io:format("has been closed: ~p\n", [IsClosed]).

-spec delete_enries() -> 'ok'.
delete_enries() ->
    Method = 'delete',
    hackney:start(),
    io:format("delete all entries from list ~p\n",[get_list_id()]),
    ReqHeaders = [
        {<<"Content-Type">>, <<"application/json">>},
        {?AUTH_TOKEN_NAME, get_auth()}
    ],
    {ok, _, _, Ref} = hackney:request(Method, get_entries_url(), ReqHeaders, <<>>),
    {ok, Body} = hackney:body(Ref),
    io:format("body: ~p~n~n", [Body]),

    IsClosed = hackney_manager:get_state(Ref) =:= req_not_found,
    io:format("has been closed: ~p~n", [IsClosed]).

%%    {"id", Key} = lists:keyfind("id",1,json:parse(Body)),
%%    'ok'.
%%{"data": {"number": "0123", "displayname" : "List Entry"}}

%%-spec add_entry() -> any().
%%add_entry() ->
%%    ReqBody = << "{
%%         \"id\": \"some_paste_id\",
%%         \"rev\": \"some_revision_id\",
%%         \"changeset\": \"changeset in unidiff format\"
%%    }" >>.

-spec add_enry(ne_binary()) -> 'ok'.
add_enry(ReqBody) ->
    hackney:start(),
    Method = 'put',
    URL = get_entries_url(),
    ReqHeaders = [
        {<<"Content-Type">>, <<"application/json">>},
        {?AUTH_TOKEN_NAME, get_auth()}
    ],
    io:format("Method=~p\nURL=~p\nReqHeaders=~p\nReqBody=~p\n",[Method, URL, ReqHeaders, ReqBody]),
    {ok, Status, _, Ref} = hackney:request(Method, URL, ReqHeaders, ReqBody),
    io:format("status: ~p\n", [Status]),
    {ok, Body} = hackney:body(Ref),
    io:format("body: ~p~n~n", [Body]),
%%    {'ok', Ref} = {'ok', Ref} = hackney:request(Method, URL, ReqHeaders, 'stream', []),
%%    'ok' = hackney:send_body(Ref, ReqBody),
%%    {'ok', Status, _Headers, Ref} = hackney:start_response(Ref),
%%    {'ok', Body} = hackney:body(Ref),

    'ok'.

-spec gen_entries(integer()) -> 'ok'.
gen_entries(N) when N > ?MAX_ENTRIES ->
    io:format("Max enties is ~p\n",[?MAX_ENTRIES]);
gen_entries(N) ->
    io:format("start generate entries"),
    gen_entries(1,N).
gen_entries(I, N) when I == (N + 1) -> 'stop';
gen_entries(I, N) when I < (N + 1) ->
    JObj = get_entry_json(I),
    io:fwrite("entry ~p\nJObj=~p\n", [I,JObj]),
    add_enry(JObj),
    gen_entries(I + 1, N).

-spec get_entry_json(ne_binary()) -> ne_binary().
get_entry_json(Number) ->
    JsonString = io_lib:format("{\"data\": {\"number\": \"~4..0B\", \"displayname\": \"Test entry ~4..0B\"}}",[Number, Number]),
    list_to_binary(JsonString).



