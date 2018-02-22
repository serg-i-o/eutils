%%%-------------------------------------------------------------------
%%% @author serg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Feb 2018 17:08
%%%-------------------------------------------------------------------
-module(test).
-author("serg").

%% API
-export([check_exchange/0, handle_call/3, test_call/0, get_start_args/0, test_params/0, channel_requisition/1]).

-record('exchange.declare', {
    ticket = 0
    , exchange
    , type = <<"direct">>
    , passive = false
    , durable = false
    , auto_delete = false
    , internal = false
    , nowait = false
    , arguments = []
}).
-type kz_amqp_exchange() :: #'exchange.declare'{}.
-record(state, {consumers = sets:new() :: sets:set(pid())
    ,exchanges = dict:new() :: dict:dict(kz_term:ne_binary(), kz_amqp_exchange())
    ,connections = sets:new() :: sets:set(pid())
}).
-type state() :: #state{}.

-include("/home/serg/src/eutils/test_amqp/deps/kazoo_stdlib/include/kz_types.hrl").


-spec check_exchange() -> any().
check_exchange() ->
    Exchange = {'exchange.declare',0,<<"targeted">>,<<"direct">>,false,false,false,false,false,[]},
    Exchange.

-spec test_call() -> any().
test_call() ->
    Call = {kz_amqp_history, {'add_exchange', {'exchange.declare',0,<<"targeted">>,<<"direct">>,false,false,false,false,false,[]}}, 1000000},
    {_CallName, CallRequest, _CallTimeout} = Call,
    handle_call(CallRequest,self(),#state{}).


-spec handle_call(any(), kz_term:pid_ref(), state()) -> any().
handle_call({'add_exchange', #'exchange.declare'{exchange=Name}=Exchange}, _From, #state{}=State) ->
    io:format("Exchange=~p\nName=~p\nState=~p\n",[Exchange,Name,State]).

-spec test_params() -> any().
test_params() ->
    {_,_,Params,_} = get_start_args(),
    channel_requisition(Params).

-spec channel_requisition(kz_term:proplist()) -> boolean().
channel_requisition([]) -> 'false';
channel_requisition(Params) ->
    case props:get_value('broker_tag', Params) of
        'undefined' ->
            case props:get_value('broker', Params) of
                'undefined' -> io:format("broker undefined\n");
                Broker -> io:format("Broker=~p\n",[Broker])
            end;
        Tag -> io:format("BrokerTag=~p\n",[Tag])
%%            case kz_amqp_connections:broker_with_tag(Tag) of
%%                'undefined' -> kz_amqp_channel:requisition();
%%                Broker -> maybe_add_broker_connection(Broker)
%%            end
    end.

-spec get_start_args() -> kz_term:proplist().
get_start_args() ->
    StartLinkArgs = {
        {'local', webhook_shared_listener}
        ,webhook_shared_listener
        ,[
            {'bindings',
                [
                    {call, 	[{restrict_to,['PARK_PARKED','PARK_RETRIEVED','PARK_ABANDONED']}]},
                    {conf,	[{restrict_to,[doc_updates]}]},
                    {notifications,
                        [{restrict_to,
                            [low_balance,new_account,service_added,topup,transaction,
                                account_zone_change,inbound_fax,inbound_fax_error,
                                outbound_fax,outbound_fax_error,outbound_smtp_fax_error,
                                cnam_request,port_request,port_cancel,port_comment,
                                port_pending,port_rejected,port_scheduled,
                                port_unconfirmed,ported,denied_emergency_bridge,
                                deregister,first_occurrence,missed_call,register,
                                system_alert,customer_update,new_user,password_recovery,
                                voicemail_full,voicemail_new,voicemail_saved,webhook,
                                webhook_disabled]}]},
                    {conference,	[{restrict_to,[event]}]},
                    {call,	[{restrict_to,['CHANNEL_DESTROY']}]},
                    {call,	[{restrict_to,['CHANNEL_CREATE']}]},
                    {call,	[{restrict_to,['CHANNEL_BRIDGE']}]},
                    {call,	[{restrict_to,['CHANNEL_ANSWER']}]},
                    {conf,	[{restrict_to,[doc_type_updates]},{type,<<"webhook">>}]}
                ]
            }
            ,{'responders',
                [
                    {{webhooks_parking,handle},	                [{<<"call_event">>,<<"PARK_PARKED">>},
                        {<<"call_event">>,<<"PARK_RETRIEVED">>},
                        {<<"call_event">>,<<"PARK_ABANDONED">>}]
                    },
                    {{webhooks_object,handle_event},		[{<<"configuration">>,<<"*">>}]},
                    {{webhooks_notifications,handle_event},	[{<<"notification">>,<<"*">>}]},
                    {{webhooks_conference_enter,handle_event},	[{<<"conference">>,<<"*">>}]},
                    {{webhooks_channel_util,handle_event},	[{<<"call_event">>,<<"CHANNEL_DESTROY">>}]},
                    {{webhooks_channel_util,handle_event},      [{<<"call_event">>,<<"CHANNEL_CREATE">>}]},
                    {{webhooks_channel_util,handle_event},      [{<<"call_event">>,<<"CHANNEL_BRIDGE">>}]},
                    {{webhooks_channel_util,handle_event},      [{<<"call_event">>,<<"CHANNEL_ANSWER">>}]},
                    {{webhooks_shared_listener,handle_doc_type_update},   [{<<"configuration">>,<<"doc_type_update">>}]}]
            }
            ,{'queue_name', <<"webhooks_shared_listener">>}
            ,{'queue_options', [{'exclusive', 'false'}]}
            ,{'consume_options', [{'exclusive', 'false'}]}
        ]
        ,[]
    },
    StartLinkArgs.
