%% Copyright (c) 2009
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol
%%
%% @author Jacob Vorreuter
%% @author Bart van Deenen
%% @author Enrique Paz
%%
%% @doc a binary protocol memcached client
%%
%%
%% I've added some small modifications: The Key can now be any erlang term
%% (value still only a binary).
%%
%% The application is only using a localhost memcache server.
%%
%% The gen_server is now called via a 'poolboy' pool mechanism.

-module(erlmc_conn).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
	     handle_info/2, terminate/2, code_change/3]).

%% Op codes
-define(OP_Get,       16#00).
-define(OP_Set,       16#01).
-define(OP_Add,       16#02).
-define(OP_Replace,   16#03).
-define(OP_Delete,    16#04).
-define(OP_Increment, 16#05).
-define(OP_Decrement, 16#06).
-define(OP_Quit,      16#07).
-define(OP_Flush,     16#08).
-define(OP_GetQ,      16#09).
-define(OP_Noop,      16#0A).
-define(OP_Version,   16#0B).
-define(OP_GetK,      16#0C).
-define(OP_GetKQ,     16#0D).
-define(OP_Append,    16#0E).
-define(OP_Prepend,   16#0F).
-define(OP_Stat,      16#10).

-record(request, {
        op_code,
        data_type=16#00,
        reserved=16#00,
        opaque=16#00,
        cas=16#00,
        extras = <<>>,
        key = <<>>,
        value = <<>>
}).

-record(response, {
        op_code,
        data_type,
        status,
        opaque,
        cas,
        extras,
        key,
        value,
        key_size,
        extras_size,
        body_size
}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc start the memcache worker.
%% you can use the memcache pool properties to set non default parameters
%% @see memcache:init/1
-spec start_link([{term(), term()}]) -> {ok, pid()} | ignore | {error, term()} .
start_link(Args) ->
	Host = proplists:get_value( host, Args),
	Port = proplists:get_value( port, Args),
	gen_server:start_link(?MODULE, {Host, Port}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
-type state()::port().

-type call_type()::{get, memcache:key()}
    | {add, memcache:key(), memcache:value(), memcache:expiration()}
    | {set, memcache:key(), memcache:value(), memcache:expiration()}
    | {replace, memcache:key(), memcache:value(), memcache:expiration()}
    | {delete, memcache:key()}
    | {increment, memcache:key(), memcache:value(), memcache:value(), memcache:expiration()}
    | {increment, memcache:key(), memcache:value(), memcache:value(), memcache:expiration()}
    | {append, memcache:key(), memcache:value()} | {prepend, memcache:key(), memcache:value()}
    | stats | flush | {flush, memcache:expiration()} | quit | version | any().


%% @private
-spec init({Host::string(), Port::pos_integer()}) -> {ok, state()} | {stop, Error::term()}.
init({Host, Port}) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
			{ok, Socket};
        Error ->
			{stop,Error}
    end.

%% @private
-spec handle_call(call_type(), {pid(), Tag::term()}, state()) ->
    {reply, memcache:value(), state()} | {stop, term(), {error, term(), state()}}
    | {noreply, state()}.

handle_call({get, K}, _From, Socket) ->
	Key = key_to_binary(K),
    case send_recv(Socket, #request{op_code=?OP_GetK, key=Key}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		#response{key=Key, value=Value} ->
			{reply, Value, Socket}
	end;

handle_call({add, K, Value, Expiration}, _From, Socket) ->
	Key = key_to_binary(K),
    case send_recv(Socket, #request{op_code=?OP_Add,
                                    extras = <<16#deadbeef:32, Expiration:32>>,
                                    key=Key, value=Value}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({set, K, Value, Expiration}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Set,
                                    extras = <<16#deadbeef:32, Expiration:32>>,
                                    key=Key, value=Value}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({replace, K, Value, Expiration}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Replace,
                                    extras = <<16#deadbeef:32, Expiration:32>>,
                                    key=Key, value=Value}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({delete, K}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Delete, key=Key}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({increment, K, Value, Initial, Expiration}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Increment,
                                    extras = <<Value:64, Initial:64, Expiration:32>>,
                                    key=Key}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({decrement, K, Value, Initial, Expiration}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Decrement,
                                    extras = <<Value:64, Initial:64, Expiration:32>>,
                                    key=Key}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({append, K, Value}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Append, key=Key, value=Value}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({prepend, K, Value}, _From, Socket) ->
	Key = key_to_binary(K),
	case send_recv(Socket, #request{op_code=?OP_Prepend, key=Key, value=Value}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call(stats, _From, Socket) ->
	send(Socket, #request{op_code=?OP_Stat}),
    case collect_stats_from_socket(Socket) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Reply ->
    		{reply, Reply, Socket}
	end;

handle_call(flush, _From, Socket) ->
	case send_recv(Socket, #request{op_code=?OP_Flush}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call({flush, Expiration}, _From, Socket) ->
	case send_recv(Socket, #request{op_code=?OP_Flush, extras = <<Expiration:32>>}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call(quit, _From, Socket) ->
	send_recv(Socket, #request{op_code=?OP_Quit}),
	gen_tcp:close(Socket),
    {stop, normal, undefined};

handle_call(version, _From, Socket) ->
	case send_recv(Socket, #request{op_code=?OP_Version}) of
		{error, Err} ->
			{stop, Err, {error, Err}, Socket};
		Resp ->
    		{reply, Resp#response.value, Socket}
	end;

handle_call(_, _From, Socket) -> {reply, {error, invalid_call}, Socket}.

%% @private
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

%% @private
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(any(), state()) -> ok.
terminate(_Reason, Socket) ->
	case is_port(Socket) of
		true -> gen_tcp:close(Socket);
		false -> ok
	end, ok.

%% @private
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec collect_stats_from_socket(port()) -> [{atom(), list()}] | {error, term()}.
collect_stats_from_socket(Socket) ->
    collect_stats_from_socket(Socket, []).

-spec collect_stats_from_socket(port(), [{atom(), list()}]) ->
    [{atom(), list()}] | {error, term()}.
collect_stats_from_socket(Socket, Acc) ->
    case recv(Socket) of
		{error, Err} ->
			{error, Err};
        #response{body_size=0} ->
            Acc;
        #response{key=Key, value=Value} ->
            collect_stats_from_socket(Socket,
                                      [{binary_to_atom(Key, utf8), binary_to_list(Value)}|Acc])
    end.

-spec send_recv(port(), #request{}) -> {error, term()} | #response{}.
send_recv(Socket, Request) ->
    ok = send(Socket, Request),
    recv(Socket).

-spec send(port(), #request{}) -> ok | {error, term()}.
send(Socket, Request) ->
    Bin = encode_request(Request),
    gen_tcp:send(Socket, Bin).

-spec recv(port()) -> {error, term()} | #response{}.
recv(Socket) ->
    case recv_header(Socket) of
		{error, Err} ->
			{error, Err};
		HdrResp ->
    		recv_body(Socket, HdrResp)
    end.

-spec encode_request(#request{}) -> binary().
encode_request(Request) when is_record(Request, request) ->
    Magic = 16#80,
    Opcode = Request#request.op_code,
    KeySize = size(Request#request.key),
    Extras = Request#request.extras,
    ExtrasSize = size(Extras),
    DataType = Request#request.data_type,
    Reserved = Request#request.reserved,
    Body = <<Extras:ExtrasSize/binary, (Request#request.key)/binary, (Request#request.value)/binary>>,
    BodySize = size(Body),
    Opaque = Request#request.opaque,
    CAS = Request#request.cas,
    <<Magic:8, Opcode:8, KeySize:16, ExtrasSize:8, DataType:8, Reserved:16, BodySize:32, Opaque:32, CAS:64, Body:BodySize/binary>>.

-spec recv_header(port()) -> {error, term()} | #response{}.
recv_header(Socket) ->
    decode_response_header(recv_bytes(Socket, 24)).

-spec recv_body(port(), #response{}) -> {error, term()} | #response{}.
recv_body(Socket, #response{key_size = KeySize, extras_size = ExtrasSize, body_size = BodySize}=Resp) ->
    decode_response_body(recv_bytes(Socket, BodySize), ExtrasSize, KeySize, Resp).

-spec decode_response_header({error, term()} | binary()) -> {error, term()} | #response{}.
decode_response_header({error, Err}) -> {error, Err};
decode_response_header(<<16#81:8, Opcode:8, KeySize:16, ExtrasSize:8, DataType:8, Status:16, BodySize:32, Opaque:32, CAS:64>>) ->
    #response{
        op_code = Opcode,
        data_type = DataType,
        status = Status,
        opaque = Opaque,
        cas = CAS,
        key_size = KeySize,
        extras_size = ExtrasSize,
        body_size = BodySize
    }.

-spec decode_response_body({error, term()} | binary(), pos_integer(), pos_integer(), #response{}) ->
    {error, term()} | #response{}.
decode_response_body({error, Err}, _, _, _) -> {error, Err};
decode_response_body(Bin, ExtrasSize, KeySize, Resp) ->
    <<Extras:ExtrasSize/binary, Key:KeySize/binary, Value/binary>> = Bin,
    Resp#response{
        extras = Extras,
        key = Key,
        value = Value
    }.

-spec recv_bytes(port(), pos_integer()) -> binary() | term().
recv_bytes(_, 0) -> <<>>;
recv_bytes(Socket, NumBytes) ->
    case gen_tcp:recv(Socket, NumBytes) of
        {ok, Bin} -> Bin;
        Err -> Err
    end.

-spec key_to_binary(term()) -> binary().
key_to_binary( Key) when is_integer(Key) andalso Key >= 0 -> binary:encode_unsigned(Key);
key_to_binary( Key) when is_list(Key) -> list_to_binary(Key);
key_to_binary( Key) -> term_to_binary(Key).

