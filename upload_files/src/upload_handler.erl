%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(upload_handler).

-export([init/2]).

-define(UPLOAD_DIR, "/tmp/call_records").
-define(MAX_UPLOAD_SIZE, 100000).
-define(MULTIPART_OPTIONS,[{length, 1024}, {read_length, 1024}, {read_timeout, 5000}]).

-type upload_ret() :: {'ok', cowboy_req:req(), cowboy_req:push_opts()}.
-type stop_return() :: {'stop', cowboy_req:req(), cowboy_req:push_opts()}.
-type cowboy_multipart_response() :: {'ok', cow_multipart:headers(), cowboy_req:req()} |
    {'done', cowboy_req:req()} | cowboy_req:req().
-type read_body_result() :: {ok, binary(), cowboy_req:req()} | {more, binary(), cowboy_req:req()}.



-spec init(cowboy_req:req(), cowboy_req:push_opts()) -> upload_ret().
init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    io:format("\n\n~p.init/2:\nReq=~p\nOpts=~p\nMethod=~p\n",[?MODULE, Req, Opts, Method]),
    ReqResult = upload(Method, Req, Opts),
    {ok, ReqResult, Opts}.

-spec upload(kz_term:ne_binary(), cowboy_req:req(), cowboy_req:push_opts()) -> cowboy_req:req().
upload(<<"PUT">>, Req, _Opts) ->
    get_req_data(Req);
upload(<<"POST">>, Req, _Opts) ->
    get_req_data(Req);
upload(_, Req, _Opts) ->
    handle_method_not_allowed(Req).


%% Get data handling
-spec get_req_data(cowboy_req:req()) -> cowboy_req:req() | stop_return().
get_req_data(Req) ->
    {QS, Req1} = get_query_string_data(Req),
    ContentTypeHeader = get_content_type(Req1),
    get_req_data(Req1, ContentTypeHeader, QS).

-spec get_req_data(cowboy_req:req(), kz_term:ne_binary(), kz_json:object()) -> cowboy_req:req() | stop_return().
get_req_data(Req, 'undefined', _QS) ->
    io:format("\n~p.get_req_data/3: undefined content type when getting req data\n",[?MODULE]),
    handle_wrong_content_type(Req);
get_req_data(Req, <<"multipart/form-data">>, QS) ->
    io:format("\n~p.get_req_data/3: multipart/form-data content type when getting req data\nQuerryString=~p\n",
        [?MODULE, QS]),
    maybe_extract_multipart(Req, QS);
get_req_data(Req, ContentType, QS) ->
    io:format("\n~p.get_req_data/3: may be file's content-type: ~p\nQuerryString=~p\n",
        [?MODULE, ContentType, QS]),
    extract_file(ContentType, Req).

-spec maybe_extract_multipart(cowboy_req:req(), kz_json:object()) -> cowboy_req:req() | stop_return().
maybe_extract_multipart(Req, QS) ->
    io:format("\n~p.maybe_extract_multipart/2: try extract multipart",[?MODULE]),
    try extract_multipart(Req, QS)
    catch
        _E:_R ->
            io:format("\n~p.maybe_extract_multipart/2: failed to extract multipart\n",[?MODULE]),
            handle_failed_multipart(Req)
    end.

-spec extract_multipart(cowboy_multipart_response(), kz_json:object()) -> cowboy_req:req().
extract_multipart({'done', Req}, _QS) ->
    io:format("\n~p.extract_multipart/2: read multipart done\n",[?MODULE]),
    Req;
extract_multipart({'ok', Headers, Req}, QS) ->
    ContentTypeHeader = get_content_type(Req),
    io:format("~p.extract_multipart/2: reading multipart\nHeaders=~p\nContentType=~p\nReq=~p\n",[?MODULE, Headers, ContentTypeHeader, Req]),
    Req1 = get_req_data(Req, ContentTypeHeader, QS),
    extract_multipart(cowboy_req:read_part(Req1, ?MULTIPART_OPTIONS), QS);
extract_multipart(Req, QS) ->
    io:format("\n~p.extract_multipart/2: start read multipart\n",[?MODULE]),
    extract_multipart(cowboy_req:read_part(Req, ?MULTIPART_OPTIONS), QS).


-spec extract_file(kz_term:ne_binary(), cowboy_req:req()) -> cowboy_req:req() | stop_return().
extract_file(ContentType, Req) ->
    try extract_file_part_body(ContentType, Req)
    catch
        _E:_R ->
            extract_file_body(ContentType, Req)
    end.

-spec extract_file_part_body(kz_term:ne_binary(), cowboy_req:req()) -> cowboy_req:req().
extract_file_part_body(ContentType, Req) ->
    handle_read_any_body(ContentType, cowboy_req:read_part_body(Req, #{'length'=>?MAX_UPLOAD_SIZE})).
-spec extract_file_body(kz_term:ne_binary(), cowboy_req:req()) -> cowboy_req:req() | stop_return().
extract_file_body(ContentType, Req) ->
    handle_read_any_body(ContentType, cowboy_req:read_body(Req, #{'length'=>?MAX_UPLOAD_SIZE})).


-spec handle_read_any_body(kz_term:ne_binary(), read_body_result()) -> cowboy_req:req() | stop_return().
handle_read_any_body(ContentType, ReadResult) ->
    case ReadResult of
        {'more', _, Req1} ->
            handle_max_filesize_exceeded(Req1);
        {'ok', FileContents, Req1} ->
            handle_file_contents(ContentType, Req1, FileContents);
        _ -> handle_other_errors(ReadResult)
    end.


-spec handle_file_contents(kz_term:ne_binary(), cowboy_req:req(), binary()) -> cowboy_req:req() | stop_return().
handle_file_contents(ContentType, Req, FileContent) ->
    Filename = uploaded_filename(Req),
    io:format("\n~p.handle_file_contents/3:\tFilename=~p\n",[?MODULE, Filename]),
    case cowboy_req:header(<<"content-transfer-encoding">>, Req) of
        <<"base64">> ->
            io:format("base64 encoded request coming in\n"),
            Base64Decoded = decode64(FileContent),
            check_folder_and_save(Filename, Base64Decoded, ContentType, Req);
        _Else ->
            io:format("unexpected transfer encoding: '~s'", [_Else]),
            check_folder_and_save(Filename, FileContent, ContentType, Req)
    end.


%% Error handling

-spec handle_failed_multipart(cowboy_req:req()) -> cowboy_req:req() | stop_return().
handle_failed_multipart(Req) ->
    io:format("\n\n~p.handle_failed_multipart/2: Expectation Failed\n",[?MODULE]),
    cowboy_req:reply(417, #{}, <<"Failed to read multipart">>, Req).        %% Expectation Failed
-spec handle_wrong_content_type(cowboy_req:req()) -> cowboy_req:req() | stop_return().
handle_wrong_content_type(Req) ->
    io:format("\n\n~p.handle_wrong_content_type/2: Wrong content type\n",[?MODULE]),
    cowboy_req:reply(415, #{}, <<"Wrong content type">>, Req).              %% Unsupported Media Type
-spec handle_max_filesize_exceeded(cowboy_req:req()) -> stop_return().
handle_max_filesize_exceeded(Req) ->
    io:format("\n\n~p.handle_max_filesize_exceeded/1: file size exceeded, max is ~p\n", [?MODULE, ?MAX_UPLOAD_SIZE]),
    cowboy_req:reply(413, #{}, <<"File exceeded max size">>, Req).          %% Payload Too Large
-spec handle_file_exist(cowboy_req:req()) -> stop_return().
handle_file_exist(Req) ->
    io:format("\n\n~p.handle_file_exist/1: file already exist\n", [?MODULE]),
    cowboy_req:reply(409, #{}, <<"File already exist">>, Req).              %% Conflict
-spec handle_method_not_allowed(cowboy_req:req()) -> stop_return().
handle_method_not_allowed(Req) ->
    io:format("\n\n~p.handle_method_not_allowed/1: Method Not Allowed\n", [?MODULE]),
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req).              %% Method Not Allowed
-spec handle_other_errors(cowboy_req:req()) -> stop_return().
handle_other_errors(Req) ->
    io:format("\n\n~p.handle_other_errors/1: Other error occurred\n", [?MODULE]),
    cowboy_req:reply(400, #{}, <<"Other error occurred">>, Req).            %% Bad request


%% Helper functions

-spec get_query_string_data(cowboy_req:req()) -> {kz_json:object(), cowboy_req:req()}.
get_query_string_data(Req) ->
    QS = cowboy_req:parse_qs(Req),
%%    io:format("\n~p.get_query_string_data/1:\nQueryString=~p\n",[?MODULE, QS]),
    get_query_string_data(QS, Req).

-spec get_query_string_data(kz_term:proplist(), cowboy_req:req()) -> {kz_json:object(), cowboy_req:req()}.
get_query_string_data([], Req) ->
    {kz_json:new(), Req};
get_query_string_data(QS, Req) ->
    QS1 = kz_json:from_list(QS),
%%    io:format("\n~p.get_query_string_data/2:\nQueryString=~p\nEncodedQS=~p\n", [?MODULE, QS1, kz_json:encode(QS1)]),
    {QS1, Req}.

-spec get_content_type(cowboy_req:req()) -> kz_term:api_ne_binary().
get_content_type(Req) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        'undefined' -> 'undefined';
        {Main, Sub, _Opts} ->  <<Main/binary, "/", Sub/binary>>
    end.

-spec uploaded_filename(cowboy_req:req()) -> kz_term:ne_binary().
uploaded_filename(Req) ->
    case cowboy_req:header(Req, <<"filename">>) of
        'undefined' -> default_filename();
        Filename ->
            io:format("\n~p.uploaded_filename/1: found filename on request: ~s", [?MODULE, Filename]),
            Filename
    end.

-spec default_filename() -> kz_term:ne_binary().
default_filename() ->
    TempFilename = <<"uploaded_file_", (kz_term:to_binary(kz_time:now_s()))/binary>>,
    io:format("\n~p.uploaded_filename/1: Can't find filename, get default='~p'",[?MODULE,TempFilename]),
    TempFilename.


-spec check_folder_and_save(kz_term:ne_binary(), binary(), kz_term:ne_binary(), cowboy_req:req()) ->
    cowboy_req:req() | stop_return().
check_folder_and_save(Filename, FileContents, ContentType, Req) ->
    case filelib:ensure_dir(?UPLOAD_DIR) of
        'ok' ->
            io:format("\n~p.check_folder_and_save/4: Folder exist (or created)\n",[?MODULE]),
            check_file_and_save(list_to_binary(?UPLOAD_DIR), Filename, FileContents, ContentType, Req);
        {'error', Reason} ->
            io:format("\n~p.check_folder_and_save/4: Folder '~p' not exist by reason=~p\n",
                [?MODULE, ?UPLOAD_DIR, Reason]),
            handle_other_errors(Req)
    end.


-spec check_file_and_save(kz_term:ne_binary(), kz_term:ne_binary(), binary(), kz_term:ne_binary(), cowboy_req:req()) ->
    cowboy_req:req() | stop_return().
check_file_and_save(Foldername, Filename, FileContents, ContentType, Req) ->
    case filelib:find_file(Filename, Foldername) of
        'ok' ->
            io:format("\n~p.check_file_and_save/5: file '~p' already exist\n",[?MODULE,Foldername]),
            handle_file_exist(Req);
        {'error', 'not_found'} ->
            FullFilename = <<Foldername, "/", Filename>>,
            io:format("\n~p.check_file_and_save/4: FullFilename=~p\n", [?MODULE, FullFilename]),
            save_file(FullFilename, FileContents, ContentType, Req)
    end.

-spec save_file(kz_term:ne_binary(), binary(), kz_term:ne_binary(), cowboy_req:req()) ->
    cowboy_req:req() | stop_return().
save_file(Filename, FileContents, ContentType, Req) ->
    io:format("\n~p.save_file/4:\nFilename=~p\nFileContents=~p\nContentType=~p\n",
        [?MODULE, Filename, FileContents, ContentType]),
    case file:write_file(Filename, FileContents) of
        'ok' -> Req;
        {error, Reason} ->
            io:format("Save file failed by reason '~p'\n",[Reason]),
            handle_other_errors(Req)
    end,
    'ok'.

-spec decode64(binary()) -> binary().
decode64(Bin) when is_binary(Bin) ->
    Bin2 = case byte_size(Bin) rem 4 of
               2 -> << Bin/binary, "==" >>;
               3 -> << Bin/binary, "=" >>;
               _ -> Bin
           end,
    base64:decode(Bin2).


%%    {ok, Headers, Req2} = cowboy_req:read_part(Req),
%%    {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
%%    {file, <<"inputfile">>, Filename, ContentType}
%%        = cow_multipart:form_data(Headers),
%%    io:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
%%        [Filename, ContentType, Data]),
%%    {ok, Req3, Opts}.

