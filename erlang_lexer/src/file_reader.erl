-module(file_reader).
-export([read_file/1, read_multiple_files/1, list_example_files/0, get_example_path/1]).

%% Read a single file and return its contents as a string.
-spec read_file(string()) -> {ok, string()} | {error, term()}.
read_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            {ok, binary_to_list(Binary)};
        {error, Reason} ->
            {error, {file_error, FilePath, Reason}}
    end.

%% Read multiple files concurrently using spawned processes.
%% Returns a list of {Filename, Contents} tuples in the original order.
-spec read_multiple_files([string()]) -> [{string(), {ok, string()} | {error, term()}}].
read_multiple_files(FilePaths) ->
    Parent = self(),
    Refs = lists:map(
        fun(Path) ->
            Ref = make_ref(),
            spawn(fun() ->
                Result = read_file(Path),
                Parent ! {Ref, Path, Result}
            end),
            {Ref, Path}
        end,
        FilePaths
    ),
    collect_results(Refs, []).

%% Collect results from spawned file-reading processes in order.
collect_results([], Acc) ->
    lists:reverse(Acc);
collect_results([{Ref, Path} | Rest], Acc) ->
    receive
        {Ref, Path, Result} ->
            collect_results(Rest, [{Path, Result} | Acc])
    after 5000 ->
        collect_results(Rest, [{Path, {error, timeout}} | Acc])
    end.

%% Construct the path to an example file.
-spec get_example_path(string()) -> string().
get_example_path(Filename) ->
    filename:join([get_examples_dir(), Filename]).

%% List all .txt files in the examples directory.
-spec list_example_files() -> {ok, [string()]} | {error, term()}.
list_example_files() ->
    Dir = get_examples_dir(),
    case file:list_dir(Dir) of
        {ok, Files} ->
            TxtFiles = [F || F <- Files, filename:extension(F) =:= ".txt"],
            {ok, lists:sort(TxtFiles)};
        {error, Reason} ->
            {error, {dir_error, Dir, Reason}}
    end.

%% Get the path to the examples directory relative to this module.
get_examples_dir() ->
    ScriptDir = filename:dirname(code:which(?MODULE)),
    filename:join([filename:dirname(ScriptDir), "examples"]).
