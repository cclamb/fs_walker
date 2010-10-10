-module(visitor_callback).

-export([init/1, finalize/1, visit_file/3, visit_directory/3, ok_to_visit/3]).

-record(state, {module=?MODULE, rank=0, io_dev, filename}).

-include_lib("kernel/include/file.hrl").

%%% 
%%% init takes 1 argument (integer) as an indentifier for the client
%%% init returns a State object for the callback module.
%%%
%%% -- in this example, the state contains the file name and an IO handle.
%%%
init(Index) ->
    FileName = lists:concat(["client.", Index, ".out"]),
    case file:open(FileName, [write]) of
        {ok, IoDev} ->
            State = #state{rank=Index, io_dev=IoDev, filename=FileName},
            {ok, State};
        {error, Reason} -> {error, Reason}
    end.

%%% 
%%% finalize(State) 
%%%   perform any needed cleanups before the State is destroyed.
%%%
%%% -- in this example, we close the file handle
%%%
finalize(State) ->
    IoDev = State#state.io_dev,
    file:close(IoDev).


%%%
%%% visit_file(State, Filename, Directory).
%%%   When this function is called, the current working directory is already
%%%    set to Directory.
%%%
visit_file(State, Filename, Directory) ->
    %% Get file info
    case file:read_file_info(Filename) of
        {ok, FileData} ->
            my_visit_file(State, Filename, Directory,
                          FileData#file_info.type, FileData);
        {error, Reason} ->
            fs_event:file_error(Filename, Reason)
    end,
    ok.

%%% visit_directory(State, Directory, pre|post)
%%% This function gets called before visiting stuff inside the directory (pre),
%%% and after the directory traversal is complete (post)
%%%
visit_directory(State, Directory, pre) ->
    {ok, Cwd} = file:get_cwd(),
    io:format(State#state.io_dev, "#Visiting directory ~p CWD = ~p~n",
              [Directory, Cwd]),
    ok;

visit_directory(_State, _Directory, post) -> ok.

%%% Convert an ERLANG time into a UnixEpoch time.
%%% It would be nice to lift out the constant. 
as_epoch(Date) ->
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    calendar:datetime_to_gregorian_seconds(Date) - UnixEpoch.

%%% Internal function for visiting a file. Its called with the file name information,
%%% the type of the file, and the results from read_file_info.
%%%
%%% This is the clause for a normal file.
my_visit_file(State, Filename, Dir, regular, FileData)  ->
    Atime = FileData#file_info.atime,
    Mtime = FileData#file_info.mtime,
    Ctime = FileData#file_info.ctime,
    io:format(State#state.io_dev,
              "~s/~s ~b ~b ~b ~n",
              [Dir, Filename, as_epoch(Atime), as_epoch(Ctime), as_epoch(Mtime)]);

%%% This clause handles any other cases, like links.
%%%
my_visit_file(_State, Filename, _Dir, Other, _FileData) ->
    fs_event:info_message("Ignoring ~w file  ~s to worklist!~n", [Other, Filename]).

%%% Predicate - returns {ok}/{skip}/{error, Reason}
%%% This function neeeds to be filled out!
%%% The third argument is an atom; directory if we are visiting a directory,
%%%
%%% the current fs_visitor does not call this function with any other values!
ok_to_visit(_State, Directory, directory) ->
    %% exists and is directory
    %% should check permissions here!
    case filelib:is_dir(Directory) of
        true -> {ok};
        false -> {skip};
        X -> {error,  X}
    end;

ok_to_visit(_State, _Filename, _Other) ->
    %% error?
    %% exists
    %% is directory
    %% readable
    %% executable
    {skip}.

