-module(write_file_info).
-include_lib("kernel/include/file.hrl").

- export([visit/1, visit_list/1, visit/3]).

as_epoch(Date) -> UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
                  calendar:datetime_to_gregorian_seconds(Date) - UnixEpoch.


visit(Filename)  ->  case file:read_file_info(Filename) of
                        {ok, FileData} ->   visit(Filename, FileData#file_info.type, FileData);
                        {error, _} -> io:format("Unable to stat ~s ~n", [Filename])
                  end.
                  
visit(Filename, regular, FileData)  -> Atime = FileData#file_info.atime,
                                            Mtime = FileData#file_info.mtime,
                                            Ctime = FileData#file_info.ctime,
                                            io:format("~s ~b ~b ~b ~n", [Filename, as_epoch(Atime), as_epoch(Ctime), as_epoch(Mtime)]);

visit(Filename, directory, _FileData)  -> io:format("Add ~s to worklist!~n", [Filename]);

visit(Filename, Other, _FileData)  -> io:format("Ignoring ~w file  ~s to worklist!~n", [Other, Filename]).


visit_list([]) -> [] ;
visit_list([ H | T]) -> visit(H), visit_list(T).






