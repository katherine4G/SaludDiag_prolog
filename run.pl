:- initialization(main, main).

main :-
    working_directory(Dir, Dir),
    directory_file_path(Dir, 'server', ServerDir),
    directory_file_path(ServerDir, '..', RootDir),
    directory_file_path(RootDir, 'knowledge', KnowledgeDir),
    asserta(user:file_search_path(knowledge, KnowledgeDir)),
    asserta(user:file_search_path(server, ServerDir)),
    catch(use_module(server(server)), E, (print_message(error, E), halt(1))),
    catch(server(8080), E2, (print_message(error, E2), halt(1))),
    thread_get_message(_).  % mantiene el proceso activo
