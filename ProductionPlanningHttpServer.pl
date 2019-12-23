http_server(http_dispatch, [port(5005)]).
:-http_handler(root(factory/Factory), factory(Method,Factory),[method(Method),methods([post])])





process_factory_json(post,Factory,Request):-.