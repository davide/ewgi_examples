%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc Example Index. List of examples with links.

-module(ewgi_index).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([run/2]).

run({ewgi_context, Request, _Response}, []) ->
    Body = "<html><head><title>Ewgi Examples</title></head>
<body>
<h2>Ewgi Examples</h2>
<ul>
<li><a href=\"/hello\">Hello World</a>: simple hello world (source file: src/ewgi_hello.erl)</li>
<li><a href=\"/HELLO\">HELLO WORD</a>: simple middleware transforming all the body in uppercase (source file: src/ewgi_to_upper.erl)</li>
<li><a href=\"/test.txt\">File streaming</a>: streams the the file priv/www/test.txt (source file: src/ewgi_stream_file.erl)</li>
<li><a href=\"/gzhello\">Gzip encodes the Hello World example</a>: if the browser accepts gzip encoding the result of the hello_app is gzipped (source file: src/ewgi_deflate.erl)</li>
<li><a href=\"/postex\">Post example</a>: middleware handling of POST data (source file: src/ewgi_post.erl)</li>
<li>Session examples (src/ewgi_session.erl):
<ul><li><a href=\"/session/cookie\">client-side</a>: encrypted client-side session storage using cookies (source file: src/ewgi_session_cookie_store.erl)</li>
<li><a href=\"/session/server\">server-side</a>: server-side session storage using an ets table (source file: src/ewgi_session_server_store.erl)</li>
</ul></li>
</ul>
</body>
</html>",
    ResponseHeaders = [{"Content-type", "text/html"}],
    Response = {ewgi_response, {200, "OK"}, ResponseHeaders,
                Body, undefined},
    {ewgi_context, Request, Response}.
