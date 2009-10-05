% Structure: {servers, {login, [{char, zone}, {char2, zone2}]}}
{servers,
  % Login server port
 {{port, 6900},
  {node, {localhost, login}},
  {aliter, "/path/to/aliter"}, % Absolute path to Aliter directory on node.

   % First char & zone server. Others go after in the list.
  [{"Aliter",
    [{node, {localhost, char1}},
     {aliter, "/path/to/aliter"},
     {host, {127,0,0,1}},
     {port, 6121},
     {maintenance, 0},
     {new, 0}],
    [{node, {localhost, zone1}},
     {aliter, "/path/to/aliter"},
     {host, {127,0,0,1}},
     {port, 5121}]}]}}.

% API configuration.
{api,
 [{port, 8000},
  {key, "CHANGE ME UNLESS YOU LIKE GETTING YOUR SERVER HACKED"}]}.
