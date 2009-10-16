% Name for this server
{name, "Aliter"}.

% Host (node) for this server
{host, {host, name}}.

% Public IP/port configuration
{ip, {127,0,0,1}}.
{port, 6121}.

% Path to Aliter on this server's host
{aliter, "/path/to/aliter"}.

% Node of the master zone server associated with this char server
{zone, name@host}.

% Is the server undergoing maintenance?
{maintenance, 0}.

% Is the server new?
{new, 0}.
