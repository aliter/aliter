Quick'n'Dirty Install Instructions
==================================

Some of this is a bit repetitive and may be simplified in the future:

1. `make`
2. Copy `config/` to `~/.aliter/config`
3. Rename `~/.aliter/config/char/name@host` to the name for your character
   server. An example would be `char@localhost`.
4. Rename `~/.aliter/config/zone/name@host` to the name for your zone server.
   An example would be `zone@localhost`.
5. Edit `~/.aliter/config/char/<name>/server.erl`:
    1. Set the `host` to the same information from step 3 (e.g. for
       `char@localhost`, you'd have `{host, {localhost, char}}`).
    2. Set `aliter` to the path to your Aliter directory (where this file is
       located).
    3. Set `zone` to the name of your zone server from step 3.
    4. (Optional.) Set the name for your server, and whatever IP/port
       configuration you want.
6. Repeat the same steps from step 4. for
   `~/.aliter/config/zone/<name>/server.erl`, setting `char` to the same
   information from step 4. You may also want to tweak the individual `zones`,
   changing port numbers and map lists.
7. Edit `~/.aliter/config/login/server.erl`:
    1. Set `host` to the name for your login server, e.g. `{localhost, login}`.
    2. Set `aliter` to the path to your Aliter directory (where this file is
       located).
    3. (Optional.) Set the IP/port configuration you want.
8. `make install`
9. `make start`
