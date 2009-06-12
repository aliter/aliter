Aliter is a server for the Ragnarok Online MMORPG. It is written in Haskell and aims to be incredibly flexible and speedy.

# Installing

To install Aliter, simply run this in the base directory after a fresh clone or download:

    sudo cabal install -- sudo might not be necessary

Note that you will need [GHC](http://haskell.org/ghc) and [cabal-install](http://www.haskell.org/cabal/) installed beforehand. GHC is the Glasgow Haskell Compiler, and Cabal is used for quickly installing Haskell packages.

# Configuring

Before starting up Aliter, you'll need a `~/.aliter/main.conf` configuration file. Here's a template:

    [maps]
    sdata: /path/to/sdata.grf
    load: prontera
          izlude
          new_zone01
          # etc...

    [database]
    interface: mysql # NOTE: Currently only MySQL is supported, more to come in the future
    host: localhost
    username: # MySQL username
    password: # MySQL password
    database: aliter

    [login]
    host: # Public IP here
    port: 6900 # Login server port

    [char]
    name: Aliter
          Testing
    host: # Public IP here (for Aliter)
          # Public IP here (for Testing)
    port: 6121 # Aliter port
          6122 # Testing port
    maintenance: 0 # Is Aliter in maintenance mode?
                 0 # Is Testing in maintenance mode?
    new: 0
         0

    [zone]
    host: # Public IP here
    port: 5121 # Zone port
