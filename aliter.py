import os, sys, time, termios, traceback
from struct import unpack

from twisted.internet import reactor

from app import log
from app.shared import config, maps
from app.constants import *
from app.exceptions import ScriptError
from app.misc import ttysize
from app.objects import Server, Map
from app.sessions import LoginSession, CharSession, MapSession
from app.script import Scripts
from app.tools import grf

class Aliter(object):
    def main(self):
        self.header()
        if not self.load():
            return 1
        if not self.initServers():
            return 1
        log.console('\nAliter ready.', color=ANSI_LIGHT_GREEN)
        
        # Start scheduler
        reactor.run()
        
        # Sometimes it takes a few key presses to exit... TODO: Fix
        log.console('Ctrl+C detected: Sending kill signal.\n', log.HIGH)
        self.shutdown()
        return 0
    
    # Console header
    def header(self):
        try:
            width = ttysize()
            width = int(width[1])
            if type(width) != int or width < 80:
                width = 80
        except:
            width = 80
        
        print '\033[2J\033[0;0H' + ANSI_YELLOW + '  _________ __     ___             __             ___            '.center(width)
        print ' /   _____/|__| __| _/______  _  _|__| ____    __| _/___________ '.center(width)
        print ' \\_____  \\ |  |/ __ |/ __ \\ \\/ \\/ /  |/    \\  / __ |/ __ \\_  __ \\'.center(width)
        print ' /        \\|  / /_/ \\  ___/\\     /|  |   |  \\/ /_/ \\  ___/|  | \\/'.center(width)
        print '/_______  /|__\\____ |\\___  >\\/\\_/ |__|___|  /\\____ |\\___  >__|   '.center(width)
        print '        \\/         \\/    \\/               \\/      \\/    \\/       '.center(width)
        print ANSI_RED + '-' * width
        print ANSI_WHITE + 'S I D E W I N D E R   R E V I S I O N   0 x x'.center(width)
        print ANSI_RED + '-' * width
        print ANSI_DEFAULT
    
    # Load all required data into memory
    def load(self):
        try:
            # Generate map cache if needed
            cached = 0
            for file in os.listdir(config['MapServer'][0]['mapCache']):
                if file[-4:] == '.map':
                    file = open(config['MapServer'][0]['mapCache']+'/'+file, 'r')
                    header, version = unpack('3sB', file.read(4))
                    if header == 'MAP' and version == MAP_CACHE_VERSION:
                        cached = 1
                    break
            if not cached:
                # Delete all cache files first in case they are an older version
                for file in os.listdir(config['MapServer'][0]['mapCache']):
                    if file[-4:] == '.map':
                        os.remove(config['MapServer'][0]['mapCache']+'/'+file)
                print ANSI_WHITE + 'Generating map cache:' + ANSI_DEFAULT
                grf.generateCache(config['MapServer'][0]['sdata'])
                print ANSI_WHITE + 'Generating map cache: Done' + ANSI_DEFAULT
            
            # Load maps
            print ''
            for map in config['maps']:
                if map not in maps:
                    print '\033[A\033[2KLoading maps... %s' % map
                    try:
                        maps[map] = Map(map)
                    except IOError:
                        print ANSI_LIGHT_RED + "\033[A\033[2KError loading map: %s [File doesn't exist]" % map + ANSI_DEFAULT
                        print ''
            print '\033[A\033[2KLoading maps... Done'
            
            # Load scripts
            print ''
            for file in config['scripts']:
                print '\033[A\033[2KLoading scripts... %s' % file
                try:
                    Scripts.load(file)
                except IOError:
                    print ANSI_LIGHT_RED + "\033[A\033[2KError loading script: %s [File doesn't exist]" % file + ANSI_DEFAULT
                    print ''
                except ScriptError, msg:
                    #print ANSI_LIGHT_RED + '\033[A\033[2KError loading script: %s [%s]' % (file, msg) + ANSI_DEFAULT
                    print ANSI_LIGHT_RED + 'Error loading script: %s [%s]' % (file, msg) + ANSI_DEFAULT
                    print ''
            print '\033[A\033[2KLoading scripts... Done'
        except KeyboardInterrupt:
            print ANSI_YELLOW + 'Ctrl+C detected: Shutting down Aliter.' + ANSI_DEFAULT
            sys.exit(1)
        
        # Add blank line
        print ''
        return True
    
    # Initialize servers
    def _initServer(self, serverInstance, port):
        "Start a server and handle any exceptions if needed"
        while 1:
            try:
                return reactor.listenTCP(port, serverInstance)
            except socket.error, msg:
                if msg[0] == 98:
                    log.unbound('Waiting 10 seconds...')
                    time.sleep(10)
            except:
                log.unbound('Exception raised <%s>: %s' % (sys.exc_info()[0], sys.exc_info()[1]), log.CRITICAL)
                for line in traceback.format_tb(sys.exc_info()[2]):
                    log.unbound(line[:-1], log.CRITICAL)
                return False
    
    def initServers(self):
        self.map   = self._initServer(Server(MapSession, log.map), config['MapServer'][0]['address']['port'])
        self.char  = None
        self.login = None
        if self.map:
            self.char = self._initServer(Server(CharSession, log.char), config['CharServer'].values()[0]['address']['port'])
            if self.char:
                self.login = self._initServer(Server(LoginSession, log.login), config['LoginServer']['address']['port'])
        if not self.map or not self.char or not self.login:
            log.console('Shutting down Aliter.\n', log.CRITICAL)
            if self.map:
                self.map.factory.shutdown(self.map)
            if self.char:
                self.char.factory.shutdown(self.char)
            if self.login:
                self.login.factory.shutdown(self.login)
            return False
        return True
    
    # Shutdown cleanly
    def shutdown(self):
        self.login.factory.shutdown(self.login)
        self.char.factory.shutdown(self.char)
        self.map.factory.shutdown(self.map)

if __name__ == "__main__":
    sys.exit(Aliter().main())
