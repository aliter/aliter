class InvalidPacket(Exception):
    pass

class IllegalPacket(Exception):
    pass

class ConnectionClosed(Exception):
    pass

class InvalidAccount(Exception):
    pass

class InvalidActor(Exception):
    pass

class MissingArgument(Exception):
    pass

class ScriptError(Exception):
    def __init__(self, message, line=None, file=None):
        self.message = message
        self.line    = line
        self.file    = file
    
    def __str__(self):
        if self.line:
            return 'Line %s: %s' % (self.line, self.message)
        return self.message
