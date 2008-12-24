"""
Global Aliter exceptions.
"""
class ImproperlyConfigured(Exception):
    """
    Aliter has been improperly configured.
    """
    pass

class InvalidPacket(Exception):
    """
    The sent or requested packet is invalid.
    """
    pass

class IllegalPacket(Exception):
    """
    The sent or requested packet is illegal.
    """
    pass

class ConnectionClosed(Exception):
    """
    The connection has been closed.
    """
    pass

class InvalidAccount(Exception):
    """
    The requested account is invaid.
    """
    pass

class InvalidActor(Exception):
    """
    The requseted actor is invalid.
    """
    pass

class InvalidItem(Exception):
    """
    The requsted argument is invalid.
    """
    pass

class MissingArgument(Exception):
    """
    The expression is missing an argument.
    """
    pass

class ScriptError(Exception):
    """
    An error occured while validating a script.
    """
    def __init__(self, message, line=None, file=None):
        self.message = message
        self.line    = line
        self.file    = file
    
    def __str__(self):
        if self.line:
            return 'Line %s: %s' % (self.line, self.message)
        return self.message
