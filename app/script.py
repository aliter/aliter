environment = {}

class Script(object):
    def __init__(self, code, filename = "<string>"):
        self.code = compile(code, filename, "exec")
    
    def run(self, actor):
        self.actor = actor
        
        exec self.code in { "say": self.say, "close": self.close }
    
    def say(self, message):
        self.actor.session.sendPacket(
            0xb4,
            actorID = self.actor.gameID,
            message = message + "\x00"
        )
    
    def close(self):
        self.actor.session.sendPacket(
            0xb6,
            actorID = self.actor.gameID
        )
    
