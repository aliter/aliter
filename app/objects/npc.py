class NPC(object):
    def __init__(self, id, name, position, sprite, scriptFile, scriptOffset):
        self.id     = id
        self.name   = name
        self.map    = position[0]
        self.x      = int(position[1])
        self.y      = int(position[2])
        self.dir    = int(position[3])
        self.sprite = sprite
        self.scriptFile   = scriptFile
        self.scriptOffset = scriptOffset
