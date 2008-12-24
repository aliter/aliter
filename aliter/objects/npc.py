class NPC(object):
    def __init__(self, num, id, name, position, sprite, script):
        self.num = num
        self.id = id
        self.name = name
        self.map = position[0]
        self.x = int(position[1])
        self.y = int(position[2])
        self.dir = int(position[3])
        self.sprite = int(sprite)
        self.script = script
        
        self.script.origin = self
    
    def run(self, actor):
        """
        Executes the NPC's script.
        """
        self.script.run(actor)
    

