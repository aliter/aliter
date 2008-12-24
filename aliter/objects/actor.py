class Actor(object):
    def __init__(self):
        self.direction = 0
        self.toX = self.x
        self.toY = self.y
        self.walkPath  = []
        self.walkPathOffset = 0
        self.speed = 150
        self.moveDelay = self.speed / 1000.0
    

