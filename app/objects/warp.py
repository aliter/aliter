class Warp(object):
    def __init__(self, id, position, warpTo, spanX=1, spanY=1):
        self.id = id
        self.map = position[0]
        self.x = int(position[1])
        self.y = int(position[2])
        self.toMap = warpTo[0]
        self.toX = int(warpTo[1])
        self.toY = int(warpTo[2])
        self.spanX = spanX
        self.spanY = spanY
