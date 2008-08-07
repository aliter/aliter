import zlib
from struct import unpack

from app.shared import config


class Map(object):
    def __init__(self, name):
        # Open and validate
        file   = open('%s/%s.map' % (config['MapServer'][0]['mapCache'], name), 'rb')
        header = unpack('=3sBll', file.read(12))
        if header[0] != 'MAP' or header[2] < 1 or header[3] < 1:
            return False
        
        # Define meta-data
        self.name   = name
        self.width  = header[2]
        self.height = header[3]
        
        # Decompress block data
        blocks = zlib.decompress(file.read())
        self.tiles = []
        offset = 0
        for x in range(0, self.width):
            self.tiles.append(unpack('=%sb' % self.height, blocks[offset:offset+self.height]))
            offset += self.height
        
        # Define other stuff
        self.players  = {}
        self.monsters = {}
        self.npcs     = {}
        self.warps    = {}
    
    def pathfind(self, x, y, toX, toY):
        if x == toX and y == toY:
            return
        
        # Generate walk path
        walkPath   = [[x, y, None]]
        next    = [x, y]
        lastKey = walkPath[0]
        dX, dY  = 0, 0
        while next[0] != toX or next[1] != toY:
            prev = [next[0], next[1]]
            
            if next[0] < toX:
                next[0] += 1
            if next[0] > toX:
                next[0] -= 1
            if next[1] < toY:
                next[1] += 1
            if next[1] > toY:
                next[1] -= 1
            
            # Is the tile walkable?
            if self.tiles[next[0]][next[1]] != 1:
                break # Don't bother trying to find another path atm
            
            # Is this a key point?
            nextDX, nextDY = next[0]-prev[0], next[1]-prev[1]
            if nextDX != dX or nextDY != dY:
                lastKey[2] = walkPath[-1]
                lastKey = walkPath[-1]
                dX, dY  = nextDX, nextDY
            
            walkPath.append([next[0], next[1], None])
        lastKey[2] = walkPath[-1]
        
        return walkPath
