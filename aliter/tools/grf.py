import zlib
import pickle
from struct import pack, unpack, calcsize

from aliter.constants import MAP_CACHE_VERSION
from aliter.config import main


# MAP cache format: "MAP".3s <version>.b <width>.l <height>.l {<walkable>.B[w*h] <snipable>.B[w*h]}.zLib
def generateCache(filename):
    file = open(filename)
    
    # Read and validate header
    header = unpack('=16s14sllll', file.read(46))
    if header[0] != 'Master of Magic\x00' \
    or header[5] != 0x200:
        return False
    
    totalFiles      = header[4] - header[3] + 7
    fileTableOffset = calcsize('=16s14sllll') + header[2]
    
    print 'GRF file validated'
    
    # Read file table
    file.seek(fileTableOffset)
    size   = unpack('=ll', file.read(8))
    buffer = zlib.decompress(file.read(size[0]))
    fileTable = {}
    
    print 'GRF file table read'
    
    # Generate GAT file list
    offset = 0
    while offset < size[1]:
        filenameNUL = buffer[offset:offset+128].index('\x00')
        filename    = buffer[offset:offset+filenameNUL]
        if filename[-4:] != '.gat': # We only want GAT files
            offset += filenameNUL+18
            continue
        fileTable[filename] = unpack('=xlllbl', buffer[offset+filenameNUL:offset+filenameNUL+18])
        offset += filenameNUL+18
    totalMaps = len(fileTable)
    
    print 'GAT file list generated - %s maps found' % totalMaps
    
    # Generate cache files
    print 'Caching...  0% complete'
    files = 0
    for name, map in fileTable.iteritems():
        # Decompress file
        file.seek(calcsize('=16s14sllll')+map[4])
        gatFile = zlib.decompress(file.read(map[0]))
        if len(gatFile) != map[2]:
            continue # Corrupt
        
        # Verify header
        gatHeader = unpack('=6sll', gatFile[:14])
        if gatHeader[0] != 'GRAT\x01\x02':
            continue # Corrupt
        
        # Read GAT map blocks
        gatFile  = gatFile[14:]
        offset   = 0
        gatTiles = []
        for block in range(0, gatHeader[1] * gatHeader[2]):
            blockType = gatFile[offset+16]
            if blockType == '\x00' or blockType == '\x03':
                gatTiles.append('\x01') # Walkable and snipable
            elif blockType == '\x04' or blockType == '\x05':
                gatTiles.append('\x02') # Snipable only
            else:
                gatTiles.append('\x00') # Wall
            offset += 20
        
        # Swap X and Y for quick loading
        gatTilesSwap = []
        for x in range(0, gatHeader[1]):
            for y in range(0, gatHeader[2]):
                offset = y*gatHeader[1] + x
                gatTilesSwap.append(gatTiles[offset])
        
        # Save to cache file
        name = name.rsplit('\\', 1)[-1][:-4]
        mapFile = open('%s/%s.map' % (config['MapServer'][0]['mapCache'], name), 'wb')
        mapFile.write(pack('=3sBll', 'MAP', MAP_CACHE_VERSION, gatHeader[1], gatHeader[2]))
        mapFile.write(zlib.compress(''.join(gatTilesSwap)))
        mapFile.close()
        
        files  += 1
        percent = int(float(files) / float(totalMaps) * 100)
        print '\033[A\033[2KCaching... %s%s%% complete [%s]' % (('', ' ')[percent < 10], percent, name)
    
    print '\033[A\033[2KCached %s maps' % files
    
    return True
