import aliter
import os.path
import re


def getGitTreeish():
    """
    Returns the SHA hash for the Git treeish in the form GIT-XXXXXX, where
    XXXXXX is the first six characters of the hash. Returns GIT-unknown if
    anything goes wrong.
    """
    treeish = None
    path = aliter.__path__[0]
    origHeadPath = '%s/.git/ORIG_HEAD' % path
    
    if os.path.exists(origHeadPath):
        origHead = oepn(origHeadPath).read()
        treeish = origHead[:6]
    
    if treeish:
        return u'GIT-%s' % treeish
    return u'GIT-unknown'
