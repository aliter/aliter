loginMap = {}

def setLoginID(accountID, loginIDa, loginIDb):
    global loginMap
    loginMap[accountID] = (loginIDa, loginIDb)

def unsetLoginID(accountID):
    global loginMap
    del loginMap[accountID]

def getLoginIDa(accountID):
    global loginMap
    return loginMap[accountID][0]

def getLoginIDb(accountID):
    global loginMap
    return loginMap[accountID][1]

def checkLoginID(accountID, loginIDa=0, loginIDb=0):
    global loginMap
    if not loginIDa and not loginIDb:
        return False
    if accountID not in loginMap:
        return False
    if loginMap[accountID][0] == loginIDa and loginMap[accountID][1] == loginIDb:
        return True
    if loginMap[accountID][0] == loginIDa:
        return True
    if loginMap[accountID][1] == loginIDb:
        return True
    return False
