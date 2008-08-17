import thread

# Identify the current thread to determine whether packets in other threads should be sent as threaded with Twisted
MAIN_THREAD = thread.get_ident()

# Aliter related
MAP_CACHE_VERSION = 3
AREA_SERVER, AREA_MAP, AREA_VISIBLE = range(3)

# Character stats
STAT_SPEED  = 0x00
STAT_HP     = 0x05
STAT_MAX_HP = 0x06
STAT_SP     = 0x07
STAT_MAX_SP = 0x08
STAT_STAT_POINTS  = 0x09
STAT_BASE_LEVEL   = 0x0b
STAT_SKILL_POINTS = 0x0c
STAT_WEIGHT       = 0x18 # True weight = STAT_WEIGHT / 10
STAT_MAX_WEIGHT   = 0x19 # True max weight = STAT_MAX_WEIGHT / 10
STAT_ATK_A  = 0x29
STAT_ATK_B  = 0x2a
STAT_MATK_A = 0x2b
STAT_MATK_B = 0x2c
STAT_DEF_A  = 0x2d
STAT_DEF_B  = 0x2e
STAT_MDEF_A = 0x2f
STAT_MDEF_B = 0x30
STAT_HIT    = 0x31
STAT_FLEE_A = 0x32
STAT_FLEE_B = 0x33
STAT_ASPD   = 0x35 # In 2ms units?
STAT_JOB_LEVEL = 0x37

Job_Novice = 0
Job_Swordman = 1
Job_Mage = 2
Job_Archer = 3
Job_Acolyte = 4
Job_Merchant = 5
Job_Thief = 6
Job_Knight = 7
Job_Priest = 8
Job_Wizard = 9
Job_Blacksmith = 10
Job_Hunter = 11
Job_Assassin = 12
Job_Knight2 = 13
Job_Crusader = 14
Job_Monk = 15
Job_Sage = 16
Job_Rogue = 17
Job_Alchem = 18
Job_Alchemist = 18
Job_Bard = 19
Job_Dancer = 20
Job_Crusader2 = 21
Job_Wedding = 22
Job_SuperNovice = 23
Job_Gunslinger = 24
Job_Ninja = 25
Job_Xmas = 26

Job_Novice_High = 4001
Job_Swordman_High = 4002
Job_Mage_High = 4003
Job_Archer_High = 4004
Job_Acolyte_High = 4005
Job_Merchant_High = 4006
Job_Thief_High = 4007
Job_Lord_Knight = 4008
Job_High_Priest = 4009
Job_High_Wizard = 4010
Job_Whitesmith = 4011
Job_Sniper = 4012
Job_Assassin_Cross = 4013
Job_Lord_Knight2 = 4014
Job_Paladin = 4015
Job_Champion = 4016
Job_Professor = 4017
Job_Stalker = 4018
Job_Creator = 4019
Job_Clown = 4020
Job_Gypsy = 4021
Job_Paladin2 = 4022

Job_Baby = 4023
Job_Baby_Swordman = 4024
Job_Baby_Mage = 4025
Job_Baby_Archer = 4026
Job_Baby_Acolyte = 4027
Job_Baby_Merchant = 4028
Job_Baby_Thief = 4029
Job_Baby_Knight = 4030
Job_Baby_Priest = 4031
Job_Baby_Wizard = 4032
Job_Baby_Blacksmith = 4033
Job_Baby_Hunter = 4034
Job_Baby_Assassin = 4035
Job_Baby_Knight2 = 4036
Job_Baby_Crusader = 4037
Job_Baby_Monk = 4038
Job_Baby_Sage = 4039
Job_Baby_Rogue = 4040
Job_Baby_Alchem = 4041
Job_Baby_Alchemist = 4041
Job_Baby_Bard = 4042
Job_Baby_Dancer = 4043
Job_Baby_Crusader2 = 4044
Job_Super_Baby = 4045

Job_Taekwon = 4046
Job_Star_Gladiator = 4047
Job_Star_Gladiator2 = 4048
Job_Soul_Linker = 4049

# ANSI colours
ANSI_DEFAULT     = '\033[0m'
ANSI_BLACK       = '\033[0;30m'
ANSI_DARK_GREY   = '\033[1;30m'
ANSI_RED         = '\033[0;31m'
ANSI_LIGHT_RED   = '\033[1;31m'
ANSI_GREEN       = '\033[0;32m'
ANSI_LIGHT_GREEN = '\033[1;32m'
ANSI_GOLD        = '\033[0;33m'
ANSI_YELLOW      = '\033[1;33m'
ANSI_BLUE        = '\033[0;34m'
ANSI_LIGHT_BLUE  = '\033[1;34m'
ANSI_PURPLE      = '\033[0;35m'
ANSI_MAGENTA     = '\033[1;35m'
ANSI_CYAN        = '\033[0;36m'
ANSI_LIGHT_CYAN  = '\033[1;36m'
ANSI_GREY        = '\033[0;37m'
ANSI_WHITE       = '\033[1;37m'

ANSI_BG_BLACK   = '\033[40m'
ANSI_BG_RED     = '\033[41m'
ANSI_BG_GREEN   = '\033[42m'
ANSI_BG_GOLD    = '\033[43m'
ANSI_BG_BLUE    = '\033[44m'
ANSI_BG_PURPLE  = '\033[45m'
ANSI_BG_CYAN    = '\033[46m'
ANSI_BG_GREY    = '\033[47m'
