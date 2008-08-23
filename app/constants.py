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

JOB = {
    "Novice": 0,
    "Swordsman": 1,
    "Mage": 2,
    "Archer": 3,
    "Acolyte": 4,
    "Merchant": 5,
    "Thief": 6,
    "Knight": 7,
    "Priest": 8,
    "Wizard": 9,
    "Blacksmith": 10,
    "Hunter": 11,
    "Assassin": 12,
    "Knight2": 13,
    "Crusader": 14,
    "Monk": 15,
    "Sage": 16,
    "Rogue": 17,
    "Alchem": 18,
    "Alchemist": 18,
    "Bard": 19,
    "Dancer": 20,
    "Crusader2": 21,
    "Wedding": 22,
    "SuperNovice": 23,
    "Gunslinger": 24,
    "Ninja": 25,
    "Xmas": 26,

    "Novice High": 4001,
    "Swordsman High": 4002,
    "Mage High": 4003,
    "Archer High": 4004,
    "Acolyte High": 4005,
    "Merchant High": 4006,
    "Thief High": 4007,
    "Lord Knight": 4008,
    "High Priest": 4009,
    "High Wizard": 4010,
    "Whitesmith": 4011,
    "Sniper": 4012,
    "Assassin Cross": 4013,
    "Lord Knight2": 4014,
    "Paladin": 4015,
    "Champion": 4016,
    "Professor": 4017,
    "Stalker": 4018,
    "Creator": 4019,
    "Clown": 4020,
    "Gypsy": 4021,
    "Paladin2": 4022,

    "Baby": 4023,
    "Baby Swordsman": 4024,
    "Baby Mage": 4025,
    "Baby Archer": 4026,
    "Baby Acolyte": 4027,
    "Baby Merchant": 4028,
    "Baby Thief": 4029,
    "Baby Knight": 4030,
    "Baby Priest": 4031,
    "Baby Wizard": 4032,
    "Baby Blacksmith": 4033,
    "Baby Hunter": 4034,
    "Baby Assassin": 4035,
    "Baby Knight2": 4036,
    "Baby Crusader": 4037,
    "Baby Monk": 4038,
    "Baby Sage": 4039,
    "Baby Rogue": 4040,
    "Baby Alchemist": 4041,
    "Baby Bard": 4042,
    "Baby Dancer": 4043,
    "Baby Crusader2": 4044,
    "Super Baby": 4045,

    "Taekwon": 4046,
    "Star Gladiator": 4047,
    "Star Gladiator2": 4048,
    "Soul Linker": 4049
}

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
