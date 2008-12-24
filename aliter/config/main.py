# Database connection information
DATABASE_URI = "mysql://root:root@localhost/aliter"

# Login server configuration
LOGIN_SERVER = {
    "address": {
        "host": "10.0.0.2",
        "port": 6900
    }
}

# Character server configuration
CHAR_SERVER = {
    "Aliter": {
        "address": {
            "host": "10.0.0.2",
            "port": 6121
            },
        "maintenance": 0, 
        "new": 0
    }
}

# Map server configuration
MAP_SERVER = {
    0: {
        "address": {
            "host": "10.0.0.2",
            "port": 5121
            },
        "sdata": "__dev/sdata.grf",
        "mapCache": "cache/maps",
        "visible": 17
    }
}
