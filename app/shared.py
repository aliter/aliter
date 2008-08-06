from lib import yaml

file = open('conf/config.yaml')
config = yaml.load(file.read())
file.close()

file = open('conf/scripts.yaml')
config['scripts'] = yaml.load(file.read())
file.close()

file = open('conf/maps.yaml')
config['maps'] = yaml.load(file.read())
file.close()

maps = {}
