import os

picture_dir = '/home/michael/Documents/Linguistics/Dissertation/output/300'

pictures = os.listdir(picture_dir)

pictures = [x.split('.')[0] for x in pictures]

targets = ['tassel','concert','medicine','dinosaur','carousel','dancer',
    'galaxy','monsoon','whistle','pencil','castle','faucet','croissant',
    'missile','taxi','fossil','currency','pharmacy','curtsy','cursor',
    'eruption','patient','brochure','auction','tissue','cushion','cashier',
    'hibernation','parachute','usher','dryer','balloon','antenna','tortilla',
    'microwave','weatherman','graffiti','elephant','fingerprint','cockpit',
    'theatre','bamboo','cabin','tunnel','buckle','doorbell','napkin',
    'calendar','cradle','tractor','ladle','collar','garlic','cowboy',
    'ponytail','acorn','tadpole','butterfly','camel','piano','mansion',
    'bookshelf','windshield','coronation','ocean','meditation','militia',
    'handshake','machine','milkshake','diamond','umbrella','librarian',
    'cutlery','referee','helicopter','lightning','painter','campfire',
    'mannequin','movie','traffic','candy','table','goalie','ladder','popcorn',
    'mural','apple','tire','lumber','omelet','meadow','motel','feather','gondola',
    'acrobat','darkroom','teapot','minivan']

lexdec_targets = []
