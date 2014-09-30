import os
import random

picture_dir = r'H:\transfer\ResizedPictures\300'

pictures = os.listdir(picture_dir)

pictures = [x.split('.')[0] for x in pictures]
random.shuffle(pictures)

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

lexdec_targets = ["acorn","acrobat","antenna","apple","auction","balloon",
    "bamboo","brochure","buckle","butterfly","cabin","calendar",
    "camel","campfire","candy","carousel","cashier","castle",
    "ceiling","celery","cement","ceremony","chandelier","cockpit",
    "collar","concert","cowboy","cradle","croissant","currency",
    "cursor","curtsy","cushion","cutlery","dancer","darkroom",
    "diamond","dinosaur","doorbell","dryer","elephant","eruption",
    "faucet","feather","fingerprint","fossil","galaxy","garlic",
    "goalie","gondola","graffiti","helicopter","hibernation","ladder",
    "ladle","librarian","lightning","lumber","mannequin","meadow",
    "medicine","microwave","minivan","missile","monsoon","motel",
    "movie","mural","napkin","omelet","painter","parachute",
    "patient","pencil","pharmacy","piano","ponytail","popcorn",
    "referee","saddle","safari","sailboat","satellite","sector",
    "seedling","seminar","settlement","shadow","shampoo","shareholder",
    "shelter","shiny","shoplifter","shoulder","shovel","sidewalk",
    "silver","socket","sofa","submarine","sugar","sunroof",
    "surfboard","syrup","table","tadpole","tassel","taxi",
    "teapot","theatre","tire","tissue","tortilla","tractor",
    "traffic","tunnel","umbrella","usher","weatherman","whistle"]

initial = ["ceiling","celery","cement","ceremony","chandelier","saddle",
        "safari","sailboat","satellite","sector",
    "seedling","seminar","settlement","shadow","shampoo","shareholder",
    "shelter","shiny","shoplifter","shoulder","shovel","sidewalk",
    "silver","socket","sofa","submarine","sugar","sunroof",
    "surfboard","syrup"]

#Sanity check
for w in lexdec_targets:
    if w not in targets and w not in initial:
        print("Error %s not found" % w)

for p in pictures:
    if p not in targets:
        print(p)
