module Thermite.Html.Elements where

import Thermite.Types
import Thermite.Internal

a :: forall action. Props action -> [Html action] -> Html action
a = createElementFromTagName "a"

a' :: forall action. [Html action] -> Html action
a' = a []

abbr :: forall action. Props action -> [Html action] -> Html action
abbr = createElementFromTagName "abbr"

abbr' :: forall action. [Html action] -> Html action
abbr' = abbr []

address :: forall action. Props action -> [Html action] -> Html action
address = createElementFromTagName "address"

address' :: forall action. [Html action] -> Html action
address' = address []

area :: forall action. Props action -> [Html action] -> Html action
area = createElementFromTagName "area"

area' :: forall action. [Html action] -> Html action
area' = area []

article :: forall action. Props action -> [Html action] -> Html action
article = createElementFromTagName "article"

article' :: forall action. [Html action] -> Html action
article' = article []

aside :: forall action. Props action -> [Html action] -> Html action
aside = createElementFromTagName "aside"

aside' :: forall action. [Html action] -> Html action
aside' = aside []

audio :: forall action. Props action -> [Html action] -> Html action
audio = createElementFromTagName "audio"

audio' :: forall action. [Html action] -> Html action
audio' = audio []

b :: forall action. Props action -> [Html action] -> Html action
b = createElementFromTagName "b"

b' :: forall action. [Html action] -> Html action
b' = b []

base :: forall action. Props action -> [Html action] -> Html action
base = createElementFromTagName "base"

base' :: forall action. [Html action] -> Html action
base' = base []

bdi :: forall action. Props action -> [Html action] -> Html action
bdi = createElementFromTagName "bdi"

bdi' :: forall action. [Html action] -> Html action
bdi' = bdi []

bdo :: forall action. Props action -> [Html action] -> Html action
bdo = createElementFromTagName "bdo"

bdo' :: forall action. [Html action] -> Html action
bdo' = bdo []

big :: forall action. Props action -> [Html action] -> Html action
big = createElementFromTagName "big"

big' :: forall action. [Html action] -> Html action
big' = big []

blockquote :: forall action. Props action -> [Html action] -> Html action
blockquote = createElementFromTagName "blockquote"

blockquote' :: forall action. [Html action] -> Html action
blockquote' = blockquote []

body :: forall action. Props action -> [Html action] -> Html action
body = createElementFromTagName "body"

body' :: forall action. [Html action] -> Html action
body' = body []

br :: forall action. Props action -> [Html action] -> Html action
br = createElementFromTagName "br"

br' :: forall action. [Html action] -> Html action
br' = br []

button :: forall action. Props action -> [Html action] -> Html action
button = createElementFromTagName "button"

button' :: forall action. [Html action] -> Html action
button' = button []

canvas :: forall action. Props action -> [Html action] -> Html action
canvas = createElementFromTagName "canvas"

canvas' :: forall action. [Html action] -> Html action
canvas' = canvas []

caption :: forall action. Props action -> [Html action] -> Html action
caption = createElementFromTagName "caption"

caption' :: forall action. [Html action] -> Html action
caption' = caption []

cite :: forall action. Props action -> [Html action] -> Html action
cite = createElementFromTagName "cite"

cite' :: forall action. [Html action] -> Html action
cite' = cite []

code :: forall action. Props action -> [Html action] -> Html action
code = createElementFromTagName "code"

code' :: forall action. [Html action] -> Html action
code' = code []

col :: forall action. Props action -> [Html action] -> Html action
col = createElementFromTagName "col"

col' :: forall action. [Html action] -> Html action
col' = col []

colgroup :: forall action. Props action -> [Html action] -> Html action
colgroup = createElementFromTagName "colgroup"

colgroup' :: forall action. [Html action] -> Html action
colgroup' = colgroup []

_data :: forall action. Props action -> [Html action] -> Html action
_data = createElementFromTagName "data"

_data' :: forall action. [Html action] -> Html action
_data' = _data []

datalist :: forall action. Props action -> [Html action] -> Html action
datalist = createElementFromTagName "datalist"

datalist' :: forall action. [Html action] -> Html action
datalist' = datalist []

dd :: forall action. Props action -> [Html action] -> Html action
dd = createElementFromTagName "dd"

dd' :: forall action. [Html action] -> Html action
dd' = dd []

del :: forall action. Props action -> [Html action] -> Html action
del = createElementFromTagName "del"

del' :: forall action. [Html action] -> Html action
del' = del []

details :: forall action. Props action -> [Html action] -> Html action
details = createElementFromTagName "details"

details' :: forall action. [Html action] -> Html action
details' = details []

dfn :: forall action. Props action -> [Html action] -> Html action
dfn = createElementFromTagName "dfn"

dfn' :: forall action. [Html action] -> Html action
dfn' = dfn []

dialog :: forall action. Props action -> [Html action] -> Html action
dialog = createElementFromTagName "dialog"

dialog' :: forall action. [Html action] -> Html action
dialog' = dialog []

div :: forall action. Props action -> [Html action] -> Html action
div = createElementFromTagName "div"

div' :: forall action. [Html action] -> Html action
div' = div []

dl :: forall action. Props action -> [Html action] -> Html action
dl = createElementFromTagName "dl"

dl' :: forall action. [Html action] -> Html action
dl' = dl []

dt :: forall action. Props action -> [Html action] -> Html action
dt = createElementFromTagName "dt"

dt' :: forall action. [Html action] -> Html action
dt' = dt []

em :: forall action. Props action -> [Html action] -> Html action
em = createElementFromTagName "em"

em' :: forall action. [Html action] -> Html action
em' = em []

embed :: forall action. Props action -> [Html action] -> Html action
embed = createElementFromTagName "embed"

embed' :: forall action. [Html action] -> Html action
embed' = embed []

fieldset :: forall action. Props action -> [Html action] -> Html action
fieldset = createElementFromTagName "fieldset"

fieldset' :: forall action. [Html action] -> Html action
fieldset' = fieldset []

figcaption :: forall action. Props action -> [Html action] -> Html action
figcaption = createElementFromTagName "figcaption"

figcaption' :: forall action. [Html action] -> Html action
figcaption' = figcaption []

figure :: forall action. Props action -> [Html action] -> Html action
figure = createElementFromTagName "figure"

figure' :: forall action. [Html action] -> Html action
figure' = figure []

footer :: forall action. Props action -> [Html action] -> Html action
footer = createElementFromTagName "footer"

footer' :: forall action. [Html action] -> Html action
footer' = footer []

form :: forall action. Props action -> [Html action] -> Html action
form = createElementFromTagName "form"

form' :: forall action. [Html action] -> Html action
form' = form []

h1 :: forall action. Props action -> [Html action] -> Html action
h1 = createElementFromTagName "h1"

h1' :: forall action. [Html action] -> Html action
h1' = h1 []

h2 :: forall action. Props action -> [Html action] -> Html action
h2 = createElementFromTagName "h2"

h2' :: forall action. [Html action] -> Html action
h2' = h2 []

h3 :: forall action. Props action -> [Html action] -> Html action
h3 = createElementFromTagName "h3"

h3' :: forall action. [Html action] -> Html action
h3' = h3 []

h4 :: forall action. Props action -> [Html action] -> Html action
h4 = createElementFromTagName "h4"

h4' :: forall action. [Html action] -> Html action
h4' = h4 []

h5 :: forall action. Props action -> [Html action] -> Html action
h5 = createElementFromTagName "h5"

h5' :: forall action. [Html action] -> Html action
h5' = h5 []

h6 :: forall action. Props action -> [Html action] -> Html action
h6 = createElementFromTagName "h6"

h6' :: forall action. [Html action] -> Html action
h6' = h6 []

head :: forall action. Props action -> [Html action] -> Html action
head = createElementFromTagName "head"

head' :: forall action. [Html action] -> Html action
head' = head []

header :: forall action. Props action -> [Html action] -> Html action
header = createElementFromTagName "header"

header' :: forall action. [Html action] -> Html action
header' = header []

hr :: forall action. Props action -> [Html action] -> Html action
hr = createElementFromTagName "hr"

hr' :: forall action. [Html action] -> Html action
hr' = hr []

html :: forall action. Props action -> [Html action] -> Html action
html = createElementFromTagName "html"

html' :: forall action. [Html action] -> Html action
html' = html []

i :: forall action. Props action -> [Html action] -> Html action
i = createElementFromTagName "i"

i' :: forall action. [Html action] -> Html action
i' = i []

iframe :: forall action. Props action -> [Html action] -> Html action
iframe = createElementFromTagName "iframe"

iframe' :: forall action. [Html action] -> Html action
iframe' = iframe []

img :: forall action. Props action -> [Html action] -> Html action
img = createElementFromTagName "img"

img' :: forall action. [Html action] -> Html action
img' = img []

input :: forall action. Props action -> [Html action] -> Html action
input = createElementFromTagName "input"

input' :: forall action. [Html action] -> Html action
input' = input []

ins :: forall action. Props action -> [Html action] -> Html action
ins = createElementFromTagName "ins"

ins' :: forall action. [Html action] -> Html action
ins' = ins []

kbd :: forall action. Props action -> [Html action] -> Html action
kbd = createElementFromTagName "kbd"

kbd' :: forall action. [Html action] -> Html action
kbd' = kbd []

keygen :: forall action. Props action -> [Html action] -> Html action
keygen = createElementFromTagName "keygen"

keygen' :: forall action. [Html action] -> Html action
keygen' = keygen []

label :: forall action. Props action -> [Html action] -> Html action
label = createElementFromTagName "label"

label' :: forall action. [Html action] -> Html action
label' = label []

legend :: forall action. Props action -> [Html action] -> Html action
legend = createElementFromTagName "legend"

legend' :: forall action. [Html action] -> Html action
legend' = legend []

li :: forall action. Props action -> [Html action] -> Html action
li = createElementFromTagName "li"

li' :: forall action. [Html action] -> Html action
li' = li []

link :: forall action. Props action -> [Html action] -> Html action
link = createElementFromTagName "link"

link' :: forall action. [Html action] -> Html action
link' = body []

main :: forall action. Props action -> [Html action] -> Html action
main = createElementFromTagName "main"

main' :: forall action. [Html action] -> Html action
main' = main []

map :: forall action. Props action -> [Html action] -> Html action
map = createElementFromTagName "map"

map' :: forall action. [Html action] -> Html action
map' = map []

mark :: forall action. Props action -> [Html action] -> Html action
mark = createElementFromTagName "mark"

mark' :: forall action. [Html action] -> Html action
mark' = mark []

menu :: forall action. Props action -> [Html action] -> Html action
menu = createElementFromTagName "menu"

menu' :: forall action. [Html action] -> Html action
menu' = menu []

menuitem :: forall action. Props action -> [Html action] -> Html action
menuitem = createElementFromTagName "menuitem"

menuitem' :: forall action. [Html action] -> Html action
menuitem' = menuitem []

meta :: forall action. Props action -> [Html action] -> Html action
meta = createElementFromTagName "meta"

meta' :: forall action. [Html action] -> Html action
meta' = meta []

meter :: forall action. Props action -> [Html action] -> Html action
meter = createElementFromTagName "meter"

meter' :: forall action. [Html action] -> Html action
meter' = meter []

nav :: forall action. Props action -> [Html action] -> Html action
nav = createElementFromTagName "nav"

nav' :: forall action. [Html action] -> Html action
nav' = nav []

noscript :: forall action. Props action -> [Html action] -> Html action
noscript = createElementFromTagName "noscript"

noscript' :: forall action. [Html action] -> Html action
noscript' = noscript []

object :: forall action. Props action -> [Html action] -> Html action
object = createElementFromTagName "object"

object' :: forall action. [Html action] -> Html action
object' = object []

ol :: forall action. Props action -> [Html action] -> Html action
ol = createElementFromTagName "ol"

ol' :: forall action. [Html action] -> Html action
ol' = ol []

optgroup :: forall action. Props action -> [Html action] -> Html action
optgroup = createElementFromTagName "optgroup"

optgroup' :: forall action. [Html action] -> Html action
optgroup' = optgroup []

option :: forall action. Props action -> [Html action] -> Html action
option = createElementFromTagName "option"

option' :: forall action. [Html action] -> Html action
option' = option []

output :: forall action. Props action -> [Html action] -> Html action
output = createElementFromTagName "output"

output' :: forall action. [Html action] -> Html action
output' = output []

p :: forall action. Props action -> [Html action] -> Html action
p = createElementFromTagName "p"

p' :: forall action. [Html action] -> Html action
p' = p []

param :: forall action. Props action -> [Html action] -> Html action
param = createElementFromTagName "param"

param' :: forall action. [Html action] -> Html action
param' = param []

picture :: forall action. Props action -> [Html action] -> Html action
picture = createElementFromTagName "picture"

picture' :: forall action. [Html action] -> Html action
picture' = picture []

pre :: forall action. Props action -> [Html action] -> Html action
pre = createElementFromTagName "pre"

pre' :: forall action. [Html action] -> Html action
pre' = pre []

progress :: forall action. Props action -> [Html action] -> Html action
progress = createElementFromTagName "progress"

progress' :: forall action. [Html action] -> Html action
progress' = progress []

q :: forall action. Props action -> [Html action] -> Html action
q = createElementFromTagName "q"

q' :: forall action. [Html action] -> Html action
q' = q []

rp :: forall action. Props action -> [Html action] -> Html action
rp = createElementFromTagName "rp"

rp' :: forall action. [Html action] -> Html action
rp' = rp []

rt :: forall action. Props action -> [Html action] -> Html action
rt = createElementFromTagName "rt"

rt' :: forall action. [Html action] -> Html action
rt' = rt []

ruby :: forall action. Props action -> [Html action] -> Html action
ruby = createElementFromTagName "ruby"

ruby' :: forall action. [Html action] -> Html action
ruby' = ruby []

s :: forall action. Props action -> [Html action] -> Html action
s = createElementFromTagName "s"

s' :: forall action. [Html action] -> Html action
s' = s []

samp :: forall action. Props action -> [Html action] -> Html action
samp = createElementFromTagName "samp"

samp' :: forall action. [Html action] -> Html action
samp' = samp []

script :: forall action. Props action -> [Html action] -> Html action
script = createElementFromTagName "script"

script' :: forall action. [Html action] -> Html action
script' = script []

section :: forall action. Props action -> [Html action] -> Html action
section = createElementFromTagName "section"

section' :: forall action. [Html action] -> Html action
section' = section []

select :: forall action. Props action -> [Html action] -> Html action
select = createElementFromTagName "select"

select' :: forall action. [Html action] -> Html action
select' = select []

small :: forall action. Props action -> [Html action] -> Html action
small = createElementFromTagName "small"

small' :: forall action. [Html action] -> Html action
small' = small []

source :: forall action. Props action -> [Html action] -> Html action
source = createElementFromTagName "source"

source' :: forall action. [Html action] -> Html action
source' = source []

span :: forall action. Props action -> [Html action] -> Html action
span = createElementFromTagName "span"

span' :: forall action. [Html action] -> Html action
span' = span []

strong :: forall action. Props action -> [Html action] -> Html action
strong = createElementFromTagName "strong"

strong' :: forall action. [Html action] -> Html action
strong' = strong []

style :: forall action. Props action -> [Html action] -> Html action
style = createElementFromTagName "style"

style' :: forall action. [Html action] -> Html action
style' = style []

sub :: forall action. Props action -> [Html action] -> Html action
sub = createElementFromTagName "sub"

sub' :: forall action. [Html action] -> Html action
sub' = sub []

summary :: forall action. Props action -> [Html action] -> Html action
summary = createElementFromTagName "summary"

summary' :: forall action. [Html action] -> Html action
summary' = summary []

sup :: forall action. Props action -> [Html action] -> Html action
sup = createElementFromTagName "sup"

sup' :: forall action. [Html action] -> Html action
sup' = sup []

table :: forall action. Props action -> [Html action] -> Html action
table = createElementFromTagName "table"

table' :: forall action. [Html action] -> Html action
table' = table []

tbody :: forall action. Props action -> [Html action] -> Html action
tbody = createElementFromTagName "tbody"

tbody' :: forall action. [Html action] -> Html action
tbody' = tbody []

td :: forall action. Props action -> [Html action] -> Html action
td = createElementFromTagName "td"

td' :: forall action. [Html action] -> Html action
td' = td []

textarea :: forall action. Props action -> [Html action] -> Html action
textarea = createElementFromTagName "textarea"

textarea' :: forall action. [Html action] -> Html action
textarea' = textarea []

tfoot :: forall action. Props action -> [Html action] -> Html action
tfoot = createElementFromTagName "tfoot"

tfoot' :: forall action. [Html action] -> Html action
tfoot' = tfoot []

th :: forall action. Props action -> [Html action] -> Html action
th = createElementFromTagName "th"

th' :: forall action. [Html action] -> Html action
th' = th []

thead :: forall action. Props action -> [Html action] -> Html action
thead = createElementFromTagName "thead"

thead' :: forall action. [Html action] -> Html action
thead' = thead []

time :: forall action. Props action -> [Html action] -> Html action
time = createElementFromTagName "time"

time' :: forall action. [Html action] -> Html action
time' = time []

title :: forall action. Props action -> [Html action] -> Html action
title = createElementFromTagName "title"

title' :: forall action. [Html action] -> Html action
title' = title []

tr :: forall action. Props action -> [Html action] -> Html action
tr = createElementFromTagName "tr"

tr' :: forall action. [Html action] -> Html action
tr' = tr []

track :: forall action. Props action -> [Html action] -> Html action
track = createElementFromTagName "track"

track' :: forall action. [Html action] -> Html action
track' = track []

u :: forall action. Props action -> [Html action] -> Html action
u = createElementFromTagName "u"

u' :: forall action. [Html action] -> Html action
u' = u []

ul :: forall action. Props action -> [Html action] -> Html action
ul = createElementFromTagName "ul"

ul' :: forall action. [Html action] -> Html action
ul' = ul []

var :: forall action. Props action -> [Html action] -> Html action
var = createElementFromTagName "var"

var' :: forall action. [Html action] -> Html action
var' = var []

video :: forall action. Props action -> [Html action] -> Html action
video = createElementFromTagName "video"

video' :: forall action. [Html action] -> Html action
video' = video []

wbr :: forall action. Props action -> [Html action] -> Html action
wbr = createElementFromTagName "body"

wbr' :: forall action. [Html action] -> Html action
wbr' = wbr []
