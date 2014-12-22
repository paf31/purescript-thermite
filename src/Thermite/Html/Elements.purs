module Thermite.Html.Elements where
    
import Thermite.Html    
    
a :: forall action. Props action -> [Html action] -> Html action
a = createElement "a"

a' :: forall action. [Html action] -> Html action
a' = a []

abbr :: forall action. Props action -> [Html action] -> Html action
abbr = createElement "abbr"

abbr' :: forall action. [Html action] -> Html action
abbr' = abbr []

address :: forall action. Props action -> [Html action] -> Html action
address = createElement "address"

address' :: forall action. [Html action] -> Html action
address' = address []

area :: forall action. Props action -> [Html action] -> Html action
area = createElement "area"

area' :: forall action. [Html action] -> Html action
area' = area []

article :: forall action. Props action -> [Html action] -> Html action
article = createElement "article"

article' :: forall action. [Html action] -> Html action
article' = article []

aside :: forall action. Props action -> [Html action] -> Html action
aside = createElement "aside"

aside' :: forall action. [Html action] -> Html action
aside' = aside []

audio :: forall action. Props action -> [Html action] -> Html action
audio = createElement "audio"

audio' :: forall action. [Html action] -> Html action
audio' = audio []

b :: forall action. Props action -> [Html action] -> Html action
b = createElement "b"

b' :: forall action. [Html action] -> Html action
b' = b []

base :: forall action. Props action -> [Html action] -> Html action
base = createElement "base"

base' :: forall action. [Html action] -> Html action
base' = base []

bdi :: forall action. Props action -> [Html action] -> Html action
bdi = createElement "bdi"

bdi' :: forall action. [Html action] -> Html action
bdi' = bdi []

bdo :: forall action. Props action -> [Html action] -> Html action
bdo = createElement "bdo"

bdo' :: forall action. [Html action] -> Html action
bdo' = bdo []

big :: forall action. Props action -> [Html action] -> Html action
big = createElement "big"

big' :: forall action. [Html action] -> Html action
big' = big []

blockquote :: forall action. Props action -> [Html action] -> Html action
blockquote = createElement "blockquote"

blockquote' :: forall action. [Html action] -> Html action
blockquote' = blockquote []

body :: forall action. Props action -> [Html action] -> Html action
body = createElement "body"

body' :: forall action. [Html action] -> Html action
body' = body []

br :: forall action. Props action -> [Html action] -> Html action
br = createElement "br"

br' :: forall action. [Html action] -> Html action
br' = br []

button :: forall action. Props action -> [Html action] -> Html action
button = createElement "button"

button' :: forall action. [Html action] -> Html action
button' = button []

canvas :: forall action. Props action -> [Html action] -> Html action
canvas = createElement "canvas"

canvas' :: forall action. [Html action] -> Html action
canvas' = canvas []

caption :: forall action. Props action -> [Html action] -> Html action
caption = createElement "caption"

caption' :: forall action. [Html action] -> Html action
caption' = caption []

cite :: forall action. Props action -> [Html action] -> Html action
cite = createElement "cite"

cite' :: forall action. [Html action] -> Html action
cite' = cite []

code :: forall action. Props action -> [Html action] -> Html action
code = createElement "code"

code' :: forall action. [Html action] -> Html action
code' = code []

col :: forall action. Props action -> [Html action] -> Html action
col = createElement "col"

col' :: forall action. [Html action] -> Html action
col' = col []

colgroup :: forall action. Props action -> [Html action] -> Html action
colgroup = createElement "colgroup"

colgroup' :: forall action. [Html action] -> Html action
colgroup' = colgroup []

_data :: forall action. Props action -> [Html action] -> Html action
_data = createElement "data"

_data' :: forall action. [Html action] -> Html action
_data' = _data []

datalist :: forall action. Props action -> [Html action] -> Html action
datalist = createElement "datalist"

datalist' :: forall action. [Html action] -> Html action
datalist' = datalist []

dd :: forall action. Props action -> [Html action] -> Html action
dd = createElement "dd"

dd' :: forall action. [Html action] -> Html action
dd' = dd []

del :: forall action. Props action -> [Html action] -> Html action
del = createElement "del"

del' :: forall action. [Html action] -> Html action
del' = del []

details :: forall action. Props action -> [Html action] -> Html action
details = createElement "details"

details' :: forall action. [Html action] -> Html action
details' = details []

dfn :: forall action. Props action -> [Html action] -> Html action
dfn = createElement "dfn"

dfn' :: forall action. [Html action] -> Html action
dfn' = dfn []

dialog :: forall action. Props action -> [Html action] -> Html action
dialog = createElement "dialog"

dialog' :: forall action. [Html action] -> Html action
dialog' = dialog []

div :: forall action. Props action -> [Html action] -> Html action
div = createElement "div"

div' :: forall action. [Html action] -> Html action
div' = div []

dl :: forall action. Props action -> [Html action] -> Html action
dl = createElement "dl"

dl' :: forall action. [Html action] -> Html action
dl' = dl []

dt :: forall action. Props action -> [Html action] -> Html action
dt = createElement "dt"

dt' :: forall action. [Html action] -> Html action
dt' = dt []

em :: forall action. Props action -> [Html action] -> Html action
em = createElement "em"

em' :: forall action. [Html action] -> Html action
em' = em []

embed :: forall action. Props action -> [Html action] -> Html action
embed = createElement "embed"

embed' :: forall action. [Html action] -> Html action
embed' = embed []

fieldset :: forall action. Props action -> [Html action] -> Html action
fieldset = createElement "fieldset"

fieldset' :: forall action. [Html action] -> Html action
fieldset' = fieldset []

figcaption :: forall action. Props action -> [Html action] -> Html action
figcaption = createElement "figcaption"

figcaption' :: forall action. [Html action] -> Html action
figcaption' = figcaption []

figure :: forall action. Props action -> [Html action] -> Html action
figure = createElement "figure"

figure' :: forall action. [Html action] -> Html action
figure' = figure []

footer :: forall action. Props action -> [Html action] -> Html action
footer = createElement "footer"

footer' :: forall action. [Html action] -> Html action
footer' = footer []

form :: forall action. Props action -> [Html action] -> Html action
form = createElement "form"

form' :: forall action. [Html action] -> Html action
form' = form []

h1 :: forall action. Props action -> [Html action] -> Html action
h1 = createElement "h1"

h1' :: forall action. [Html action] -> Html action
h1' = h1 []

h2 :: forall action. Props action -> [Html action] -> Html action
h2 = createElement "h2"

h2' :: forall action. [Html action] -> Html action
h2' = h2 []

h3 :: forall action. Props action -> [Html action] -> Html action
h3 = createElement "h3"

h3' :: forall action. [Html action] -> Html action
h3' = h3 []

h4 :: forall action. Props action -> [Html action] -> Html action
h4 = createElement "h4"

h4' :: forall action. [Html action] -> Html action
h4' = h4 []

h5 :: forall action. Props action -> [Html action] -> Html action
h5 = createElement "h5"

h5' :: forall action. [Html action] -> Html action
h5' = h5 []

h6 :: forall action. Props action -> [Html action] -> Html action
h6 = createElement "h6"

h6' :: forall action. [Html action] -> Html action
h6' = h6 []

head :: forall action. Props action -> [Html action] -> Html action
head = createElement "head"

head' :: forall action. [Html action] -> Html action
head' = head []

header :: forall action. Props action -> [Html action] -> Html action
header = createElement "header"

header' :: forall action. [Html action] -> Html action
header' = header []

hr :: forall action. Props action -> [Html action] -> Html action
hr = createElement "hr"

hr' :: forall action. [Html action] -> Html action
hr' = hr []

html :: forall action. Props action -> [Html action] -> Html action
html = createElement "html"

html' :: forall action. [Html action] -> Html action
html' = html []

i :: forall action. Props action -> [Html action] -> Html action
i = createElement "i"

i' :: forall action. [Html action] -> Html action
i' = i []

iframe :: forall action. Props action -> [Html action] -> Html action
iframe = createElement "iframe"

iframe' :: forall action. [Html action] -> Html action
iframe' = iframe []

img :: forall action. Props action -> [Html action] -> Html action
img = createElement "img"

img' :: forall action. [Html action] -> Html action
img' = img []

input :: forall action. Props action -> [Html action] -> Html action
input = createElement "input"

input' :: forall action. [Html action] -> Html action
input' = input []

ins :: forall action. Props action -> [Html action] -> Html action
ins = createElement "ins"

ins' :: forall action. [Html action] -> Html action
ins' = ins []

kbd :: forall action. Props action -> [Html action] -> Html action
kbd = createElement "kbd"

kbd' :: forall action. [Html action] -> Html action
kbd' = kbd []

keygen :: forall action. Props action -> [Html action] -> Html action
keygen = createElement "keygen"

keygen' :: forall action. [Html action] -> Html action
keygen' = keygen []

label :: forall action. Props action -> [Html action] -> Html action
label = createElement "label"

label' :: forall action. [Html action] -> Html action
label' = label []

legend :: forall action. Props action -> [Html action] -> Html action
legend = createElement "legend"

legend' :: forall action. [Html action] -> Html action
legend' = legend []

li :: forall action. Props action -> [Html action] -> Html action
li = createElement "li"

li' :: forall action. [Html action] -> Html action
li' = li []

link :: forall action. Props action -> [Html action] -> Html action
link = createElement "link"

link' :: forall action. [Html action] -> Html action
link' = body []

main :: forall action. Props action -> [Html action] -> Html action
main = createElement "main"

main' :: forall action. [Html action] -> Html action
main' = main []

map :: forall action. Props action -> [Html action] -> Html action
map = createElement "map"

map' :: forall action. [Html action] -> Html action
map' = map []

mark :: forall action. Props action -> [Html action] -> Html action
mark = createElement "mark"

mark' :: forall action. [Html action] -> Html action
mark' = mark []

menu :: forall action. Props action -> [Html action] -> Html action
menu = createElement "menu"

menu' :: forall action. [Html action] -> Html action
menu' = menu []

menuitem :: forall action. Props action -> [Html action] -> Html action
menuitem = createElement "menuitem"

menuitem' :: forall action. [Html action] -> Html action
menuitem' = menuitem []

meta :: forall action. Props action -> [Html action] -> Html action
meta = createElement "meta"

meta' :: forall action. [Html action] -> Html action
meta' = meta []

meter :: forall action. Props action -> [Html action] -> Html action
meter = createElement "meter"

meter' :: forall action. [Html action] -> Html action
meter' = meter []

nav :: forall action. Props action -> [Html action] -> Html action
nav = createElement "nav"

nav' :: forall action. [Html action] -> Html action
nav' = nav []

noscript :: forall action. Props action -> [Html action] -> Html action
noscript = createElement "noscript"

noscript' :: forall action. [Html action] -> Html action
noscript' = noscript []

object :: forall action. Props action -> [Html action] -> Html action
object = createElement "object"

object' :: forall action. [Html action] -> Html action
object' = object []

ol :: forall action. Props action -> [Html action] -> Html action
ol = createElement "ol"

ol' :: forall action. [Html action] -> Html action
ol' = ol []

optgroup :: forall action. Props action -> [Html action] -> Html action
optgroup = createElement "optgroup"

optgroup' :: forall action. [Html action] -> Html action
optgroup' = optgroup []

option :: forall action. Props action -> [Html action] -> Html action
option = createElement "option"

option' :: forall action. [Html action] -> Html action
option' = option []

output :: forall action. Props action -> [Html action] -> Html action
output = createElement "output"

output' :: forall action. [Html action] -> Html action
output' = output []

p :: forall action. Props action -> [Html action] -> Html action
p = createElement "p"

p' :: forall action. [Html action] -> Html action
p' = p []

param :: forall action. Props action -> [Html action] -> Html action
param = createElement "param"

param' :: forall action. [Html action] -> Html action
param' = param []

picture :: forall action. Props action -> [Html action] -> Html action
picture = createElement "picture"

picture' :: forall action. [Html action] -> Html action
picture' = picture []

pre :: forall action. Props action -> [Html action] -> Html action
pre = createElement "pre"

pre' :: forall action. [Html action] -> Html action
pre' = pre []

progress :: forall action. Props action -> [Html action] -> Html action
progress = createElement "progress"

progress' :: forall action. [Html action] -> Html action
progress' = progress []

q :: forall action. Props action -> [Html action] -> Html action
q = createElement "q"

q' :: forall action. [Html action] -> Html action
q' = q []

rp :: forall action. Props action -> [Html action] -> Html action
rp = createElement "rp"

rp' :: forall action. [Html action] -> Html action
rp' = rp []

rt :: forall action. Props action -> [Html action] -> Html action
rt = createElement "rt"

rt' :: forall action. [Html action] -> Html action
rt' = rt []

ruby :: forall action. Props action -> [Html action] -> Html action
ruby = createElement "ruby"

ruby' :: forall action. [Html action] -> Html action
ruby' = ruby []

s :: forall action. Props action -> [Html action] -> Html action
s = createElement "s"

s' :: forall action. [Html action] -> Html action
s' = s []

samp :: forall action. Props action -> [Html action] -> Html action
samp = createElement "samp"

samp' :: forall action. [Html action] -> Html action
samp' = samp []

script :: forall action. Props action -> [Html action] -> Html action
script = createElement "script"

script' :: forall action. [Html action] -> Html action
script' = script []

section :: forall action. Props action -> [Html action] -> Html action
section = createElement "section"

section' :: forall action. [Html action] -> Html action
section' = section []

select :: forall action. Props action -> [Html action] -> Html action
select = createElement "select"

select' :: forall action. [Html action] -> Html action
select' = select []

small :: forall action. Props action -> [Html action] -> Html action
small = createElement "small"

small' :: forall action. [Html action] -> Html action
small' = small []

source :: forall action. Props action -> [Html action] -> Html action
source = createElement "source"

source' :: forall action. [Html action] -> Html action
source' = source []

span :: forall action. Props action -> [Html action] -> Html action
span = createElement "span"

span' :: forall action. [Html action] -> Html action
span' = span []

strong :: forall action. Props action -> [Html action] -> Html action
strong = createElement "strong"

strong' :: forall action. [Html action] -> Html action
strong' = strong []

style :: forall action. Props action -> [Html action] -> Html action
style = createElement "style"

style' :: forall action. [Html action] -> Html action
style' = style []

sub :: forall action. Props action -> [Html action] -> Html action
sub = createElement "sub"

sub' :: forall action. [Html action] -> Html action
sub' = sub []

summary :: forall action. Props action -> [Html action] -> Html action
summary = createElement "summary"

summary' :: forall action. [Html action] -> Html action
summary' = summary []

sup :: forall action. Props action -> [Html action] -> Html action
sup = createElement "sup"

sup' :: forall action. [Html action] -> Html action
sup' = sup []

table :: forall action. Props action -> [Html action] -> Html action
table = createElement "table"

table' :: forall action. [Html action] -> Html action
table' = table []

tbody :: forall action. Props action -> [Html action] -> Html action
tbody = createElement "tbody"

tbody' :: forall action. [Html action] -> Html action
tbody' = tbody []

td :: forall action. Props action -> [Html action] -> Html action
td = createElement "td"

td' :: forall action. [Html action] -> Html action
td' = td []

textarea :: forall action. Props action -> [Html action] -> Html action
textarea = createElement "textarea"

textarea' :: forall action. [Html action] -> Html action
textarea' = textarea []

tfoot :: forall action. Props action -> [Html action] -> Html action
tfoot = createElement "tfoot"

tfoot' :: forall action. [Html action] -> Html action
tfoot' = tfoot []

th :: forall action. Props action -> [Html action] -> Html action
th = createElement "th"

th' :: forall action. [Html action] -> Html action
th' = th []

thead :: forall action. Props action -> [Html action] -> Html action
thead = createElement "thead"

thead' :: forall action. [Html action] -> Html action
thead' = thead []

time :: forall action. Props action -> [Html action] -> Html action
time = createElement "time"

time' :: forall action. [Html action] -> Html action
time' = time []

title :: forall action. Props action -> [Html action] -> Html action
title = createElement "title"

title' :: forall action. [Html action] -> Html action
title' = title []

tr :: forall action. Props action -> [Html action] -> Html action
tr = createElement "tr"

tr' :: forall action. [Html action] -> Html action
tr' = tr []

track :: forall action. Props action -> [Html action] -> Html action
track = createElement "track"

track' :: forall action. [Html action] -> Html action
track' = track []

u :: forall action. Props action -> [Html action] -> Html action
u = createElement "u"

u' :: forall action. [Html action] -> Html action
u' = u []

ul :: forall action. Props action -> [Html action] -> Html action
ul = createElement "ul"

ul' :: forall action. [Html action] -> Html action
ul' = ul []

var :: forall action. Props action -> [Html action] -> Html action
var = createElement "var"

var' :: forall action. [Html action] -> Html action
var' = var []

video :: forall action. Props action -> [Html action] -> Html action
video = createElement "video"

video' :: forall action. [Html action] -> Html action
video' = video []

wbr :: forall action. Props action -> [Html action] -> Html action
wbr = createElement "body"

wbr' :: forall action. [Html action] -> Html action
wbr' = wbr []