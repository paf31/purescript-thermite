module Thermite.Html.Elements where

import Data.Monoid

import Thermite.Types
import Thermite.Internal
    
a :: forall action. Attr action -> [Html action] -> Html action
a = createElementImpl "a"

a' :: forall action. [Html action] -> Html action
a' = a mempty

abbr :: forall action. Attr action -> [Html action] -> Html action
abbr = createElementImpl "abbr"

abbr' :: forall action. [Html action] -> Html action
abbr' = abbr mempty

address :: forall action. Attr action -> [Html action] -> Html action
address = createElementImpl "address"

address' :: forall action. [Html action] -> Html action
address' = address mempty

area :: forall action. Attr action -> [Html action] -> Html action
area = createElementImpl "area"

area' :: forall action. [Html action] -> Html action
area' = area mempty

article :: forall action. Attr action -> [Html action] -> Html action
article = createElementImpl "article"

article' :: forall action. [Html action] -> Html action
article' = article mempty

aside :: forall action. Attr action -> [Html action] -> Html action
aside = createElementImpl "aside"

aside' :: forall action. [Html action] -> Html action
aside' = aside mempty

audio :: forall action. Attr action -> [Html action] -> Html action
audio = createElementImpl "audio"

audio' :: forall action. [Html action] -> Html action
audio' = audio mempty

b :: forall action. Attr action -> [Html action] -> Html action
b = createElementImpl "b"

b' :: forall action. [Html action] -> Html action
b' = b mempty

base :: forall action. Attr action -> [Html action] -> Html action
base = createElementImpl "base"

base' :: forall action. [Html action] -> Html action
base' = base mempty

bdi :: forall action. Attr action -> [Html action] -> Html action
bdi = createElementImpl "bdi"

bdi' :: forall action. [Html action] -> Html action
bdi' = bdi mempty

bdo :: forall action. Attr action -> [Html action] -> Html action
bdo = createElementImpl "bdo"

bdo' :: forall action. [Html action] -> Html action
bdo' = bdo mempty

big :: forall action. Attr action -> [Html action] -> Html action
big = createElementImpl "big"

big' :: forall action. [Html action] -> Html action
big' = big mempty

blockquote :: forall action. Attr action -> [Html action] -> Html action
blockquote = createElementImpl "blockquote"

blockquote' :: forall action. [Html action] -> Html action
blockquote' = blockquote mempty

body :: forall action. Attr action -> [Html action] -> Html action
body = createElementImpl "body"

body' :: forall action. [Html action] -> Html action
body' = body mempty

br :: forall action. Attr action -> [Html action] -> Html action
br = createElementImpl "br"

br' :: forall action. [Html action] -> Html action
br' = br mempty

button :: forall action. Attr action -> [Html action] -> Html action
button = createElementImpl "button"

button' :: forall action. [Html action] -> Html action
button' = button mempty

canvas :: forall action. Attr action -> [Html action] -> Html action
canvas = createElementImpl "canvas"

canvas' :: forall action. [Html action] -> Html action
canvas' = canvas mempty

caption :: forall action. Attr action -> [Html action] -> Html action
caption = createElementImpl "caption"

caption' :: forall action. [Html action] -> Html action
caption' = caption mempty

cite :: forall action. Attr action -> [Html action] -> Html action
cite = createElementImpl "cite"

cite' :: forall action. [Html action] -> Html action
cite' = cite mempty

code :: forall action. Attr action -> [Html action] -> Html action
code = createElementImpl "code"

code' :: forall action. [Html action] -> Html action
code' = code mempty

col :: forall action. Attr action -> [Html action] -> Html action
col = createElementImpl "col"

col' :: forall action. [Html action] -> Html action
col' = col mempty

colgroup :: forall action. Attr action -> [Html action] -> Html action
colgroup = createElementImpl "colgroup"

colgroup' :: forall action. [Html action] -> Html action
colgroup' = colgroup mempty

_data :: forall action. Attr action -> [Html action] -> Html action
_data = createElementImpl "data"

_data' :: forall action. [Html action] -> Html action
_data' = _data mempty

datalist :: forall action. Attr action -> [Html action] -> Html action
datalist = createElementImpl "datalist"

datalist' :: forall action. [Html action] -> Html action
datalist' = datalist mempty

dd :: forall action. Attr action -> [Html action] -> Html action
dd = createElementImpl "dd"

dd' :: forall action. [Html action] -> Html action
dd' = dd mempty

del :: forall action. Attr action -> [Html action] -> Html action
del = createElementImpl "del"

del' :: forall action. [Html action] -> Html action
del' = del mempty

details :: forall action. Attr action -> [Html action] -> Html action
details = createElementImpl "details"

details' :: forall action. [Html action] -> Html action
details' = details mempty

dfn :: forall action. Attr action -> [Html action] -> Html action
dfn = createElementImpl "dfn"

dfn' :: forall action. [Html action] -> Html action
dfn' = dfn mempty

dialog :: forall action. Attr action -> [Html action] -> Html action
dialog = createElementImpl "dialog"

dialog' :: forall action. [Html action] -> Html action
dialog' = dialog mempty

div :: forall action. Attr action -> [Html action] -> Html action
div = createElementImpl "div"

div' :: forall action. [Html action] -> Html action
div' = div mempty

dl :: forall action. Attr action -> [Html action] -> Html action
dl = createElementImpl "dl"

dl' :: forall action. [Html action] -> Html action
dl' = dl mempty

dt :: forall action. Attr action -> [Html action] -> Html action
dt = createElementImpl "dt"

dt' :: forall action. [Html action] -> Html action
dt' = dt mempty

em :: forall action. Attr action -> [Html action] -> Html action
em = createElementImpl "em"

em' :: forall action. [Html action] -> Html action
em' = em mempty

embed :: forall action. Attr action -> [Html action] -> Html action
embed = createElementImpl "embed"

embed' :: forall action. [Html action] -> Html action
embed' = embed mempty

fieldset :: forall action. Attr action -> [Html action] -> Html action
fieldset = createElementImpl "fieldset"

fieldset' :: forall action. [Html action] -> Html action
fieldset' = fieldset mempty

figcaption :: forall action. Attr action -> [Html action] -> Html action
figcaption = createElementImpl "figcaption"

figcaption' :: forall action. [Html action] -> Html action
figcaption' = figcaption mempty

figure :: forall action. Attr action -> [Html action] -> Html action
figure = createElementImpl "figure"

figure' :: forall action. [Html action] -> Html action
figure' = figure mempty

footer :: forall action. Attr action -> [Html action] -> Html action
footer = createElementImpl "footer"

footer' :: forall action. [Html action] -> Html action
footer' = footer mempty

form :: forall action. Attr action -> [Html action] -> Html action
form = createElementImpl "form"

form' :: forall action. [Html action] -> Html action
form' = form mempty

h1 :: forall action. Attr action -> [Html action] -> Html action
h1 = createElementImpl "h1"

h1' :: forall action. [Html action] -> Html action
h1' = h1 mempty

h2 :: forall action. Attr action -> [Html action] -> Html action
h2 = createElementImpl "h2"

h2' :: forall action. [Html action] -> Html action
h2' = h2 mempty

h3 :: forall action. Attr action -> [Html action] -> Html action
h3 = createElementImpl "h3"

h3' :: forall action. [Html action] -> Html action
h3' = h3 mempty

h4 :: forall action. Attr action -> [Html action] -> Html action
h4 = createElementImpl "h4"

h4' :: forall action. [Html action] -> Html action
h4' = h4 mempty

h5 :: forall action. Attr action -> [Html action] -> Html action
h5 = createElementImpl "h5"

h5' :: forall action. [Html action] -> Html action
h5' = h5 mempty

h6 :: forall action. Attr action -> [Html action] -> Html action
h6 = createElementImpl "h6"

h6' :: forall action. [Html action] -> Html action
h6' = h6 mempty

head :: forall action. Attr action -> [Html action] -> Html action
head = createElementImpl "head"

head' :: forall action. [Html action] -> Html action
head' = head mempty

header :: forall action. Attr action -> [Html action] -> Html action
header = createElementImpl "header"

header' :: forall action. [Html action] -> Html action
header' = header mempty

hr :: forall action. Attr action -> [Html action] -> Html action
hr = createElementImpl "hr"

hr' :: forall action. [Html action] -> Html action
hr' = hr mempty

html :: forall action. Attr action -> [Html action] -> Html action
html = createElementImpl "html"

html' :: forall action. [Html action] -> Html action
html' = html mempty

i :: forall action. Attr action -> [Html action] -> Html action
i = createElementImpl "i"

i' :: forall action. [Html action] -> Html action
i' = i mempty

iframe :: forall action. Attr action -> [Html action] -> Html action
iframe = createElementImpl "iframe"

iframe' :: forall action. [Html action] -> Html action
iframe' = iframe mempty

img :: forall action. Attr action -> [Html action] -> Html action
img = createElementImpl "img"

img' :: forall action. [Html action] -> Html action
img' = img mempty

input :: forall action. Attr action -> [Html action] -> Html action
input = createElementImpl "input"

input' :: forall action. [Html action] -> Html action
input' = input mempty

ins :: forall action. Attr action -> [Html action] -> Html action
ins = createElementImpl "ins"

ins' :: forall action. [Html action] -> Html action
ins' = ins mempty

kbd :: forall action. Attr action -> [Html action] -> Html action
kbd = createElementImpl "kbd"

kbd' :: forall action. [Html action] -> Html action
kbd' = kbd mempty

keygen :: forall action. Attr action -> [Html action] -> Html action
keygen = createElementImpl "keygen"

keygen' :: forall action. [Html action] -> Html action
keygen' = keygen mempty

label :: forall action. Attr action -> [Html action] -> Html action
label = createElementImpl "label"

label' :: forall action. [Html action] -> Html action
label' = label mempty

legend :: forall action. Attr action -> [Html action] -> Html action
legend = createElementImpl "legend"

legend' :: forall action. [Html action] -> Html action
legend' = legend mempty

li :: forall action. Attr action -> [Html action] -> Html action
li = createElementImpl "li"

li' :: forall action. [Html action] -> Html action
li' = li mempty

link :: forall action. Attr action -> [Html action] -> Html action
link = createElementImpl "link"

link' :: forall action. [Html action] -> Html action
link' = body mempty

main :: forall action. Attr action -> [Html action] -> Html action
main = createElementImpl "main"

main' :: forall action. [Html action] -> Html action
main' = main mempty

map :: forall action. Attr action -> [Html action] -> Html action
map = createElementImpl "map"

map' :: forall action. [Html action] -> Html action
map' = map mempty

mark :: forall action. Attr action -> [Html action] -> Html action
mark = createElementImpl "mark"

mark' :: forall action. [Html action] -> Html action
mark' = mark mempty

menu :: forall action. Attr action -> [Html action] -> Html action
menu = createElementImpl "menu"

menu' :: forall action. [Html action] -> Html action
menu' = menu mempty

menuitem :: forall action. Attr action -> [Html action] -> Html action
menuitem = createElementImpl "menuitem"

menuitem' :: forall action. [Html action] -> Html action
menuitem' = menuitem mempty

meta :: forall action. Attr action -> [Html action] -> Html action
meta = createElementImpl "meta"

meta' :: forall action. [Html action] -> Html action
meta' = meta mempty

meter :: forall action. Attr action -> [Html action] -> Html action
meter = createElementImpl "meter"

meter' :: forall action. [Html action] -> Html action
meter' = meter mempty

nav :: forall action. Attr action -> [Html action] -> Html action
nav = createElementImpl "nav"

nav' :: forall action. [Html action] -> Html action
nav' = nav mempty

noscript :: forall action. Attr action -> [Html action] -> Html action
noscript = createElementImpl "noscript"

noscript' :: forall action. [Html action] -> Html action
noscript' = noscript mempty

object :: forall action. Attr action -> [Html action] -> Html action
object = createElementImpl "object"

object' :: forall action. [Html action] -> Html action
object' = object mempty

ol :: forall action. Attr action -> [Html action] -> Html action
ol = createElementImpl "ol"

ol' :: forall action. [Html action] -> Html action
ol' = ol mempty

optgroup :: forall action. Attr action -> [Html action] -> Html action
optgroup = createElementImpl "optgroup"

optgroup' :: forall action. [Html action] -> Html action
optgroup' = optgroup mempty

option :: forall action. Attr action -> [Html action] -> Html action
option = createElementImpl "option"

option' :: forall action. [Html action] -> Html action
option' = option mempty

output :: forall action. Attr action -> [Html action] -> Html action
output = createElementImpl "output"

output' :: forall action. [Html action] -> Html action
output' = output mempty

p :: forall action. Attr action -> [Html action] -> Html action
p = createElementImpl "p"

p' :: forall action. [Html action] -> Html action
p' = p mempty

param :: forall action. Attr action -> [Html action] -> Html action
param = createElementImpl "param"

param' :: forall action. [Html action] -> Html action
param' = param mempty

picture :: forall action. Attr action -> [Html action] -> Html action
picture = createElementImpl "picture"

picture' :: forall action. [Html action] -> Html action
picture' = picture mempty

pre :: forall action. Attr action -> [Html action] -> Html action
pre = createElementImpl "pre"

pre' :: forall action. [Html action] -> Html action
pre' = pre mempty

progress :: forall action. Attr action -> [Html action] -> Html action
progress = createElementImpl "progress"

progress' :: forall action. [Html action] -> Html action
progress' = progress mempty

q :: forall action. Attr action -> [Html action] -> Html action
q = createElementImpl "q"

q' :: forall action. [Html action] -> Html action
q' = q mempty

rp :: forall action. Attr action -> [Html action] -> Html action
rp = createElementImpl "rp"

rp' :: forall action. [Html action] -> Html action
rp' = rp mempty

rt :: forall action. Attr action -> [Html action] -> Html action
rt = createElementImpl "rt"

rt' :: forall action. [Html action] -> Html action
rt' = rt mempty

ruby :: forall action. Attr action -> [Html action] -> Html action
ruby = createElementImpl "ruby"

ruby' :: forall action. [Html action] -> Html action
ruby' = ruby mempty

s :: forall action. Attr action -> [Html action] -> Html action
s = createElementImpl "s"

s' :: forall action. [Html action] -> Html action
s' = s mempty

samp :: forall action. Attr action -> [Html action] -> Html action
samp = createElementImpl "samp"

samp' :: forall action. [Html action] -> Html action
samp' = samp mempty

script :: forall action. Attr action -> [Html action] -> Html action
script = createElementImpl "script"

script' :: forall action. [Html action] -> Html action
script' = script mempty

section :: forall action. Attr action -> [Html action] -> Html action
section = createElementImpl "section"

section' :: forall action. [Html action] -> Html action
section' = section mempty

select :: forall action. Attr action -> [Html action] -> Html action
select = createElementImpl "select"

select' :: forall action. [Html action] -> Html action
select' = select mempty

small :: forall action. Attr action -> [Html action] -> Html action
small = createElementImpl "small"

small' :: forall action. [Html action] -> Html action
small' = small mempty

source :: forall action. Attr action -> [Html action] -> Html action
source = createElementImpl "source"

source' :: forall action. [Html action] -> Html action
source' = source mempty

span :: forall action. Attr action -> [Html action] -> Html action
span = createElementImpl "span"

span' :: forall action. [Html action] -> Html action
span' = span mempty

strong :: forall action. Attr action -> [Html action] -> Html action
strong = createElementImpl "strong"

strong' :: forall action. [Html action] -> Html action
strong' = strong mempty

style :: forall action. Attr action -> [Html action] -> Html action
style = createElementImpl "style"

style' :: forall action. [Html action] -> Html action
style' = style mempty

sub :: forall action. Attr action -> [Html action] -> Html action
sub = createElementImpl "sub"

sub' :: forall action. [Html action] -> Html action
sub' = sub mempty

summary :: forall action. Attr action -> [Html action] -> Html action
summary = createElementImpl "summary"

summary' :: forall action. [Html action] -> Html action
summary' = summary mempty

sup :: forall action. Attr action -> [Html action] -> Html action
sup = createElementImpl "sup"

sup' :: forall action. [Html action] -> Html action
sup' = sup mempty

table :: forall action. Attr action -> [Html action] -> Html action
table = createElementImpl "table"

table' :: forall action. [Html action] -> Html action
table' = table mempty

tbody :: forall action. Attr action -> [Html action] -> Html action
tbody = createElementImpl "tbody"

tbody' :: forall action. [Html action] -> Html action
tbody' = tbody mempty

td :: forall action. Attr action -> [Html action] -> Html action
td = createElementImpl "td"

td' :: forall action. [Html action] -> Html action
td' = td mempty

textarea :: forall action. Attr action -> [Html action] -> Html action
textarea = createElementImpl "textarea"

textarea' :: forall action. [Html action] -> Html action
textarea' = textarea mempty

tfoot :: forall action. Attr action -> [Html action] -> Html action
tfoot = createElementImpl "tfoot"

tfoot' :: forall action. [Html action] -> Html action
tfoot' = tfoot mempty

th :: forall action. Attr action -> [Html action] -> Html action
th = createElementImpl "th"

th' :: forall action. [Html action] -> Html action
th' = th mempty

thead :: forall action. Attr action -> [Html action] -> Html action
thead = createElementImpl "thead"

thead' :: forall action. [Html action] -> Html action
thead' = thead mempty

time :: forall action. Attr action -> [Html action] -> Html action
time = createElementImpl "time"

time' :: forall action. [Html action] -> Html action
time' = time mempty

title :: forall action. Attr action -> [Html action] -> Html action
title = createElementImpl "title"

title' :: forall action. [Html action] -> Html action
title' = title mempty

tr :: forall action. Attr action -> [Html action] -> Html action
tr = createElementImpl "tr"

tr' :: forall action. [Html action] -> Html action
tr' = tr mempty

track :: forall action. Attr action -> [Html action] -> Html action
track = createElementImpl "track"

track' :: forall action. [Html action] -> Html action
track' = track mempty

u :: forall action. Attr action -> [Html action] -> Html action
u = createElementImpl "u"

u' :: forall action. [Html action] -> Html action
u' = u mempty

ul :: forall action. Attr action -> [Html action] -> Html action
ul = createElementImpl "ul"

ul' :: forall action. [Html action] -> Html action
ul' = ul mempty

var :: forall action. Attr action -> [Html action] -> Html action
var = createElementImpl "var"

var' :: forall action. [Html action] -> Html action
var' = var mempty

video :: forall action. Attr action -> [Html action] -> Html action
video = createElementImpl "video"

video' :: forall action. [Html action] -> Html action
video' = video mempty

wbr :: forall action. Attr action -> [Html action] -> Html action
wbr = createElementImpl "body"

wbr' :: forall action. [Html action] -> Html action
wbr' = wbr mempty
