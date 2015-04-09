-- | This module defines helper functions for creating HTML elements.

module Thermite.Html.Elements where

import Data.Monoid

import Thermite.Types
import Thermite.Internal
    
a :: forall eff. Attr -> [Html eff] -> Html eff
a = createElementImpl "a"

a' :: forall eff. [Html eff] -> Html eff
a' = a mempty

abbr :: forall eff. Attr -> [Html eff] -> Html eff
abbr = createElementImpl "abbr"

abbr' :: forall eff. [Html eff] -> Html eff
abbr' = abbr mempty

address :: forall eff. Attr -> [Html eff] -> Html eff
address = createElementImpl "address"

address' :: forall eff. [Html eff] -> Html eff
address' = address mempty

area :: forall eff. Attr -> [Html eff] -> Html eff
area = createElementImpl "area"

area' :: forall eff. [Html eff] -> Html eff
area' = area mempty

article :: forall eff. Attr -> [Html eff] -> Html eff
article = createElementImpl "article"

article' :: forall eff. [Html eff] -> Html eff
article' = article mempty

aside :: forall eff. Attr -> [Html eff] -> Html eff
aside = createElementImpl "aside"

aside' :: forall eff. [Html eff] -> Html eff
aside' = aside mempty

audio :: forall eff. Attr -> [Html eff] -> Html eff
audio = createElementImpl "audio"

audio' :: forall eff. [Html eff] -> Html eff
audio' = audio mempty

b :: forall eff. Attr -> [Html eff] -> Html eff
b = createElementImpl "b"

b' :: forall eff. [Html eff] -> Html eff
b' = b mempty

base :: forall eff. Attr -> [Html eff] -> Html eff
base = createElementImpl "base"

base' :: forall eff. [Html eff] -> Html eff
base' = base mempty

bdi :: forall eff. Attr -> [Html eff] -> Html eff
bdi = createElementImpl "bdi"

bdi' :: forall eff. [Html eff] -> Html eff
bdi' = bdi mempty

bdo :: forall eff. Attr -> [Html eff] -> Html eff
bdo = createElementImpl "bdo"

bdo' :: forall eff. [Html eff] -> Html eff
bdo' = bdo mempty

big :: forall eff. Attr -> [Html eff] -> Html eff
big = createElementImpl "big"

big' :: forall eff. [Html eff] -> Html eff
big' = big mempty

blockquote :: forall eff. Attr -> [Html eff] -> Html eff
blockquote = createElementImpl "blockquote"

blockquote' :: forall eff. [Html eff] -> Html eff
blockquote' = blockquote mempty

body :: forall eff. Attr -> [Html eff] -> Html eff
body = createElementImpl "body"

body' :: forall eff. [Html eff] -> Html eff
body' = body mempty

br :: forall eff. Attr -> [Html eff] -> Html eff
br = createElementImpl "br"

br' :: forall eff. [Html eff] -> Html eff
br' = br mempty

button :: forall eff. Attr -> [Html eff] -> Html eff
button = createElementImpl "button"

button' :: forall eff. [Html eff] -> Html eff
button' = button mempty

canvas :: forall eff. Attr -> [Html eff] -> Html eff
canvas = createElementImpl "canvas"

canvas' :: forall eff. [Html eff] -> Html eff
canvas' = canvas mempty

caption :: forall eff. Attr -> [Html eff] -> Html eff
caption = createElementImpl "caption"

caption' :: forall eff. [Html eff] -> Html eff
caption' = caption mempty

cite :: forall eff. Attr -> [Html eff] -> Html eff
cite = createElementImpl "cite"

cite' :: forall eff. [Html eff] -> Html eff
cite' = cite mempty

code :: forall eff. Attr -> [Html eff] -> Html eff
code = createElementImpl "code"

code' :: forall eff. [Html eff] -> Html eff
code' = code mempty

col :: forall eff. Attr -> [Html eff] -> Html eff
col = createElementImpl "col"

col' :: forall eff. [Html eff] -> Html eff
col' = col mempty

colgroup :: forall eff. Attr -> [Html eff] -> Html eff
colgroup = createElementImpl "colgroup"

colgroup' :: forall eff. [Html eff] -> Html eff
colgroup' = colgroup mempty

_data :: forall eff. Attr -> [Html eff] -> Html eff
_data = createElementImpl "data"

_data' :: forall eff. [Html eff] -> Html eff
_data' = _data mempty

datalist :: forall eff. Attr -> [Html eff] -> Html eff
datalist = createElementImpl "datalist"

datalist' :: forall eff. [Html eff] -> Html eff
datalist' = datalist mempty

dd :: forall eff. Attr -> [Html eff] -> Html eff
dd = createElementImpl "dd"

dd' :: forall eff. [Html eff] -> Html eff
dd' = dd mempty

del :: forall eff. Attr -> [Html eff] -> Html eff
del = createElementImpl "del"

del' :: forall eff. [Html eff] -> Html eff
del' = del mempty

details :: forall eff. Attr -> [Html eff] -> Html eff
details = createElementImpl "details"

details' :: forall eff. [Html eff] -> Html eff
details' = details mempty

dfn :: forall eff. Attr -> [Html eff] -> Html eff
dfn = createElementImpl "dfn"

dfn' :: forall eff. [Html eff] -> Html eff
dfn' = dfn mempty

dialog :: forall eff. Attr -> [Html eff] -> Html eff
dialog = createElementImpl "dialog"

dialog' :: forall eff. [Html eff] -> Html eff
dialog' = dialog mempty

div :: forall eff. Attr -> [Html eff] -> Html eff
div = createElementImpl "div"

div' :: forall eff. [Html eff] -> Html eff
div' = div mempty

dl :: forall eff. Attr -> [Html eff] -> Html eff
dl = createElementImpl "dl"

dl' :: forall eff. [Html eff] -> Html eff
dl' = dl mempty

dt :: forall eff. Attr -> [Html eff] -> Html eff
dt = createElementImpl "dt"

dt' :: forall eff. [Html eff] -> Html eff
dt' = dt mempty

em :: forall eff. Attr -> [Html eff] -> Html eff
em = createElementImpl "em"

em' :: forall eff. [Html eff] -> Html eff
em' = em mempty

embed :: forall eff. Attr -> [Html eff] -> Html eff
embed = createElementImpl "embed"

embed' :: forall eff. [Html eff] -> Html eff
embed' = embed mempty

fieldset :: forall eff. Attr -> [Html eff] -> Html eff
fieldset = createElementImpl "fieldset"

fieldset' :: forall eff. [Html eff] -> Html eff
fieldset' = fieldset mempty

figcaption :: forall eff. Attr -> [Html eff] -> Html eff
figcaption = createElementImpl "figcaption"

figcaption' :: forall eff. [Html eff] -> Html eff
figcaption' = figcaption mempty

figure :: forall eff. Attr -> [Html eff] -> Html eff
figure = createElementImpl "figure"

figure' :: forall eff. [Html eff] -> Html eff
figure' = figure mempty

footer :: forall eff. Attr -> [Html eff] -> Html eff
footer = createElementImpl "footer"

footer' :: forall eff. [Html eff] -> Html eff
footer' = footer mempty

form :: forall eff. Attr -> [Html eff] -> Html eff
form = createElementImpl "form"

form' :: forall eff. [Html eff] -> Html eff
form' = form mempty

h1 :: forall eff. Attr -> [Html eff] -> Html eff
h1 = createElementImpl "h1"

h1' :: forall eff. [Html eff] -> Html eff
h1' = h1 mempty

h2 :: forall eff. Attr -> [Html eff] -> Html eff
h2 = createElementImpl "h2"

h2' :: forall eff. [Html eff] -> Html eff
h2' = h2 mempty

h3 :: forall eff. Attr -> [Html eff] -> Html eff
h3 = createElementImpl "h3"

h3' :: forall eff. [Html eff] -> Html eff
h3' = h3 mempty

h4 :: forall eff. Attr -> [Html eff] -> Html eff
h4 = createElementImpl "h4"

h4' :: forall eff. [Html eff] -> Html eff
h4' = h4 mempty

h5 :: forall eff. Attr -> [Html eff] -> Html eff
h5 = createElementImpl "h5"

h5' :: forall eff. [Html eff] -> Html eff
h5' = h5 mempty

h6 :: forall eff. Attr -> [Html eff] -> Html eff
h6 = createElementImpl "h6"

h6' :: forall eff. [Html eff] -> Html eff
h6' = h6 mempty

head :: forall eff. Attr -> [Html eff] -> Html eff
head = createElementImpl "head"

head' :: forall eff. [Html eff] -> Html eff
head' = head mempty

header :: forall eff. Attr -> [Html eff] -> Html eff
header = createElementImpl "header"

header' :: forall eff. [Html eff] -> Html eff
header' = header mempty

hr :: forall eff. Attr -> [Html eff] -> Html eff
hr = createElementImpl "hr"

hr' :: forall eff. [Html eff] -> Html eff
hr' = hr mempty

html :: forall eff. Attr -> [Html eff] -> Html eff
html = createElementImpl "html"

html' :: forall eff. [Html eff] -> Html eff
html' = html mempty

i :: forall eff. Attr -> [Html eff] -> Html eff
i = createElementImpl "i"

i' :: forall eff. [Html eff] -> Html eff
i' = i mempty

iframe :: forall eff. Attr -> [Html eff] -> Html eff
iframe = createElementImpl "iframe"

iframe' :: forall eff. [Html eff] -> Html eff
iframe' = iframe mempty

img :: forall eff. Attr -> [Html eff] -> Html eff
img = createElementImpl "img"

img' :: forall eff. [Html eff] -> Html eff
img' = img mempty

input :: forall eff. Attr -> [Html eff] -> Html eff
input = createElementImpl "input"

input' :: forall eff. [Html eff] -> Html eff
input' = input mempty

ins :: forall eff. Attr -> [Html eff] -> Html eff
ins = createElementImpl "ins"

ins' :: forall eff. [Html eff] -> Html eff
ins' = ins mempty

kbd :: forall eff. Attr -> [Html eff] -> Html eff
kbd = createElementImpl "kbd"

kbd' :: forall eff. [Html eff] -> Html eff
kbd' = kbd mempty

keygen :: forall eff. Attr -> [Html eff] -> Html eff
keygen = createElementImpl "keygen"

keygen' :: forall eff. [Html eff] -> Html eff
keygen' = keygen mempty

label :: forall eff. Attr -> [Html eff] -> Html eff
label = createElementImpl "label"

label' :: forall eff. [Html eff] -> Html eff
label' = label mempty

legend :: forall eff. Attr -> [Html eff] -> Html eff
legend = createElementImpl "legend"

legend' :: forall eff. [Html eff] -> Html eff
legend' = legend mempty

li :: forall eff. Attr -> [Html eff] -> Html eff
li = createElementImpl "li"

li' :: forall eff. [Html eff] -> Html eff
li' = li mempty

link :: forall eff. Attr -> [Html eff] -> Html eff
link = createElementImpl "link"

link' :: forall eff. [Html eff] -> Html eff
link' = body mempty

main :: forall eff. Attr -> [Html eff] -> Html eff
main = createElementImpl "main"

main' :: forall eff. [Html eff] -> Html eff
main' = main mempty

map :: forall eff. Attr -> [Html eff] -> Html eff
map = createElementImpl "map"

map' :: forall eff. [Html eff] -> Html eff
map' = map mempty

mark :: forall eff. Attr -> [Html eff] -> Html eff
mark = createElementImpl "mark"

mark' :: forall eff. [Html eff] -> Html eff
mark' = mark mempty

menu :: forall eff. Attr -> [Html eff] -> Html eff
menu = createElementImpl "menu"

menu' :: forall eff. [Html eff] -> Html eff
menu' = menu mempty

menuitem :: forall eff. Attr -> [Html eff] -> Html eff
menuitem = createElementImpl "menuitem"

menuitem' :: forall eff. [Html eff] -> Html eff
menuitem' = menuitem mempty

meta :: forall eff. Attr -> [Html eff] -> Html eff
meta = createElementImpl "meta"

meta' :: forall eff. [Html eff] -> Html eff
meta' = meta mempty

meter :: forall eff. Attr -> [Html eff] -> Html eff
meter = createElementImpl "meter"

meter' :: forall eff. [Html eff] -> Html eff
meter' = meter mempty

nav :: forall eff. Attr -> [Html eff] -> Html eff
nav = createElementImpl "nav"

nav' :: forall eff. [Html eff] -> Html eff
nav' = nav mempty

noscript :: forall eff. Attr -> [Html eff] -> Html eff
noscript = createElementImpl "noscript"

noscript' :: forall eff. [Html eff] -> Html eff
noscript' = noscript mempty

object :: forall eff. Attr -> [Html eff] -> Html eff
object = createElementImpl "object"

object' :: forall eff. [Html eff] -> Html eff
object' = object mempty

ol :: forall eff. Attr -> [Html eff] -> Html eff
ol = createElementImpl "ol"

ol' :: forall eff. [Html eff] -> Html eff
ol' = ol mempty

optgroup :: forall eff. Attr -> [Html eff] -> Html eff
optgroup = createElementImpl "optgroup"

optgroup' :: forall eff. [Html eff] -> Html eff
optgroup' = optgroup mempty

option :: forall eff. Attr -> [Html eff] -> Html eff
option = createElementImpl "option"

option' :: forall eff. [Html eff] -> Html eff
option' = option mempty

output :: forall eff. Attr -> [Html eff] -> Html eff
output = createElementImpl "output"

output' :: forall eff. [Html eff] -> Html eff
output' = output mempty

p :: forall eff. Attr -> [Html eff] -> Html eff
p = createElementImpl "p"

p' :: forall eff. [Html eff] -> Html eff
p' = p mempty

param :: forall eff. Attr -> [Html eff] -> Html eff
param = createElementImpl "param"

param' :: forall eff. [Html eff] -> Html eff
param' = param mempty

picture :: forall eff. Attr -> [Html eff] -> Html eff
picture = createElementImpl "picture"

picture' :: forall eff. [Html eff] -> Html eff
picture' = picture mempty

pre :: forall eff. Attr -> [Html eff] -> Html eff
pre = createElementImpl "pre"

pre' :: forall eff. [Html eff] -> Html eff
pre' = pre mempty

progress :: forall eff. Attr -> [Html eff] -> Html eff
progress = createElementImpl "progress"

progress' :: forall eff. [Html eff] -> Html eff
progress' = progress mempty

q :: forall eff. Attr -> [Html eff] -> Html eff
q = createElementImpl "q"

q' :: forall eff. [Html eff] -> Html eff
q' = q mempty

rp :: forall eff. Attr -> [Html eff] -> Html eff
rp = createElementImpl "rp"

rp' :: forall eff. [Html eff] -> Html eff
rp' = rp mempty

rt :: forall eff. Attr -> [Html eff] -> Html eff
rt = createElementImpl "rt"

rt' :: forall eff. [Html eff] -> Html eff
rt' = rt mempty

ruby :: forall eff. Attr -> [Html eff] -> Html eff
ruby = createElementImpl "ruby"

ruby' :: forall eff. [Html eff] -> Html eff
ruby' = ruby mempty

s :: forall eff. Attr -> [Html eff] -> Html eff
s = createElementImpl "s"

s' :: forall eff. [Html eff] -> Html eff
s' = s mempty

samp :: forall eff. Attr -> [Html eff] -> Html eff
samp = createElementImpl "samp"

samp' :: forall eff. [Html eff] -> Html eff
samp' = samp mempty

script :: forall eff. Attr -> [Html eff] -> Html eff
script = createElementImpl "script"

script' :: forall eff. [Html eff] -> Html eff
script' = script mempty

section :: forall eff. Attr -> [Html eff] -> Html eff
section = createElementImpl "section"

section' :: forall eff. [Html eff] -> Html eff
section' = section mempty

select :: forall eff. Attr -> [Html eff] -> Html eff
select = createElementImpl "select"

select' :: forall eff. [Html eff] -> Html eff
select' = select mempty

small :: forall eff. Attr -> [Html eff] -> Html eff
small = createElementImpl "small"

small' :: forall eff. [Html eff] -> Html eff
small' = small mempty

source :: forall eff. Attr -> [Html eff] -> Html eff
source = createElementImpl "source"

source' :: forall eff. [Html eff] -> Html eff
source' = source mempty

span :: forall eff. Attr -> [Html eff] -> Html eff
span = createElementImpl "span"

span' :: forall eff. [Html eff] -> Html eff
span' = span mempty

strong :: forall eff. Attr -> [Html eff] -> Html eff
strong = createElementImpl "strong"

strong' :: forall eff. [Html eff] -> Html eff
strong' = strong mempty

style :: forall eff. Attr -> [Html eff] -> Html eff
style = createElementImpl "style"

style' :: forall eff. [Html eff] -> Html eff
style' = style mempty

sub :: forall eff. Attr -> [Html eff] -> Html eff
sub = createElementImpl "sub"

sub' :: forall eff. [Html eff] -> Html eff
sub' = sub mempty

summary :: forall eff. Attr -> [Html eff] -> Html eff
summary = createElementImpl "summary"

summary' :: forall eff. [Html eff] -> Html eff
summary' = summary mempty

sup :: forall eff. Attr -> [Html eff] -> Html eff
sup = createElementImpl "sup"

sup' :: forall eff. [Html eff] -> Html eff
sup' = sup mempty

table :: forall eff. Attr -> [Html eff] -> Html eff
table = createElementImpl "table"

table' :: forall eff. [Html eff] -> Html eff
table' = table mempty

tbody :: forall eff. Attr -> [Html eff] -> Html eff
tbody = createElementImpl "tbody"

tbody' :: forall eff. [Html eff] -> Html eff
tbody' = tbody mempty

td :: forall eff. Attr -> [Html eff] -> Html eff
td = createElementImpl "td"

td' :: forall eff. [Html eff] -> Html eff
td' = td mempty

textarea :: forall eff. Attr -> [Html eff] -> Html eff
textarea = createElementImpl "textarea"

textarea' :: forall eff. [Html eff] -> Html eff
textarea' = textarea mempty

tfoot :: forall eff. Attr -> [Html eff] -> Html eff
tfoot = createElementImpl "tfoot"

tfoot' :: forall eff. [Html eff] -> Html eff
tfoot' = tfoot mempty

th :: forall eff. Attr -> [Html eff] -> Html eff
th = createElementImpl "th"

th' :: forall eff. [Html eff] -> Html eff
th' = th mempty

thead :: forall eff. Attr -> [Html eff] -> Html eff
thead = createElementImpl "thead"

thead' :: forall eff. [Html eff] -> Html eff
thead' = thead mempty

time :: forall eff. Attr -> [Html eff] -> Html eff
time = createElementImpl "time"

time' :: forall eff. [Html eff] -> Html eff
time' = time mempty

title :: forall eff. Attr -> [Html eff] -> Html eff
title = createElementImpl "title"

title' :: forall eff. [Html eff] -> Html eff
title' = title mempty

tr :: forall eff. Attr -> [Html eff] -> Html eff
tr = createElementImpl "tr"

tr' :: forall eff. [Html eff] -> Html eff
tr' = tr mempty

track :: forall eff. Attr -> [Html eff] -> Html eff
track = createElementImpl "track"

track' :: forall eff. [Html eff] -> Html eff
track' = track mempty

u :: forall eff. Attr -> [Html eff] -> Html eff
u = createElementImpl "u"

u' :: forall eff. [Html eff] -> Html eff
u' = u mempty

ul :: forall eff. Attr -> [Html eff] -> Html eff
ul = createElementImpl "ul"

ul' :: forall eff. [Html eff] -> Html eff
ul' = ul mempty

var :: forall eff. Attr -> [Html eff] -> Html eff
var = createElementImpl "var"

var' :: forall eff. [Html eff] -> Html eff
var' = var mempty

video :: forall eff. Attr -> [Html eff] -> Html eff
video = createElementImpl "video"

video' :: forall eff. [Html eff] -> Html eff
video' = video mempty

wbr :: forall eff. Attr -> [Html eff] -> Html eff
wbr = createElementImpl "body"

wbr' :: forall eff. [Html eff] -> Html eff
wbr' = wbr mempty
