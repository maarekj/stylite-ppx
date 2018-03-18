type globalValue = [ | `inherit_ | `initial | `unset];

type globalAble('t) = [ | `global(globalValue) | `normal('t)];

type tuple('a) = [ | `one('a) | `two('a, 'a) | `three('a, 'a, 'a) | `four('a, 'a, 'a, 'a)];

type couple('a) = [ | `one('a) | `two('a, 'a)];

type length = [
  | `ch(float)
  | `cm(float)
  | `em(float)
  | `ex(float)
  | `mm(float)
  | `pt(int)
  | `px(int)
  | `rem(float)
  | `vh(float)
  | `vmax(float)
  | `vmin(float)
  | `vw(float)
  | `zero
];

type percent = float;

type brWidth = [ | `length(length) | `thin | `medium | `thick];

type brStyle = [ | `none | `hidden | `dotted | `dashed | `solid | `double | `groove | `ridge | `inset | `outset];

type color = [
  | `hex(int, int, int)
  | `rgb(int, int, int)
  | `rgba(int, int, int, float)
  | `hsl(int, int, int)
  | `hsla(int, int, int, float)
  | `currentcolor
  | `transparent
];

type marginValue = [ | `length(length) | `percent(percent) | `auto];

type paddingValue = [ | `length(length) | `percent(percent)];

type borderValue = (option(brWidth), option(brStyle), option(color));

type radiusValue = [ | `length(length) | `percent(percent)];

type attachment = [ | `fixed | `scroll | `local];

type clipBox = [ | `borderBox | `paddingBox | `contentBox | `text];

type backgroundImageValue = [ | `url(string)];

type declaration = [
  | `margin(globalAble(tuple(marginValue)))
  | `marginTop(globalAble(marginValue))
  | `marginBottom(globalAble(marginValue))
  | `marginLeft(globalAble(marginValue))
  | `marginRight(globalAble(marginValue))
  | `padding(globalAble(tuple(paddingValue)))
  | `paddingTop(globalAble(paddingValue))
  | `paddingBottom(globalAble(paddingValue))
  | `paddingLeft(globalAble(paddingValue))
  | `paddingRight(globalAble(paddingValue))
  | `border(globalAble(borderValue))
  | `borderTop(globalAble(borderValue))
  | `borderBottom(globalAble(borderValue))
  | `borderLeft(globalAble(borderValue))
  | `borderRight(globalAble(borderValue))
  | `borderStyle(globalAble(tuple(brStyle)))
  | `borderTopStyle(globalAble(brStyle))
  | `borderBottomStyle(globalAble(brStyle))
  | `borderLeftStyle(globalAble(brStyle))
  | `borderRightStyle(globalAble(brStyle))
  | `borderWidth(globalAble(tuple(brWidth)))
  | `borderTopWidth(globalAble(brWidth))
  | `borderBottomWidth(globalAble(brWidth))
  | `borderLeftWidth(globalAble(brWidth))
  | `borderRightWidth(globalAble(brWidth))
  | `borderColor(globalAble(tuple(color)))
  | `borderTopColor(globalAble(color))
  | `borderBottomColor(globalAble(color))
  | `borderLeftColor(globalAble(color))
  | `borderRightColor(globalAble(color))
  | `borderRadius(globalAble(tuple(radiusValue)))
  | `borderTopLeftRadius(globalAble(couple(radiusValue)))
  | `borderTopRightRadius(globalAble(couple(radiusValue)))
  | `borderBottomLeftRadius(globalAble(couple(radiusValue)))
  | `borderBottomRightRadius(globalAble(couple(radiusValue)))
  | `backgroundColor(globalAble(couple(color)))
  | `backgroundAttachment(globalAble(couple(attachment)))
  | `backgroundClip(globalAble(clipBox))
  | `backgroundImage(globalAble(option(list(backgroundImageValue))))
];