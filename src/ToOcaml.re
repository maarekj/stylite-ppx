let optDef = (def, opt) =>
  switch opt {
  | None => def
  | Some(v) => v
  };

let optMap = (map, opt) =>
  switch opt {
  | None => None
  | Some(v) => Some(map(v))
  };

let createIdent = v => Ast_helper.Exp.ident({txt: Longident.parse(v), loc: Location.none});

let createString = s => Ast_helper.Exp.constant(Const_string(s, None));

let createFloat = f => Ast_helper.Exp.constant(Const_float(string_of_float(f)));

let createInt = i => Ast_helper.Exp.constant(Const_int(i));

let createList = (f, list) => {
  let rec aux = list =>
    switch list {
    | [] => [%expr []]
    | [a, ...rest] => [%expr [[%e f(a)], ...[%e aux(rest)]]]
    };
  aux(list);
};

let printGlobalAble = (func: 'a => Parsetree.expression, value: Css.globalAble('a)) =>
  switch value {
  | `global(`inherit_) => [%expr `global(`inherit_)]
  | `global(`initial) => [%expr `global(`initial)]
  | `global(`unset) => [%expr `global(`unset)]
  | `normal(v) => [%expr `normal([%e func(v)])]
  };

let printTuple = (func, value) =>
  switch value {
  | `one(v1) => [%expr `one([%e func(v1)])]
  | `two(v1, v2) => [%expr `two(([%e func(v1)], [%e func(v2)]))]
  | `three(v1, v2, v3) => [%expr `three(([%e func(v1)], [%e func(v2)], [%e func(v3)]))]
  | `four(v1, v2, v3, v4) => [%expr `four(([%e func(v1)], [%e func(v2)], [%e func(v3)], [%e func(v4)]))]
  };

let printDyn = value =>
  switch value {
  | `auto => [%expr `auto]
  | `ch(f) => [%expr `ch([%e createFloat(f)])]
  | `cm(f) => [%expr `cm([%e createFloat(f)])]
  | `em(f) => [%expr `em([%e createFloat(f)])]
  | `mm(f) => [%expr `mm([%e createFloat(f)])]
  | `ex(f) => [%expr `ex([%e createFloat(f)])]
  | `pt(i) => [%expr `pt([%e createInt(i)])]
  | `px(i) => [%expr `px([%e createInt(i)])]
  | `rem(f) => [%expr `rem([%e createFloat(f)])]
  | `vh(f) => [%expr `vh([%e createFloat(f)])]
  | `vmax(f) => [%expr `vmax([%e createFloat(f)])]
  | `vmin(f) => [%expr `vmin([%e createFloat(f)])]
  | `vw(f) => [%expr `vw([%e createFloat(f)])]
  | `zero => [%expr `zero]
  | `rgb(r, g, b) => [%expr `rgb(([%e createInt(r)], [%e createInt(g)], [%e createInt(b)]))]
  | `rgba(r, g, b, a) => [%expr `rgba(([%e createInt(r)], [%e createInt(g)], [%e createInt(b)], [%e createFloat(a)]))]
  | `hsl(h, s, l) => [%expr `hsl(([%e createInt(h)], [%e createInt(s)], [%e createInt(l)]))]
  | `hsla(h, s, l, a) => [%expr `hsla(([%e createInt(h)], [%e createInt(s)], [%e createInt(l)], [%e createFloat(a)]))]
  | `hex(r, g, b) => [%expr `hex(([%e createInt(r)], [%e createInt(g)], [%e createInt(b)]))]
  | `currentcolor => [%expr `currentcolor]
  | `transparent => [%expr `transparent]
  | `percent(f) => [%expr `percent([%e createFloat(f)])]
  | `fixed => [%expr `fixed]
  | `scroll => [%expr `scroll]
  | `local => [%expr `local]
  | `borderBox => [%expr `borderBox]
  | `paddingBox => [%expr `paddingBox]
  | `contentBox => [%expr `contentBox]
  | `text => [%expr `text]
  | `variable(v) => createIdent(v)
  };

let printBrWidth = (v: Css.brWidth) =>
  switch v {
  | `length(v) => [%expr `length([%e printDyn(v)])]
  | `thin => [%expr `thin]
  | `medium => [%expr `medium]
  | `thick => [%expr `thick]
  | `variable(_) as v => printDyn(v)
  };

let printBrStyle = (v: Css.brStyle) =>
  switch v {
  | `none => [%expr `none]
  | `hidden => [%expr `hidden]
  | `dotted => [%expr `dotted]
  | `dashed => [%expr `dashed]
  | `solid => [%expr `solid]
  | `double => [%expr `double]
  | `groove => [%expr `groove]
  | `ridge => [%expr `ridge]
  | `inset => [%expr `inset]
  | `outset => [%expr `outset]
  | `variable(_) as v => printDyn(v)
  };

let printMarginValue = (v: Css.marginValue) =>
  switch v {
  | `length(v) => [%expr `length([%e printDyn(v)])]
  | `percent(_) as v => printDyn(v)
  | `variable(_) as v => printDyn(v)
  | `auto as v => printDyn(v)
  };

let printPaddingValue = (v: Css.paddingValue) =>
  switch v {
  | `length(v) => [%expr `length([%e printDyn(v)])]
  | `percent(_) as v => printDyn(v)
  | `variable(_) as v => printDyn(v)
  };

let printBorderValue = (v: Css.borderValue) => {
  let (w, s, c) = v;
  let somify = v => [%expr Some([%e v])];
  let w = w |> optMap(printBrWidth) |> optMap(somify) |> optDef([%expr None]);
  let s = s |> optMap(printBrStyle) |> optMap(somify) |> optDef([%expr None]);
  let c = c |> optMap(printDyn) |> optMap(somify) |> optDef([%expr None]);
  [%expr ([%e w], [%e s], [%e c])];
};

let printRadiusValue = (v: Css.radiusValue) =>
  switch v {
  | `length(v) => [%expr `length([%e printDyn(v)])]
  | `percent(_) as v => printDyn(v)
  | `variable(_) as v => printDyn(v)
  };

let printBackgroundImage = (v: option(list(Css.backgroundImageValue))) => {
  let printInner = (v: Css.backgroundImageValue) =>
    switch v {
    | `url(v) => [%expr `url([%e createString(v)])]
    | `variable(_) as v => printDyn(v)
    };
  switch v {
  | None => [%expr None]
  | Some(v) => [%expr Some([%e createList(printInner, v)])]
  };
};

let printDeclaration = (declaration: Css.declaration) =>
  switch declaration {
  | `margin(v) => [%expr `margin([%e printGlobalAble(printTuple(printMarginValue), v)])]
  | `marginTop(v) => [%expr `marginTop([%e printGlobalAble(printMarginValue, v)])]
  | `marginBottom(v) => [%expr `marginBottom([%e printGlobalAble(printMarginValue, v)])]
  | `marginLeft(v) => [%expr `marginLeft([%e printGlobalAble(printMarginValue, v)])]
  | `marginRight(v) => [%expr `marginRight([%e printGlobalAble(printMarginValue, v)])]
  | `padding(v) => [%expr `padding([%e printGlobalAble(printTuple(printPaddingValue), v)])]
  | `paddingTop(v) => [%expr `paddingTop([%e printGlobalAble(printPaddingValue, v)])]
  | `paddingBottom(v) => [%expr `paddingBottom([%e printGlobalAble(printPaddingValue, v)])]
  | `paddingLeft(v) => [%expr `paddingLeft([%e printGlobalAble(printPaddingValue, v)])]
  | `paddingRight(v) => [%expr `paddingRight([%e printGlobalAble(printPaddingValue, v)])]
  | `border(v) => [%expr `border([%e printGlobalAble(printBorderValue, v)])]
  | `borderTop(v) => [%expr `borderTop([%e printGlobalAble(printBorderValue, v)])]
  | `borderBottom(v) => [%expr `borderBottom([%e printGlobalAble(printBorderValue, v)])]
  | `borderLeft(v) => [%expr `borderLeft([%e printGlobalAble(printBorderValue, v)])]
  | `borderRight(v) => [%expr `borderRight([%e printGlobalAble(printBorderValue, v)])]
  | `borderStyle(v) => [%expr `borderStyle([%e printGlobalAble(printTuple(printBrStyle), v)])]
  | `borderTopStyle(v) => [%expr `borderTopStyle([%e printGlobalAble(printBrStyle, v)])]
  | `borderBottomStyle(v) => [%expr `borderBottomStyle([%e printGlobalAble(printBrStyle, v)])]
  | `borderLeftStyle(v) => [%expr `borderLeftStyle([%e printGlobalAble(printBrStyle, v)])]
  | `borderRightStyle(v) => [%expr `borderRightStyle([%e printGlobalAble(printBrStyle, v)])]
  | `borderWidth(v) => [%expr `borderWidth([%e printGlobalAble(printTuple(printBrWidth), v)])]
  | `borderTopWidth(v) => [%expr `borderTopWidth([%e printGlobalAble(printBrWidth, v)])]
  | `borderBottomWidth(v) => [%expr `borderBottomWidth([%e printGlobalAble(printBrWidth, v)])]
  | `borderLeftWidth(v) => [%expr `borderLeftWidth([%e printGlobalAble(printBrWidth, v)])]
  | `borderRightWidth(v) => [%expr `borderRightWidth([%e printGlobalAble(printBrWidth, v)])]
  | `borderColor(v) => [%expr `borderColor([%e printGlobalAble(printTuple(printDyn), v)])]
  | `borderTopColor(v) => [%expr `borderTopColor([%e printGlobalAble(printDyn, v)])]
  | `borderBottomColor(v) => [%expr `borderBottomColor([%e printGlobalAble(printDyn, v)])]
  | `borderLeftColor(v) => [%expr `borderLeftColor([%e printGlobalAble(printDyn, v)])]
  | `borderRightColor(v) => [%expr `borderRightColor([%e printGlobalAble(printDyn, v)])]
  | `borderRadius(v) => [%expr `borderRadius([%e printGlobalAble(printTuple(printRadiusValue), v)])]
  | `borderTopLeftRadius(v) => [%expr `borderTopLeftRadius([%e printGlobalAble(printTuple(printRadiusValue), v)])]
  | `borderTopRightRadius(v) => [%expr `borderTopRightRadius([%e printGlobalAble(printTuple(printRadiusValue), v)])]
  | `borderBottomLeftRadius(v) => [%expr
      `borderBottomLeftRadius([%e printGlobalAble(printTuple(printRadiusValue), v)])
    ]
  | `borderBottomRightRadius(v) => [%expr
      `borderBottomRightRadius([%e printGlobalAble(printTuple(printRadiusValue), v)])
    ]
  | `backgroundColor(v) => [%expr `backgroundColor([%e printGlobalAble(printTuple(printDyn), v)])]
  | `backgroundAttachment(v) => [%expr `backgroundAttachment([%e printGlobalAble(printTuple(printDyn), v)])]
  | `backgroundClip(v) => [%expr `backgroundClip([%e printGlobalAble(printDyn, v)])]
  | `backgroundImage(v) => [%expr `backgroundImage([%e printGlobalAble(printBackgroundImage, v)])]
  };

let printListDeclaration = (list: list(Css.declaration)) => createList(printDeclaration, list);