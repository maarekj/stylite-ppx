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

let stringOfFloat = f =>
  if (float_of_int(truncate(f)) == f) {
    string_of_int(truncate(f));
  } else {
    string_of_float(f);
  };

let stringOfGlobalValue = v =>
  switch v {
  | `inherit_ => "inherit"
  | `initial => "initial"
  | `unset => "unset"
  };

let printGlobalAble = (func: 'a => string, value: StylitePpx.globalAble('a)) =>
  switch value {
  | `global(v) => stringOfGlobalValue(v)
  | `normal(v) => func(v)
  };

let printList = (sep, f, list) => {
  let rec aux = list =>
    switch list {
    | [] => ""
    | [decl] => f(decl)
    | [decl, ...rest] => f(decl) ++ sep ++ aux(rest)
    };
  String.trim(aux(list));
};

let printDyn = value =>
  switch value {
  | `ch(f) => stringOfFloat(f) ++ "ch"
  | `cm(f) => stringOfFloat(f) ++ "cm"
  | `em(f) => stringOfFloat(f) ++ "em"
  | `mm(f) => stringOfFloat(f) ++ "mm"
  | `ex(f) => stringOfFloat(f) ++ "ex"
  | `pt(i) => string_of_int(i) ++ "pt"
  | `px(i) => string_of_int(i) ++ "px"
  | `rem(f) => stringOfFloat(f) ++ "rem"
  | `vh(f) => stringOfFloat(f) ++ "vh"
  | `vmax(f) => stringOfFloat(f) ++ "vmax"
  | `vmin(f) => stringOfFloat(f) ++ "vmin"
  | `vw(f) => stringOfFloat(f) ++ "vw"
  | `zero => "0"
  | `rgb(r, g, b) => Printf.sprintf("rgb(%i, %i, %i)", r, g, b)
  | `rgba(r, g, b, a) => Printf.sprintf("rgba(%i, %i, %i, %s)", r, g, b, stringOfFloat(a))
  | `hsl(h, s, l) => Printf.sprintf("hsl(%i, %i, %i)", h, s, l)
  | `hsla(h, s, l, a) => Printf.sprintf("hsla(%i, %i, %i, %s)", h, s, l, stringOfFloat(a))
  | `hex(r, g, b) => Printf.sprintf("#%02X%02X%02X", r, g, b)
  | `currentcolor => "currentcolor"
  | `transparent => "transparent"
  | `percent(v) => stringOfFloat(v) ++ "%"
  | `fixed => "fixed"
  | `scroll => "scroll"
  | `local => "local"
  | `borderBox => "border-box"
  | `paddingBox => "padding-box"
  | `contentBox => "content-box"
  | `text => "text"
  };

let printTuple = (func, value) =>
  switch value {
  | `one(v1) => func(v1)
  | `two(v1, v2) => func(v1) ++ " " ++ func(v2)
  | `three(v1, v2, v3) => func(v1) ++ " " ++ func(v2) ++ " " ++ func(v3)
  | `four(v1, v2, v3, v4) => func(v1) ++ " " ++ func(v2) ++ " " ++ func(v3) ++ " " ++ func(v4)
  };

let printMarginValue = (v: StylitePpx.marginValue) =>
  switch v {
  | `length(v) => printDyn(v)
  | `percent(v) => printDyn(`percent(v))
  | `auto => "auto"
  };

let printPaddingValue = (v: StylitePpx.paddingValue) =>
  switch v {
  | `length(v) => printDyn(v)
  | `percent(v) => printDyn(`percent(v))
  };

let printBrWidth = (v: StylitePpx.brWidth) =>
  switch v {
  | `length(v) => printDyn(v)
  | `thin => "thin"
  | `medium => "medium"
  | `thick => "thick"
  };

let printBrStyle = (v: StylitePpx.brStyle) =>
  switch v {
  | `none => "none"
  | `hidden => "hidden"
  | `dotted => "dotted"
  | `dashed => "dashed"
  | `solid => "solid"
  | `double => "double"
  | `groove => "groove"
  | `ridge => "ridge"
  | `inset => "inset"
  | `outset => "outset"
  };

let printBorderValue = (v: StylitePpx.borderValue) => {
  let (w, s, c) = v;
  let w = w |> optMap(printBrWidth) |> optDef("");
  let s = s |> optMap(printBrStyle) |> optDef("");
  let c = c |> optMap(printDyn) |> optDef("");
  String.trim(List.fold_left((acc, e) => acc ++ (e == "" ? "" : e ++ " "), "", [w, s, c]));
};

let printRadiusValue = (v: StylitePpx.radiusValue) =>
  switch v {
  | `length(v) => printDyn(v)
  | `percent(v) => printDyn(`percent(v))
  };

let printBackgroundImage = (v: option(list(StylitePpx.backgroundImageValue))) => {
  let printInner = v =>
    switch v {
    | `url(v) => "url(\"" ++ v ++ "\")"
    };
  switch v {
  | None => "none"
  | Some(v) => printList(", ", printInner, v)
  };
};

let printDeclaration = (declaration: StylitePpx.declaration) =>
  switch declaration {
  | `margin(v) => "margin: " ++ printGlobalAble(printTuple(printMarginValue), v) ++ ";"
  | `marginTop(v) => "margin-top: " ++ printGlobalAble(printMarginValue, v) ++ ";"
  | `marginBottom(v) => "margin-bottom: " ++ printGlobalAble(printMarginValue, v) ++ ";"
  | `marginLeft(v) => "margin-left: " ++ printGlobalAble(printMarginValue, v) ++ ";"
  | `marginRight(v) => "margin-right: " ++ printGlobalAble(printMarginValue, v) ++ ";"
  | `padding(v) => "padding: " ++ printGlobalAble(printTuple(printPaddingValue), v) ++ ";"
  | `paddingTop(v) => "padding-top: " ++ printGlobalAble(printPaddingValue, v) ++ ";"
  | `paddingBottom(v) => "padding-bottom: " ++ printGlobalAble(printPaddingValue, v) ++ ";"
  | `paddingLeft(v) => "padding-left: " ++ printGlobalAble(printPaddingValue, v) ++ ";"
  | `paddingRight(v) => "padding-right: " ++ printGlobalAble(printPaddingValue, v) ++ ";"
  | `border(v) => "border: " ++ printGlobalAble(printBorderValue, v) ++ ";"
  | `borderTop(v) => "border-top: " ++ printGlobalAble(printBorderValue, v) ++ ";"
  | `borderBottom(v) => "border-bottom: " ++ printGlobalAble(printBorderValue, v) ++ ";"
  | `borderLeft(v) => "border-left: " ++ printGlobalAble(printBorderValue, v) ++ ";"
  | `borderRight(v) => "border-right: " ++ printGlobalAble(printBorderValue, v) ++ ";"
  | `borderStyle(v) => "border-style: " ++ printGlobalAble(printTuple(printBrStyle), v) ++ ";"
  | `borderTopStyle(v) => "border-top-style: " ++ printGlobalAble(printBrStyle, v) ++ ";"
  | `borderBottomStyle(v) => "border-bottom-style: " ++ printGlobalAble(printBrStyle, v) ++ ";"
  | `borderLeftStyle(v) => "border-left-style: " ++ printGlobalAble(printBrStyle, v) ++ ";"
  | `borderRightStyle(v) => "border-right-style: " ++ printGlobalAble(printBrStyle, v) ++ ";"
  | `borderWidth(v) => "border-width: " ++ printGlobalAble(printTuple(printBrWidth), v) ++ ";"
  | `borderTopWidth(v) => "border-top-width: " ++ printGlobalAble(printBrWidth, v) ++ ";"
  | `borderBottomWidth(v) => "border-bottom-width: " ++ printGlobalAble(printBrWidth, v) ++ ";"
  | `borderLeftWidth(v) => "border-left-width: " ++ printGlobalAble(printBrWidth, v) ++ ";"
  | `borderRightWidth(v) => "border-right-width: " ++ printGlobalAble(printBrWidth, v) ++ ";"
  | `borderColor(v) => "border-color: " ++ printGlobalAble(printTuple(printDyn), v) ++ ";"
  | `borderTopColor(v) => "border-top-color: " ++ printGlobalAble(printDyn, v) ++ ";"
  | `borderBottomColor(v) => "border-bottom-color: " ++ printGlobalAble(printDyn, v) ++ ";"
  | `borderLeftColor(v) => "border-left-color: " ++ printGlobalAble(printDyn, v) ++ ";"
  | `borderRightColor(v) => "border-right-color: " ++ printGlobalAble(printDyn, v) ++ ";"
  | `borderRadius(v) => "border-radius: " ++ printGlobalAble(printTuple(printRadiusValue), v) ++ ";"
  | `borderTopLeftRadius(v) => "border-top-left-radius: " ++ printGlobalAble(printTuple(printRadiusValue), v) ++ ";"
  | `borderTopRightRadius(v) => "border-top-right-radius: " ++ printGlobalAble(printTuple(printRadiusValue), v) ++ ";"
  | `borderBottomLeftRadius(v) =>
    "border-bottom-left-radius: " ++ printGlobalAble(printTuple(printRadiusValue), v) ++ ";"
  | `borderBottomRightRadius(v) =>
    "border-bottom-right-radius: " ++ printGlobalAble(printTuple(printRadiusValue), v) ++ ";"
  | `backgroundColor(v) => "border-color: " ++ printGlobalAble(printTuple(printDyn), v) ++ ";"
  | `backgroundAttachment(v) => "backgroun-attachment: " ++ printGlobalAble(printTuple(printDyn), v) ++ ";"
  | `backgroundClip(v) => "background-clip: " ++ printGlobalAble(printDyn, v) ++ ";"
  | `backgroundImage(v) => "background-image: " ++ printGlobalAble(printBackgroundImage, v) ++ ";"
  };

let printListDeclaration = printList("\n", printDeclaration);