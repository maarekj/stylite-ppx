%{
    let integer_or_percent_to_255 value =
        match value with
        | `integer i -> i
        | `percent p -> truncate ((p *. 255.) /. 100.)
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token <string> DOLLAR_ID
%token <float> PERCENT
%token <int> PX
%token <float> EM
%token <float> REM
%token <float> CM
%token <float> MM
%token <int * int * int> HEX
%token ZERO URL
%token WS AUTO INHERIT UNSET INITIAL MARGIN MARGIN_TOP MARGIN_BOTTOM MARGIN_LEFT MARGIN_RIGHT PADDING PADDING_TOP PADDING_BOTTOM PADDING_LEFT PADDING_RIGHT
%token BORDER BORDER_TOP BORDER_BOTTOM BORDER_LEFT BORDER_RIGHT BORDER_WIDTH BORDER_TOP_WIDTH BORDER_BOTTOM_WIDTH BORDER_LEFT_WIDTH BORDER_RIGHT_WIDTH BORDER_STYLE BORDER_TOP_STYLE BORDER_BOTTOM_STYLE BORDER_LEFT_STYLE BORDER_RIGHT_STYLE BORDER_COLOR BORDER_TOP_COLOR BORDER_BOTTOM_COLOR BORDER_LEFT_COLOR BORDER_RIGHT_COLOR
%token BORDER_RADIUS BORDER_TOP_LEFT_RADIUS BORDER_TOP_RIGHT_RADIUS BORDER_BOTTOM_LEFT_RADIUS BORDER_BOTTOM_RIGHT_RADIUS
%token BACKGROUND BACKGROUND_COLOR BACKGROUND_ATTACHMENT BACKGROUND_CLIP BACKGROUND_IMAGE BACKGROUND_POSITION BACKGROUND_POSITION_X BACKGROUND_POSITION_Y BACKGROUND_REPEAT BACKGROUND_SIZE
%token THIN MEDIUM THICK NONE HIDDEN DOTTED DASHED SOLID DOUBLE GROOVE RIDGE INSET OUTSET
%token SCROLL FIXED LOCAL
%token BORDER_BOX PADDING_BOX CONTENT_BOX TEXT
%token TRANSPARENT RGB RGBA HSL HSLA CURRENT_COLOR
%token COMMA SEMI_COLON COLON
%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE LEFT_BRACK RIGHT_BRACK
%token EOF

%start <Css.declaration list> list_declaration
%start <Css.declaration> declaration
%%

list_declaration:
    | WS?; list = _list_declaration;                                        { list }

_list_declaration:
    | decl = declaration; WS?; rest = _list_declaration                     { decl :: rest }
    | EOF                                                                   { [] }

%inline decl(ATTR, X):
    | ATTR; COLON; WS?; v = X; SEMI_COLON   { v }

declaration:
    | v = decl(MARGIN, global_tuple(margin_value))                              { `margin(v) }
    | v = decl(MARGIN_TOP, global_or(margin_value))                             { `marginTop(v) }
    | v = decl(MARGIN_BOTTOM, global_or(margin_value))                          { `marginBottom(v) }
    | v = decl(MARGIN_LEFT, global_or(margin_value))                            { `marginLeft(v) }
    | v = decl(MARGIN_RIGHT, global_or(margin_value))                           { `marginRight(v) }
    | v = decl(PADDING, global_tuple(padding_value))                            { `padding(v) }
    | v = decl(PADDING_TOP, global_or(padding_value))                           { `paddingTop(v) }
    | v = decl(PADDING_BOTTOM, global_or(padding_value))                        { `paddingBottom(v) }
    | v = decl(PADDING_LEFT, global_or(padding_value))                          { `paddingLeft(v) }
    | v = decl(PADDING_RIGHT, global_or(padding_value))                         { `paddingRight(v) }
    | v = decl(BORDER, global_or(border_value))                                 { `border(v) }
    | v = decl(BORDER_TOP, global_or(border_value))                             { `borderTop(v) }
    | v = decl(BORDER_BOTTOM, global_or(border_value))                          { `borderBottom(v) }
    | v = decl(BORDER_LEFT, global_or(border_value))                            { `borderLeft(v) }
    | v = decl(BORDER_RIGHT, global_or(border_value))                           { `borderRight(v) }
    | v = decl(BORDER_STYLE, global_tuple(br_style))                            { `borderStyle(v) }
    | v = decl(BORDER_TOP_STYLE, global_or(br_style))                           { `borderTopStyle(v) }
    | v = decl(BORDER_BOTTOM_STYLE, global_or(br_style))                        { `borderBottomStyle(v) }
    | v = decl(BORDER_LEFT_STYLE, global_or(br_style))                          { `borderLeftStyle(v) }
    | v = decl(BORDER_RIGHT_STYLE, global_or(br_style))                         { `borderRightStyle(v) }
    | v = decl(BORDER_WIDTH, global_tuple(br_width))                            { `borderWidth(v) }
    | v = decl(BORDER_TOP_WIDTH, global_or(br_width))                           { `borderTopWidth(v) }
    | v = decl(BORDER_BOTTOM_WIDTH, global_or(br_width))                        { `borderBottomWidth(v) }
    | v = decl(BORDER_LEFT_WIDTH, global_or(br_width))                          { `borderLeftWidth(v) }
    | v = decl(BORDER_RIGHT_WIDTH, global_or(br_width))                         { `borderRightWidth(v) }
    | v = decl(BORDER_COLOR, global_tuple(color))                               { `borderColor(v) }
    | v = decl(BORDER_TOP_COLOR, global_or(color))                              { `borderTopColor(v) }
    | v = decl(BORDER_BOTTOM_COLOR, global_or(color))                           { `borderBottomColor(v) }
    | v = decl(BORDER_LEFT_COLOR, global_or(color))                             { `borderLeftColor(v) }
    | v = decl(BORDER_RIGHT_COLOR, global_or(color))                            { `borderRightColor(v) }
    | v = decl(BORDER_RADIUS, global_tuple(radius_value))                       { `borderRadius(v) }
    | v = decl(BORDER_TOP_LEFT_RADIUS, global_couple(radius_value))             { `borderTopLeftRadius(v) }
    | v = decl(BORDER_TOP_RIGHT_RADIUS, global_couple(radius_value))            { `borderTopRightRadius(v) }
    | v = decl(BORDER_BOTTOM_LEFT_RADIUS, global_couple(radius_value))          { `borderBottomLeftRadius(v) }
    | v = decl(BORDER_BOTTOM_RIGHT_RADIUS, global_couple(radius_value))         { `borderBottomRightRadius(v) }
    | v = decl(BACKGROUND_COLOR, global_couple(color))                          { `backgroundColor(v) }
    | v = decl(BACKGROUND_ATTACHMENT, global_couple(attachment))                { `backgroundAttachment(v) }
    | v = decl(BACKGROUND_CLIP, global_or(clipBox))                             { `backgroundClip(v) }
    | v = decl(BACKGROUND_IMAGE, global_or(background_image_values))            { `backgroundImage(v) }

global_value:
    | INITIAL               { `initial } 
    | UNSET                 { `unset }
    | INHERIT               { `inherit_ }

%inline global_or(X):
    | g = global_value      { `global(g) }
    | v = X                 { `normal(v) }

%inline global_tuple(X):
    | g = global_value                                  { `global(g) }
    | v1 = X                                            { `normal(`one(v1)) }
    | v1 = X; WS; v2 = X                                { `normal(`two((v1, v2))) }
    | v1 = X; WS; v2 = X; WS; v3 = X                    { `normal(`three((v1, v2, v3))) }
    | v1 = X; WS; v2 = X; WS; v3 = X; WS; v4 = X        { `normal(`four((v1, v2, v3, v4))) };

%inline global_couple(X):
    | g = global_value                                  { `global(g) }
    | v1 = X                                            { `normal(`one(v1)) }
    | v1 = X; WS; v2 = X                                { `normal(`two((v1, v2))) }

length:
    | ZERO                  { `zero }
    | v = PX                { `px(v) }
    | v = EM                { `em(v) }
    | v = REM               { `rem(v) }
    | v = CM                { `cm(v) }
    | v = MM                { `mm(v) }

int:
    | i = INT               { i }
    | ZERO                  { 0 }

integer_or_percent:
    | v = PERCENT           { `percent(v) }
    | i = INT               { `integer(i) }
    | ZERO                  { `integer(0) }

float:
    | i = int               { float_of_int(i) }
    | f = FLOAT             { f }

color:
    | RGB; LEFT_PAREN; WS?; r = integer_or_percent; WS?; COMMA; WS?; g = integer_or_percent; WS?; COMMA; WS?; b = integer_or_percent; WS?; RIGHT_PAREN
        {`rgb(integer_or_percent_to_255(r), integer_or_percent_to_255(g), integer_or_percent_to_255(b))}
    | RGBA; LEFT_PAREN; WS?; r = integer_or_percent; WS?; COMMA; WS?; g = integer_or_percent; WS?; COMMA; WS?; b = integer_or_percent; WS?; COMMA; WS?; a = float; WS?; RIGHT_PAREN
        {`rgba(integer_or_percent_to_255(r), integer_or_percent_to_255(g), integer_or_percent_to_255(b), a)}
    | CURRENT_COLOR 
        { `currentcolor }
    | TRANSPARENT
        { `transparent }
    | h = HEX
        { let (r, g, b) = h in `hex(r, g, b) }
    | v = DOLLAR_ID
        { `variable(v) }

margin_value:
    | l = length            { `length(l) }
    | v = PERCENT           { `percent(v) }
    | v = DOLLAR_ID         { `variable(v) }
    | AUTO                  { `auto }

padding_value:
    | l = length            { `length(l) }
    | v = PERCENT           { `percent(v) }
    | v = DOLLAR_ID         { `variable(v) }

br_style:
    | NONE 				{ `none }
    | HIDDEN 			{ `hidden }
    | DOTTED 			{ `dotted }
    | DASHED 			{ `dashed }
    | SOLID 			{ `solid }
    | DOUBLE 			{ `double }
    | GROOVE 			{ `groove }
    | RIDGE 			{ `ridge }
    | INSET 			{ `inset }
    | OUTSET 			{ `outset }
    | v = DOLLAR_ID		{ `variable(v) }

br_width:
    | l = length            { `length(l) }
    | THIN                  { `thin }
    | MEDIUM                { `medium }
    | THICK                 { `thick }
    | v = DOLLAR_ID         { `variable(v) }

attachment:
    | SCROLL                { `scroll }
    | FIXED                 { `fixed }
    | LOCAL                 { `local }
    | v = DOLLAR_ID         { `variable(v) }

border_value:
    | w = br_width; WS; s = br_style; WS; c = color                                 { (Some(w), Some(s), Some(c)) }
    | w = br_width; WS; s = br_style                                                { (Some(w), Some(s), None) }
    | w = br_width                                                                  { (Some(w), None, None) }

radius_value:
    | l = length            { `length(l) }
    | v = PERCENT           { `percent(v) }
    | v = DOLLAR_ID         { `variable(v) }

clipBox:
    | BORDER_BOX            { `borderBox }
    | PADDING_BOX           { `paddingBox }
    | CONTENT_BOX           { `contentBox }
    | TEXT                  { `text }
    | v = DOLLAR_ID         { `variable(v) }

url:
    | URL; LEFT_PAREN; s = STRING; RIGHT_PAREN  { `url(s) }

background_image_value:
    | v = url                   { v }
    | v = DOLLAR_ID             { `variable(v) }

comma_ws:
    | COMMA; WS?                { };

background_image_values:
    | NONE                                                              { None }
    | l = separated_nonempty_list(comma_ws, background_image_value)     { Some(l) }