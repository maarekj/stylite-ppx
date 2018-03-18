{
open Lexing
open CssParser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let int_of_hex str = int_of_string ("0x" ^ str)
let int_of_hex_double str = int_of_string ("0x" ^ (str ^ str))
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = '-'? digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' ] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let dollarId = '$' id
let percent = float "%"
let px = int "px"
let rem = float "rem"
let em = float "em"
let cm = float "cm"
let mm = float "mm"
let hex3 = '#' ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f']
let hex6 = '#' ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f']

rule read =
  parse
  | hex3                  { HEX((int_of_hex_double (String.sub (Lexing.lexeme lexbuf) 1 1)), (int_of_hex_double (String.sub (Lexing.lexeme lexbuf) 2 1)), (int_of_hex_double (String.sub (Lexing.lexeme lexbuf) 3 1))) }
  | hex6                  { HEX((int_of_hex (String.sub (Lexing.lexeme lexbuf) 1 2)), (int_of_hex (String.sub (Lexing.lexeme lexbuf) 3 2)), (int_of_hex (String.sub (Lexing.lexeme lexbuf) 5 2))) }
  | mm                    { MM(float_of_string (String.sub (Lexing.lexeme lexbuf) 0 ((String.length (Lexing.lexeme lexbuf)) - 2))) }
  | cm                    { CM(float_of_string (String.sub (Lexing.lexeme lexbuf) 0 ((String.length (Lexing.lexeme lexbuf)) - 2))) }
  | rem                   { REM(float_of_string (String.sub (Lexing.lexeme lexbuf) 0 ((String.length (Lexing.lexeme lexbuf)) - 3))) }
  | em                    { EM(float_of_string (String.sub (Lexing.lexeme lexbuf) 0 ((String.length (Lexing.lexeme lexbuf)) - 2))) }
  | px                    { PX(int_of_string (String.sub (Lexing.lexeme lexbuf) 0 ((String.length (Lexing.lexeme lexbuf)) - 2))) }
  | percent               { PERCENT(float_of_string (String.sub (Lexing.lexeme lexbuf) 0 ((String.length (Lexing.lexeme lexbuf)) - 1))) }
  | white                 { WS }
  | newline               { next_line lexbuf; read lexbuf }
  | dollarId              { DOLLAR_ID(String.sub (Lexing.lexeme lexbuf) 1 ((String.length (Lexing.lexeme lexbuf)) - 1)) }
  | '0'                   { ZERO }
  | int                   { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | float                 { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
  | "url"	  		          { URL }
  | "transparent"	  		  { TRANSPARENT }
  | "border" 						  { BORDER }
  | "border-top" 				  { BORDER_TOP }
  | "border-bottom" 			{ BORDER_BOTTOM }
  | "border-left" 			  { BORDER_LEFT }
  | "border-right" 				{ BORDER_RIGHT }
  | "border-width" 				{ BORDER_WIDTH }
  | "border-top-width" 		{ BORDER_TOP_WIDTH }
  | "border-bottom-width" { BORDER_BOTTOM_WIDTH }
  | "border-left-width" 	{ BORDER_LEFT_WIDTH }
  | "border-right-width" 	{ BORDER_RIGHT_WIDTH }
  | "border-style" 				{ BORDER_STYLE }
  | "border-top-style" 		{ BORDER_TOP_STYLE }
  | "border-bottom-style" { BORDER_BOTTOM_STYLE }
  | "border-left-style" 	{ BORDER_LEFT_STYLE }
  | "border-right-style" 	{ BORDER_RIGHT_STYLE }
  | "border-color" 				{ BORDER_COLOR }
  | "border-top-color" 		{ BORDER_TOP_COLOR }
  | "border-bottom-color" { BORDER_BOTTOM_COLOR }
  | "border-left-color" 	{ BORDER_LEFT_COLOR }
  | "border-right-color" 	{ BORDER_RIGHT_COLOR }
  | "border-radius"                 { BORDER_RADIUS }
  | "border-top-left-radius"        { BORDER_TOP_LEFT_RADIUS }
  | "border-top-right-radius"       { BORDER_TOP_RIGHT_RADIUS }
  | "border-bottom-left-radius"     { BORDER_BOTTOM_LEFT_RADIUS }
  | "border-bottom-right-radius"    { BORDER_BOTTOM_RIGHT_RADIUS }
  | "currentcolor" | "currentColor" { CURRENT_COLOR }
  | "rgb" 			        { RGB }
  | "rgba" 			        { RGBA }
  | "hsl" 			        { HSL }
  | "hsla" 			        { HSLA }
  | "thin" 				      { THIN }
  | "medium" 				    { MEDIUM }
  | "thick" 				    { THICK }
  | "none" 				      { NONE }
  | "hidden" 				    { HIDDEN }
  | "dotted" 				    { DOTTED }
  | "dashed" 				    { DASHED }
  | "solid" 				    { SOLID }
  | "double" 				    { DOUBLE }
  | "groove" 				    { GROOVE }
  | "ridge" 				    { RIDGE }
  | "inset" 				    { INSET }
  | "outset" 				    { OUTSET }
  | "auto"              { AUTO}
  | "margin"            { MARGIN }
  | "margin-top"        { MARGIN_TOP }
  | "margin-bottom"     { MARGIN_BOTTOM }
  | "margin-left"       { MARGIN_LEFT }
  | "margin-right"      { MARGIN_RIGHT }
  | "padding"           { PADDING }
  | "padding-top"       { PADDING_TOP }
  | "padding-bottom"    { PADDING_BOTTOM }
  | "padding-left"      { PADDING_LEFT }
  | "padding-right"     { PADDING_RIGHT }
  | "inherit"           { INHERIT }
  | "initial"           { INITIAL }
  | "unset"             { UNSET }  
  | "scroll"            { SCROLL }  
  | "fixed"             { FIXED }  
  | "local"             { LOCAL }
  | "background"					    { BACKGROUND }
  | "background-color"					{ BACKGROUND_COLOR }
  | "background-attachment"				{ BACKGROUND_ATTACHMENT }
  | "background-clip"					{ BACKGROUND_CLIP }
  | "background-image"					{ BACKGROUND_IMAGE }
  | "background-position"				{ BACKGROUND_POSITION }
  | "background-position-x"				{ BACKGROUND_POSITION_X }
  | "background-position-y"				{ BACKGROUND_POSITION_Y }
  | "background-repeat"					{ BACKGROUND_REPEAT }
  | "background-size"					{ BACKGROUND_SIZE }
  | "border-box" { BORDER_BOX }
  | "padding-box" { PADDING_BOX }
  | "content-box" { CONTENT_BOX }
  | "text" { TEXT }
  | '"'                 { read_string (Buffer.create 17) lexbuf }
  | '('                 { LEFT_PAREN }
  | ')'                 { RIGHT_PAREN }
  | '{'                 { LEFT_BRACE }
  | '}'                 { RIGHT_BRACE }
  | '['                 { LEFT_BRACK }
  | ']'                 { RIGHT_BRACK }
  | ':'                 { COLON }
  | ','                 { COMMA }
  | ';'                 { SEMI_COLON }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
