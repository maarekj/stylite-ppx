open Asttypes;

open Parsetree;

open Ast_mapper;

let addPos = (base, pos) => {
  open Lexing;
  open Location;
  let (_, l, col) = Location.get_pos_info(base);
  let (_, nl, ncol) = Location.get_pos_info(pos);
  {pos_fname: base.pos_fname, pos_lnum: l + nl - 1, pos_bol: 0, pos_cnum: col + ncol};
};

let addLoc = (base, position) =>
  Location.{
    loc_start: addPos(base.loc_start, position),
    loc_end: addPos(base.loc_start, {...position, pos_lnum: position.pos_lnum}),
    loc_ghost: false
  };

let test_mapper = argv => {
  ...default_mapper,
  expr: (mapper, expr) =>
    switch expr {
    | {
        pexp_desc:
          Pexp_extension((
            {txt: "stylite"},
            PStr([{pstr_desc: Pstr_eval({pexp_loc: loc, pexp_desc: Pexp_constant(Const_string(css, _))}, _)}])
          ))
      } =>
      let lexbuf = Lexing.from_string(css);
      try {
        let result = CssParser.list_declaration(CssLexer.read, lexbuf);
        ToOcaml.printListDeclaration(result);
      } {
      | CssParser.Error => raise(Location.Error(Location.error(~loc=addLoc(loc, lexbuf.lex_curr_p), "Parsing error")))
      | CssLexer.SyntaxError(error) =>
        raise(Location.Error(Location.error(~loc=addLoc(loc, lexbuf.lex_curr_p), error)))
      };
    | other => default_mapper.expr(mapper, other)
    }
};

let () = register("stylite", test_mapper);