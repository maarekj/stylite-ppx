let prettyPrint = str => {
  let lexbuf = Lexing.from_string(str);
  let result = CssParser.list_declaration(CssLexer.read, lexbuf);
  PrettyPrint.printListDeclaration(result);
};

let checkPrettyPrint = (desc, css) => (
  desc,
  `Quick,
  () => Alcotest.(check(string))(desc, String.trim(css), String.trim(prettyPrint(css)))
);

let cpp = checkPrettyPrint;

let checkRaises = (desc, exn, css) => (
  desc,
  `Quick,
  () => Alcotest.(check_raises)(desc, exn, () => ignore(prettyPrint(css)))
);

let parsingTest = [
  cpp("Margin zero", "margin: 0;"),
  cpp("Margin one", "margin: 1px;"),
  cpp("Margin two", "margin: 1px 2px;"),
  cpp("Margin three", "margin: 1px 2px 3px;"),
  cpp("Margin four", "margin: 1px 2px 3px 4px;"),
  cpp("Margin four with percent", "margin: 1px 2% 10% 4px;"),
  cpp("Margin four with auto", "margin: 1px auto auto 4px;"),
  cpp("Margin variable", "margin: $a;"),
  cpp("Margin two variable", "margin: $a $b;"),
  cpp(
    "Margin top bottom left right",
    {|
margin-top: 0;
margin-bottom: 10px;
margin-left: 20%;
margin-right: auto;
margin-top: $a;
|}
  ),
  cpp(
    "Margin mozilla example",
    {|
margin: 1em;
margin: -3px;
margin: 5% auto;
margin: 1em auto 2em;
margin: 2px 1em 0 auto;
margin: inherit;
margin: initial;
margin: unset;
margin-top: 10px;
margin-top: 1em;
margin-top: 5%;
margin-top: auto;
margin-top: inherit;
margin-top: initial;
margin-top: unset;
margin-bottom: 10px;
margin-bottom: -1em;
margin-bottom: -5%;
margin-bottom: auto;
margin-bottom: inherit;
margin-bottom: initial;
margin-bottom: unset;
margin-left: -10px;
margin-left: 1em;
margin-left: 5%;
margin-left: auto;
margin-left: inherit;
margin-left: initial;
margin-left: unset;
margin-right: 10px;
margin-right: 1em;
margin-right: 5%;
margin-right: auto;
margin-right: inherit;
margin-right: initial;
margin-right: unset;
|}
  ),
  cpp("Padding zero", "padding: 0;"),
  cpp("Padding one", "padding: 1px;"),
  cpp("Padding two", "padding: 1px 0;"),
  cpp("Padding three", "padding: 1px 2px 3px;"),
  cpp("Padding four", "padding: 1px 2px 3px 4px;"),
  cpp("Padding four with percent", "padding: 1px 20% 30% 4px;"),
  cpp("Padding variable", "padding: $a;"),
  cpp("Padding two variable", "padding: $a $b;"),
  checkRaises("Padding four with auto should be failed", CssParser.Error, "padding: 1px auto;"),
  cpp(
    "Padding top bottom left right",
    {|
padding-top: 0;
padding-bottom: 10px;
padding-left: 20%;
padding-right: 30px;
padding-top: $a;
|}
  ),
  cpp(
    "Padding mozilla example",
    {|
padding: 1em;
padding: 5% 10%;
padding: 1em 2em 2em;
padding: 5px 1em 0 2em;
padding: inherit;
padding: initial;
padding: unset;
padding-top: 0.5em;
padding-top: 0;
padding-top: 2cm;
padding-top: 10%;
padding-top: inherit;
padding-top: initial;
padding-top: unset;
    |}
  ),
  cpp(
    "Border style mozilla example",
    {|
border-top-style: none;
border-bottom-style: hidden;
border-left-style: dotted;
border-right-style: dashed;
border-top-style: solid;
border-bottom-style: double;
border-left-style: groove;
border-right-style: ridge;
border-top-style: inset;
border-top-style: outset;
border-style: dotted solid;
border-style: hidden double dashed;
border-style: none solid dotted dashed;
border-style: inherit;
border-style: initial;
border-style: unset;
|}
  ),
  cpp(
    "Border width mozilla example",
    {|
border-width: thin;
border-width: medium;
border-width: thick;
border-width: 4px;
border-width: 1.2rem;
border-width: 2px 1.5em;
border-width: 1px 2em 1.5cm;
border-width: 1px 2em 0 4rem;
border-width: inherit;
border-width: initial;
border-width: unset;
|}
  ),
  cpp(
    "Border color mozilla example",
    {|
border-top-color: #FF00EE;
border-bottom-color: rgb(255, 0, 0);
border-left-color: transparent;
border-right-color: currentcolor;
border-top-color: $var;
border-bottom-color: inherit;
border-left-color: initial;
border-right-color: unset;
border-color: transparent;
border-color: rgb(0, 10, 50) #F015CA;
border-color: #FF0000 rgb(240, 30, 50) #00FF00;
border-color: #FF0000 #00FF00 #0000FF transparent;
border-color: $var;
border-color: $var $var2;
border-color: $var $var2 $var3;
border-color: $var $var2 $var3 $var4;
border-color: inherit;
border-color: initial;
border-color: unset;
|}
  ),
  cpp(
    "Border mozilla example",
    {|
border: 1px;
border: 2px dotted;
border: 2px outset #ABCDEF;
border: medium dashed rgb(100, 10, 150);
border: inherit;
border: initial;
border: unset;
border-top: 1px;
border-bottom: 2px dotted;
border-left: 2px outset rgba(10, 20, 100, 0.5);
border-right: medium dashed rgb(100, 10, 150);
border-top: inherit;
border-left: initial;
border-right: unset;
|}
  ),
  cpp(
    "Border radius",
    {|
border-radius: 10px;
border-radius: 10px 20em;
border-radius: 10px 20em 30rem;
border-radius: 10px -20em 30rem -10%;
border-radius: $var1 $var2 10%;
border-top-left-radius: $var1;
border-top-left-radius: 1em;
border-top-right-radius: 1rem;
border-bottom-right-radius: 1%;
border-bottom-left-radius: 1px;
border-bottom-left-radius: inherit;
border-bottom-left-radius: initial;
border-bottom-left-radius: unset;
border-top-left-radius: $var1 $v2;
border-top-left-radius: 1em 10%;
border-top-right-radius: 1rem 0;
border-bottom-right-radius: 0 1%;
border-bottom-left-radius: -10px 1px;
border-radius: inherit;
border-radius: initial;
border-radius: unset;
|}
  )
];

let () = Alcotest.run("Parsing test", [("parsing_test", parsingTest)]);