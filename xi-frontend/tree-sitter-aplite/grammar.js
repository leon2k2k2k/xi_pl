// we need to rewrite grammar to make it better with error handling, and allow multi-line comment.

function separated(expr, seperator) {
  const noTrailingSep = seq(expr, repeat(seq(seperator, expr)));

  return seq(noTrailingSep, optional(seperator));
}

const PRECEDENCE = {
  MEM: 40,
  APP: 30,
  FUN: 20,
  LAM: 10,
  BANG: 0,
  BINARY: 1,
  OR: 2,
  AND: 3,
  COMP: 4,
  ADD: 5,
  MUL: 6,
  EXP: 7,
};

module.exports = grammar({
  name: "aplite",

  extras: ($) => [
    $.line_comment,
    $.newline,
    $.whitespace,
  ],

  word: ($) => $.ident,

  rules: {
    source_file: ($) => repeat($._stmt),

    line_comment: (_$) =>
      token(seq(
        "//",
        /.*/,
      )),

    whitespace: (_$) => / +/g,

    newline: (_$) => /\r\n|\n|\r/,

    _stmt: ($) =>
      choice(
        $.decorator_stmt,
        $.let_stmt,
        $.do_stmt,
        $.val_stmt,
        $.fn_stmt,
        $.import_stmt,
        $.transport_stmt,
        $.ffi_stmt,
        $.if_stmt,
        $.with_stmt,
        $.enum_stmt,
        $.struct_stmt,
      ),

    decorator_stmt: ($) => seq("@", $.ident, optional(seq("transport", $.ident)), $._stmt),

    let_stmt: ($) =>
      seq("let", $.ident, optional(seq(":", $._expr)), "=", $._expr),

    do_stmt: ($) => seq("do", $._expr),

    val_stmt: ($) => seq("val", $._expr),

    fn_stmt: ($) =>
      seq("fn", $.ident, $.binders, optional(seq("->", $._expr)), $.stmt_expr),

    import_stmt: ($) => seq("import", $.string_expr, $.import_components),

    import_components: ($) => seq("{", separated($.ident, ","), "}"),

    transport_stmt: ($) => seq("transport", $.string_expr, $.dict_expr),

    // transport_components: ($) => seq("{", separated($.ident, ","), "}"),

    ffi_stmt: ($) => seq("ffi", $.string_expr, $.dict_expr),

    if_stmt: ($) =>
      seq(
        $.if_phrase,
        optional(seq(repeat($.else_if_phrase), $.else_phrase)),
      ),

    if_phrase: ($) => seq("if", $._expr, $._expr),

    else_if_phrase: ($) => seq("else if", $._expr, $._expr),

    else_phrase: ($) => seq("else", $._expr),

    with_stmt: ($) => seq("with", $.ident, $._stmt),

    enum_stmt: ($) =>
      seq(
        "enum",
        $.ident,
        optional($.binders),
        $.enum_components,
      ),

    enum_components: ($) => seq("{", separated($.enum_component, ","), "}"),

    enum_component: ($) => seq($.ident, optional($.tuple_expr)),

    struct_stmt: ($) =>
      seq(
        "struct",
        $.ident,
        optional($.binders),
        $.struct_components,
      ),

    struct_components: ($) => seq("{", separated($.struct_component, ","), "}"),

    struct_component: ($) => seq($.ident, ":", $._expr),

    ident: (_$) => /\p{XID_Start}\p{XID_Continue}*/u,

    _expr: ($) =>
      choice(
        $.ident_expr,
        $.type_expr,
        $.bang_expr,
        $.app_expr,
        $.fun_expr,
        $.lambda_expr,
        $.pi_expr,
        $.stmt_expr,
        $.member_expr,
        $.string_expr,
        $.number_expr,
        $.binary_expr,
        $.dict_expr,
        $.tuple_expr,
        $._paren_expr,
        $.list_expr,
      ),

    ident_expr: ($) => $.ident,
    type_expr: ($) => "Type",

    bang_expr: ($) => prec(PRECEDENCE.BANG, seq($._expr, "!")),

    app_expr: ($) => prec.left(PRECEDENCE.APP, seq($._expr, $._expr)),

    fun_expr: ($) => prec.right(PRECEDENCE.FUN, seq($._expr, "->", $._expr)),

    lambda_expr: ($) => prec(PRECEDENCE.LAM, seq("lambda", $.binders, $._expr)),

    pi_expr: ($) => prec(PRECEDENCE.LAM, seq("Pi", $.binders, $._expr)),

    stmt_expr: ($) => seq("{", repeat($._stmt), "}"),

    member_expr: ($) => prec.left(PRECEDENCE.MEM, seq($._expr, ".", $._expr)),

    string_expr: ($) =>
      seq(
        '"',
        repeat($.string_component),
        '"',
      ),

    string_component: ($) =>
      token.immediate(choice(
        /[^"\\]+/,
        /\\[^xu0-7]/,
        /\\[0-7]{1,3}/,
        /\\x[0-9a-fA-F]{2}/,
        /\\u[0-9a-fA-F]{4}/,
        /\\u{[0-9a-fA-F]+}/,
      )),

    number_expr: ($) => /-?[\d]+/,

    binary_expr: ($) => {
      const precedence_table = [
        [PRECEDENCE.AND, "&&"],
        [PRECEDENCE.OR, "||"],
        [PRECEDENCE.COMP, choice("==", "!=", "<", "<=", ">", ">=")],
        [PRECEDENCE.ADD, choice("+", "-")],
        [PRECEDENCE.MUL, choice("*", "/", "%")],
        [PRECEDENCE.EXP, choice("^")],
      ];

      return choice(
        ...precedence_table.map(([precedence, op]) =>
          prec.left(
            precedence,
            seq($._expr, op, $._expr),
          )
        ),
      );
    },

    // binary_expr: ($) =>
    //   prec(PRECEDENCE.BINARY, seq($._expr, $.binary_op, $._expr)),

    // binary_op: ($) => choice("&&", "//", "==", "!=", "<", "<=", ">", ">="),

    // a Binder is | <one or more of var_name : Expr> |
    binders: ($) => seq("|", separated($.binder_component, ","), "|"),

    binder_component: ($) =>
      seq(optional($.mut), $.ident, optional(seq(":", $._expr))),

    mut: ($) => "&mut",

    dict_expr: ($) => seq("{", separated($.dict_component, ","), "}"),

    dict_component: ($) => seq($.ident, ":", $._expr),

    tuple_expr: ($) =>
      choice(
        seq("(", ")"),
        seq("(", $._expr, ",", optional(separated($._expr, ",")), ")"),
      ),

    _paren_expr: ($) => seq("(", $._expr, ")"),

    list_expr: ($) => seq("[", optional(separated($._expr, ",")), "]"),
  },
});
