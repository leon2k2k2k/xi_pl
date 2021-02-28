function separated(expr, seperator) {
    const no_trailing_sep = seq(expr, repeat(seq(seperator, expr)));

    return seq(no_trailing_sep, optional(seperator));
}

const PRECEDENCE = {
    APP: 3,
    FUN: 2,
    LAM: 1,
}

module.exports = grammar({
    name: 'aplite',
    extras: $ => [
        $.line_comment,
        /[\s]/
    ],

    rules: {
        source_file: $ => repeat($._stmt),

        line_comment: $ => token(seq(
            '//', /.*/
        )),

        // a source file is a list of statements

        // a Stmt is
        // 1. let < var_name : Ident> <var_type: Expr>? = <body: Expr>
        // 2. do <body: Expr>
        // 3. val <body: Expr>

        _stmt: $ => choice(
            $.let_stmt,
            $.do_stmt,
            $.val_stmt,
            $.fn_stmt,
        ),

        let_stmt: $ => seq("let", $.ident, optional(seq(":", $._expr)), "=", $._expr),

        do_stmt: $ => seq("do", $._expr),

        val_stmt: $ => seq("val", $._expr),

        fn_stmt: $ => seq("fn", $.ident, optional($.binders), optional(seq("->", $._expr)), $._expr),

        // an Ident is a sequence of alphanumeric characters
        ident: $ => /[A-Za-z0-9_]+/,

        // an Expr is
        // 1. an Ident
        // 2. "Type"
        // 3. <body : Expr>!
        // 4. <lhs: <rhs: Expr>
        // 5. {a list of statements }
        // 6. lambda <binders: <body: Expr>
        // 7. Pi <binder: <body: Expr>

        _expr: $ => choice(
            $.ident,
            $.type_expr,
            $.bang_expr,
            $.app_expr,
            $.fun_expr,
            $.lambda_expr,
            $.pi_expr,
            $.stmt_expr,
            $.paren_expr,
        ),

        type_expr: $ => "Type",

        bang_expr: $ => seq($._expr, "!"),

        app_expr: $ => prec.left(PRECEDENCE.APP, seq($._expr, $._expr)),

        fun_expr: $ => prec.right(PRECEDENCE.FUN, seq($._expr, "->", $._expr)),

        lambda_expr: $ => prec(PRECEDENCE.LAM, seq("lambda", $.binders, $._expr)),

        pi_expr: $ => prec(PRECEDENCE.LAM, seq("Pi", $.binders, $._expr)),

        stmt_expr: $ => seq("{", repeat($._stmt), "}"),

        paren_expr: $ => seq("(", $._expr, ")"),

        // a Binder is | <one or more of var_name : Expr> |
        binders: $ => seq("|", separated($.binder_component, ","), "|"),

        binder_component: $ => seq($.ident, optional(seq(":", $._expr))),

    }
});
