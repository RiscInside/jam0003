import gleam/option

pub type UnOp {
  Neg
  Not
  Paren
}

pub type BinOp {
  Add
  Sub
  Mul
  Div
  Mod
  And
  Or
  Gt
  Lt
  Ge
  Le
  Eq
  Ne
}

pub type Expr(annotation_type) {
  Expr(annotation: annotation_type, data: ExprData(annotation_type))
}

pub type ExprData(annotation_type) {
  IntegerLiteral(val: Int)
  BooleanLiteral(val: Bool)
  StringLiteral(val: String)
  UnOpExpr(op: UnOp, child: Expr(annotation_type))
  BinOpExpr(op: BinOp, lhs: Expr(annotation_type), rhs: Expr(annotation_type))
  VarExpr(name: String)
  CallExpr(fun: Expr(annotation_type), args: List(Argument(annotation_type)))
}

pub type Type(annotation_type) {
  TypeData(annotation: annotation_type, data: TypeData(annotation_type))
}

pub type TypeData(annotation_type) {
  InvalidTypeMarker
  IntegerType
  BooleanType
  StringType
  FunctionType(args: List(Type(annotation_type)), out: Type(annotation_type))
}

pub type Argument(expr_annotation_type) {
  Capture(expr: Expr(expr_annotation_type))
  Wildcard
}

pub type Statement(
  expr_annotation_type,
  type_annotation_type,
  statement_annotation_type,
  block_annotation_type,
) {
  Statement(
    annotation: statement_annotation_type,
    data: StatementData(
      expr_annotation_type,
      type_annotation_type,
      statement_annotation_type,
      block_annotation_type,
    ),
  )
}

pub type StatementData(
  expr_annotation_type,
  type_annotation_type,
  statement_annotation_type,
  block_annotation_type,
) {
  DeclarationStmt(
    name: String,
    ty: option.Option(Type(type_annotation_type)),
    value: Expr(expr_annotation_type),
  )
  AssignmentStmt(
    lhs: Expr(expr_annotation_type),
    rhs: Expr(expr_annotation_type),
  )
  ReturnStmt(value: Expr(expr_annotation_type))
  IfStmt(
    options: List(
      #(
        Expr(expr_annotation_type),
        Block(
          expr_annotation_type,
          type_annotation_type,
          statement_annotation_type,
          block_annotation_type,
        ),
      ),
    ),
  )
  WhileStmt(
    Expr(expr_annotation_type),
    Block(
      expr_annotation_type,
      type_annotation_type,
      statement_annotation_type,
      block_annotation_type,
    ),
  )
  BreakStmt
  ContinueStmt
  TodoStmt
  AssertStmt(Expr(expr_annotation_type))
}

pub type Block(
  expr_annotation_type,
  type_annotation_type,
  statement_annotation_type,
  block_annotation_type,
) {
  Block(
    annotation: block_annotation_type,
    data: List(
      Statement(
        expr_annotation_type,
        type_annotation_type,
        statement_annotation_type,
        block_annotation_type,
      ),
    ),
  )
}

pub type Function(
  expr_annotation_type,
  type_annotation_type,
  statement_annotation_type,
  block_annotation_type,
  function_annotation_type,
) {
  Function(
    annotation: function_annotation_type,
    name: String,
    args: List(#(String, Type(type_annotation_type))),
    ret_type: Type(type_annotation_type),
    body: Block(
      expr_annotation_type,
      type_annotation_type,
      statement_annotation_type,
      block_annotation_type,
    ),
  )
}
