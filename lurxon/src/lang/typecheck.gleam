import gleam/option
import gleam/result
import gleam/map
import gleam/list
import lang/ast

pub type Type {
  InvalidTypeMarker
  IntegerType
  BooleanType
  StringType
}

pub type TypeAdapter(old_annotation_type, new_annotation_type) {
  TypeAdapter(
    encode_type: fn(old_annotation_type, Type) -> new_annotation_type,
    decode_type: fn(new_annotation_type) -> Type,
  )
}

pub type TypeMapping =
  List(map.Map(String, Type))

fn lookup_variable_type(mapping: TypeMapping, name: String) -> Type {
  mapping
  |> list.find(map.has_key(_, name))
  |> result.then(map.get(_, name))
  |> result.unwrap(InvalidTypeMarker)
}

fn push_variable_scope(mapping: TypeMapping) -> TypeMapping {
  [map.from_list([]), ..mapping]
}

fn declare_variable_type(
  mapping: TypeMapping,
  name: String,
  ty: Type,
) -> TypeMapping {
  case mapping {
    [cur_scope, ..other_scopes] -> [
      cur_scope
      |> map.insert(name, ty),
      ..other_scopes
    ]
    [] ->
      push_variable_scope([])
      |> declare_variable_type(name, ty)
  }
}

pub fn typecheck_expr(
  adapter: TypeAdapter(old_annotation_type, new_annotation_type),
  expr: ast.Expr(old_annotation_type),
  constraint: option.Option(Type),
  mapping: TypeMapping,
) -> ast.Expr(new_annotation_type) {
  let ast.Expr(old_annotation, old_data) = expr
  let augment_with_type = fn(new_data, inferred_type) {
    ast.Expr(adapter.encode_type(old_annotation, inferred_type), new_data)
  }
  let typecheck_wrapper = fn(expr, constraint) {
    typecheck_expr(adapter, expr, constraint, mapping)
  }
  case old_data {
    ast.IntegerLiteral(val) ->
      augment_with_type(ast.IntegerLiteral(val), IntegerType)

    ast.BooleanLiteral(val) ->
      augment_with_type(ast.BooleanLiteral(val), BooleanType)

    ast.StringLiteral(val) ->
      augment_with_type(ast.StringLiteral(val), StringType)

    ast.UnOpExpr(op, child) -> {
      let new_child = typecheck_wrapper(child, option.None)
      let child_type =
        new_child.annotation
        |> adapter.decode_type()
      augment_with_type(
        ast.UnOpExpr(op, new_child),
        case op {
          ast.Neg ->
            case child_type {
              IntegerType -> IntegerType
              _other -> InvalidTypeMarker
            }
          ast.Not ->
            case child_type {
              BooleanType -> BooleanType
              _other -> InvalidTypeMarker
            }
          ast.Paren -> child_type
        },
      )
    }

    ast.BinOpExpr(op, lhs, rhs) -> {
      let new_lhs = typecheck_wrapper(lhs, option.None)
      let new_rhs = typecheck_wrapper(rhs, option.None)
      let lhs_type =
        new_lhs.annotation
        |> adapter.decode_type()
      let rhs_type =
        new_rhs.annotation
        |> adapter.decode_type()
      augment_with_type(
        ast.BinOpExpr(op, new_lhs, new_rhs),
        case op {
          ast.Add | ast.Sub | ast.Mul | ast.Div | ast.Mod ->
            case #(lhs_type, rhs_type) {
              #(IntegerType, IntegerType) -> IntegerType
              _other -> InvalidTypeMarker
            }
          ast.And | ast.Or ->
            case #(lhs_type, rhs_type) {
              #(BooleanType, BooleanType) -> BooleanType
              _other -> InvalidTypeMarker
            }
          ast.Gt | ast.Lt | ast.Ge | ast.Le ->
            case #(lhs_type, rhs_type) {
              #(IntegerType, IntegerType) -> BooleanType
              _other -> InvalidTypeMarker
            }
          ast.Eq | ast.Ne ->
            case lhs_type == rhs_type {
              True -> BooleanType
              _other -> InvalidTypeMarker
            }
        },
      )
    }

    ast.VarExpr(name) ->
      name
      |> lookup_variable_type(mapping, _)
      |> augment_with_type(ast.VarExpr(name), _)
  }
}
