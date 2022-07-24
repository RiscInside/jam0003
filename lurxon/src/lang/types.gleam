import gleam/option
import gleam/result
import gleam/map
import gleam/list
import gleam/pair
import lang/ast

pub type Type {
  InvalidTypeMarker
  IntegerType
  BooleanType
  StringType
  FunctionType(args: List(Type), out: Type)
}

pub type TypeAdapter(old_annotation_type, new_annotation_type) {
  TypeAdapter(
    encode_type: fn(old_annotation_type, Type) -> new_annotation_type,
    decode_type: fn(new_annotation_type) -> Type,
  )
}

type TypeMappingEntry {
  TypeMappingEntry(ty: Type, mutable: Bool)
}

type TypeMapping =
  List(map.Map(String, TypeMappingEntry))

fn lookup_variable_entry(
  mapping: TypeMapping,
  name: String,
) -> option.Option(TypeMappingEntry) {
  mapping
  |> list.find(map.has_key(_, name))
  |> result.then(map.get(_, name))
  |> option.from_result()
}

fn lookup_variable_type_read(mapping: TypeMapping, name: String) -> Type {
  case lookup_variable_entry(mapping, name) {
    option.Some(TypeMappingEntry(ty, _)) -> ty
    other -> todo
  }
  // report unknown variable
}

fn lookup_variable_type_write(mapping: TypeMapping, name: String) -> Type {
  case lookup_variable_entry(mapping, name) {
    option.Some(TypeMappingEntry(ty, True)) -> ty
    option.Some(TypeMappingEntry(ty, False)) -> todo
    // report "can't mutate immutable variable" error
    other -> todo
  }
  // report unknown variable
}

fn is_subtype_of(ty1: Type, ty2: Type) -> Bool {
  ty1 == InvalidTypeMarker || ty1 == ty2
}

fn typecheck_call_expr(
  adapter: TypeAdapter(old_annotation_type, new_annotation_type),
  old_annotation: old_annotation_type,
  new_fun: ast.Expr(new_annotation_type),
  args: List(ast.Argument(old_annotation_type)),
  mapping: TypeMapping,
) -> ast.Expr(new_annotation_type) {
  let fun_type = adapter.decode_type(new_fun.annotation)
  case fun_type {
    FunctionType(arg_types, out_type) ->
      case list.strict_zip(arg_types, args) {
        Ok(pairs) -> {
          // run typechecking on arguments
          let new_arg_pairs =
            list.map(
              pairs,
              fn(pair) {
                let #(ty, arg) = pair
                case arg {
                  ast.Capture(expr) -> #(
                    ty,
                    ast.Capture(typecheck_expr(
                      adapter,
                      expr,
                      option.Some(ty),
                      mapping,
                    )),
                  )
                  ast.Wildcard -> #(ty, ast.Wildcard)
                }
              },
            )
          // check that argument types match
          case list.find(
            list.range(0, list.length(new_arg_pairs))
            |> list.zip(new_arg_pairs),
            fn(elem) {
              let #(_, #(ty, arg)) = elem
              case arg {
                ast.Capture(expr) -> {
                  let inferred_arg_type = adapter.decode_type(expr.annotation)
                  !is_subtype_of(inferred_arg_type, ty)
                }
                ast.Wildcard -> False
              }
            },
          ) {
            Ok(#(no, #(ty, arg))) -> todo
            // report type mismatch
            _ -> Nil
          }
          // construct new rewritten call expression node
          let new_args =
            new_arg_pairs
            |> list.map(pair.second)
          let new_data = ast.CallExpr(new_fun, new_args)
          // get closure argument types
          let arg_types =
            list.filter(
              new_arg_pairs,
              fn(elem) {
                case elem {
                  #(_, ast.Wildcard) -> True
                  other -> False
                }
              },
            )
            |> list.map(pair.first)
          // if there are no wildcards present, we simply return the out_type, otherwise its a closure
          let result_type = case arg_types {
            [] -> out_type
            other -> FunctionType(other, out_type)
          }
          ast.Expr(adapter.encode_type(old_annotation, result_type), new_data)
        }
        // report "invalid number of arguments" error
        Error(list.LengthMismatch) -> todo
      }
    // report attempt to call a non-function error
    other -> todo
  }
}

fn push_variable_scope(mapping: TypeMapping) -> TypeMapping {
  [map.from_list([]), ..mapping]
}

fn declare_variable_type(
  mapping: TypeMapping,
  name: String,
  ty: Type,
  mutable: Bool,
) -> TypeMapping {
  case mapping {
    [cur_scope, ..other_scopes] -> [
      cur_scope
      |> map.insert(name, TypeMappingEntry(ty, mutable)),
      ..other_scopes
    ]
    [] ->
      push_variable_scope([])
      |> declare_variable_type(name, ty, mutable)
  }
}

fn typecheck_expr(
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
              _other -> todo
            }
          // report unexpected operand types for binop
          ast.And | ast.Or ->
            case #(lhs_type, rhs_type) {
              #(BooleanType, BooleanType) -> BooleanType
              _other -> todo
            }
          // report unexpected operand types for binop
          ast.Gt | ast.Lt | ast.Ge | ast.Le ->
            case #(lhs_type, rhs_type) {
              #(IntegerType, IntegerType) -> BooleanType
              _other -> todo
            }
          // report unexpected operand types for binop
          ast.Eq | ast.Ne ->
            case lhs_type == rhs_type {
              True -> BooleanType
              _other -> todo
            }
        },
      )
    }

    // report unexpected operand types for binop
    ast.VarExpr(name) ->
      name
      |> lookup_variable_type_read(mapping, _)
      |> augment_with_type(ast.VarExpr(name), _)

    ast.CallExpr(fun, args) ->
      typecheck_call_expr(
        adapter,
        old_annotation,
        typecheck_wrapper(fun, option.None),
        args,
        mapping,
      )
  }
}
