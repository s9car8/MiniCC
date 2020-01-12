#include <assert.h>
#include "lexer.h"
#include "parse.h"


#define M std::move

TypeSpec parse_type_spec(LexCtx &lctx, ParseCtx &pctx)
{
	if (match_token(lctx, TOKEN_INT)) {
		return {TYPESPEC_INT};
	}
	else if (match_token(lctx, TOKEN_FLOAT)) {
		return {TYPESPEC_FLOAT};
	}
	else if (match_token(lctx, TOKEN_CHAR)) {
		return {TYPESPEC_CHAR};
	}
	else {
		//unexpected_token_error(lctx);
		return {TYPESPEC_NONE};
	}
}

std::vector<Param> parse_params(LexCtx &lctx, ParseCtx &pctx)
{
	std::vector<Param> params;
	TypeSpec type_spec;
	std::string name;

	if ((type_spec = parse_type_spec(lctx, pctx)).kind == TYPESPEC_NONE)
		return params;
	expect_token(lctx, TOKEN_IDENTIFIER);
	name = std::get<std::string>(lctx.current_token.val);
	params.push_back(Param{type_spec, name});

	while (match_token(lctx, ',')) {
		type_spec = parse_type_spec(lctx, pctx);
		expect_token(lctx, TOKEN_IDENTIFIER);
		name = std::get<std::string>(lctx.current_token.val);
		params.push_back(Param{type_spec, name});
	}
	return params;
}

Expr parse_expr(LexCtx &lctx, ParseCtx &pctx);

std::vector<Expr> parse_args(LexCtx &lctx, ParseCtx &pctx)
{
	std::vector<Expr> args;

	do {
		auto expr = parse_expr(lctx, pctx);
		if (expr.kind == EXPR_NONE) break;
		args.push_back(M(expr));
	} while (match_token(lctx, ','));
	return args;
}

Expr parse_base_expr(LexCtx &lctx, ParseCtx &pctx)
{
	if (match_token(lctx, TOKEN_IDENTIFIER)) {
		const auto &name = std::get<std::string>(lctx.current_token.val);
		if (match_token(lctx, '(')) {
			auto args = parse_args(lctx, pctx);
			expect_token(lctx, ')');
			return {EXPR_CALL, Expr::Call{name, M(args)}};
		}
		return {EXPR_IDENTIFIER, name};
	}
	else if (match_token(lctx, TOKEN_INTEGER)) {
		return {EXPR_INTEGER, std::get<int>(lctx.current_token.val)};
	}
	else if (match_token(lctx, TOKEN_REAL)) {
		return {EXPR_REAL, std::get<float>(lctx.current_token.val)};
	}
	else if (match_token(lctx, TOKEN_CHARACTER)) {
		return {EXPR_CHARACTER, std::get<char>(lctx.current_token.val)};
	}
	else if (match_token(lctx, '(')) {
		auto expr = parse_expr(lctx, pctx);
		expect_token(lctx, ')');
		return expr;
	}
	else {
		return {EXPR_NONE};
	}
}

Expr parse_mul_expr(LexCtx &lctx, ParseCtx &pctx)
{
	Expr expr;

	if ((expr = parse_base_expr(lctx, pctx)).kind == EXPR_NONE) {
		return {EXPR_NONE};
	}

	if (match_token(lctx, '*')) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_mul_expr(lctx, pctx))};
		return {EXPR_MUL, M(binary)};
	}
	else if (match_token(lctx, '/')) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_mul_expr(lctx, pctx))};
		return {EXPR_DIV, M(binary)};
	}
	else {
		return expr;
	}
}

Expr parse_add_expr(LexCtx &lctx, ParseCtx &pctx)
{
	Expr expr;

	if ((expr = parse_mul_expr(lctx, pctx)).kind == EXPR_NONE) {
		return {EXPR_NONE};
	}

	if (match_token(lctx, '+')) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_add_expr(lctx, pctx))};
		return {EXPR_ADD, M(binary)};
	}
	else if (match_token(lctx, '/')) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_add_expr(lctx, pctx))};
		return {EXPR_SUB, M(binary)};
	}
	else {
		return expr;
	}
}

Expr parse_cmp_rel_expr(LexCtx &lctx, ParseCtx &pctx)
{
	Expr expr;

	if ((expr = parse_add_expr(lctx, pctx)).kind == EXPR_NONE) {
		return {EXPR_NONE};
	}

	if (match_token(lctx, '<')) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_cmp_rel_expr(lctx, pctx))};
		return {EXPR_LESS, M(binary)};
	}
	else if (match_token(lctx, TOKEN_LEQ)) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_cmp_rel_expr(lctx, pctx))};
		return {EXPR_LESS_EQUAL, M(binary)};
	}
	else if (match_token(lctx, '>')) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_cmp_rel_expr(lctx, pctx))};
		return {EXPR_GREAT, M(binary)};
	}
	else if (match_token(lctx, TOKEN_GEQ)) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_cmp_rel_expr(lctx, pctx))};
		return {EXPR_GREAT_EQUAL, M(binary)};
	}
	else {
		return expr;
	}
}

Expr parse_cmp_eq_expr(LexCtx &lctx, ParseCtx &pctx)
{
	Expr expr;

	if ((expr = parse_cmp_rel_expr(lctx, pctx)).kind == EXPR_NONE) {
		return {EXPR_NONE};
	}

	if (match_token(lctx, TOKEN_EQ)) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_cmp_eq_expr(lctx, pctx))};
		return {EXPR_EQUAL, M(binary)};
	}
	else if (match_token(lctx, TOKEN_NEQ)) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_cmp_eq_expr(lctx, pctx))};
		return {EXPR_NOT_EQUAL, M(binary)};
	}
	else {
		return expr;
	}
}

Expr parse_logical_and_expr(LexCtx &lctx, ParseCtx &pctx)
{
	Expr expr;

	if ((expr = parse_cmp_eq_expr(lctx, pctx)).kind == EXPR_NONE) {
		return {EXPR_NONE};
	}

	if (match_token(lctx, TOKEN_AND)) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_logical_and_expr(lctx, pctx))};
		return {EXPR_LOGICAL_AND, M(binary)};
	}
	else {
		return expr;
	}
}

Expr parse_logical_or_expr(LexCtx &lctx, ParseCtx &pctx)
{
	Expr expr;

	if ((expr = parse_logical_and_expr(lctx, pctx)).kind == EXPR_NONE) {
		return {EXPR_NONE};
	}

	if (match_token(lctx, TOKEN_OR)) {
		auto binary = Expr::Binary{std::make_unique<Expr>(M(expr)), std::make_unique<Expr>(parse_logical_or_expr(lctx, pctx))};
		return {EXPR_LOGICAL_OR, M(binary)};
	}
	else {
		return expr;
	}
}

Expr parse_expr(LexCtx &lctx, ParseCtx &pctx)
{
	return parse_logical_or_expr(lctx, pctx);
}

Stmt parse_stmt(LexCtx &lctx, ParseCtx &pctx);

auto parse_stmts(LexCtx &lctx, ParseCtx &pctx)
{
	std::vector<Stmt> stmts;

	while (true) {
		auto stmt = parse_stmt(lctx, pctx);
		if (stmt.kind == STMT_NONE) break;
		stmts.push_back(M(stmt));
	}
	return M(stmts);
}

Decl parse_decl(LexCtx &lctx, ParseCtx &pctx);

Stmt parse_stmt(LexCtx &lctx, ParseCtx &pctx)
{
	Decl decl;
	Expr expr;

	if ((decl = parse_decl(lctx, pctx)).kind != DECL_NONE) {
		assert(decl.kind == DECL_VAR);
		return {STMT_DECL, std::make_unique<Decl>(M(decl))};
	}
	else if ((expr = parse_expr(lctx, pctx)).kind != EXPR_NONE) {
		expect_token(lctx, ';');

		return {STMT_EXPR, std::make_unique<Expr>(M(expr))};
	}
	else if (match_token(lctx, '{')) {
		auto block = parse_stmts(lctx, pctx);
		expect_token(lctx, '}');

		return {STMT_BLOCK, M(block)};
	}
	else if (match_token(lctx, TOKEN_IF)) {
		expect_token(lctx, '(');
		auto cond = parse_expr(lctx, pctx);
		expect_token(lctx, ')');
		auto then_stmt = parse_stmt(lctx, pctx);

		if (match_token(lctx, TOKEN_ELSE)) {
			auto else_stmt = parse_stmt(lctx, pctx);

			auto if_stmt = Stmt::If{std::make_unique<Expr>(M(cond)), std::make_unique<Stmt>(M(then_stmt)), std::make_unique<Stmt>(M(else_stmt))};
			return {STMT_IF, M(if_stmt)};
		}
		else {
			auto if_stmt = Stmt::If{std::make_unique<Expr>(M(cond)), std::make_unique<Stmt>(M(then_stmt))};
			return {STMT_IF, M(if_stmt)};
		}
	}
	else if (match_token(lctx, TOKEN_WHILE)) {
		expect_token(lctx, '(');
		auto cond = parse_expr(lctx, pctx);
		expect_token(lctx, ')');
		auto stmt = parse_stmt(lctx, pctx);

		auto while_stmt = Stmt::While{std::make_unique<Expr>(M(cond)), std::make_unique<Stmt>(M(stmt))};
		return {STMT_WHILE, M(while_stmt)};
	}
	else if (match_token(lctx, TOKEN_RETURN)) {
		auto expr = std::make_unique<Expr>(parse_expr(lctx, pctx));
		expect_token(lctx, ';');

		return {STMT_RETURN, M(expr)};
	}
	else {
		return {STMT_NONE};
	}
}

Decl parse_decl(LexCtx &lctx, ParseCtx &pctx)
{
	TypeSpec type_spec;

	if ((type_spec = parse_type_spec(lctx, pctx)).kind != TYPESPEC_NONE) {
		expect_token(lctx, TOKEN_IDENTIFIER);
		const auto &name = std::get<std::string>(lctx.current_token.val);

		if (match_token(lctx, '(')) {
			auto params = parse_params(lctx, pctx);
			expect_token(lctx, ')');
			expect_token(lctx, '{');
			auto body = parse_stmts(lctx, pctx);
			expect_token(lctx, '}');

			return {DECL_FUNC, name, Decl::Func{type_spec, params, M(body)}};
		}
		else if (match_token(lctx, '=')) {
			auto expr = parse_expr(lctx, pctx);
			expect_token(lctx, ';');

			return {DECL_VAR, name, Decl::Var{type_spec, std::move(expr)}};
		}
		else if (match_token(lctx, ';')) {
			return {DECL_VAR, name, Decl::Var{type_spec, {EXPR_NONE}}};
		}
		else {
			unexpected_token_error(lctx);
		}
	}
	else {
		return {DECL_NONE};
	}

}