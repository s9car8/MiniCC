#include <stack>
#include <set>
#include "common.h"
#include "parse.h"
#include "ir_code_gen.h"


void print_op(const Op &op)
{
#define RN(idx) idx
	switch (op.kind) {
	case OpKind::ADD:
		printf("%%%d <- ADD %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::MUL:
		printf("%%%d <- MUL %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::DIV:
		printf("%%%d <- DIV %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::NEG:
		printf("%%%d <- NEG %%%d\n", RN(op.result), RN(op.arg1));
		break;
	case OpKind::COPY:
		printf("%%%d <- %%%d\n", RN(op.result), RN(op.arg1));
		break;
	case OpKind::JUMP:
		printf("JUMP %d\n", RN(op.result));
		break;
	case OpKind::IFEQ:
		printf("JUMP %d IFEQ %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::IFNEQ:
		printf("JUMP %d IFNEQ %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::IFL:
		printf("JUMP %d IFL %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::IFLE:
		printf("JUMP %d IFLE %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::IFG:
		printf("JUMP %d IFG %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::IFGE:
		printf("JUMP %d IFGE %%%d, %%%d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::PARAM:
		printf("PARAM %%%d\n", RN(op.arg1));
		break;
	case OpKind::CALL:
		printf("%%%d <- CALL %%%d, %d\n", RN(op.result), RN(op.arg1), RN(op.arg2));
		break;
	case OpKind::RET:
		printf("RET %%%d\n", RN(op.arg1));
		break;
	}
#undef RN
}

void print_func_ops(const Function &func)
{
	printf("%s:%d\n", "function", func.params_number);
	for (size_t i = 0; i < func.ops.size(); ++i) {
		printf("%d: ", i);
		print_op(func.ops[i]);
	}
}

static DataDesc infer_type(TypeSpec type_spec)
{
	switch (type_spec.kind) {
	case TYPESPEC_INT:
		return {DATATYPE_INT, 4};
	case TYPESPEC_CHAR:
		return {DATATYPE_INT, 1};
	case TYPESPEC_FLOAT:
		return {DATATYPE_REAL, 4};
	}
}

static DataDesc common_type(DataDesc data_desc1, DataDesc data_desc2)
{	
	if (data_desc1.type == data_desc2.type) {
		return {data_desc1.type, MAX(data_desc1.size, data_desc2.size)};
	}
	else if (data_desc1.type > data_desc2.type) {
		return {data_desc1.type, data_desc1.size};
	}
	else {
		return {data_desc2.type, data_desc2.size};
	}
}

static size_t create_global_ref(const Module &module, Function &func, const std::string &name)
{
	size_t ref_index = func.refs.size();
	auto it = module.name2global.find(name);
	assert(it != std::end(module.name2global));
	func.refs.push_back({Ref::GLOBAL, (size_t)it->second});
	func.global_scope.emplace(name, ref_index);
	return ref_index;
}

static size_t create_func_ref(const Module &module, Function &func, const std::string &name)
{
	size_t ref_index = func.refs.size();
	auto it = module.name2func.find(name);
	assert(it != std::end(module.name2func));
	func.refs.push_back({Ref::FUNCTION, it->second});
	func.func_scope.emplace(name, ref_index);
	return ref_index;
}

static size_t create_local_ref(Function &func, const std::string &name, DataDesc data_desc)
{
	Ref ref = {Ref::LOCAL};
	ref.data_desc = data_desc;

	size_t ref_index = func.refs.size();
	func.refs.push_back(ref);
	func.scopes.back().emplace(name, ref_index);
	return ref_index;
}

static size_t create_tmp_ref(Function &func, DataDesc data_desc)
{
	Ref ref = {Ref::TEMPORARY};
	ref.data_desc = data_desc;

	size_t ref_index = func.refs.size();
	func.refs.push_back(ref);
	return ref_index;
}

static size_t create_literal_ref(const Module &module, Function &func, std::variant<long, float, std::string> data)
{
	size_t ref_index = func.refs.size();
	if (std::holds_alternative<std::string>(data)) {
		// module.
	}
	else {
		Ref ref = {Ref::LITERAL};
		ref.data = std::get<long>(data);
		func.refs.push_back(ref);
	}
	return ref_index;
}

static size_t lookup_ref_for_var(const Module &module, Function &func, const std::string &name)
{
	for (auto scope_it = std::crbegin(func.scopes); scope_it != std::crend(func.scopes); ++scope_it) {
		auto it = scope_it->find(name);

		if (it != std::end(*scope_it)) {
			return it->second;
		}
	}

	auto it = func.global_scope.find(name);

	if (it != std::end(func.global_scope)) {
		return it->second;
	}

	return create_global_ref(module, func, name);
}

static size_t lookup_ref_for_func(const Module &module, Function &func, const std::string &name)
{
	auto it = func.func_scope.find(name);

	if (it != std::end(func.func_scope)) {
		return it->second;
	}

	return create_func_ref(module, func, name);
}

static const Function &lookup_func(const Module &module, const std::string &name)
{
	auto it = module.name2func.find(name);
	assert(it != std::end(module.name2func));
	return module.funcs[it->second];
}

typedef struct {
	size_t index;
	DataDesc data_desc;
} RefInfo;

static RefInfo build_func_expr_and_return_ref_info(const Module &module, Function &func, const Expr &expr)
{
	switch (expr.kind) {
	case EXPR_IDENTIFIER: {
		const auto &name = std::get<std::string>(expr.data);
		size_t ref_idx = lookup_ref_for_var(module, func, name);
		return {ref_idx, func.refs[ref_idx].data_desc};
	}
	case EXPR_INTEGER: {
		size_t ref_idx = create_literal_ref(module, func, (long)std::get<int>(expr.data));
		return {ref_idx, {DATATYPE_INT, 4}};
	}
	case EXPR_REAL: {
		size_t ref_idx = create_literal_ref(module, func, (float)std::get<float>(expr.data));
		return {ref_idx, {DATATYPE_REAL, 4}};
	}
	case EXPR_CHARACTER: {
		size_t ref_idx = create_literal_ref(module, func, (long)std::get<char>(expr.data));
		return {ref_idx, {DATATYPE_INT, 1}};
	}
	case EXPR_CALL: {
		const auto &call_expr = std::get<Expr::Call>(expr.data);
		for (const auto &expr : call_expr.args) {
			size_t param_ref_idx = build_func_expr_and_return_ref_info(module, func, expr).index;
			func.ops.push_back({OpKind::PARAM, 0, param_ref_idx});
		}
		size_t func_ref_idx = lookup_ref_for_func(module, func, call_expr.fname);
		auto data_desc = lookup_func(module, call_expr.fname).ret_data_desc;
		size_t func_res_ref_idx = create_tmp_ref(func, data_desc);
		func.ops.push_back({OpKind::CALL, func_res_ref_idx, func_ref_idx, (size_t)call_expr.args.size()});
		return {func_res_ref_idx, data_desc};
	}
	case EXPR_MUL: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		auto data_desc = common_type(arg1_ref_info.data_desc, arg2_ref_info.data_desc);
		size_t mul_res_ref_idx = create_tmp_ref(func, data_desc);
		func.ops.push_back({OpKind::MUL, mul_res_ref_idx, arg1_ref_info.index, arg2_ref_info.index});
		return {mul_res_ref_idx, data_desc};
	}
	case EXPR_DIV: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		auto data_desc = common_type(arg1_ref_info.data_desc, arg2_ref_info.data_desc);
		size_t div_res_ref_idx = create_tmp_ref(func, data_desc);
		func.ops.push_back({OpKind::DIV, div_res_ref_idx, arg1_ref_info.index, arg2_ref_info.index});
		return {div_res_ref_idx, data_desc};
	}
	case EXPR_ADD: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		auto data_desc = common_type(arg1_ref_info.data_desc, arg2_ref_info.data_desc);
		size_t add_res_ref_idx = create_tmp_ref(func, data_desc);
		func.ops.push_back({OpKind::ADD, add_res_ref_idx, arg1_ref_info.index, arg2_ref_info.index});
		return {add_res_ref_idx, data_desc};
	}
	case EXPR_SUB: {
		// NOTE(sergey): For now we represent subtraction as a combination of negation and addition.
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		const auto &neg_data_desc = arg2_ref_info.data_desc;
		size_t neg_res_ref_idx = create_tmp_ref(func, neg_data_desc);
		func.ops.push_back({OpKind::NEG, neg_res_ref_idx, arg2_ref_info.index});
		auto add_data_desc = common_type(arg1_ref_info.data_desc, neg_data_desc);
		size_t sub_res_ref_idx = create_tmp_ref(func, add_data_desc);
		func.ops.push_back({OpKind::ADD, sub_res_ref_idx, arg1_ref_info.index, neg_res_ref_idx});
		return {sub_res_ref_idx, add_data_desc};
	}
	case EXPR_EQUAL:
	case EXPR_NOT_EQUAL:
	case EXPR_LESS:
	case EXPR_LESS_EQUAL:
	case EXPR_GREAT:
	case EXPR_GREAT_EQUAL:
		break;
	}
}

static void build_func_logical(const Module &module, Function &func, const Expr &expr, bool jump_on_false, std::vector<size_t> &unresolved_jumps)
{
	switch (expr.kind) {
	case EXPR_LESS: {
		auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		unresolved_jumps.push_back(func.ops.size());
		if (jump_on_false)
			func.ops.push_back({OpKind::IFGE, 0, arg1_ref_info.index, arg2_ref_info.index});
		else
			func.ops.push_back({OpKind::IFL, 0, arg1_ref_info.index, arg2_ref_info.index});
		break;
	}
	case EXPR_GREAT_EQUAL: {
		auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		unresolved_jumps.push_back(func.ops.size());
		if (jump_on_false)
			func.ops.push_back({OpKind::IFL, 0, arg1_ref_info.index, arg2_ref_info.index});
		else
			func.ops.push_back({OpKind::IFGE, 0, arg1_ref_info.index, arg2_ref_info.index});
		break;
	}
	case EXPR_EQUAL: {
		auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		unresolved_jumps.push_back(func.ops.size());
		if (jump_on_false)
			func.ops.push_back({OpKind::IFNEQ, 0, arg1_ref_info.index, arg2_ref_info.index});
		else
			func.ops.push_back({OpKind::IFEQ, 0, arg1_ref_info.index, arg2_ref_info.index});
		break;
	}
	case EXPR_NOT_EQUAL: {
		auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto arg1_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr1);
		auto arg2_ref_info = build_func_expr_and_return_ref_info(module, func, *bin_expr.expr2);
		unresolved_jumps.push_back(func.ops.size());
		if (jump_on_false)
			func.ops.push_back({OpKind::IFEQ, 0, arg1_ref_info.index, arg2_ref_info.index});
		else
			func.ops.push_back({OpKind::IFNEQ, 0, arg1_ref_info.index, arg2_ref_info.index});
		break;
	}
	default: /* same as for {EXPR_NOT_EQUAL, expr1, 0} */ {
		auto expr_ref_info = build_func_expr_and_return_ref_info(module, func, expr);
		auto zero_ref_idx = create_literal_ref(module, func, 0L);
		unresolved_jumps.push_back(func.ops.size());
		if (jump_on_false)
			func.ops.push_back({OpKind::IFEQ, 0, expr_ref_info .index, zero_ref_idx});
		else
			func.ops.push_back({OpKind::IFNEQ, 0, expr_ref_info .index, zero_ref_idx});
		break;
	}
	case EXPR_LOGICAL_AND: {
		std::vector<size_t> unresolved;
		auto &bin_expr = std::get<Expr::Binary>(expr.data);
		if (jump_on_false) {
			build_func_logical(module, func, *bin_expr.expr1, true, unresolved_jumps);
			build_func_logical(module, func, *bin_expr.expr2, true, unresolved_jumps);
		}
		else {
			build_func_logical(module, func, *bin_expr.expr1, true, unresolved);
			build_func_logical(module, func, *bin_expr.expr2, false, unresolved_jumps);
		}
		for (size_t op_idx : unresolved) func.ops[op_idx].result = func.ops.size();
		break;
	}
	case EXPR_LOGICAL_OR: {
		std::vector<size_t> unresolved;
		auto &bin_expr = std::get<Expr::Binary>(expr.data);
		if (jump_on_false) {
			build_func_logical(module, func, *bin_expr.expr1, false, unresolved);
			build_func_logical(module, func, *bin_expr.expr2, true, unresolved_jumps);
		}
		else {
			build_func_logical(module, func, *bin_expr.expr1, false, unresolved_jumps);
			build_func_logical(module, func, *bin_expr.expr2, false, unresolved_jumps);
		}
		for (size_t op_idx : unresolved) func.ops[op_idx].result = func.ops.size();
		break;
	}
	}
}

static void build_func_block(const Module &module, Function &func, const std::vector<Stmt> &block);

static void build_func_stmt(const Module &module, Function &func, const Stmt &stmt)
{
	switch (stmt.kind) {
	case STMT_DECL: {
		const auto &decl = *std::get<Stmt::Decl>(stmt.data);
		assert_msg(decl.kind == DECL_VAR, "Only variable declaration are allowed inside function.\n");
		const auto &var_decl = std::get<Decl::Var>(decl.data);
		auto var_ref_idx = create_local_ref(func, decl.name, infer_type(var_decl.type_spec));
		if (var_decl.init_expr.kind != EXPR_NONE) {
			auto init_expr_ref_info = build_func_expr_and_return_ref_info(module, func, var_decl.init_expr);
			func.ops.push_back({OpKind::COPY, var_ref_idx, init_expr_ref_info.index});
		}
		break;
	}
	case STMT_EXPR: {
		const auto &expr = *std::get<Stmt::Expr>(stmt.data);
		build_func_expr_and_return_ref_info(module, func, expr);
		break;
	}
	case STMT_BLOCK: {
		const auto &block = std::get<Stmt::Block>(stmt.data);
		build_func_block(module, func, block);
		break;
	}
	case STMT_IF: {
		const auto &if_stmt = std::get<Stmt::If>(stmt.data);
		std::vector<size_t> unresolved_jumps;
		build_func_logical(module, func, *if_stmt.cond, true, unresolved_jumps);
		build_func_stmt(module, func, *if_stmt.then_stmt);
		for (auto op_idx : unresolved_jumps) func.ops[op_idx].result = func.ops.size();
		build_func_stmt(module, func, *if_stmt.else_stmt);
		break;
	}
	case STMT_WHILE: {
		const auto &while_stmt = std::get<Stmt::While>(stmt.data);
		std::vector<size_t> unresolved_jumps;
		size_t check_op_idx = func.ops.size();
		build_func_logical(module, func, *while_stmt.cond, true, unresolved_jumps);
		build_func_stmt(module, func, *while_stmt.stmt);
		func.ops.push_back({OpKind::JUMP, check_op_idx});
		for (size_t op_idx : unresolved_jumps) func.ops[op_idx].result = func.ops.size();
		break;
	}
	case STMT_RETURN: {
		const auto &expr = *std::get<Stmt::Expr>(stmt.data);
		auto ref_info = build_func_expr_and_return_ref_info(module, func, expr);
		func.ops.push_back({OpKind::RET, 0, ref_info.index});
		break;
	}
	}
}

static void build_func_block(const Module &module, Function &func, const std::vector<Stmt> &block)
{
	func.scopes.emplace_back();
	for (const auto &stmt : block) {
		build_func_stmt(module, func, stmt);
	}
	func.scopes.pop_back();
}

static void build_func(Module &module, const std::string &name, 
	const TypeSpec &ret_type, const std::vector<Param> &params, const std::vector<Stmt> &body)
{
	Function func;

	func.ret_data_desc = infer_type(ret_type);
	func.scopes.emplace_back();
	func.params_number = params.size();
	for (auto &p : params) {
		create_local_ref(func, p.name, infer_type(p.type_spec));
	}
	for (auto &stmt : body) {
		build_func_stmt(module, func, stmt);
	}
	func.scopes.pop_back();
	//build_func_basic_blocks(func);
	print_func_ops(func);
	module.name2func.emplace(name, module.funcs.size());
	module.funcs.push_back(func);
}

static auto eval_global_expr(const Module &module, const Expr &expr)
	-> std::variant<long, float, std::string>
{
	switch (expr.kind) {
	case EXPR_IDENTIFIER: {
		auto it = module.name2global.find(std::get<std::string>(expr.data));
		if (it == std::end(module.name2global)) {
			panic("Undefined variable");
		}
		return module.globals[it->second].data;
	}
	case EXPR_INTEGER:
		return (long)std::get<int>(expr.data);
	case EXPR_REAL:
		return std::get<float>(expr.data);
	case EXPR_CHARACTER:
		return (long)std::get<int>(expr.data);
	case EXPR_MUL: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto x = eval_global_expr(module, *bin_expr.expr1);
		auto y = eval_global_expr(module, *bin_expr.expr2);
		assert_msg(x.index() == y.index(), "Different subexpression types.");
		assert(std::holds_alternative<long>(x));
		return std::get<long>(x) * std::get<long>(y);
	}
	case EXPR_DIV: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto x = eval_global_expr(module, *bin_expr.expr1);
		auto y = eval_global_expr(module, *bin_expr.expr2);
		assert_msg(x.index() == y.index(), "Different subexpression types.");
		assert(std::holds_alternative<long>(x));
		return std::get<long>(x) / std::get<long>(y);
	}
	case EXPR_ADD: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto x = eval_global_expr(module, *bin_expr.expr1);
		auto y = eval_global_expr(module, *bin_expr.expr2);
		assert_msg(x.index() == y.index(), "Different subexpression types.");
		assert(std::holds_alternative<long>(x));
		return std::get<long>(x) + std::get<long>(y);
	}
	case EXPR_SUB: {
		const auto &bin_expr = std::get<Expr::Binary>(expr.data);
		auto x = eval_global_expr(module, *bin_expr.expr1);
		auto y = eval_global_expr(module, *bin_expr.expr2);
		assert_msg(x.index() == y.index(), "Different subexpression types.");
		assert(std::holds_alternative<long>(x));
		return std::get<long>(x) - std::get<long>(y);
	}
	default:
		panic("Too complicated expression!\n");
	}
}

static void build_global_var(Module &module, const std::string &name, const TypeSpec &type_spec, const Expr &init_expr)
{
	auto data_desc = infer_type(type_spec);

	module.name2global.emplace(name, module.globals.size());
	if (init_expr.kind == EXPR_NONE) {
		module.globals.push_back(MemSlot{MemSlot::UNINITIALIZED, data_desc});
	}
	else {
		if (auto &&data = eval_global_expr(module, init_expr); std::holds_alternative<long>(data)) {
			module.globals.push_back(MemSlot{MemSlot::INITIALIZED, data_desc, std::get<long>(data)});
		}
	}
}

Module build_module(LexCtx &lctx, ParseCtx &pctx)
{
	Module module;

	while (true) {
		Decl decl = parse_decl(lctx, pctx);
		switch (decl.kind) {
		case DECL_VAR: {
			const auto &var_decl = std::get<Decl::Var>(decl.data);
			build_global_var(module, decl.name, var_decl.type_spec, var_decl.init_expr);
			break;
		}
		case DECL_FUNC: {
			const auto &func_decl = std::get<Decl::Func>(decl.data);
			build_func(module, decl.name, func_decl.ret_type, func_decl.params, func_decl.body);
			break;
		}
		}
	}
	return module;
}