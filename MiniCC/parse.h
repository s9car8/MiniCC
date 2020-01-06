#pragma once
#include <vector>
#include <string>
#include <variant>
#include <memory>

typedef struct ParseCtx ParseCtx;

enum TypeSpecKind {
	TYPESPEC_NONE,
	TYPESPEC_INT,
	TYPESPEC_FLOAT,
	TYPESPEC_CHAR,
};

typedef struct TypeSpec {
	enum TypeSpecKind kind;
} TypeSpec;

typedef struct Param {
	TypeSpec type_spec;
	std::string name;
} Param;

enum ExprKind {
	EXPR_NONE,

	// Base
	EXPR_IDENTIFIER,
	EXPR_INTEGER,
	EXPR_REAL,
	EXPR_CHARACTER,
	EXPR_CALL,

	// Mul
	EXPR_MUL,
	EXPR_DIV,

	// Add
	EXPR_ADD,
	EXPR_SUB,

	// Cmp relational
	EXPR_LESS,
	EXPR_LESS_EQUAL,
	EXPR_GREAT,
	EXPR_GREAT_EQUAL,

	// Cmp equality
	EXPR_EQUAL,
	EXPR_NOT_EQUAL,

	// Logical and
	EXPR_LOGICAL_AND,

	// Logical or
	EXPR_LOGICAL_OR
};

struct Expr {
	struct Binary {
		std::unique_ptr<Expr> expr1, expr2;
	};
	struct Call {
		std::string fname;
		std::vector<Expr> args;
	};

	enum ExprKind kind;
	std::variant<int, float, char, std::string, Binary, Call> data;
};

typedef struct Decl Decl;

struct Stmt {
	struct If {
		std::unique_ptr<Expr> cond;
		std::unique_ptr<Stmt> then_stmt;
		std::unique_ptr<Stmt> else_stmt;
	};
	struct While {
		std::unique_ptr<Expr> cond;
		std::unique_ptr<Stmt> stmt;
	};
	typedef std::unique_ptr<Decl> Decl;
	typedef std::unique_ptr<Expr> Expr;
	typedef std::vector<Stmt> Block;

	enum StmtKind kind;
	std::variant<Decl, Expr, If, While, Block> data;
};

enum DeclKind {
	DECL_NONE,
	DECL_VAR,
	DECL_FUNC,
};

struct Decl {
	struct Func {
		TypeSpec ret_type;
		std::vector<Param> params;
		std::vector<Stmt> body;
	};
	struct Var {
		TypeSpec type_spec;
		Expr init_expr;
	};

	enum DeclKind kind;
	std::string name;
	std::variant<Func, Var> data;
};

typedef struct ParseCtx {
	// It should contain symbol table to be able make parser's decisions based on symbol's related information.
} ParseCtx;

typedef struct LexCtx LexCtx;

Decl parse_decl(LexCtx &lctx, ParseCtx &pctx);
