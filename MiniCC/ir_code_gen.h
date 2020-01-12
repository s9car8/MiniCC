#pragma once

#include <stdlib.h>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <variant>

typedef enum DataType {
	DATATYPE_INT,
	DATATYPE_REAL,
	DATATYPE_PTR,
} DataType;

typedef struct DataDesc {
	DataType type;
	size_t size;
} DataDesc;

typedef struct Ref {
	enum Kind {GLOBAL, FUNCTION, LOCAL, TEMPORARY, LITERAL} kind;
	// GLOBAL - index
	// FUNCTION - index
	// LOCAL - type, size
	// TEMPORARY - type, size
	// LITERAL - value
	//
	// NOTE(sergey): We going to separate variable initialization to two separate refs: Ref::LOCAL = Ref::LITERAL
	union {
		size_t index;
		DataDesc data_desc;
		std::variant<long, float> data;
	};
} Ref;

typedef enum class OpKind {
	// Binary && unary operations
	ADD, // result := arg1 + arg2
	MUL, // result := arg1 * arg2
	DIV, // result := arg1 / arg2
	NEG, // result := - arg1
	NOT, // result := !arg1

	// Copy
	COPY, // result - destination, arg1 - source

	// Unconditional && conditional jumps
	JUMP, // result - target
	IFEQ, // result - target, arg1, arg2 - 
	IFNEQ,
	IFL,
	IFLE,
	IFG,
	IFGE,

	// Procedure calls and returns
	PARAM, // arg1 - argument value
	CALL, // arg1 - function reference var, arg2 - number of params, result - functions return value var
	RET, // arg1 - return value
} OpKind;

struct Op {
	OpKind kind;
	size_t result, arg1, arg2; // ref index | IR instruction index
};

struct Function {
	DataDesc ret_data_desc;
	size_t params_number;
	std::vector<Ref> refs;
	std::vector<std::unordered_map<std::string, size_t>> scopes;
	std::unordered_map<std::string, size_t> global_scope;
	std::unordered_map<std::string, size_t> func_scope;
	std::vector<Op> ops;
};

struct MemSlot {
	enum Kind {INITIALIZED, UNINITIALIZED, READONLY} kind;
	DataDesc data_desc;
	std::variant<long, float, std::string> data;
};

struct Module {
	std::vector<Function> funcs;
	std::vector<MemSlot> globals;
	std::unordered_map<std::string, size_t> name2func;
	std::unordered_map<std::string, size_t> name2global;
};

typedef struct LexCtx LexCtx;
typedef struct ParseCtx ParseCtx;

Module build_module(LexCtx &lctx, ParseCtx &pctx);
