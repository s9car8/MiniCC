#pragma once
#include <string>
#include <variant>


enum TokenKind {
	TOKEN_FIRST = CHAR_MAX + 1,
	TOKEN_EOS = TOKEN_FIRST,

	// Keywords
	TOKEN_CHAR, // = CHAR_MAX + 1,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_WHILE,
	TOKEN_RETURN,

	// Operations
	TOKEN_PP, // ++
	TOKEN_MM, // --
	TOKEN_ARR, // ->
	TOKEN_LSH, // <<
	TOKEN_RSH, // >>
	TOKEN_LEQ, // <=
	TOKEN_GEQ, // >=
	TOKEN_EQ, // ==
	TOKEN_NEQ, // !=
	TOKEN_AND, // &&
	TOKEN_OR, // ||
	TOKEN_ADD_ASSIGN, // +=
	TOKEN_SUB_ASSIGN, // -=
	TOKEN_MUL_ASSIGN, // *=
	TOKEN_DIV_ASSIGN, // /=
	TOKEN_MOD_ASSIGN, // %=
	TOKEN_LSH_ASSIGN, // <<=
	TOKEN_RSH_ASSIGN, // >>=
	TOKEN_AND_ASSIGN, // &=
	TOKEN_XOR_ASSIGN, // ^=
	TOKEN_OR_ASSIGN, // |=

	// Literals
	TOKEN_INTEGER,
	TOKEN_REAL,
	TOKEN_CHARACTER,
	TOKEN_STRING,
	TOKEN_IDENTIFIER,

	// Special
	TOKEN_WHITESPACE,
	TOKEN_NEWLINE,
	TOKEN_OTHER,

	TOKEN_NUMBER,
};

typedef struct Token {
	unsigned kind; /* enum TokenKind || char-code */
	const char *start, *end;
	std::variant<int, float, char, std::string> val;
} Token;

typedef struct Location {
	std::string filename;
	size_t line, column;
} Location;

typedef struct LexCtx {
	const char *stream;
	Token current_token;
	Location loc;
} Context;

void unexpected_token_error(LexCtx &ctx);
bool match_token(LexCtx &ctx, unsigned expected_kind);
bool expect_token(LexCtx &ctx, unsigned expected_kind);
