#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include <variant>
#include <memory>
#include "regex.h"


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

typedef struct Context {
	const char *stream;
	Token current_token;
	Location loc;
} Context;

static int scan_integer(const char *str, const char *end)
{
	static int char2digit[CHAR_MAX+1];
	static bool inited;

	if (!inited) {
		inited = true;
		char2digit['0'] = 0;
		char2digit['1'] = 1;
		char2digit['2'] = 2;
		char2digit['3'] = 3;
		char2digit['4'] = 4;
		char2digit['5'] = 5;
		char2digit['6'] = 6;
		char2digit['7'] = 7;
		char2digit['8'] = 8;
		char2digit['9'] = 9;
		char2digit['A'] = 0xA;
		char2digit['B'] = 0xB;
		char2digit['C'] = 0xC;
		char2digit['D'] = 0xD;
		char2digit['E'] = 0xE;
		char2digit['F'] = 0xF;
		char2digit['a'] = 0xa;
		char2digit['b'] = 0xb;
		char2digit['c'] = 0xc;
		char2digit['d'] = 0xd;
		char2digit['e'] = 0xe;
		char2digit['f'] = 0xf;
	}

	int sign = 1;

	switch (*str) {
	case '-': sign = -1;
	case '+': ++str;
	}

	int base = 10;

	if (*str == '0') {
		++str;
		switch (*str) {
		case 'x': case 'X': ++str; base = 16; break;
		case 'b': case 'B': ++str; base = 2; break;
		default: base = 8; break;
		}
	}

	int result = 0;

	while (str != end) {
		result = result * base + char2digit[*str];
		++str;
	}
	return sign * result;
}

static float scan_float(const char *str, const char *end)
{
	auto str_copy_ptr = std::make_unique<char>(str - end + 1);
	strcpy(str_copy_ptr.get(), str);
	return (float) atof(str_copy_ptr.get());
}

static inline char special_char(char ch)
{
	switch (ch)	{
	case '\'': return 0x27;
	case '\"': return 0x22;
	case '\?': return 0x3f;
	case '\\': return 0x5c;
	case 'a': return 0x07;
	case 'b': return 0x08;
	case 'f': return 0x0c;
	case 'n': return 0x0a;
	case 'r': return 0x0d;
	case 't': return 0x09;
	case 'v': return 0x0b;
	}
}

static char scan_char(const char *begin, const char *end)
{
	if (*begin != '\\')
		return *begin;

	return special_char(*++begin);
}

static std::string scan_string(const char *begin, const char *end)
{
	std::string str;

	assert(end - begin >= 2);
	// Remove quotes
	++begin;
	--end;

	while (begin != end) {
		if (*begin != '\\')
			str += *begin;
		str += special_char(*++begin);
		++begin;
	}
	return str;
}

void print_token(const Token &token)
{
	static const char *token_name[TOKEN_NUMBER];
	static bool inited;

#define TOKEN_NAME(token) token_name[token] = #token
	if (!inited) {
		inited = true;
		TOKEN_NAME(TOKEN_EOS);
		TOKEN_NAME(TOKEN_CHAR);
		TOKEN_NAME(TOKEN_INT);
		TOKEN_NAME(TOKEN_FLOAT);
		TOKEN_NAME(TOKEN_IF);
		TOKEN_NAME(TOKEN_ELSE);
		TOKEN_NAME(TOKEN_WHILE);
		TOKEN_NAME(TOKEN_RETURN);
		TOKEN_NAME(TOKEN_PP);
		TOKEN_NAME(TOKEN_MM);
		TOKEN_NAME(TOKEN_ARR);
		TOKEN_NAME(TOKEN_LSH);
		TOKEN_NAME(TOKEN_RSH);
		TOKEN_NAME(TOKEN_LEQ);
		TOKEN_NAME(TOKEN_GEQ);
		TOKEN_NAME(TOKEN_EQ);
		TOKEN_NAME(TOKEN_NEQ);
		TOKEN_NAME(TOKEN_AND);
		TOKEN_NAME(TOKEN_OR);
		TOKEN_NAME(TOKEN_ADD_ASSIGN);
		TOKEN_NAME(TOKEN_SUB_ASSIGN);
		TOKEN_NAME(TOKEN_MUL_ASSIGN);
		TOKEN_NAME(TOKEN_DIV_ASSIGN);
		TOKEN_NAME(TOKEN_MOD_ASSIGN);
		TOKEN_NAME(TOKEN_LSH_ASSIGN);
		TOKEN_NAME(TOKEN_RSH_ASSIGN);
		TOKEN_NAME(TOKEN_AND_ASSIGN);
		TOKEN_NAME(TOKEN_XOR_ASSIGN);
		TOKEN_NAME(TOKEN_OR_ASSIGN);
		TOKEN_NAME(TOKEN_INTEGER);
		TOKEN_NAME(TOKEN_REAL);
		TOKEN_NAME(TOKEN_CHARACTER);
		TOKEN_NAME(TOKEN_STRING);
		TOKEN_NAME(TOKEN_IDENTIFIER);
		TOKEN_NAME(TOKEN_WHITESPACE);
		TOKEN_NAME(TOKEN_NEWLINE);
	}
#undef TOKEN_NAME
	
	if (token.kind < TOKEN_FIRST) {
		printf("<%c", (char)token.kind);
	}
	else {
		printf("<%s", token_name[token.kind]);
	}
	switch (token.kind) {
	case TOKEN_INTEGER:
		printf(", %d>", std::get<int>(token.val));
		break;
	case TOKEN_STRING:
	case TOKEN_IDENTIFIER:
		printf(", %s>", std::get<std::string>(token.val).c_str());
		break;
	default:
		printf(">");
	}
}

Token next_token(Context &ctx)
{
	// SSYM1 = [ab\d]; SSYM2 = c
	// ASTERIX = SSYM1 ? ; SSYM1 + ; SSYM1 *
	// CAT = ASTERIX1 ASTERIX2
	// OR = CAT1 | CAT2

	static std::pair<const char *, unsigned> regex2token_mapping[] = {
		//{"([1-9]b|c)*", 5},
		//{"a", 7},

		// List keywords
		{"char", TOKEN_CHAR},
		{"int", TOKEN_INT},
		{"float", TOKEN_FLOAT},
		{"if", TOKEN_IF},
		{"else", TOKEN_ELSE},
		{"while", TOKEN_WHILE},
		{"return", TOKEN_RETURN},

		// List operators
		{"\\+\\+", TOKEN_PP},
		{"--", TOKEN_MM},
		{"->", TOKEN_ARR},
		{"<<", TOKEN_LSH},
		{">>", TOKEN_RSH},
		{"<=", TOKEN_LEQ},
		{">=", TOKEN_GEQ},
		{"==", TOKEN_EQ},
		{"!=", TOKEN_NEQ},
		{"&&", TOKEN_AND},
		{"\\|\\|", TOKEN_OR},
		{"\\+=", TOKEN_ADD_ASSIGN},
		{"-=", TOKEN_SUB_ASSIGN},
		{"\\*=", TOKEN_MUL_ASSIGN},
		{"/=", TOKEN_DIV_ASSIGN},
		{"%=", TOKEN_MOD_ASSIGN},
		{"<<=", TOKEN_LSH_ASSIGN},
		{">>=", TOKEN_RSH_ASSIGN},
		{"&=", TOKEN_AND_ASSIGN},
		{"\\^=", TOKEN_XOR_ASSIGN},
		{"\\|=", TOKEN_OR_ASSIGN},

		// List literals
		{"[+-]?([0-9]+|0[0-7]+|0[bB][0-1]+|0[xX][a-fA-F0-9]+)", TOKEN_INTEGER},
		{"[+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([+-]?[eE][0-9]+)?", TOKEN_REAL},
		{"'\\?\\w'", TOKEN_CHARACTER},
		{"\"\\S\"", TOKEN_STRING},
		{"[_a-zA-Z][_a-zA-Z0-9]*", TOKEN_IDENTIFIER},

		// List special
		{"[\t ]+", TOKEN_WHITESPACE},
		{"\n", TOKEN_NEWLINE},
		{".", TOKEN_OTHER},
	};
	static regex::RegexClassifier regex_classifier(
		std::begin(regex2token_mapping), std::end(regex2token_mapping));

	Token &token = ctx.current_token;

repeat:
	ctx.loc.column += token.end - token.start;
	token.start = ctx.stream;
	auto classification_res = regex_classifier.classify(ctx.stream);
	auto cls = (*ctx.stream) ? classification_res.first : TOKEN_EOS;
	assert(cls >= 0);
	token.kind = cls;
	token.end = classification_res.second;

	switch (token.kind) {
	case TOKEN_INTEGER:
		token.val.emplace<int>(scan_integer(token.start, token.end));
		break;
	case TOKEN_REAL:
		token.val.emplace<float>(scan_float(token.start, token.end));
		break;
	case TOKEN_CHARACTER:
		token.val.emplace<char>(scan_char(token.start, token.end));
		break;
	case TOKEN_STRING:
		token.val.emplace<std::string>(scan_string(token.start, token.end));
		break;
	case TOKEN_IDENTIFIER:
		token.val.emplace<std::string>(token.start, token.end);
		break;
	case TOKEN_WHITESPACE:
		ctx.stream = token.end;
		goto repeat;
		break;
	case TOKEN_NEWLINE:
		ctx.loc.column = 0;
		++ctx.loc.line;
		ctx.stream = token.end;
		goto repeat;
		break;
	case TOKEN_OTHER:
		token.kind = *token.start;
		break;
	default:
		break;
	}
	return token;
}

void unexpected_token_error(Context &ctx)
{
	printf("<filename>:%d:%d: Unexpected token ", ctx.loc.line, ctx.loc.column);
	print_token(ctx.current_token);
	printf("\n");
	exit(1);
}

bool match_token(Context &ctx, unsigned expected_kind)
{
	next_token(ctx);
	if (ctx.current_token.kind == expected_kind) {
		ctx.stream = ctx.current_token.end;
		printf("Matched: ");
		print_token(ctx.current_token);
		printf("\n");
	}
	return ctx.current_token.kind == expected_kind;
}

bool expect_token(Context &ctx, unsigned expected_kind)
{
	if (match_token(ctx, expected_kind)) {
		return true;
	}
	else {
		printf("Unexpected token: ");
		print_token(ctx.current_token);
		printf("\n");
		return false;
	}
}

#define assert_token(ctx, kind) assert(match_token(ctx, kind));
#define assert_token_int(ctx, value) assert(match_token(ctx, TOKEN_INTEGER) && std::get<int>(ctx.current_token.val) == value)

void test_tokenizer()
{
	{
		Context ctx = {"char float if else while return"};
		//printf("Expected: %d; Got: %d\n", TOKEN_CHAR, next_token(&ctx).kind);
		assert_token(ctx, TOKEN_CHAR);
		assert_token(ctx, TOKEN_FLOAT);
		assert_token(ctx, TOKEN_IF);
		assert_token(ctx, TOKEN_ELSE);
		assert_token(ctx, TOKEN_WHILE);
		assert_token(ctx, TOKEN_RETURN);
	}

	{
		Context ctx = {"123\t0b101\n0xAbCdEf"};
		assert_token_int(ctx, 123);
		assert_token_int(ctx, 5);
		assert_token_int(ctx, 0xAbCdEf);
	}
}

int main()
{
	test_tokenizer();
	return 0;
}
