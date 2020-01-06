#define  _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include "lexer.h"
#include "parse.h"


#define assert_token(ctx, kind) assert(match_token(ctx, kind));
#define assert_token_int(ctx, value) assert(match_token(ctx, TOKEN_INTEGER) && std::get<int>(ctx.current_token.val) == value)

void test_tokenizer()
{
	{
		LexCtx ctx = {"char float if else while return"};
		//printf("Expected: %d; Got: %d\n", TOKEN_CHAR, next_token(&ctx).kind);
		assert_token(ctx, TOKEN_CHAR);
		assert_token(ctx, TOKEN_FLOAT);
		assert_token(ctx, TOKEN_IF);
		assert_token(ctx, TOKEN_ELSE);
		assert_token(ctx, TOKEN_WHILE);
		assert_token(ctx, TOKEN_RETURN);
	}

	{
		LexCtx ctx = {"123\t0b101\n0xAbCdEf"};
		assert_token_int(ctx, 123);
		assert_token_int(ctx, 5);
		assert_token_int(ctx, 0xAbCdEf);
	}
}

void test_parser()
{
	{
		LexCtx lctx = {"int add(int a, int b) { return a + b; }"};
		ParseCtx pctx = {};
		auto decl = parse_decl(lctx, pctx);
		printf("kind: %d\n", decl.kind);
		for (auto& stmt : std::get<Decl::Func>(decl.data).body) {
			printf("stmt.kind: %d\n", stmt.kind);
		}
	}
}

void run_tests()
{
	test_tokenizer();
	printf("\n");
	test_parser();
}

int main()
{
	run_tests();

	FILE *f = fopen("test/test1.c", "r");
	fseek(f, 0, SEEK_END);
	size_t source_length = ftell(f);
	char *buffer = (char*)malloc(source_length + 1);
	fseek(f, 0, SEEK_SET);
	source_length = fread(buffer, 1, source_length, f);
	buffer[source_length] = '\0';

	LexCtx lctx = {buffer, {"test/test1.c"}};
	ParseCtx pctx = {};
	while (parse_decl(lctx, pctx).kind != DECL_NONE);
	//ModuleInfo module_info = build_module(lctx, pctx);

	return 0;
}
