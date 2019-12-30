#include <assert.h>
#include <variant>
#include "lexer.h"


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
