#include <stddef.h>
#include <limits.h>
#include <utility>
#include <array>
#include <stack>
#include <iterator>
#include <assert.h>
#include <iostream>


struct RegexClassifier {
	static const unsigned ALPHABET_POWER = CHAR_MAX + 1;

	unsigned* table[ALPHABET_POWER];

	template <typename It>
	RegexClassifier(It begin, It end)
	{
		compile(begin, end);
	}

	template <typename It>
	void compile(It begin, It end)
	{
		// 
		// 
		// 
		// 
		// 

		unsigned char state_index = 0;

		for (It it = begin; it != end; ++it)
		{
			auto regex_ptr = it->first;
			auto cls = it->second;

			// --RULES--
			// OR -> OR '|' CAT
			// OR -> CAT
			// CAT -> CAT ASTERIX
			// CAT -> ASTERIX
			// ASTERIX -> SSET '*'
			// ASTERIX -> SSET '+'
			// ASTERIX -> SSET '?'
			// ASTERIX -> SSET
			// SSET -> '(' OR ')'
			// SSET -> '[' SYMS ']'
			// SSET -> SYM
			// SYMS -> SYMS SYM
			// SYMS -> SYM
			// SYM -> '\' sym
			// SYM -> sym

			// --RULES--					--CHOICE--
			// OR -> CAT OR1				{(,[,\,other,*,+,?}
			// OR1 -> '|' CAT OR1			{|}
			// OR1 -> e						{),-|}
			// CAT -> ASTERIX CAT1			{(,[,\,other,*,+,?}
			// CAT1 -> ASTERIX CAT1			{(,[,\,other,*,+,?}
			// CAT1 -> e					{|,),-|}
			// ASTERIX -> SSET ASTERIX1		{(,[,\,other}
			// ASTERIX1 -> '*'				{*}
			// ASTERIX1 -> '+'				{+}
			// ASTERIX1 -> '?'				{?}
			// ASTERIX1 -> e				{(,[,\,other,|,),-|}
			// SSET -> '(' OR ')'			{(}
			// SSET -> '[' SYMS ']'			{[}
			// SSET -> SYM					{\,other}
			// SYMS -> SYM SYMS1			{\,other}
			// SYMS1 -> SYM SYMS1			{\,other}
			// SYMS1 -> e					{]}
			// SYM -> '\' sym				{\}
			// SYM -> sym					{other}
			// SYM -> sym '-' sym

			enum {OR = 0*CHAR_MAX + 1, OR1, CAT, CAT1, ASTERIX, ASTERIX1, SSET, SYMS, SYMS1, SYM};

			std::stack<unsigned> stack;
			stack.push(OR);

			auto replace = [&](const std::initializer_list<unsigned>& seq, bool do_shift = false) mutable {
				stack.pop();
				for (auto x : seq)
					stack.push(x);
				if (do_shift)
					++regex_ptr;
			};

			while (!stack.empty()) {
				auto m = stack.top();

				switch (m) {
				case OR:
					switch (*regex_ptr) {
					case '(': case '[': default: case '*': case '+': case '?': replace({ OR1, CAT }); break;
					case '\0': stack.pop(); break;
					}
					break;
				case OR1:
					switch (*regex_ptr) {
					case '|': replace({ OR1, CAT }, true); break;
					case ')': case '\0': stack.pop(); break;
					}
					break;
				case CAT:
					switch (*regex_ptr) {
					case '(': case '[': case '\\': default: case '*': case '+': case '?': replace({ CAT1, ASTERIX }); break;
					}
					break;
				case CAT1:
					switch (*regex_ptr) {
					case '(': case '[': case '\\': default: case '*': case '+': case '?': replace({ CAT1, ASTERIX }); break;
					case '|': case ')': case '\0': stack.pop(); break;
					}
					break;
				case ASTERIX:
					switch (*regex_ptr) {
					case '(': case '[': case '\\': default: replace({ ASTERIX1, SSET }); break;
					}
					break;
				case ASTERIX1:
					switch (*regex_ptr) {
					case '*': stack.pop(); ++regex_ptr; break;
					case '+': stack.pop(); ++regex_ptr; break;
					case '?': stack.pop(); ++regex_ptr; break;
					case '(': case '[': case '\\': default: case '|': case ')': case '\0': stack.pop(); break;
					}
					break;
				case SSET:
					switch (*regex_ptr) {
					case '(': replace({ ')', OR }, true); break;
					case '[': replace({ ']', SYMS }, true); break;
					case '\\': default: replace({ SYM }); break;
					}
					break;
				case SYMS:
					switch (*regex_ptr) {
					case '\\': default: replace({ SYMS1, SYM }); break;
					}
					break;
				case SYMS1:
					switch (*regex_ptr) {
					case '\\': default: replace({ SYMS1, SYM }); break;
					case ']': stack.pop(); break;
					}
					break;
				case SYM:
					stack.pop();
					if (*regex_ptr == '\\') {
						++regex_ptr;
					}
					++regex_ptr;
					if (*regex_ptr == '-') {
						regex_ptr += 2;
					}
					break;
				case ')':
				case ']':
					assert(m == *regex_ptr);
					stack.pop();
					++regex_ptr;
					break;
				}

				printf("%s | ", regex_ptr);
				auto s = stack;
				while (!s.empty()) {
					printf("%d ", s.top());
					s.pop();
				}
				printf("\n");
			}
			printf("-------\n");
		}
	}
};

typedef enum class TokenKind {
} TokenKind;

typedef struct Token {
	unsigned kind; /* enum TokenKind || char-code */ 
	union {
	};
} Token;

Token nextToken()
{
	// SSYM1 = [ab\d]; SSYM2 = c
	// ASTERIX = SSYM1 ? ; SSYM1 + ; SSYM1 *
	// CAT = ASTERIX1 ASTERIX2
	// OR = CAT1 | CAT2

	static std::pair<const char*, unsigned> regex2token_mapping[] = {
		{"([1-9]b|c)*", 5},
	};
	static RegexClassifier regex_classifier(
		std::cbegin(regex2token_mapping), std::cend(regex2token_mapping));

	return { 0 };
}

int main()
{
	auto token = nextToken();
	return 0;
}