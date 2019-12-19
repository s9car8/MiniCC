#include <limits.h>
#include <ctype.h>
#include <stdlib.h>
#include <utility>
#include <array>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <stack>
#include <iterator>
#include <algorithm>
#include <numeric>
#include <functional>
#include <assert.h>
#include <iostream>

namespace regex { struct State; }

namespace std {
template <> struct hash<set<regex::State*>> {
	size_t operator()(const set<regex::State*>& set) const;
};
}

namespace regex {


struct State : std::unordered_multimap<char, State*> {
	State* add_edge(char sym, State* next) {
		return std::unordered_multimap<char, State*>::emplace(sym, next)->second;
	}
};

struct StateBuffer : std::vector<State> {
	StateBuffer() : std::vector<State>(1000) {}
	State* create_state() {
		std::vector<State>::push_back({});
		return &std::vector<State>::back();
	}
};

struct NFA {
	State* start_state;
	// NOTE(sergey): We assume that we create NFAs with a single accepting state.
	State* accepting_state;

	NFA(StateBuffer& sb) : start_state(sb.create_state()), accepting_state(sb.create_state()) { }

	NFA(State* s, State* a) : start_state(s), accepting_state(a) { }

	NFA(StateBuffer& sb, char sym)
	{
		start_state = sb.create_state();
		accepting_state = sb.create_state();

		start_state->add_edge(sym, accepting_state);
	}

	void add_transition(char sym)
	{
		start_state->add_edge(sym, accepting_state);
	}

	void add_transitions(char* syms, unsigned n)
	{
		for (unsigned i = 0; i < n; ++i)
			start_state->add_edge(syms[i], accepting_state);
	}

	NFA union_with(StateBuffer& sb, NFA& other)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		start->add_edge('\0', start_state);
		accepting_state->add_edge('\0', accepting);
		start->add_edge('\0', other.start_state);
		other.accepting_state->add_edge('\0', accepting);

		return { start, accepting };
	}

	NFA cat_with(StateBuffer& sb, NFA& other)
	{
		accepting_state->add_edge('\0', other.start_state);
		return { start_state, other.accepting_state };
	}

	NFA optional(StateBuffer& sb)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		start->add_edge('\0', start_state);
		accepting_state->add_edge('\0', accepting);
		start->add_edge('\0', accepting);

		return { start, accepting };
	}

	NFA plus(StateBuffer& sb)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		accepting_state->add_edge('\0', start_state);
		start->add_edge('\0', start_state);
		accepting_state->add_edge('\0', accepting);

		return { start, accepting };
	}

	NFA star(StateBuffer& sb)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		accepting_state->add_edge('\0', start_state);
		start->add_edge('\0', start_state);
		accepting_state->add_edge('\0', accepting);
		start->add_edge('\0', accepting);

		return { start, accepting };
	}
};

struct DFA {
	std::vector<std::array<short, CHAR_MAX + 1>> transition_table;

	short create_state()
	{
		short state_idx = transition_table.size();
		transition_table.emplace_back();
		transition_table.back().fill(-1);
		return state_idx;
	}

	void add_transition(short origin, short target, char ch)
	{
		transition_table[origin][ch] = target;
	}
};
}

size_t std::hash<std::set<regex::State*>>::operator()(const std::set<regex::State*>& set) const
{
	std::hash<regex::State*> hash;
	return std::accumulate(std::cbegin(set), std::end(set), size_t(), [&hash](size_t acc, auto x) {
		return acc ^ hash(x);
	});
}

namespace regex {
auto nfas2dfa(const std::vector<std::pair<NFA, unsigned>> &nfaclses)
{
	std::unordered_map<std::set<State*>, short> states;
	std::stack<std::pair<std::set<State*>, short>> states_stack;
	std::map<State *, unsigned> nfa_state2cls;
	std::unordered_map<short, unsigned> dfa_state2cls;
	DFA dfa;

	auto move = [](const std::set<State*>& states, char ch) {
		std::set<State*> res;
		for (auto &s : states) {
			auto range = s->equal_range(ch);
			for (auto it = range.first; it != range.second; ++it)
				res.emplace(it->second);
		}
		return res;
	};

	auto closure = [](const std::set<State*> &ss) {
		std::stack<const State *> stack;
		std::set<State*> res{std::begin(ss), std::end(ss)};

		std::for_each(std::begin(ss), std::end(ss), [&stack](auto x) mutable { stack.push(x); });
		while (!stack.empty()) {
			auto s = stack.top();
			stack.pop();

			auto range = s->equal_range('\0');
			for (auto it = range.first; it != range.second; ++it) {
				if (res.insert(it->second).second) stack.push(it->second);
			}
		}
		return res;
	};

	{
		std::set<State*> start_states;
		for (const auto &x : nfaclses) {
			start_states.emplace(x.first.start_state);
			nfa_state2cls.emplace(x.first.accepting_state, x.second);
		}
		auto p = std::make_pair(closure(start_states), dfa.create_state());
		states.emplace(p);
		states_stack.push(p);
	}
	while (!states_stack.empty()) {
		auto t = states_stack.top();
		states_stack.pop();

		for (int ch = 1; ch <= CHAR_MAX; ++ch) {
			auto u = closure(move(t.first, ch));
			if (u.empty()) continue;
			auto u_it = states.find(u);
			if (u_it == states.end()) {
				auto p = std::make_pair(u, dfa.create_state());
				auto emplace_res = states.insert(p);
				states_stack.push(p);
				u_it = emplace_res.first;

				for (auto x : u) {
					auto state2cls_it = nfa_state2cls.find(x);
					if (state2cls_it != nfa_state2cls.end()) {
						dfa_state2cls[u_it->second] = state2cls_it->second;
						break;
					}
				}
			}
			dfa.add_transition(t.second, u_it->second, ch);
		}
	}
	return std::make_pair(dfa, dfa_state2cls);
}


struct RegexClassifier {
	static const unsigned ALPHABET_POWER = CHAR_MAX + 1;

	regex::DFA dfa;
	std::unordered_map<short, unsigned> state2cls;

	template <typename It>
	RegexClassifier(It begin, It end)
	{
		compile(begin, end);
	}

	template <typename It>
	void compile(It begin, It end)
	{
		StateBuffer state_buffer;
		std::vector<std::pair<NFA, unsigned>> nfa4clses;

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

			enum { SYM_STACK_BOTTOM = CHAR_MAX + 1, OR, CAT, ASTERIX, SSET, SYMS, SYM };
			enum { PANIC, SHIFT, RECOGNIZE };

			std::stack<NFA> nfa_stack;
			std::stack<unsigned> sym_stack;
			bool inside_brackets = false;

			auto recognize = [&sym_stack, &nfa_stack, &state_buffer, &inside_brackets]() mutable {
				unsigned m;
			handle_state0:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case CAT: goto handle_state1;
				case ASTERIX: goto handle_state3;
				case '*': goto handle_state4;
				case '+': goto handle_state5;
				case '?': goto handle_state6;
				case SSET: goto push_asterix;
				case ')': goto handle_state7;
				case ']': goto handle_state9;
				case SYM: goto push_sset;
				default:
					sym_stack.push(m);
					if (inside_brackets) {
						goto handle_syms;
					}
					else {
						goto handle_sym;
					}
				}
			handle_state1:
				m = sym_stack.top();
				switch (m) {
				case '|': sym_stack.pop(); goto handle_state2;
				default: goto push_or;
				}
			handle_state2:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case OR: goto handle_or;
				}
			handle_state3:
				m = sym_stack.top();
				switch (m) {
				case CAT: sym_stack.pop(); goto handle_cat;
				default: goto push_cat;
				}
			handle_state4:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case SSET: goto handle_asterix_star;
				}
			handle_state5:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case SSET: goto handle_asterix_plus;
				}
			handle_state6:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case SSET: goto handle_asterix_question;
				}
			handle_state7:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case OR: goto handle_state8;
				}
			handle_state8:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case '(': goto push_sset;
				}
			handle_state9:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case SYMS: goto handle_state10;
				}
			handle_state10:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case '[': goto push_sset;
				}
			handle_or: {
				auto cat_nfa = nfa_stack.top(); nfa_stack.pop();
				auto or_nfa = nfa_stack.top(); nfa_stack.pop();
				nfa_stack.push(or_nfa.union_with(state_buffer, cat_nfa));
				goto push_or;
			}
			push_or:
				sym_stack.push(OR);
				return;
			handle_cat: {
				auto asterix_nfa = nfa_stack.top(); nfa_stack.pop();
				auto cat_nfa = nfa_stack.top(); nfa_stack.pop();
				nfa_stack.push(cat_nfa.cat_with(state_buffer, asterix_nfa));
				goto push_cat;
			}
			push_cat:
				sym_stack.push(CAT);
				return;
			handle_asterix_star: {
				auto sset_nfa = nfa_stack.top(); nfa_stack.pop();
				nfa_stack.push(sset_nfa.star(state_buffer));
				goto push_asterix;
			}
			handle_asterix_plus: {
				auto sset_nfa = nfa_stack.top(); nfa_stack.pop();
				nfa_stack.push(sset_nfa.plus(state_buffer));
				goto push_asterix;
			}
			handle_asterix_question: {
				auto sset_nfa = nfa_stack.top(); nfa_stack.pop();
				nfa_stack.push(sset_nfa.optional(state_buffer));
				goto push_asterix;
			}
			push_asterix:
				sym_stack.push(ASTERIX);
				return;
			push_sset:
				sym_stack.push(SSET);
				return;
			handle_syms: {
				NFA nfa(state_buffer);
				unsigned sym = sym_stack.top();
				sym_stack.pop();

				while (sym_stack.top() != '[') {
#define FOR_CH(start, end) \
	for (int ch = (start); ch < (end); ++ch)
#define FOR_CH_IF(start, end, pred) \
	FOR_CH(start, end) if (pred(ch))
					switch (sym_stack.top()) {
					case '-':
						sym_stack.pop();
						FOR_CH(sym_stack.top(), sym + 1) nfa.add_transition(ch);
						sym = CHAR_MAX + 1;
						break;
					case '\\':
						switch (sym) {
						case '.': nfa.add_transition('.'); break;
						case 'd': FOR_CH_IF(0, CHAR_MAX + 1, isdigit) nfa.add_transition(ch); break;
						case 'D': FOR_CH_IF(0, CHAR_MAX + 1, !isdigit) nfa.add_transition(ch); break;
						case 's': FOR_CH_IF(0, CHAR_MAX + 1, isspace) nfa.add_transition(ch); break;
						case 'S': FOR_CH_IF(0, CHAR_MAX + 1, !isspace) nfa.add_transition(ch); break;
						case 'w': FOR_CH_IF(0, CHAR_MAX + 1, isalpha) nfa.add_transition(ch); nfa.add_transition('_');  break;
						case 'W': FOR_CH_IF(0, '_', !isalpha) nfa.add_transition(ch); FOR_CH_IF('_' + 1, CHAR_MAX + 1, !isalpha) nfa.add_transition(ch);  break;
						}
						sym = CHAR_MAX + 1;
						break;
					default:
						if (sym < CHAR_MAX + 1) {
							switch (sym) {
							case '.': FOR_CH(0, CHAR_MAX) nfa.add_transition(ch); break;
							default: nfa.add_transition(sym); break;
							}
						}
						sym = sym_stack.top();
					}
					sym_stack.pop();
#undef FOR_CH_IF
#undef FOR_CH
			}
			nfa_stack.push(nfa);
				goto push_syms;
			}
			push_syms:
				sym_stack.push(SYMS);
				return;
			handle_sym: {
				nfa_stack.push({state_buffer, (char)sym_stack.top()});
				sym_stack.pop();
				goto push_sym;
			}
			push_sym:
				sym_stack.push(SYM);
				return;
			};

			auto action = [&inside_brackets](unsigned m, char ch) {
				switch (m) {
				case SYM_STACK_BOTTOM:
					switch (ch) {
					case '(': case '[': case '\\': default: return SHIFT;
					}
					break;
				case OR:
					switch (ch) {
					case '|': case ')': return SHIFT;
					default: return PANIC;
					}
					break;
				case CAT:
					switch (ch) {
					case '(': case '[': case '\\': default: return SHIFT;
					case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				case ASTERIX:
					switch (ch) {
					case '(': case '[': case '\\': default: case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				case SSET:
					switch (ch) {
					case '*': case '+': case '?': return SHIFT;
					case '(': case '[': case '\\': default: case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				case SYMS:
					switch (ch) {
					case ']': return SHIFT;
					}
					break;
				case SYM:
					switch (ch) {
					case '*': case '+': case '?': case '(': case '[': case '\\': default: case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				case '|':
					switch (ch) {
					case '(': case '[': case '\\': default: return SHIFT;
					}
					break;
				case '*':
				case '+':
				case '?':
					switch (ch) {
					case '(': case '[': case '\\': default: case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				case '(':
					switch (ch) {
					case '(': case '[': case '\\': default: return SHIFT;
					}
					break;
				case '[':
					switch (ch) {
					default: return SHIFT;
					}
					break;
				case ')':
				case ']':
					switch (ch) {
					case '*': case '+': case '?': case '(': case '[': case '\\': default: case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				case '\\':
				default:
					if (inside_brackets) {
						switch (ch) {
						case ']': return RECOGNIZE;
						default: return SHIFT;
						}
					}
					else {
						return RECOGNIZE;
					}
				}
			};

			sym_stack.push(SYM_STACK_BOTTOM);
			while (true) {
				auto sym = *regex_ptr;
				auto m = sym_stack.top();
				auto a = action(m, sym);

				switch (a) {
				case PANIC: break;
				case SHIFT: assert(*regex_ptr); sym_stack.push(*regex_ptr++); break;
				case RECOGNIZE:
					recognize();
					if (sym_stack.top() == OR && *regex_ptr == '\0') {
						goto next_regex;
					}
				}

				switch (sym) {
				case '[': inside_brackets = true; break;
				case ']': inside_brackets = false; break;
				}
			}
		next_regex:;
			assert(nfa_stack.size() == 1);
			nfa4clses.push_back({nfa_stack.top(), (unsigned)cls});
		}

		auto res = nfas2dfa(nfa4clses);
		dfa = res.first;
		state2cls = res.second;
	}

	auto classify(const char *str) const
	{
		short state = 0;
		int cls_backup = -1;
		const char *str_backup = str;

		while (*str) {
			state = dfa.transition_table[state][*str];
			++str;

			if (state == -1)
				break;

			auto cls = state2cls.find(state);
			if (cls != state2cls.cend()) {
				cls_backup = cls->second;
				str_backup = str;
			}
		}
		return std::make_pair(cls_backup, str_backup);
	}
};

}

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
	static regex::RegexClassifier regex_classifier(
		std::cbegin(regex2token_mapping), std::cend(regex2token_mapping));

	printf("#%d\n", regex_classifier.classify("5bc7b"));
	return { 0 };
}

int main()
{
	auto token = nextToken();
	return 0;
}