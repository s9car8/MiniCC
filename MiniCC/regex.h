#pragma once

#include <stdint.h>
#include <limits.h>
#include <utility>
#include <array>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <stack>
#include <algorithm>
#include <numeric>
#include <functional>
#include <assert.h>

namespace regex { struct State; }

namespace std {

template <> struct hash<set<regex::State*>> {
	size_t operator()(const set<regex::State*>& set) const;
};

}

namespace regex {

struct State : std::unordered_multimap<char, State*> {
	State* add_edge(char sym, State *next) {
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
	enum { EPS = '\0' };

	State *start_state;
	// NOTE(sergey): We assume that we create NFAs with a single accepting state.
	State *accepting_state;

	NFA(StateBuffer &sb) : start_state(sb.create_state()), accepting_state(sb.create_state()) { }

	NFA(State *s, State *a) : start_state(s), accepting_state(a) { }

	NFA(StateBuffer &sb, char sym)
		: start_state(sb.create_state()), accepting_state(sb.create_state())
	{
		start_state->add_edge(sym, accepting_state);
	}

	void add_transition(char sym)
	{
		start_state->add_edge(sym, accepting_state);
	}

	void add_transitions(char *syms, unsigned n)
	{
		for (unsigned i = 0; i < n; ++i)
			start_state->add_edge(syms[i], accepting_state);
	}

	NFA invert(StateBuffer &sb)
	{
		NFA invertion(sb);

		for (unsigned i = 0; i < CHAR_MAX + 1; ++i)
			invertion.add_transition(i);
		for (auto it = start_state->cbegin(); it != start_state->cend(); ++it)
			invertion.start_state->erase(it->first);

		return invertion;
	}

	NFA union_with(StateBuffer &sb, NFA &other)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		start->add_edge(EPS, start_state);
		accepting_state->add_edge(EPS, accepting);
		start->add_edge(EPS, other.start_state);
		other.accepting_state->add_edge(EPS, accepting);

		return {start, accepting};
	}

	NFA cat_with(StateBuffer &sb, NFA &other)
	{
		accepting_state->add_edge(EPS, other.start_state);
		return {start_state, other.accepting_state};
	}

	NFA optional(StateBuffer &sb)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		start->add_edge(EPS, start_state);
		accepting_state->add_edge(EPS, accepting);
		start->add_edge(EPS, accepting);

		return {start, accepting};
	}

	NFA plus(StateBuffer &sb)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		accepting_state->add_edge(EPS, start_state);
		start->add_edge(EPS, start_state);
		accepting_state->add_edge(EPS, accepting);

		return {start, accepting};
	}

	NFA star(StateBuffer &sb)
	{
		auto start = sb.create_state();
		auto accepting = sb.create_state();

		accepting_state->add_edge(EPS, start_state);
		start->add_edge(EPS, start_state);
		accepting_state->add_edge(EPS, accepting);
		start->add_edge(EPS, accepting);

		return {start, accepting};
	}
};

struct DFA {
	std::vector<std::array<short, CHAR_MAX + 1>> transition_table;

	short create_state()
	{
		short state_idx = (short)transition_table.size();
		transition_table.emplace_back();
		transition_table.back().fill(-1);
		return state_idx;
	}

	void add_transition(short origin, short target, char ch)
	{
		transition_table[origin][ch] = target;
	}
};

auto nfas2dfa(const std::vector<std::pair<NFA, unsigned>> &nfaclses)
	-> std::pair<DFA, std::unordered_map<short, unsigned>>;

}


namespace regex {

#define FOR_CH(start, stop) \
	for (int ch = (start); ch < (stop); ++ch)
#define FOR_CH_IF(start, stop, pred) \
	FOR_CH(start, stop) if (pred(ch))

static void add_transitions_for_range(NFA &nfa, int start, int stop)
{
	FOR_CH(start, stop) nfa.add_transition(ch);
}

static void add_transitions_for_escaped(NFA &nfa, int sym)
{
	switch (sym) {
	case '.': nfa.add_transition('.'); break;
	case 'd':
		FOR_CH_IF(0, CHAR_MAX + 1, isdigit) nfa.add_transition(ch);
		break;
	case 'D':
		FOR_CH_IF(0, CHAR_MAX + 1, !isdigit) nfa.add_transition(ch);
		break;
	case 's':
		FOR_CH_IF(0, CHAR_MAX + 1, isspace) nfa.add_transition(ch);
		break;
	case 'S':
		FOR_CH_IF(0, CHAR_MAX + 1, !isspace) nfa.add_transition(ch);
		break;
	case 'w':
		FOR_CH_IF(0, CHAR_MAX + 1, isalpha) nfa.add_transition(ch);
		nfa.add_transition('_');
		break;
	case 'W':
		FOR_CH_IF(0, '_', !isalpha) nfa.add_transition(ch);
		FOR_CH_IF('_' + 1, CHAR_MAX + 1, !isalpha) nfa.add_transition(ch);
		break;
	default:
		nfa.add_transition(sym);
		break;
	}
}

#undef FOR_CH_IF
#undef FOR_CH

struct RegexClassifier {
	DFA dfa;
	std::unordered_map<short, unsigned> state2cls;
	typedef std::pair<const char*, unsigned> *RegexClsIt;

	//template <typename RegexClsIt>
	RegexClassifier(RegexClsIt begin, RegexClsIt end)
	{
		compile(begin, end);
	}

	//template <typename RegexClsIt>
	void compile(RegexClsIt begin, RegexClsIt end)
	{
		static_assert(std::is_same<decltype(begin->first), const char*>::value
			&& std::is_same<decltype(begin->second), unsigned>::value);

		StateBuffer state_buffer;
		std::vector<std::pair<NFA, unsigned>> nfa4clses;

		for (auto it = begin; it != end; ++it)
		{
			const char *regex_ptr = it->first;
			unsigned cls = it->second;

			// --ALPHABET--
			// |,*,+,?,(,),[,],^,-,\,.,other
			//
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
			// SSET -> '[' '^' SYMS ']'
			// SSET -> SYM
			// SYMS -> {'\' sym | sym | sym '-' sym}*
			// SYM -> {'\' sym | sym}

			enum Nonterminal { SYM_STACK_BOTTOM = CHAR_MAX + 1, OR, CAT, ASTERIX, SSET, SYMS, SYM };
			enum Action { PANIC, SHIFT, RECOGNIZE };
			enum class Position { NORMAL, INSIDE_SSET, INSIDE_ESCAPED };

			std::stack<NFA> nfa_stack;
			std::stack<unsigned> sym_stack;
			Position position = Position::NORMAL;

			auto recognize = [&sym_stack, &nfa_stack, &state_buffer, &position]() mutable {
				unsigned m;
			handle_state0:
				m = sym_stack.top();
				switch (position) {
				case Position::INSIDE_ESCAPED: goto handle_sym;
				case Position::INSIDE_SSET: goto handle_syms;
				case Position::NORMAL:
					switch (m) {
					case CAT: sym_stack.pop(); goto handle_state1;
					case ASTERIX: sym_stack.pop(); goto handle_state3;
					case '*': sym_stack.pop(); goto handle_state4;
					case '+': sym_stack.pop(); goto handle_state5;
					case '?': sym_stack.pop(); goto handle_state6;
					case SSET: sym_stack.pop(); goto push_asterix;
					case ')': sym_stack.pop(); goto handle_state7;
					case ']': sym_stack.pop(); goto handle_state9;
					case SYM: sym_stack.pop(); goto push_sset;
					default: goto handle_sym;
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
				case '^': goto handle_state11;
				}
			handle_state11:
				m = sym_stack.top(); sym_stack.pop();
				switch (m) {
				case '[': goto handle_exclusive_sset;
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
			handle_exclusive_sset: {
				auto syms_nfa = nfa_stack.top(); nfa_stack.pop();
				nfa_stack.push(syms_nfa.invert(state_buffer));
				goto push_sset;
			}
			push_sset:
				sym_stack.push(SSET);
				return;
			handle_syms: {
				NFA nfa(state_buffer);

				while (sym_stack.top() != '[') {
					char sym = (char)sym_stack.top(); sym_stack.pop();
					if (sym_stack.top() == '-') {
						sym_stack.pop();
						add_transitions_for_range(nfa, sym_stack.top(), sym + 1);
						sym_stack.pop();
					}
					else if (sym_stack.top() == '\\') {
						sym_stack.pop();
						add_transitions_for_escaped(nfa, sym);
					}
					else {
						nfa.add_transition(sym);
					}
				}
				nfa_stack.push(nfa);
				goto push_syms;
			}
			push_syms:
				sym_stack.push(SYMS);
				return;
			handle_sym: {
				NFA nfa(state_buffer);

				char sym = (char)sym_stack.top(); sym_stack.pop();
				if (sym_stack.top() == '\\') {
					sym_stack.pop();
					add_transitions_for_escaped(nfa, sym);
				}
				else if (sym == '.')
				add_transitions_for_range(nfa, 0, CHAR_MAX + 1);
				else
				nfa.add_transition(sym);

				nfa_stack.push(nfa);
				goto push_sym;
			}
			push_sym:
				sym_stack.push(SYM);
				return;
			};

			auto action = [&position](unsigned m, char ch) {
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
					default: return PANIC;
					}
					break;
				case SYM:
					switch (ch) {
					case '*': case '+': case '?': case '(': case '[': case '\\': default: case '|': case ')': case '\0': return RECOGNIZE;
					}
					break;
				}

				switch (position) {
				case Position::INSIDE_SSET:
					switch (ch) {
					case ']': return RECOGNIZE;
					default: return SHIFT;
					}
				case Position::INSIDE_ESCAPED:
					switch (m) {
					case '\\': return SHIFT;
					default: return RECOGNIZE;
					}
				case Position::NORMAL:
					switch (m) {
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
						case '^': default: return SHIFT;
						}
						break;
					case '^':
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
						return SHIFT;
					default:
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

				switch (position) {
				case Position::NORMAL:
					switch (sym_stack.top()) {
					case  '\\': position = Position::INSIDE_ESCAPED; break;
					case '[': position = Position::INSIDE_SSET; break;
					}
					break;
				case Position::INSIDE_ESCAPED:
					if (a == RECOGNIZE) position = Position::NORMAL;
					break;
				case Position::INSIDE_SSET:
					switch (sym) {
					case ']': position = Position::NORMAL; break;
					}
					break;
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