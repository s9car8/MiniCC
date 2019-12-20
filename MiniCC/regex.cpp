#include "regex.h"



size_t std::hash<std::set<regex::State*>>::operator()(const std::set<regex::State*> &set) const
{
	std::hash<regex::State*> hash;
	return std::accumulate(std::cbegin(set), std::end(set), size_t(), [&hash](size_t acc, auto x) {
		return acc ^ hash(x);
		});
}

namespace regex {

auto nfas2dfa(const std::vector<std::pair<NFA, unsigned>> &nfaclses)
	-> std::pair<DFA, std::unordered_map<short, unsigned>>
{
	std::unordered_map<std::set<State*>, short> states;
	std::stack<std::pair<std::set<State*>, short>> states_stack;
	std::map<State*, unsigned> nfa_state2cls;
	std::unordered_map<short, unsigned> dfa_state2cls;
	DFA dfa;

	auto move = [](const std::set<State*> &states, char ch) {
		std::set<State*> res;
		for (auto& s : states) {
			auto range = s->equal_range(ch);
			for (auto it = range.first; it != range.second; ++it)
				res.emplace(it->second);
		}
		return res;
	};

	auto closure = [](const std::set<State*> &ss) {
		std::stack<const State*> stack;
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
		for (const auto& x : nfaclses) {
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

}