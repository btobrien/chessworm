#pragma once

template <typename SourceContainer, typename TargetContainer>
void append(const SourceContainer& source, TargetContainer& target) {
	for (auto x : source) {
		target.push_back(x);
	}
}

template <typename SourceContainer, typename TargetContainer>
void cpy(const SourceContainer& source, TargetContainer& target) {
	target.clear();
	append(source, target);
}





