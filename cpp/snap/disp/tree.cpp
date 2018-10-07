
#include <string>
#include <iostream>

void print_line(const std::string& str) {
	bool isDone = false;
	for (auto& c : str) {
		if (isDone) {
			std::cout << c;
			continue;
		}
		switch(c) {
			case '|':
				std::cout << "\u2502";
				break;
			case '+':
				std::cout << "\u251C";
				break;
			case '\\':
				std::cout << "\u2514";
				break;
			case '_':
				std::cout << "\u2500";
				isDone = true;
				break;
			default:
				std::cout << c;
				break;
		}
	}
	std::cout << std::endl;
}
