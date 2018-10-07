
#include <string>
#include <iostream>

void print_line(const std::string& str) {
	bool isDone = false;
	for (auto c : str) {
		if (c == '_')
			isDone = true;
		else if (c == '+')
			c = isDone ? c : '|';
		std::cout << c;
	}
	std::cout << std::endl;
}
