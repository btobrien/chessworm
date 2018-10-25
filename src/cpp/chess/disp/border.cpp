#include <iostream>
#include <string>

const int BOARD_WIDTH = 8;

int main(int argc, char** argv) {
	int width = (2 * BOARD_WIDTH) + 1;
	if  (argc > 1)
		width = std::stoi(argv[1]);
	std::cout << "\u250C";
	for (int col = 0; col < width; col++)
		std::cout << "\u2500";
	std::cout << "\u2510" << std::endl;
	std::string line;
	while (getline(std::cin, line)) {
		std::cout << "\u2502";
		std::cout << line;
		std::cout << "\u2502" << std::endl;
	}
	std::cout << "\u2514";
	for (int col = 0; col < width; col++)
		std::cout << "\u2500";
	std::cout << "\u2518" << std::endl;
	return 0;
}
