#include <iostream>
#include <string>
#include <sstream>
#include <vector>

using std::string;
using std::vector;
using std::stringstream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

void print_line(const string&);

int main(int argc, char* argv[]) {

	vector<string> grid; //make ptrs
	string line;

	while (getline(cin, line)) {
		stringstream ss(line);
		while(getline(ss, line, '/')) {
			if (!line.empty())
				grid.push_back(line);
		}
	}

	for (int i = grid.size() - 1; i > 0; i--) {
		int j = 0;
		while (grid[i-1].length() > j && grid[i].length() > j && grid[i][j] == grid[i-1][j])
			j++;
		while (j > 0 && grid[i][j] != 32)
			j--;
		if (j == 0)
			continue;
		grid[i][j] = '_';
		while (--j >= 0)
			grid[i][j] = ' ';
	}

	int last = grid.size()-1;
	int branch = grid[last].find('_');
	if (branch != string::npos)
		grid[last][branch-1] = '\\';

	for (int i = grid.size() - 2; i > 0; i--) {
		int branch = grid[i].find('_');
		if (branch == string::npos)
			continue;
		if (branch-1 >= grid[i+1].length())
			grid[i][branch-1] = '\\';
		else {
			char down = grid[i+1][branch-1];
			grid[i][branch-1] = (down == '|' || down == '\\') ? '+' : '\\';
		}
		for (int j = 0; j < branch-1 && j < grid[i+1].length(); j++) {
			char down = grid[i+1][j];
			if (down == '|' || down == '\\' || down == '+')
				grid[i][j] = '|';	
		}
	}
	for (auto& s : grid)
		print_line(s);
}
