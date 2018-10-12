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

void print_line(const string&, bool isTop);

void Collapse(vector<string>& grid) {
	if (grid.size() <= 1)
		return;
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
			grid[i][branch-1] = (down == '|' || down == '\\' || down == '+') ? '+' : '\\';
		}
		for (int j = 0; j < branch-1 && j < grid[i+1].length(); j++) {
			char down = grid[i+1][j];
			if (down == '|' || down == '\\' || down == '+')
				grid[i][j] = '|';	
		}
	}
}

int main(int argc, char* argv[]) {

	int line = argc > 1 ? std::stoi(argv[1]) : 0;
	int depth = argc > 2 ? std::stoi(argv[2]) : 1;
	vector<string> top;
	vector<string> bottom;
	string middle;
	string str;

	while (getline(cin, str)) {
		stringstream ss(str);
		while(getline(ss, str, '/')) {
			if (str.empty())
				continue;
			if (top.size() < line)
				top.push_back(str);
			else if (top.size() == line) {
				top.push_back(str);
				middle = str;
				bottom.push_back(str);
			}
			else
				bottom.push_back(str);
		}
	}
	std::reverse(top.begin(), top.end());
	Collapse(top);
	Collapse(bottom);
	std::reverse(top.begin(), top.end());
	top.pop_back();
	bottom.erase(bottom.begin());
	for (auto& s : top)
		print_line(s, true);
	int spaces = 0;
	for (auto& c : middle) {
		if (c == 32)
			spaces++;
		if (spaces == depth)
			cout << "\033[1;34m";
		else if (spaces == depth + 1)
			cout << "\033[0m";
		cout << c;
	}
	cout << "\033[0m";
	cout << endl;
	for (auto& s : bottom)
		print_line(s, false);
}

