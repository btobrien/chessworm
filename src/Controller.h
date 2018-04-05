
class Controller {
public:
private:
	void BackSpace() {
		int x, y;
		getyx(win, y, x);
		wmove(win, y, x - 1);
		wdelch(win);
	}
	bool IsDeleteChar(char input) {
		return input == KEY_BACKSPACE || input == KEY_DC || input == 127 || input == 8;
	}
	std::string GetEcho() {
		string result;
		char input = getch();
		while(input != '\n' && input != KEY_ENTER) {
			if (input == 27)
				return "";
			if (IsDeleteChar(input)) {
				if (!result.empty()) {
					result.pop_back();
					BackSpace(commandWin);
				}
			}
			else  {
				waddch(win, input);
				result += input;
			}
			wrefresh(commandWin);
			input = getch();
		}
		return result;
	}

};
