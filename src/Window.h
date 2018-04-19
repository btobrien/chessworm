
struct coordinate {
	int x;
	int y;
};

struct frame : dimension, coordinate {};


class Stream {
public:
	~Stream() {
		delwin(win);
	}

	void Clear() {
		werase(win);
		wmove(win, 0, 0);
	}

	void Write(std::string str, int colorPair = 0);
	void Write(char ch, int colorPair = 0);
	// overload stream operator!!!
	void EnableAttribute(int attr);
	void DisableAttribute(int attr);

	void Refresh();
	frame frame();

protected:
	Win* _win; //??
private:
	const frame _frame;
};

class Window : public Stream {
public:
	coordinate coordinate() {
		coordinate coord;
		return coord;
	}
		
	bool TryMove(int x, int y) {
		wmove(_win, y, x);
		return true;
	}

	bool TryMove(coordinate coord) {
		return TryMove(coord.x, coord.y);
	}

	bool TryBackSpace() {
		auto coord = coordinate();
		TryMove(coord.x - 1, coord.y);
		wdelch(_win);
		return true;
	}
};

class EchoWindow : public Window {
	std::string GetEcho() {
		string result;
		char input = getch();
		while(input != '\n' && input != KEY_ENTER) {
			if (input == 27)
				return "";
			if (IsDeleteChar(input) && !result.empty()) {
				TryBackSpace();
				result.pop_back();
			}
			else  {
				WriteText(input);
				result += input;
			}
			Refresh();
			input = getch();
		}
		return result;
	}

	bool IsDeleteChar(char input) {
		return input == KEY_BACKSPACE || input == KEY_DC || input == 127 || input == 8;
	}
};

class Display {
public:
	Display(Dimensions dim, const Displayer* disp = nullptr) : _win(dim), _disp(disp) {}

	bool TryDisplay() {
		if (!_disp)
			return false;
		_win.Clear();
		_disp.DisplayTo(&_win); // exception safety?
		_win.Refresh();
		return true;
	}
	
	const Displayer* GetDisplayer();
	const void SetDisplayer(Displayer* disp);

private:
	Window _win;
	const Displayer* _disp; // make shared ptr
};

template <typename T>
class Adjustable : public T {
	void TrySetFrame(frame newFrame);
};

