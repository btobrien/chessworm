
struct Dimensions {
	int height;
	int width;
	int x;
	int y;
}

class Window {
public:
	Window(Dimensions dim);
	~Window() {
		delwin(win);
	}
	void Reset() {
		werase(win);
		wmove(win, 0, 0);
	}
	void AddString(std::string str, int colorPair = 0);
	void AddChar(std::string chr, int colorPair = 0);
	void AddAttribute(int attr);
	void RemoveAttribute(int attr);
	static InitPair(int color0, int color1) {
		init_pair(color0, color1);
	}
	void Refresh();
	int height();
	int width();

protected:
	Win* win;
private:
	const Dimension dimension;

};

class Display {

	Display(Dimensions dim) : Window(dim), displayer(nullptr) {}
	Display(Dimensions dim, const Displayer& disp) : Window(dim), displayer(disp) {}

	bool TryDisplay() {
		if (!displayer)
			return false;
		displayer.DisplayTo(win); // exception safety?
		win.Refresh();
		return true;
	}
	
	const Displayer* GetDisplayer();

	const Displayer* TradeDisplayers(const Displayer* newDisplayer) {
		auto oldDisplayer = displayer;
		displayer = newDisplayer;
		return oldDisplayer;
	}

	void TradeDisplayers(const Display& rhs) {
		displayer = rhs.TradeDisplayers(displayer);
	}

private:
	Window win;
	const Displayer* displayer; // make shared ptr?
}
