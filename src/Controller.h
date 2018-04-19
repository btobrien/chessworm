class Controller {
public:
	virtual bool Execute() {
		char input = getch();
		string move;
		switch (input) {
			case 'f':
				boardDisplayer.Flip();
				break;
			case 'g':
				if (getch() = 'g')
					UndoAll(board);
				break;
			case 'G':
				RedoAll(board);
				break;
			case '/':
				if (!board.TryMove(view.GetMoveEcho()))
					view.Flash();
				break;
			case 'j':
				if (!board.TryRedo())
					view.Flash();
				break;
			case 'k':
				if (!board.TryUndo())
					view.Flash();
				break;
			case 'n':
			case 'N':
				break;
			case ':':
				if (view.GetCommandEcho() == "q")
					return false;
				break;
			default:
				view.Write("ERROR: key not recognized");
		}
		return true;
	}
private:
	TreeBoard board;
	View view;
	BoardDisplayer boardDisplayer;
	ChoiceDisplayer choiceDisplayer;
	HistoryDisplayer historyDisplayer;
	CommentDisplayer commentDisplayer;
};
