#pragma once

template <typename T>
bool DisplayBoard(const T& board, bool is_flipped) override {
	for (int i = 0; i < BOARD_WIDTH; i++) {
		if (!is_flipped) {
			int row = BOARD_WIDTH - i - 1;
			for (int column = 0; column < BOARD_WIDTH; column++) {
				DisplaySquare(board[row * BOARD_WIDTH + column], IsLightSquare(row, column));
			}
		}
		else {
			int row = i;
			for (int column = BOARD_WIDTH - 1; column >= 0; column--) {
				DisplaySquare(board[row * BOARD_WIDTH + column], IsLightSquare(row, column));
			}
		}
	}
}

inline bool IsLightSquare(int row, int column) {
	return (row + column) % 2;
}
