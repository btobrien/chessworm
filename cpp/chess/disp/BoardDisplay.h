#pragma once

static const int BOARD_LENGTH = 8;

template <typename T>
bool DisplayBoard(const T& board, bool is_flipped) override {
	for (int i = 0; i < BOARD_LENGTH; i++) {
		if (!is_flipped) {
			int row = BOARD_LENGTH - i - 1;
			for (int column = 0; column < squaresPerRow; column++) {
				DisplaySquare(board[row * squaresPerRow + column], IsLightSquare(row, column));
			}
		}
		else {
			int row = i;
			for (int column = squaresPerRow - 1; column >= 0; column--) {
				DisplaySquare(board[row * squaresPerRow + column], IsLightSquare(row, column));
			}
		}
	}
}

inline bool IsLightSquare(int row, int column) {
	return (row + column) % 2;
}
