#pragma once

#include <fstream>
#include <string>

class Logger {
public:
	static std::ofstream& file();
	static void log(const std::string& message, int tag = INFO);
	static void open(std::string fileName = "");
	static void close();

	static const int INFO = 0;
	static const int WARN = 1;
	static const int INPUT = 2;
	static const int ERROR = 3;

private:
	static std::string toString(int tag);
	static std::string _name;
	static std::ofstream* _file;
};

