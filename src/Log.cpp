
#include "Log.h"
using std::ofstream;
using std::string;
using std::endl;

string Logger::_name = "chess.log";
ofstream* Logger::_file = nullptr;

ofstream& Logger::file() {
	if (!_file)
		open();
	return *_file;
}

void Logger::log(const string& message, int tag) {
	if (!_file)
		open();
	*_file << ToString(tag) << " " << message << endl;
}

void Logger::open(string fileName) {
	if (!fileName.empty() && fileName != _name) {
		close();
		_name = fileName;
	}
	if (_file)
		return;
	_file = new ofstream();
	_file->open(_name);
}

void Logger::close() {
	if (!_file)
		return;
	_file->close();
	delete _file;
	_file = nullptr;
}

std::string Logger::ToString(int tag) {
	switch(tag) {
		case INFO: return "[INFO]";
		case WARN: return "[WARN]";
		case INPUT: return "[INPU]";
		case ERROR: return "[ERRO]";
	}
}
