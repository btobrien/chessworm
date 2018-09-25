#pragma once

struct CastleFlags {
	CastleFlags();
	bool whiteCastleShort;
	bool whiteCastleLong;
	bool blackCastleShort;
	bool blackCastleLong;
};

inline CastleFlags::CastleFlags() :
	whiteCastleShort(true),
	whiteCastleLong(true),
	blackCastleShort(true),
	blackCastleLong(true) {}
