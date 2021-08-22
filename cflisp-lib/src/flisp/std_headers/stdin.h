#pragma once
// stdin.h

// DOES NOT WORK SINCE IT'S BUILT FOR A BUFFERED INPUT SYSTEM
//
// Reads string from terminal to address pointed to by X.
// Continues till nullbyte (which is also stored)
// Returns amount of characters written.
int read_str(char *buf);
