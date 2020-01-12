#pragma once

#include <stdarg.h>
#include <stdio.h>
#include <assert.h>

static void panic(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	vprintf(fmt, args);
	va_end(args);
}

#define assert_msg(cond, msg) assert((cond) && (msg))

#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
