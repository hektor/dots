#include "block.h"

static const Block blocks[] = {
  /*
   * Icon
   * Command
   * Update interval
   * Update signal
   */
  {"", "sb-date",      60, 0}, // Week, time, day
  {"", "sb-pomo",      1,  0}, // Pomodoro timer
  {"", "sb-tasks",     5,  0}, // Tasks done
  {"", "sb-anki",   5,  0},    // Reviews done
  {"", "sb-internet",  5,  0}, // Ethernet & WiFi status
  {"", "sb-battery",   60, 0}, // Battery level
  {"", "sb-portfolio", 600, 0} // Stocks
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " ";
static unsigned int delimLen = 0;
